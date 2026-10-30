//! Incremental geometric placement availability, independent of color, suicide,
//! captures, or eye policy. Each in-bounds coordinate counts the exclusion disks
//! covering it. Only zero/nonzero transitions change the dense free-coordinate list.
//! Storage is about 10 MB per index; no full-grid rebuild is needed after a move.

use continugo_rules::{Action, Board, COORD_RANGE, COORDINATE_ACTIONS, Position, Spot};

const ABSENT: u32 = u32::MAX;

pub struct GeometricAvailability {
    board: Board,
    coverage: Vec<u16>,
    slots: Vec<u32>,
    available: Vec<Action>,
    stones: Vec<Spot>,
}

impl GeometricAvailability {
    pub fn new(position: &Position) -> Self {
        let mut index = Self {
            board: position.board(),
            coverage: vec![0; COORDINATE_ACTIONS as usize],
            slots: vec![ABSENT; COORDINATE_ACTIONS as usize],
            available: Vec::with_capacity(COORDINATE_ACTIONS as usize),
            stones: Vec::new(),
        };
        for id in 0..COORDINATE_ACTIONS {
            let spot = Spot {
                x: (id % COORD_RANGE as u32) as i32,
                y: (id / COORD_RANGE as u32) as i32,
            };
            if index.board.contains(spot) {
                index.slots[id as usize] = index.available.len() as u32;
                index
                    .available
                    .push(Action::from_index(id).expect("coordinate index"));
            }
        }
        index.sync(position);
        index
    }

    /// Geometrically possible coordinates, NOT a full legality mask. Order is
    /// unspecified and may change when stones are added or removed. Pass is absent.
    pub fn coordinates(&self) -> &[Action] {
        &self.available
    }

    /// None means outside placement bounds; zero means no stone blocks this point.
    pub fn coverage(&self, spot: Spot) -> Option<u16> {
        self.board
            .contains(spot)
            .then(|| self.coverage[coordinate_id(spot)])
    }

    pub fn is_available(&self, spot: Spot) -> bool {
        self.coverage(spot) == Some(0)
    }

    /// Follow the actual resulting position, including every captured or otherwise
    /// removed stone. Passes are no-ops. Board configuration changes rebuild safely.
    pub fn sync(&mut self, position: &Position) {
        if self.board != position.board() {
            *self = Self::new(position);
            return;
        }
        if self
            .stones
            .iter()
            .copied()
            .eq(position.stones().iter().map(|stone| stone.spot))
        {
            return;
        }
        let removed: Vec<_> = self
            .stones
            .iter()
            .copied()
            .filter(|spot| !position.stones().iter().any(|stone| stone.spot == *spot))
            .collect();
        let added: Vec<_> = position
            .stones()
            .iter()
            .filter(|stone| !self.stones.contains(&stone.spot))
            .map(|stone| stone.spot)
            .collect();
        for spot in removed {
            self.update_disk(spot, false);
        }
        for spot in added {
            self.update_disk(spot, true);
        }
        self.stones.clear();
        self.stones
            .extend(position.stones().iter().map(|stone| stone.spot));
    }

    fn update_disk(&mut self, centre: Spot, adding: bool) {
        let reach = self.board.diameter() - 1;
        let radius = self.board.stone_radius();
        let left = (centre.x - reach).max(radius);
        let right = (centre.x + reach).min(COORD_RANGE - radius);
        let top = (centre.y - reach).max(radius);
        let bottom = (centre.y + reach).min(COORD_RANGE - radius);
        for y in top..=bottom {
            for x in left..=right {
                let spot = Spot { x, y };
                if !self.board.overlaps(centre, spot) {
                    continue;
                }
                let id = coordinate_id(spot);
                let count = &mut self.coverage[id];
                if adding {
                    *count = count.checked_add(1).expect("coverage overflow");
                    if *count == 1 {
                        let slot = self.slots[id] as usize;
                        self.available.swap_remove(slot);
                        self.slots[id] = ABSENT;
                        if slot < self.available.len() {
                            self.slots[self.available[slot].index() as usize] = slot as u32;
                        }
                    }
                } else {
                    *count = count
                        .checked_sub(1)
                        .expect("removing an untracked exclusion disk");
                    if *count == 0 {
                        self.slots[id] = self.available.len() as u32;
                        self.available
                            .push(Action::from_index(id as u32).expect("coordinate index"));
                    }
                }
            }
        }
    }
}

fn coordinate_id(spot: Spot) -> usize {
    (spot.y * COORD_RANGE + spot.x) as usize
}

#[cfg(test)]
mod tests {
    use super::*;
    use continugo_rules::{Color, Game, Stone, sgf};

    fn assert_matches(index: &GeometricAvailability, position: &Position) {
        let mut listed = vec![false; COORDINATE_ACTIONS as usize];
        for (slot, action) in index.coordinates().iter().enumerate() {
            let id = action.index() as usize;
            assert!(!listed[id], "duplicate coordinate");
            listed[id] = true;
            assert_eq!(index.slots[id], slot as u32);
        }
        for y in 0..COORD_RANGE {
            for x in 0..COORD_RANGE {
                let spot = Spot { x, y };
                let expected = position.can_place(spot);
                assert_eq!(index.is_available(spot), expected, "{spot:?}");
                assert_eq!(listed[coordinate_id(spot)], expected, "{spot:?}");
            }
        }
    }

    #[test]
    fn overlapping_disks_stay_blocked_until_the_last_stone_is_removed() {
        let empty = Position::default();
        let one = empty
            .play(Stone {
                color: Color::Black,
                spot: Spot { x: 200, y: 200 },
            })
            .unwrap();
        let two = one
            .play(Stone {
                color: Color::White,
                spot: Spot { x: 276, y: 200 },
            })
            .unwrap();
        let mut index = GeometricAvailability::new(&empty);
        index.sync(&one);
        index.sync(&two);
        let overlap = Spot { x: 238, y: 200 };
        assert_eq!(index.coverage(overlap), Some(2));
        assert_eq!(index.coverage(Spot { x: 352, y: 200 }), Some(0)); // Exact touching is allowed.
        assert_matches(&index, &two);
        index.sync(&one);
        assert_eq!(index.coverage(overlap), Some(1));
        assert_matches(&index, &one);
        index.sync(&empty);
        assert_eq!(index.coverage(overlap), Some(0));
        assert_matches(&index, &empty);
    }

    #[test]
    fn placements_passes_and_board_changes_match_rules_across_sizes() {
        let mut index = GeometricAvailability::new(&Position::default());
        for size in [9, 13, 19] {
            let mut game = Game::new(Board::new(size).unwrap());
            index.sync(game.position());
            let radius = game.board().stone_radius();
            for spot in [
                Spot {
                    x: radius,
                    y: radius,
                },
                Spot { x: 500, y: 500 },
                Spot { x: 800, y: 800 },
            ] {
                game = game.play(spot).unwrap();
                index.sync(game.position());
            }
            let before_pass = index.coordinates().to_vec();
            game = game.pass().unwrap();
            index.sync(game.position());
            assert_eq!(index.coordinates(), before_pass);
            assert_matches(&index, game.position());
        }
    }

    #[test]
    fn a_multi_stone_capture_reopens_all_captured_centres() {
        let fixture = include_str!("../../../fixtures/rules/capture-four.txt");
        let record = fixture
            .lines()
            .nth(1)
            .unwrap()
            .strip_prefix("record: ")
            .unwrap();
        let before = Game::from_record(&sgf::decode(record).unwrap()).unwrap();
        let after = before.play(Spot { x: 839, y: 240 }).unwrap();
        let mut index = GeometricAvailability::new(before.position());
        index.sync(after.position());
        let removed: Vec<_> = before
            .position()
            .stones()
            .iter()
            .filter(|stone| !after.position().stones().contains(stone))
            .collect();
        assert_eq!(removed.len(), 4);
        for stone in removed {
            assert_eq!(index.coverage(stone.spot), Some(0));
        }
        assert_matches(&index, after.position());
    }

    #[test]
    fn extreme_board_sizes_and_strict_overlap_boundaries() {
        let board = Board::new(1).unwrap();
        let index = GeometricAvailability::new(&Position::new(board));
        assert_eq!(index.coordinates(), [Action::from_index(500_500).unwrap()]);
        assert_eq!(index.coverage(Spot { x: 0, y: 0 }), None);
        let board = Board::new(500).unwrap();
        let position = Position::new(board)
            .play(Stone {
                color: Color::Black,
                spot: Spot { x: 500, y: 500 },
            })
            .unwrap();
        let index = GeometricAvailability::new(&position);
        assert_eq!(index.coordinates().len(), 999 * 999 - 9);
        assert_eq!(index.coverage(Spot { x: 501, y: 501 }), Some(1));
        assert_eq!(index.coverage(Spot { x: 502, y: 500 }), Some(0));
    }
}
