//! Opt-in, disjoint timings inside move application. Ordinary `play` calls do
//! not read clocks or accumulate counters.

#[derive(Debug, Default)]
pub struct PlayProfile {
    pub calls: u64,
    pub group_builds: u64,
    pub liberty_queries: u64,
    pub total_seconds: f64,
    pub geometry_seconds: f64,
    pub cloning_seconds: f64,
    pub groups_seconds: f64,
    pub liberties_seconds: f64,
    pub removal_seconds: f64,
}

impl PlayProfile {
    pub fn summary(&self) -> String {
        let mut lines = vec![format!(
            "{} placements, {} group builds, {} liberty queries, {:.6}s total",
            self.calls, self.group_builds, self.liberty_queries, self.total_seconds
        )];
        for (name, seconds) in [
            ("geometry", self.geometry_seconds),
            ("position cloning", self.cloning_seconds),
            ("group construction", self.groups_seconds),
            ("liberty checks", self.liberties_seconds),
            ("stone removal", self.removal_seconds),
        ] {
            let percentage = if self.total_seconds > 0.0 {
                100.0 * seconds / self.total_seconds
            } else {
                0.0
            };
            lines.push(format!("  {name}: {seconds:.6}s ({percentage:.2}%)"));
        }
        lines.join("\n")
    }
}
