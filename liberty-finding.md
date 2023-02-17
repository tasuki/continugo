# Notes on liberty finding

- steps: Number of adjustments before giving up, the more the merrier.
- shifts: Number of places to start exploring from, the more the merrier.
- stoneForce: How far does an existing stone place our potential liberty.
- scaleFactor: Scale the whole shift at each step.

Adding steps probably cheaper than adding shifts.

stoneForce: Intuition says 1.2 - x * 1.1 is good:
            Push ever slightly further than necessary.

scaleFactor: Intuition says step/steps is good:
             Move fast at first then slow down.

## 20 steps

Explore stoneForce and scaleFactor.

- steps: 20, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 1.0 : 11/16
- steps: 20, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.5 : 14/16
- steps: 20, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.4 : 14/16
- steps: 20, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.3 : 16/16
- steps: 20, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.2 : 16/16
- steps: 20, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.1 : 14/16
- steps: 20, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor step/steps : 16/16

- steps: 20, shifts  6, stoneForce   1 - x      , scaleFactor 1.0 : 13/16
- steps: 20, shifts  6, stoneForce   1 - x      , scaleFactor 0.5 : 13/16
- steps: 20, shifts  6, stoneForce   1 - x      , scaleFactor 0.4 : 13/16
- steps: 20, shifts  6, stoneForce   1 - x      , scaleFactor 0.3 : 16/16
- steps: 20, shifts  6, stoneForce   1 - x      , scaleFactor 0.2 : 13/16
- steps: 20, shifts  6, stoneForce   1 - x      , scaleFactor 0.1 : 14/16
- steps: 20, shifts  6, stoneForce   1 - x      , scaleFactor step/steps : 13/16

Inconclusive:
With the heightened stoneForce, scaleFactor 1 is too jumpy.
With the usual stoneForce, small scale factors fail to reach destination.

## 10 steps

Explore stoneForce and scaleFactor, confirm that more shifts is better.

- steps: 10, shifts  6, stoneForce   1 - x      , scaleFactor 1.0 : 13/16
- steps: 10, shifts  6, stoneForce   1 - x      , scaleFactor 0.5 : 13/16
- steps: 10, shifts  6, stoneForce   1 - x      , scaleFactor 0.3 : 14/16
- steps: 10, shifts  6, stoneForce   1 - x      , scaleFactor 0.1 :  8/16
- steps: 10, shifts  6, stoneForce   1 - x      , scaleFactor step/steps : 13/16

- steps: 10, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 1.0 : 11/16
- steps: 10, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.5 : 12/16
- steps: 10, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.3 : 14/16
- steps: 10, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor 0.1 :  9/16
- steps: 10, shifts  6, stoneForce 1.2 - x * 1.1, scaleFactor step/steps : 16/16 !!! surprisingly good !!!

- steps: 10, shifts 12, stoneForce   1 - x      , scaleFactor 1.0 : 16/16
- steps: 10, shifts 12, stoneForce   1 - x      , scaleFactor 0.5 : 16/16
- steps: 10, shifts 12, stoneForce   1 - x      , scaleFactor 0.3 : 14/16
- steps: 10, shifts 12, stoneForce   1 - x      , scaleFactor 0.1 :  8/16
- steps: 10, shifts 12, stoneForce   1 - x      , scaleFactor step/steps : 16/16

- steps: 10, shifts 12, stoneForce 1.2 - x * 1.1, scaleFactor 1.0 : 11/16
- steps: 10, shifts 12, stoneForce 1.2 - x * 1.1, scaleFactor 0.5 : 14/16
- steps: 10, shifts 12, stoneForce 1.2 - x * 1.1, scaleFactor 0.3 : 16/16
- steps: 10, shifts 12, stoneForce 1.2 - x * 1.1, scaleFactor 0.1 :  9/16
- steps: 10, shifts 12, stoneForce 1.2 - x * 1.1, scaleFactor step/steps : 16/16

Conclusions:
- With the lower amount of steps, heightened stoneForce is too jumpy,
  apart from scaleFactor step/steps, which is unusually good.
- With 12 shifts, everything is


## Explore stone forces

- steps: 10, shifts  6, stoneForce 1.0  - x * 1.1, scaleFactor step/steps :  8/16
- steps: 10, shifts  6, stoneForce 1.1  - x * 1.1, scaleFactor step/steps : 16/16
- steps: 10, shifts  6, stoneForce 1.15 - x * 1.1, scaleFactor step/steps : 14/16 !!! surprisingly bad !!!
- steps: 10, shifts  6, stoneForce 1.2  - x * 1.1, scaleFactor step/steps : 16/16
- steps: 10, shifts  6, stoneForce 1.3  - x * 1.1, scaleFactor step/steps : 14/16
- steps: 10, shifts  6, stoneForce 1.4  - x * 1.1, scaleFactor step/steps : 10/16
- steps: 10, shifts  6, stoneForce 1.5  - x * 1.1, scaleFactor step/steps : 12/16

- steps: 10, shifts  6, stoneForce 1.2  - x * 1.0, scaleFactor step/steps :  14/16
- steps: 10, shifts  6, stoneForce 1.2  - x * 1.1, scaleFactor step/steps :  16/16
- steps: 10, shifts  6, stoneForce 1.2  - x * 1.2, scaleFactor step/steps :  16/16
- steps: 10, shifts  6, stoneForce 1.2  - x * 1.3, scaleFactor step/steps :   8/16

### Recap stone forces

- steps: 10, shifts  6, stoneForce 1.0  - x * 1.0, scaleFactor step/steps : 13/16
- steps: 10, shifts  6, stoneForce 1.1  - x * 1.1, scaleFactor step/steps : 16/16
- steps: 10, shifts  6, stoneForce 1.2  - x * 1.2, scaleFactor step/steps : 16/16
- steps: 10, shifts  6, stoneForce 1.3  - x * 1.3, scaleFactor step/steps : 16/16
- steps: 10, shifts  6, stoneForce 1.4  - x * 1.4, scaleFactor step/steps : 14/16 !!! surprisingly bad !!!
- steps: 10, shifts  6, stoneForce 1.5  - x * 1.5, scaleFactor step/steps : 16/16
- steps: 10, shifts  6, stoneForce 2.0  - x * 2.0, scaleFactor step/steps : 16/16
- steps: 10, shifts  6, stoneForce 3.0  - x * 3.0, scaleFactor step/steps : 14/16

### Riff on the bad one

- steps: 10, shifts  6, stoneForce 1.4  - x * 1.4, scaleFactor step/steps : 14/16
- steps: 20, shifts  6, stoneForce 1.4  - x * 1.4, scaleFactor step/steps : 14/16 !!! surprisingly bad !!!
- steps: 10, shifts 12, stoneForce 1.4  - x * 1.4, scaleFactor step/steps : 16/16
- steps: 20, shifts 12, stoneForce 1.4  - x * 1.4, scaleFactor step/steps : 16/16 !!! 12 saves it !!!

TL;DR intuitions mostly hold?

# And then there was brute force

It is somehow fast enough? And somehow verifying the distance of all the stone pairs (lots of ^2 and sqrt) is way way way faster than using set subtraction? Some numbers: calculating "nearby stones" times 16 000 distances takes way less time than doing "nearby stones" number of set subtractions. My conclusion is that ^2 and sqrt are basically free, while constructing and diffing sets is hugely expensive.

## Profiling

Just doing some mouse-moving for 14 seconds, 4600 ms scripting total.

self time | total time | activity
      775 |        805 | Go.overlaps
      666 |       2204 | List.any
      338 |        355 | Liberties.normalize
      215 |       1875 | Play.findGroupWithoutLiberties
      151 |        238 | Go.isWithinBoard
