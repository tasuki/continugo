module SamplePositions exposing (..)

import Go exposing (..)


empty =
    []


basicTest =
    [ createStone Black { x = 560, y = 314 }
    , createStone White { x = 561, y = 234 } -- up
    , createStone White { x = 635, y = 331 } -- lower right
    , createStone White { x = 653, y = 415 } -- lower right further
    , createStone White { x = 505, y = 368 } -- lower left
    , createStone White { x = 543, y = 455 } -- lower left further
    ]


twoLibertiesLowerRight =
    [ createStone Black <| Spot 879 881
    , createStone White <| Spot 881 793 -- up
    , createStone White <| Spot 961 877 -- right
    , createStone White <| Spot 879 962 -- down
    , createStone White <| Spot 796 876 -- left
    ]


nearestPlayableTest =
    [ createStone White <| Spot 200 200
    , createStone White <| Spot 280 200
    ]