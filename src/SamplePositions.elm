module SamplePositions exposing (..)

import Go exposing (..)
import Sgf


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


suicideOkWhenCapture =
    [ createStone White <| Spot 842 74
    , createStone White <| Spot 909 151
    , createStone Black <| Spot 730 73
    , createStone Black <| Spot 800 161
    , createStone Black <| Spot 876 234
    , createStone Black <| Spot 961 240
    ]


upperRightTesuji =
    Sgf.decode <|
        String.concat
            [ "B[oAfg];W[fhnT];B[nZpx];W[fYff];B[hFcS];W[fccO];B[ncdj];W[hFoX];"
            , "B[dJnS];W[dLpq];B[fhmv];W[clnY];B[gFnQ];W[ghpH];B[iGnS];W[jCoZ];"
            , "B[kHoa];W[pyjc];B[pylO];W[qGgH];B[qTfk];W[qWkw];B[rAlO];W[spfC];"
            , "B[syee];W[qpdp]"
            ]
