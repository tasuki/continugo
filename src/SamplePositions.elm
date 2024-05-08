module SamplePositions exposing (..)

import Go exposing (..)
import Sgf


empty =
    []


basicTest =
    [ createPlay Black { x = 560, y = 314 }
    , createPlay White { x = 561, y = 234 } -- up
    , createPlay White { x = 635, y = 331 } -- lower right
    , createPlay White { x = 653, y = 415 } -- lower right further
    , createPlay White { x = 505, y = 368 } -- lower left
    , createPlay White { x = 543, y = 455 } -- lower left further
    , createPass Black
    ]


twoLibertiesLowerRight =
    [ createPlay Black <| Spot 879 881
    , createPlay White <| Spot 881 793 -- up
    , createPlay White <| Spot 961 877 -- right
    , createPlay White <| Spot 879 962 -- down
    , createPlay White <| Spot 796 876 -- left
    ]


nearestPlayableTest =
    [ createPlay White <| Spot 200 200
    , createPlay White <| Spot 280 200
    ]


suicideOkWhenCapture =
    [ createPlay White <| Spot 842 74
    , createPlay White <| Spot 909 151
    , createPlay Black <| Spot 730 73
    , createPlay Black <| Spot 800 161
    , createPlay Black <| Spot 876 234
    , createPlay Black <| Spot 961 240
    ]


upperRightTesuji =
    Sgf.decode <|
        String.concat
            [ "B[oAfg];W[fhnT];B[nZpx];W[fYff];B[hFcS];W[fccO];B[ncdj];W[hFoX];"
            , "B[dJnS];W[dLpq];B[fhmv];W[clnY];B[gFnQ];W[ghpH];B[iGnS];W[jCoZ];"
            , "B[kHoa];W[pyjc];B[pylO];W[qGgH];B[qTfk];W[qWkw];B[rAlO];W[spfC];"
            , "B[syee];W[qpdp]"
            ]
