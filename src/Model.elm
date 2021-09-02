module Model exposing (..)

import Random


type Msg
    = EndRound
    | EndPhase
    | GiveAttention BuildingType BuildingAttention
    | SaveRandomInt Int
    | Build BuildingType FactionName Index
    | ChangeGameState GameState
    | StartGameOver
    | Noop


type alias Index =
    Int


rollCmd : Cmd Msg
rollCmd =
    Random.generate SaveRandomInt (Random.int 0 1)


type alias Guards =
    Int


type
    BuildingType
    -- US
    = PsycheDancers
    | ThirdEyeCleansers
    | MonumentOfUs Guards
      -- THEM
    | ChildrenOfNihil
    | SoulEngineers
    | MonumentOfThem Guards


type BuildingAttention
    = Anima
    | Animus
    | AnimaAnimus
    | NoAttention


type BuildingLevel
    = Low
    | Mid
    | High
    | Destroyed


type Building
    = Building BuildingType BuildingAttention BuildingLevel
    | NoBuilding


type alias Buildings =
    List Building


type FactionName
    = ThoseWhoLove
    | ThoseWhoPoison


type alias People =
    List Person


type Person
    = Person Affiliation


type Affiliation
    = Love
    | Neutral
    | Poison


type Faction
    = Faction FactionName Buildings People


type alias Round =
    Int


type alias GameTitle =
    String


type alias Attention =
    Int


type alias PeopleCount =
    Int


type GameState
    = GameStart
    | GameLevel
    | GameWon
    | GameLost


type Phase
    = BuildingPhase
    | AttentionPhase
    | ResolutionPhase
    | OpponentPhase


type alias Model =
    { round : Round
    , p1 : Faction
    , p2 : Faction
    , attention : Attention
    , maxAttention : Attention
    , maxGuards : Guards
    , title : GameTitle
    , p1PeopleToChange : PeopleCount
    , p2PeopleToChange : PeopleCount
    , gameState : GameState
    , phase : Phase
    , randomInt : Int
    , log : String
    }


noCmd =
    Cmd.batch []


initialAttention : number
initialAttention =
    -- initialAttention always starts with 0
    -- meaning: player hasn't used any attention yet
    0


initialMaxAttention : number
initialMaxAttention =
    convertMonumentLevelToMaxAttention initP1MonumentLevel


initialData =
    { round = 1
    , p1 = initP1
    , p2 = initP2
    , maxAttention = initialMaxAttention

    -- , p1MaxAttention = (convertMonumentLevelToMaxAttention initP1MonumentLevel)
    -- , p2MaxAttention = (convertMonumentLevelToMaxAttention initP2MonumentLevel)
    -- BELOW: current player (no API data)
    , title = "Monuments is what's going to remain of us"
    , gameState = GameLevel
    , phase = BuildingPhase
    , attention = initialAttention
    , maxGuards = 3
    , randomInt = 0
    , p1PeopleToChange = 0
    , p2PeopleToChange = 0
    , log = ""
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( initialData
    , initialCmds
    )


initialCmds =
    Cmd.batch [ rollCmd ]


initP1MonumentLevel =
    Low


initP2MonumentLevel =
    Low


convertMonumentLevelToMaxAttention bLevel =
    case bLevel of
        Low ->
            1

        Mid ->
            2

        High ->
            3

        Destroyed ->
            0


initP1Buldings =
    -- [ Building ThirdEyeCleansers NoAttention Low
    --       , Building (MonumentOfUs 1) NoAttention initP1MonumentLevel
    --       , Building PsycheDancers NoAttention Low
    --       ]
    [ NoBuilding
    , Building (MonumentOfUs 1) NoAttention initP1MonumentLevel
    , NoBuilding
    ]


initP1People =
    List.repeat 5 <| Person Love


initP1 =
    Faction ThoseWhoLove initP1Buldings initP1People



-- {
--   "faction": {
--     "name": "ThoseWhoLove",
--     "buildings": [
--       {
--         "name": "MonumentOfUs 1",
--         "attention": "NoAttention",
--         "level": "Low"
--       }
--     ],
--     "people": [
--       "Love", "Love", "Love", "Poison", "Poison"
--     ],
--   }
-- }


initP2 =
    Faction ThoseWhoPoison
        [ Building ChildrenOfNihil NoAttention Low
        , Building (MonumentOfThem 0) NoAttention Low
        , Building SoulEngineers NoAttention Low
        ]
        (List.repeat 5 <| Person Poison)


dummyBuilding : Building
dummyBuilding =
    NoBuilding
