module Model exposing (..)

import Random


type Msg
    = EndRound
    | GiveAttention BuildingType BuildingAttention
    | SaveRandomInt Int


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
    , randomInt : Int
    }


noCmd =
    Cmd.batch []


initialAttention =
    0


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { round = 1
      , p1 = initP1
      , p2 = initP2
      , attention = initialAttention
      , maxAttention = 3
      , maxGuards = 3
      , title = "Monuments is what's going to remain of us"
      , p1PeopleToChange = 0
      , p2PeopleToChange = 0
      , gameState = GameLevel
      , randomInt = 0
      }
    , Cmd.batch [ rollCmd ]
    )


initP1 =
    Faction ThoseWhoLove
        [ NoBuilding
        , Building (MonumentOfUs 1) NoAttention High
        , Building PsycheDancers NoAttention Low
        ]
        (List.repeat 5 <| Person Love)


initP2 =
    Faction ThoseWhoPoison
        [ Building ChildrenOfNihil NoAttention Low
        , Building (MonumentOfThem 1) NoAttention High
        , Building SoulEngineers NoAttention Low
        ]
        (List.repeat 5 <| Person Poison)
