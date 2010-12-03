module GameEvent

open Cards

type GameEvent =
    | SystemCall 
    | Betting of int*int*Type*Card list*int*int*int*int*History list (* level (to call) * pot * stage * table * numberOfPlayers? * live players * followers * raises *)
    | Check of double*Context
    | Bet of int*double*Context
    | Raise of int*double*Context
    | Fold
    | Folded
    | Call of int*double*Context
    | Blind of Size
    | Hole of Hand
    | OK
    | Error of string
    | BettingFin
    | Winner
    | Show
    | Rollout of Hand
    | Normal
and
    Size =
    | Small of int
    | Big of int
and
    Type =
    | PreFlop of History list
    | Flop of History list
    | Turn of History list
    | River of History list
    | ShowDown of History list
    | NonExistantStage
and History = 
    | History of Type*int*GameEvent*Card list
    | H_Event of HistoryStage*string*HistoryEvent*Card list*double*Context
    | Nil
and HistoryEvent = 
    | H_Call
    | H_Raise
    | H_Bet
    | H_Check
    | H_Ignore
    | H_Fold
and HistoryStage =
    | H_PreFlop
    | H_Flop
    | H_Turn
    | H_River
    | H_Error
    with
    static member FromType (gameType) = 
        match gameType with
        | PreFlop(_)    -> H_PreFlop
        | Flop(_)       -> H_Flop
        | Turn(_)       -> H_Turn
        | River(_)      -> H_River
        | _             -> printf("Error in HistoryStage"); H_Error
and Context = 
    | C1 = 1
    | C2 = 2
    | C3 = 3
    | C4 = 4
    | None = 5

type Evaluation = 
    | Phase1
    | Phase2
    | Phase3

type POT = 
    | High = 4
    | Average = 3
    | Low = 2
    | PotError = 1

type FINDLER = 
    | High = 3
    | Average = 2
    | Small = 1
    | Abysmal = 0

type BETLVL = 
    | High = 3
    | Med = 2
    | Low = 1