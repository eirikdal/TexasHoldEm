module HandStrength

open System
open Combiner
open Cards
open PowerRating
open TableDeck

type STR =
    | VeryStrong = 5
    | Strong = 4 
    | Medium = 3
    | Weak = 2
    | VeryWeak = 1
    | Unknown = 1000

type HandStrength () = 
    (* calculates relative handstrength compared to 
        the community cards plus all other possible combination of holecards *)
    static member CalcStrengthOf (hand, table, (deck:'Card list), (nPlayers:int)) =
        let comb = Combiner.CombinationsOf(deck, 2)
        let you = Table.CalcPower (hand, table)
        let opp = comb |> List.map (fun c -> Table.CalcPower(Hand(c.Head, c.Tail.Head), table))
        // for each possible combination of hole cards do...
        let (w, t, l) =
            opp |> List.fold (fun (w,t,l) opp ->
                if PowerRating.op_GreaterThan(opp, you) then
                    (w,t,l+1.0) // loss <- loss + 1
                elif PowerRating.op_LessThan(opp, you) then
                    (w+1.0,t,l) // win <- win + 1
                else
                    (w,t+1.0,l)) (0.0,0.0,0.0)

        let n = Convert.ToDouble(nPlayers)

        let f = ((w+t/2.0)/(w+t+l))
        
        Math.Pow(f, (float)nPlayers)

    static member EvalStrength(s) =
        match s with 
        | f when f >= 0.0 && f < 0.1 -> STR.VeryWeak
        | f when f >= 0.1 && f < 0.2 -> STR.Weak
        | f when f >= 0.2 && f < 0.4 -> STR.Medium
        | f when f >= 0.4 && f < 0.6 -> STR.Strong
        | f when f >= 0.6 -> STR.VeryStrong
        | _ -> STR.Unknown