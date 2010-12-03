module Game

open Dealer
open Cards
open GameEvent
open PlayerManager

type Game() =
    let d = new Dealer ()

    member this.GetDealer with get () = d

    member this.Setup(p, phase) = 
        match phase with
        | Phase3 -> 
            for n in 1 .. (p/2) do
                let name = sprintf "Bot %i" n
                d.AddPlayer (5000, name, Phase2)
            for n in (p/2 + 1) .. p do
                let name = sprintf "Bot %i" n
                d.AddPlayer (5000, name, Phase3)
        | phase -> 
            for n in 1 .. p do
                let name = sprintf "Bot %i" n
                d.AddPlayer (5000, name, phase)

    member this.Run() =
        d.ShuffleDeck()
        d.DealHoleCards()
        (* start pre-flop bidding *)
        let history = d.Begin(true, PreFlop([]))

        let live = d.Live(history)

        if d.Live(history) < 2 then
            d.Showdown(history, live)
        else
            let rec loopRound t =
                let mutable r = d.Deal(t)
                let ht = d.Begin(false, t)

                let live = d.Live(ht)
                if live < 2 then
                    r <- ShowDown(ht)
        
                (* a little late-night-hack to fix the bug with the dealer dealing out turn and river cards
                    even though there is only 1 player left.. *)
                match r with
                | ShowDown(h) -> ShowDown(h |> List.append(ht))
                | Flop(h) -> 
                    if d.Live(h) > 1 then 
                        loopRound (Flop(h |> List.append(ht)))
                    else
                        ShowDown(h |> List.append(ht))
                | Turn(h) -> 
                    if d.Live(h) > 1 then 
                        loopRound (Turn(h |> List.append(ht)))
                    else
                        ShowDown(h |> List.append(ht))
                | River(h) -> 
                    if d.Live(h) > 1 then 
                        loopRound (River(h |> List.append(ht)))
                    else
                        ShowDown(h |> List.append(ht))
                | _ -> loopRound r

            // start dealing the flop, the rest will take care of itself
            match loopRound (Flop(history)) with
            | ShowDown(his) ->
                let live = d.Live(his)
                d.Showdown(his, live)