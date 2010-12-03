module RolloutSim

open TableDeck
open Cards
open GameEvent
open Dealer
open PlayerManager
open EquivalenceClass
open Combiner
open Game
open System.Data

type RolloutSim() =
    let m_eqClass = new EquivalenceClass()

    (* run for k = 2 .. 10 players on the table *)
    member this.RunSimulation(runs) = 
        let mutable k = 10
        let mutable sum = 0
        let simStart = System.DateTime.Now
        let game = new Game()
        let d = game.GetDealer

        for i in 2 .. 14 do
            for j in i .. 14 do
                
                (* do for both unsuited and suited cards *)
                for r in 1 .. 2 do
                    let rolloutHand = 
                        match r with
                        | 1 -> 
                            if i <> j then
                                Hand(Card.Get(i, S), Card.Get(j, S))
                            else
                                Hand(Card.Get(i, S), Card.Get(j, H))
                        | 2 -> Hand(Card.Get(i, S), Card.Get(j, H))
                        | _ -> Hand(Null, Null)

                    d.Rollout <- Rollout(rolloutHand)
                    
                    (* do this k times for k players *)
                    for p in 1 .. k do
                        (* add the 1 .. i .. k other players *)
                        game.Setup(p, Phase1)
                        (* add the rollout bot *)
                        d.AddRolloutBot()

                        (* v, n for statistics *)
                        let mutable (v:double) = 0.0
                        let mutable (n:double) = 0.0

                        (* and lastly, repeat this r times *)
                        for it in 1 .. runs do
                            let winner = game.Run() 

                            if winner.Rollout then
                                v <- v+1.0
                            sum <- sum + 1
                            n <- n+1.0

                        let (avg:double) = v/n

                        m_eqClass.Update(i,j,rolloutHand.GetEquivalenceClass(),p, avg)

                        d.Reset()

        (* dump the statistics to disk *)
        m_eqClass.WriteToFile()
        printfn "*** %i simulations done in %i seconds ***" sum (System.DateTime.Now.Subtract(simStart).Seconds)
        System.Console.ReadLine()
       
        ()