// Learn more about F# at http://fsharp.net
open Cards
open PowerRating
open GameEvent
open PlayerManager
open TableDeck
open RolloutSim
open Game
open EquivalenceClass
open System.IO

 
(* for debugging purposes *) 
let twoPairs    = [Card.Get(14,H);Card.Get(14,S);Card.Get(13,H);Card.Get(13,S);Card.Get(10,S)]
let threeKind   = [Card.Get(14,H);Card.Get(14,S);Card.Get(14,C);Card.Get(2,H);Card.Get(4,S)]
let fourKind    = [Card.Get(14,H);Card.Get(14,S);Card.Get(14,C);Card.Get(14,D);Card.Get(4,S)]
let fullHouse   = [Card.Get(14,H);Card.Get(14,S);Card.Get(13,H);Card.Get(13,S);Card.Get(13,C)]
let pair        = [Card.Get(14,H);Card.Get(14,S);Card.Get(7,C);Card.Get(2,H);Card.Get(4,S)]
let highCard    = [Card.Get(14,H);Card.Get(7,S);Card.Get(6,H);Card.Get(13,S);Card.Get(10,S)]
let straight    = [Card.Get(14,H);Card.Get(13,S);Card.Get(12,H);Card.Get(11,S);Card.Get(10,S)]

[<EntryPoint>]
let main args = 
    if File.Exists("eqstream.dat") = true then
        let game = new Game()
        game.Setup(4, Phase2)

        for i in 1 .. 1000 do
            game.Run()

        let stat = [ for p in game.GetDealer.PlayerManager.PlayerList do
                        yield sprintf "%s %i" (p.Name.ToString()) p.Cash ]
        
        let mutable i = 0
        let mutable s = sprintf "results%i.txt" i
        
        while System.IO.File.Exists(s) do
            i <- i + 1
            s <- sprintf "results%i.txt" i
            
        System.IO.File.WriteAllLines(s, stat)
    else
        let rSim = new RolloutSim()
        rSim.RunSimulation(1000)
          
    0

