module EquivalenceClass

open System.IO
open Cards
open TableDeck
open GameEvent
open Combiner

type EquivalenceClass() =
    let m_Deck = new Deck()
    let mutable m_Classes = []

    // a table with a cell for each possible combination of Ranks
    //let m_eqTable = Array2D.create(Array2D.create(0) 5 10) 14 14
    // each cell has an array of 5 rows and 10 columns
    let m_eqTable = Array4D.create<double> 14 14 5 10 0.0

    member this.Size with get () = m_Classes.Length
    
    member this.Update(row, col, hand, players, (data:double)) = 
        let nHand = match hand with
        | Connected(r1,r2) -> 0
        | Suited(r1,r2) -> 1
        | Unsuited(r1,r2) -> 2
        | SuitedConnectors(r1,r2) -> 3
        | Pocket(r1,r2) -> 4
        | None -> -1
        | _ -> -1
        
        m_eqTable.[(row-2), (col-2), nHand, (players-1)] <- data
    member this.Update(row, col, hand, players, (data:double)) =
        m_eqTable.[(row-2), (col-2), hand, (players-1)] <- data

    member this.Get(i,j,k,r) =
        let nHand = match k with
        | Connected(r1,r2) -> 0
        | Suited(r1,r2) -> 1
        | Unsuited(r1,r2) -> 2
        | SuitedConnectors(r1,r2) -> 3
        | Pocket(r1,r2) -> 4
        | None -> -1
        | _ -> -1

        m_eqTable.[i-2,j-2,nHand,r-1]

    member this.Get(i,j,k,r) =
        m_eqTable.[i-2,j-2,k,r-1]

    member this.PrintToFile() = 
        let dump = [ for i in 2 .. 14 do
                        for j in 2 .. 14 do 
                            let temp = [ for n in 0 .. 4 do 
                                            yield this.Get(i,j,n,6) ]

                            yield sprintf "%f" (List.max temp) ]
                       
        let rec format str lc ac = 
            match str with
            | a::b::c::d::e::f::g::h::i::j::k::l::m::t -> 
                let dump = sprintf "%i\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s" lc a b c d e f g h i j k l m
                format t (lc+1) (ac |> List.append ([dump]))
            | [] -> ac

        let strings = format dump 2 ["\t2\t\t3\t\t4\t\t5\t\t6\t\t7\t\t8\t\t9\t\t10\t\t11\t\t12\t\t13\t\t14";]
        System.IO.File.WriteAllLines("eqdump.txt", strings |> List.rev)

    member this.ReadFromFile() = 
        try
            let reader = new BinaryReader(new FileStream("eqstream.dat", FileMode.Open))
        
            let mutable n = 0

            for i in 2 .. 14 do
                for j in 2 .. 14 do 
                    for k in 0 .. 4 do
                        for r in 1 .. 10 do
                            let (v:System.Double) = reader.ReadDouble()
                            n <- n + 1
                            this.Update(i,j,k,r,v)

            true
        with ex ->
            false
        
    member this.WriteToFile() = 
        let writer = new System.IO.BinaryWriter(new FileStream("eqstream.dat", FileMode.CreateNew))

        for i in 2 .. 14 do
            for j in 2 .. 14 do 
                for k in 0 .. 4 do
                    for r in 1 .. 10 do
                        writer.Write(this.Get(i,j,k,r))

    member this.GetCombinations() = 
        m_Deck.Reset(Normal)

        let possible = [ for pair in Combiner.CombinationsOf(m_Deck.Deck,2) do
                            yield Hand(pair.Head, (pair.Tail).Head) ]
        
        let classes = [ for hand in possible do
                            yield hand.GetEquivalenceClass()]

        for c in classes do
            if m_Classes |> List.exists (fun x -> if c = x then true else false) <> true then
                m_Classes <- m_Classes |> List.append [c]
        