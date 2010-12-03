module TableDeck

open Cards
open PowerRating
open GameEvent
open Combiner

type Deck() = 
    let mutable (m_Deck : 'Card list) = []
    let random = System.Random(System.DateTime.Now.Second)

    member this.Reset(rollout) = 
        let (c1, c2) = match rollout with 
        | Rollout(hand) -> match hand with
                                | Hand(c1,c2) -> (c1, c2)
                                | _ -> (Null, Null)
        | _ -> (Null, Null)

        m_Deck <- Deck.Create([c1;c2])

    static member Create(cards:Card list) =
        [ for x in [1..4] do
                for y in [2..14] do 
                    let card = match x with 
                        | 1 -> Card(Rank.fromInt y, H)
                        | 2 -> Card(Rank.fromInt y, S)
                        | 3 -> Card(Rank.fromInt y, C)
                        | 4 -> Card(Rank.fromInt y, D)
                        | _ -> Null

                    let b = cards |> List.exists (fun card ->
                        if (card.Rank().toInt() = y && card.Suit().toInt() = x) then true else false)

                    if b <> true then 
                        yield card ]
                                 
    // shuffle the deck (assign a random number to each of the 52 cards and sort accordingly)
    member this.Shuffle () = 
        m_Deck <- m_Deck 
        |> List.map (fun x -> match x with Card(rank, suit) -> (rank, suit, random.Next (1, 52)))
        |> List.sortWith (fun (x, y, i) (n, m, j) -> if i = j then 0 elif i > j then 1 else -1)
        |> List.map (fun (rank, suit, i) -> Card(rank, suit))

    member this.DrawCards (n) : 'a list = 
        let deck = m_Deck
        
        let rec Draw i deck = 
            if i > 0 then
                match deck with 
                | h::t -> h::Draw (i-1) t
                | [] -> []
            else 
                m_Deck <- deck
                []
            
        Draw n deck
        
    member this.Sort () = 
        m_Deck <- m_Deck |> List.sortWith (fun x y -> match x with Card(r,s) -> match y with Card (u,v) -> if u > r then 1 elif u < r then -1 else 0)

    (* Sorts by function f (taking as input 2 instances of Card) *)
    member this.SortBy f =
        m_Deck <- m_Deck |> List.sortWith (fun x y -> match x with Card(r,s) -> match y with Card (u,v) -> f x y)

    member this.Deck with get () = m_Deck
                        and set (deck) = m_Deck <- deck

type Table() =
    let mutable m_sTable = ""
    let mutable m_cTable = []
    //let mutable m_List = new System.Collections.Generic.Dictionary<string, PowerRating>()
    
    (*do
        let deck = Deck.Create([])

        let (test5:(string*PowerRating) list) =  
            Combiner.CombinationsOf(deck, 5)
            |> List.map(fun (x:Card list) -> (Card.PrintCards (x), PowerRating.FromHand x))
            
            
        for pow in test5 do
            m_List.Add((fst pow), (snd pow))
            //m_List.[Card.PrintCards((fst pow))] = (snd pow)

        ()
      *)  
    member this.AddToTable (cards) = 
        m_cTable <- m_cTable |> List.append cards

        for card in cards do
            m_sTable <- m_sTable + sprintf  "%s " (card.ToString())

    override this.ToString() =
        m_sTable

    member this.ToCards() = 
        m_cTable

    member this.CleanTable() =
        m_sTable <- ""
        m_cTable <- []

    member this.CalcPower(hand:Hand) = 
        (*Combiner.CombinationsOf((m_cTable |> List.append (hand.ToCards())), 5)
        |> List.map (fun x -> m_List.[Card.PrintCards(x)] (*PowerRating.FromHand x*))
        |> List.sort
        |> List.head*)
        Combiner.CombinationsOf((m_cTable |> List.append (hand.ToCards())), 5)
        |> List.map (fun x -> PowerRating.FromHand x)
        |> List.sort
        |> List.head
        
    static member CalcPower(hand:Hand, table) = 
        Combiner.CombinationsOf((table |> List.append (hand.ToCards())), 5)
        |> List.map (fun x -> PowerRating.FromHand x)
        |> List.sort
        |> List.head
        