module Cards

type Suit = 
    | H | S | C | D | E of string
    with 
    override this.ToString() = 
        match this with
        | H -> "h"
        | S -> "s"
        | C -> "c"
        | D -> "d"
        | E(s) -> s
    member this.toInt() = 
        match this with
        | H -> 1
        | S -> 2
        | C -> 3
        | D -> 4
        | E(s) -> 0
    
type Rank =
    | N of int | E of string
    with 
    override this.ToString() = 
        match this with
        | N(14) -> "A"
        | N(13) -> "K"
        | N(12) -> "Q"
        | N(11) -> "J"
        | N(10) -> "T"
        | N(n) -> n.ToString()
        | E(s) -> s.ToString()
    static member fromInt (x) = 
        match x with 
        | n when n > 1 && n <= 14 -> N(n) 
        | n -> E("card value is out of bounds")
    member this.toInt () = 
        match this with
        | N(n) -> n
        | E(s) -> 0
    static member (-) (r1:Rank, r2:Rank) = 
        r1.toInt() - r2.toInt() 

type Card = 
    | Card of Rank*Suit
    | Null
    with
    override this.ToString() = match this with Card(rank, suit) -> sprintf "%s%s" (rank.ToString()) (suit.ToString())
    member this.Suit() = match this with Card(rank, suit) -> suit | Null -> Suit.E("")
    member this.Rank() = match this with Card(rank, suit) -> rank | Null -> Rank.E("")
    static member (<) (c1:Card, c2:Card) =
        match c1 with Card(r1, s1) -> match c2 with Card(r2,s2) -> if r1.toInt() < r2.toInt() then true else false
    static member (>) (c1:Card, c2:Card) = 
        match c1 with Card(r1,s1) -> match c2 with Card(r2,s2) -> if r1.toInt() > r2.toInt() then true else false
    static member (<=) (c1:Card, c2:Card) =
        match c1 with Card(r1, s1) -> match c2 with Card(r2,s2) -> if r1.toInt() <= r2.toInt() then true else false
    static member (>=) (c1:Card, c2:Card) =
        match c1 with Card(r1, s1) -> match c2 with Card(r2,s2) -> if r1.toInt() >= r2.toInt() then true else false
    static member Get(rank, suit) =
        Card(Rank.fromInt rank, suit)
    static member (-) (c1:Card, c2:Card) = 
        match c1 with Card(r1,s1) -> match c2 with Card(r2,s2) -> r1-r2
    static member PrintCards (cards:Card list) = 
        let mutable cs = ""
        for card in cards do
            cs <- cs + sprintf "%s " (card.ToString())
        cs

[<CustomEquality>]
[<NoComparison>]
type Hand = 
    | Hand of Card*Card
    | Table of Card list
    | Empty
    with
    static member Get (card1, card2) = 
        Hand(card1, card2)
    member this.ToCards() = 
        match this with
        | Hand(card1, card2) -> [card1;card2]
        | _ -> []
    override this.Equals(obj) = 
        let o = unbox<Hand>(obj)
        match o with 
        | Hand(c1,c2) ->
            match this with
            | Hand(t1,t2) ->
                if c1 = t1 && c2 = t2 then true else false
            | _ -> false
        | _ -> false
    override this.ToString() =
        match this with
        | Hand(c1,c2) -> c1.ToString() + ", " + c2.ToString()
        | Table(clist) -> "Bla, bla, not implemented yet..."
        | Empty -> "Player have no cards"
    member this.GetEquivalenceClass() =
        match this with
        | Hand(c1,c2) ->
            let s1 = c1.Suit()
            let r1 = c1.Rank()
            let s2 = c2.Suit()
            let r2 = c2.Rank()

            if s1 = s2 && abs(r1.toInt() - r2.toInt()) = 1 then
                SuitedConnectors(r1,r2)
            elif s1 = s2 then
                Suited(r1,r2)
            elif abs(r1.toInt() - r2.toInt()) = 1 then
                Connected(r1, r2)
            elif r1 = r2 then
                Pocket(r1,r2)
            elif s1 <> s2 then
                Unsuited(r1,r2)
            else
                None
        | _ -> EQ("Match not found for equivalence class")
and [<CustomEquality>] [<NoComparison>] HandRating = 
    | Suited of Rank*Rank
    | Connected of Rank*Rank
    | SuitedConnectors of Rank*Rank
    | Pocket of Rank*Rank
    | Unsuited of Rank*Rank
    | None
    | EQ of string
    with
    override this.Equals(obj) = 
        let test = unbox<HandRating>(obj)
        match test with 
        | Suited(x1,x2) -> 
            match this with
            | Suited(y1,y2) -> if x1=y1 && x2=y2 || x1=y2 && y1=x2 then true else false
            | _ -> false
        | Connected(x1,x2) ->
            match this with
            | Connected(y1,y2) -> if x1=y1 && x2=y2 || x1=y2 && y1=x2 then true else false
            | _ -> false
        | SuitedConnectors(x1,x2) ->
            match this with
            | SuitedConnectors(y1,y2) -> if x1=y1 && x2=y2 || x1=y2 && y1=x2 then true else false
            | _ -> false
        | Pocket(x1,x2) -> 
            match this with
            | Pocket(y1,y2) -> if x1=y1 && x2=y2 || x1=y2 && y1=x2 then true else false
            | _ -> false
        | Unsuited(x1,x2) -> 
            match this with
            | Unsuited(y1,y2) -> if x1=y1 && x2=y2 || x1=y2 && y1=x2 then true else false
            | _ -> false
        | _ -> false

    (* given a particular equivalence class, return a card based upon two suits of your choosing.
        suits must also be of different types. *)
    member this.FromEquivalence(s1, s2) = 
        if s1 <> s2 then
            match this with
            | Suited(r1,r2) -> Hand(Card(r1,s1), Card(r2,s1))
            | Connected(r1,r2) -> Hand(Card(r1,s1), Card(r2,s2))
            | Unsuited(r1,r2) ->  Hand(Card(r1,s1), Card(r2,s2))
            | SuitedConnectors(r1,r2) -> Hand(Card(r1,s1), Card(r2,s1))
            | Pocket(r1,r2) -> Hand(Card(r1,s1), Card(r2,s2))
            | _ -> Hand(Null,Null)
        else
            Hand(Null, Null)