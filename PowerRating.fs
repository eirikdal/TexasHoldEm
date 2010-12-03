module PowerRating

open Cards
open Combiner

[<CustomComparison>]
[<CustomEquality>]
type PowerRating = 
    | RoyalFlush    of Card list*Rank
    | StraightFlush of Card list*Rank
    | FourLike      of Card list*Rank
    | FullHouse     of Card list*Rank
    | Flush         of Card list*Rank         (* We will need to know high card, just in case *)
    | Straight      of Card list*Rank         (* We only need high card, suit is irrelevant? *)
    | ThreeLike     of Card list*Rank
    | TwoPairs      of Card list*Rank*Rank
    | Pair          of Card list*Rank 
    | HighCard      of Card list*Rank
    with
    static member FromHand (hand:'Card list) : PowerRating =
        (* Simple counting sort *)
        let CountMatches cards = 
            [ for i in 2 .. 14 do
                yield (Rank.fromInt(i), cards |> List.fold (fun ac (x:Card) -> if x.Rank().toInt() = i then ac+1 else ac) 0) ] 

        let test = CountMatches hand
        (* Takes a triplet (twos, threes, fours) and counts the number of hits *)
        let (twos, threes, fours) = 
            test
            |> List.fold (fun (twos, threes, fours) (h, i) ->
                if i = 2 then
                    (List.append [h] twos , threes, fours)
                elif i = 3 then
                    (twos, List.append [h] threes, fours)
                elif i = 4 then
                    (twos, threes, List.append [h] fours)
                else 
                    (twos, threes, fours)) ([], [], [])

        let highCard = (hand |> List.maxBy (fun (x:Card) -> x.Rank())).Rank()

        let isFlush = 
            hand |> List.forall (fun x -> if x.Suit() = (hand.Head).Suit() then true else false)
       
        let isStraight =
            let rec diff (prev:Card) (list:'Card list) = 
                match list with
                | h::t ->
                    if abs(h.Rank().toInt()-prev.Rank().toInt()) = 1 then 
                        diff h t
                    else
                        false
                | [] -> true

            hand.Tail
            |> List.sortBy (fun x -> x.Rank())
            |> diff hand.Head

        if (fours.Length = 1) then
            FourLike(hand, fours.Head)
        elif (threes.Length = 1 && twos.Length = 1) then 
            FullHouse(hand, threes.Head(*, twos.Head.Rank() *))
        elif (threes.Length = 1 && twos.Length = 0) then
            ThreeLike(hand, threes.Head)
        elif (twos.Length = 2) then
            TwoPairs(hand, twos.[0], twos.[1])
        elif (twos.Length = 1) then
            Pair(hand, twos.Head)
        elif isFlush && isStraight && highCard.toInt() = 14 then
            RoyalFlush(hand, highCard)
        elif isFlush && isStraight then
            StraightFlush(hand, highCard)
        elif isFlush then
            Flush(hand, (hand |> List.maxBy (fun x -> x.Rank())).Rank())
        elif isStraight then
            Straight(hand, highCard)
        else
            let highCard = fst (CountMatches hand |> List.maxBy (fun (h, i) -> i))

            HighCard(hand, highCard)

    (* Returns relative power rating + value of high card in case of ties *)
    static member PowerOf (h1) =
        match h1 with 
        | RoyalFlush(c, r)      -> (10,[r], c)
        | StraightFlush(c, r)   -> (9, [r], c)
        | FourLike(c, r)        -> (8, [r], c)
        | FullHouse(c, r)       -> (7, [r], c)
        | Flush(c, r)           -> (6, [r], c)
        | Straight(c, r)        -> (5, [r], c)   
        | ThreeLike(c, r)       -> (4, [r], c)
        | TwoPairs(c, r1, r2)   -> (3, [r1;r2], c)
        | Pair(c, r)            -> (2, [r], c)
        | HighCard(c, r)        -> (1, [r], c)

    override this.Equals(h) = 
        let hand = unbox<PowerRating> (h)
        hand.Equals(this)

    override this.ToString() =
        let str, c =
            match this with 
            | RoyalFlush(c, r)      -> "Royal Flush", c
            | StraightFlush(c, r)   -> "Straight Flush", c
            | FourLike(c, r)        -> "Four Like", c
            | FullHouse(c, r)       -> "Full House", c
            | Flush(c, r)           -> "Flush", c
            | Straight(c, r)        -> "Straight", c
            | ThreeLike(c, r)       -> "Three Like", c
            | TwoPairs(c, r1, r2)   -> "Two Pairs", c
            | Pair(c, r)            -> "Pair", c
            | HighCard(c, r)        -> "High Card", c

        sprintf "%s (%s)" str (Card.PrintCards(c))

    static member (<) (t1:PowerRating, t2:PowerRating) =
        let (p1, r1, c1) = PowerRating.PowerOf (t1)
        let (p2, r2, c2) = PowerRating.PowerOf (t2)

        if p1 > p2 then false
        elif p1 < p2 then true
        else
            if (List.max r1).toInt() > (List.max r2).toInt() then false
            elif (List.max r1).toInt() < (List.max r2).toInt() then true
            else (* ok ok.. sigh *)
                let sortedC1 = 
                    c1 
                    |> List.sortBy (fun x -> x.Rank())
                    |> List.rev

                let sortedC2 = 
                    c2 
                    |> List.sortBy (fun y -> y.Rank())
                    |> List.rev

                let rec findHighest list1 list2 =
                    match list1 with
                    | (x:Card)::t1 ->
                        match list2 with
                        | (y:Card)::t2 ->
                            if x.Rank().toInt() > y.Rank().toInt() then false
                            elif x.Rank().toInt() < y.Rank().toInt() then true
                            else
                                findHighest t1 t2
                        | [] -> false
                    | [] -> false
                
                findHighest sortedC1 sortedC2
    static member (>) (t1:PowerRating, t2:PowerRating) = 
        if PowerRating.op_LessThan(t1,t2) = true then false else true

    interface System.IComparable with
        override this.CompareTo (h) =
            let hand = unbox<PowerRating> (h)

            let (p1, r1, c1) = PowerRating.PowerOf (this)
            let (p2, r2, c2) = PowerRating.PowerOf (hand)

            if p1 > p2 then -1
            elif p1 < p2 then 1
            else
                if (List.max r1).toInt() > (List.max r2).toInt() then -1
                elif (List.max r1).toInt() < (List.max r2).toInt() then 1
                else 
                    let sortedC1 =
                        c1 
                        |> List.sortBy (fun x -> x.Rank())
                        |> List.rev 
                    let sortedC2 =
                        c2
                        |> List.sortBy (fun y -> y.Rank())
                        |> List.rev

                    let rec findHighest list1 list2 =
                        match list1 with
                        | (x:Card)::t1 ->
                            match list2 with
                            | (y:Card)::t2 ->
                                if x.Rank().toInt() > y.Rank().toInt() then -1
                                elif x.Rank().toInt() < y.Rank().toInt() then 1
                                else
                                    findHighest t1 t2
                            | [] -> 0
                        | [] -> 0
                
                    findHighest sortedC1 sortedC2