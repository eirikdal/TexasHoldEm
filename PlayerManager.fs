module PlayerManager

open GameEvent
open PowerRating
open Cards
open HandStrength
open OpponentModeller
open EquivalenceClass
open System

type Player(cash, name, eqClass, phase:Evaluation, opp) = 
    let (m_Opp : OpponentModeller) = opp
    let mutable m_Hand = Empty
    let mutable m_Cash = cash
    let mutable m_Name = name
    let mutable m_Pot = 0 // the resources the player has commited to the pot so far, i.e. != to table pot!
    let mutable m_bRollout = false
    let m_eqClass:EquivalenceClass = eqClass
    let mutable m_Folded = false
    let mutable ContextList = []
    let mutable m_Phase = phase
    
    member this.Pay(stakes) : int = 
        let n = stakes - this.Pot
        this.Pot <- this.Pot + n
        this.Cash <- this.Cash - n
        n

    member this.Bet(l, p:int, r, str, context) = 
        let c = 
            match l with
            | BETLVL.High -> 1.0
            | BETLVL.Med -> 0.75
            | BETLVL.Low -> 0.5

        let pot = Convert.ToDouble(p)

        let rec evenNumber n = 
            if (n % 10.0) <> 0.0 then evenNumber n+1.0 else n
            
        let d = max (Convert.ToInt32(c*pot)) 40 // minimum bet is 2*BB
        let b = this.Pay(d)

        if r < 1 then
            Bet(b, str, context)
        else
            Raise(d, str, context)
            // Raise(b, str, context) bugged?

    member this.Call(l, str, context) = 
        if l > this.Pot then
            let n = this.Pay(l)
            Call(n, str, context)
        else
            Check(str, context)

    member this.CheckFold(s, str, context) = 
        if s > this.Pot then
            Fold
        else
            Check(str, context)

    member this.PotOdds(stakes:int, pot:int) = 
        let c = System.Convert.ToDouble(stakes)
        let p = System.Convert.ToDouble(pot) //- System.Convert.ToDouble(this.Pot)
        let r = c/(c+p)

        match r with 
        | f when f >= 0.0 && f < 0.33 -> POT.Low
        | f when f >= 0.33 && f < 0.66 -> POT.Average
        | f when f >= 0.66&& f < 1.0 -> POT.High
        | _ -> printf("Error in PotOdds"); POT.PotError

    member this.Findler(pot:int, live:int, followers:int, stakes:int, raisecount:int) : float = 
         Convert.ToDouble(pot) / Convert.ToDouble((live * (raisecount + 1) * followers * stakes))

    member this.FromFindler(r) = 
        match r with 
        | f when f >= 0.2 -> FINDLER.High
        | f when f >= 0.1 && f < 0.2 -> FINDLER.Average
        | f when f >= 0.0 && f < 0.1 -> FINDLER.Small
        | f when f < 0.0 -> FINDLER.Abysmal

    member this.Action (action) =
        match action with
        | Blind(Small(small)) -> 
            let n = this.Pay(small)
            Blind(Small(n))
        | Blind(Big(big)) -> 
            let n = this.Pay(big)
            Blind(Big(n))
        | Betting(level, pot, stage, table, n, live, followers, raises, his) ->
            this.Evaluate(level, pot, stage, table, n, live, followers, raises, his)
        | Hole(hand) -> m_Hand <- hand; OK
        | _ -> Error("Unmatched exception")

    (* To determine the next course of action *)
    member this.Evaluate (stakes:int, pot, stage, table, nPlayers, live, followers, raises, his) =        
        if m_Folded <> true then
            let mutable s = 0.0
            let mutable c = Context.None

            if m_Phase <> Phase1 then 
                let (sc, cs) = 
                    match stage with 
                    | PreFlop(history) ->
                        match m_Hand with 
                        | Hand(c1,c2) -> 
                            let r1 = m_eqClass.Get(c1.Rank().toInt(), c2.Rank().toInt(), m_Hand.GetEquivalenceClass(), nPlayers)
                            let r2 = m_eqClass.Get(c2.Rank().toInt(), c1.Rank().toInt(), m_Hand.GetEquivalenceClass(), nPlayers)
                            (max r1 r2, Context.C1)
                        | _ -> (0.0, Context.None)
                    | Flop(history) ->
                        let testDeck = TableDeck.Deck.Create(m_Hand.ToCards() |> List.append table)
                        let f = HandStrength.CalcStrengthOf(m_Hand, table, testDeck, nPlayers)
                        (f, Context.C2)
                    | Turn(history) ->
                        let testDeck = TableDeck.Deck.Create(m_Hand.ToCards() |> List.append table)
                        let f = HandStrength.CalcStrengthOf(m_Hand, table, testDeck, nPlayers)
                        (f, Context.C3)
                    | River(history) -> 
                        let testDeck = TableDeck.Deck.Create(m_Hand.ToCards() |> List.append table)
                        let f = HandStrength.CalcStrengthOf(m_Hand, table, testDeck, nPlayers)
                        (f, Context.C4)
                    | _ -> (0.0, Context.None)
                s <- sc
                c <- cs
                
            let Determine (handStr, str, findlerN, potOdds, pot, avOpp, c) = 
                if (handStr = STR.VeryStrong && findlerN > FINDLER.Small) 
                    || (handStr > STR.Medium && findlerN >= FINDLER.High)
                    || (m_Phase = Phase3 && (str-0.2) > avOpp) then
                    (* bet aggressively *)
                    match potOdds with
                    | POT.High ->       this.Call(stakes, str, c)
                    | POT.Average ->    if raises < 3 then
                                            this.Bet(BETLVL.Med, pot, raises, str, c)
                                        else
                                            this.Call(stakes, str, c)
                    | POT.Low ->        if raises < 2 then
                                            this.Bet(BETLVL.High, pot, raises, str, c)
                                        else
                                            this.Call(stakes, str, c)
                elif handStr > STR.Medium && findlerN > FINDLER.Small 
                    || (handStr > STR.Weak && findlerN > FINDLER.High)
                    || (m_Phase = Phase3 && str > avOpp) then
                    (* bet moderately *)
                    match potOdds with
                    | POT.High ->       this.CheckFold(stakes, str, c)
                    | POT.Average ->    this.Call(stakes, str, c)
                    | POT.Low ->        if raises < 2 then
                                            this.Bet(BETLVL.Med, pot, raises, str, c)
                                        else
                                            this.Call(stakes, str, c)
                elif handStr > STR.Weak && findlerN > FINDLER.Abysmal 
                    || (findlerN > FINDLER.Average)
                    || (m_Phase = Phase3 && str < avOpp) then
                    (* bet cautiously *)
                    match potOdds with
                    | POT.High ->       this.CheckFold(stakes,  str, c)
                    | POT.Average ->    this.Call(stakes,       str, c)
                    | POT.Low ->        if raises < 1 then 
                                            this.Bet(BETLVL.Low, pot, raises, str, c)
                                        else
                                            this.Call(stakes, str, c)
                else
                    match potOdds with
                    | POT.High ->       this.CheckFold(stakes, str, c)
                    | POT.Average ->    this.CheckFold(stakes, str, c)
                    | POT.Low ->        this.CheckFold(stakes, str, c)
            
            match m_Phase with
            | Phase1 ->
                (* Phase1: Default to standard actions *)
                if this.Pot < stakes then
                    let n = this.Pay(stakes)
                    Call(n,0.0, Context.None)
                else if m_Pot = stakes then
                    Check(0.0, Context.None)
                else
                    m_Folded <- true; Fold
            | Phase2 ->
                let handStr = 
                    match stage with
                    | PreFlop(_) -> 
                        let preFlopStr = s + Convert.ToDouble(nPlayers) * 0.015
                        HandStrength.EvalStrength(preFlopStr)
                    | _ -> HandStrength.EvalStrength(s)
                let potOdds = this.PotOdds(stakes, pot)
                let findler = this.Findler(pot, live, followers, stakes, raises)
                let findlerN = this.FromFindler(findler)
                
                (* Phase2: Use hand strength and pre-flop evaluation to determine next action*)
                Determine(handStr, s, findlerN, potOdds, pot, 0.0, c)

            | Phase3 ->
                (* Phase3: Same as in phase 2 + opponent modelling *)
                let handStr = 
                    match stage with
                    | PreFlop(_) -> 
                        let preFlopStr = s + Convert.ToDouble(nPlayers) * 0.015
                        HandStrength.EvalStrength(preFlopStr)
                    | _ -> HandStrength.EvalStrength(s)
                let potOdds = this.PotOdds(stakes, pot)
                let findlerN = this.FromFindler(this.Findler(pot, live, followers, stakes, raises))
                let opp = m_Opp
                
                (*
                    TODO:   Find out which players are still in the game.
                            Find out if there are data for this situation
                *)
                let rec findLive stage current = 
                    let h = 
                        match stage with
                        | PreFlop(h) -> h
                        | Flop(h) -> h 
                        | Turn(h) -> h
                        | River(h) -> h
                        |> List.append (current)


                    let rec playersFolded hlist = 
                        match hlist with
                        | h::t ->
                             match h with
                             | H_Event(_,p,H_Fold,_,_,_) -> p::playersFolded t
                             | _ -> playersFolded t
                        | [] -> []

                    let rec playersInGame hlist nlist = 
                        match hlist with
                        | h::t ->
                            match h with 
                            | H_Event(_,p,_,_,_,_) -> if (nlist |> List.exists (fun x -> if x.ToString() = p.ToString() then true else false) <> true) then playersInGame t (nlist |> List.append ([p])) else playersInGame t nlist
                            | _ -> nlist
                        | [] -> nlist

                    let players = playersInGame h []
                    let folded = playersFolded h

                    let stillAlive (players:string list) (folded:string list) = 
                        players
                        |> List.filter (fun x -> if (folded |> List.exists (fun y -> if x = y then true else false)) <> true then true else false)

                    stillAlive players folded
                        
                (* find out which players are still playing *)
                let stillPlaying = findLive stage his
               
                let whatDidTheyDo players (history:History list) = 
                    [ for p in players do
                        for his in history do
                            match his with 
                            | H_Event(H_PreFlop,_,_,_,_,_) -> ()
                            | H_Event(_,pl,_,_,_,_) -> if pl = p then yield his else ()
                            | _ -> () ]

                let test = whatDidTheyDo stillPlaying his

                let fromContext c = 
                    match c with
                    | H_Flop -> Context.C2
                    | H_Turn -> Context.C3
                    | H_River -> Context.C4

                let av = 
                    [ for what in test do
                        match what with
                        | H_Event(stage,name,event,_,_,_) ->
                            let oppHistory = m_Opp.Get(name, (fromContext stage))
                        
                            for av in oppHistory do
                                match av with
                                | H_Event(st, nm, ev,_,str,_) as h1 -> if ev = event then yield str
                                | _ -> ()
                        | _ -> () ]

                let avOpp =
                    match av with
                    | h::t -> av |> List.average
                    | _ -> 0.0

                if avOpp > 0.0 then
                    ()

                Determine(handStr, s, findlerN, potOdds, pot, avOpp, c)
        else
            Folded

    member this.Reset () =
        this.Pot <- 0
        this.Folded <- false

    member this.Rollout with get() = m_bRollout
                            and set(rollout) = m_bRollout <- rollout
    member this.Pot with get () = m_Pot
                        and set (cash) = m_Pot <- cash
    member this.Cash with get () = m_Cash
                        and set (cash) = m_Cash <- cash
    member this.Hand with get () = m_Hand
                        and set (hand) = m_Hand <- hand
    member this.Name with get () = m_Name
    
    member this.Folded with get() = m_Folded
                        and set (b) = m_Folded <- b

type PlayerManager () = 
    let mutable m_playerList : 'Player list = []
    let m_eqClass = new EquivalenceClass()
    let m_OpponentModeller = new OpponentModeller()

    do
        m_eqClass.ReadFromFile()
        ()
    
    member this.AddPlayer (cash, (name:string), phase) =  
        m_playerList <- m_playerList |> List.append [new Player(cash,name,m_eqClass,phase, m_OpponentModeller)]
        m_OpponentModeller.AddOpponent(name)

    member this.AddPlayer (player) =  m_playerList <- m_playerList |> List.append [player]

    member this.PlayerList with get () = m_playerList

    member this.NumberOfPlayers with get() = m_playerList.Length

    member this.Reset () = 
        m_playerList <- []
    
    member this.Clean() =
        for player in m_playerList do
            player.Reset()

    member this.Wins (winnings, player:Player) = 
        player.Cash <- player.Cash + winnings

    member this.GetPlayer() = 
        m_playerList.Head

    member this.GetPlayer(name) = 
        m_playerList |> List.find(fun x -> if x.Name = name then true else false)

    member this.UpdateHistory(history) =
        m_OpponentModeller.UpdateFromHistory(history)

    member this.ResetPot() = 
        m_playerList |> List.map(fun (x:Player) -> x.Pot <- 0)

    member this.Deal (hand) =
        (*for player in m_playerList do
            player.Hand <- Hand(hand.Head, (hand.Tail).Head) *)
        m_playerList.Head.Hand <- hand
        this.Advance()

    (* Rotate to next player waiting (Round Robin style) *)
    member this.Advance() =
        m_playerList <- 
            m_playerList 
            |> List.rev
            |> List.append [m_playerList.Head]
            |> List.rev
        m_playerList <- m_playerList.Tail

    (* Game is ready to move on.. accept next event *)
    member this.Next ((player:Player), event) =
        //let player = m_playerList.Head
        
        let response = 
            match event with
            | Blind(Big(n)) -> player.Action(Blind(Big(n)))
            | Blind(Small(n)) -> player.Action(Blind(Small(n)))
            | Hole(hand) -> player.Action(Hole(hand))
            | Betting(level, pot, stage, table, n, live, followers, raises, his) -> player.Action(Betting(level, pot, stage, table, n, live, followers, raises, his))
            | _ -> Error ("Unmatched Action Event")

        (* after player has made his move, advance the list *)
        this.Advance()
        (* return the action the player chose *)
        response