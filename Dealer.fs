module Dealer

open Cards
open PowerRating
open GameEvent
open PlayerManager
open TableDeck
open EquivalenceClass
open OpponentModeller

type Dealer() = 
    // Create a new deck of 52 cards
    let mutable m_Deck = new Deck()
    let mutable m_playerManager = new PlayerManager()
    let mutable m_Blinds = (10, 20)
    let mutable m_noPlayers = 0
    let mutable m_Pot = 0 // we must remember to reset this each round
    let m_Table = new Table()
    let mutable m_Rollout = Normal
    let mutable m_RolloutPlayer = new Player(5000, "RolloutBot", new EquivalenceClass(), Phase1, new OpponentModeller())

    member this.PlayerManager with get () = m_playerManager

    (* prints game stuff according to event *)
    member this.PrintGameMsg (msg, eventCode) =
        match eventCode with
        | SystemCall -> System.Console.ForegroundColor <- System.ConsoleColor.Cyan
        | Fold ->       System.Console.ForegroundColor <- System.ConsoleColor.Red
        | Bet(n,f,v) ->     System.Console.ForegroundColor <- System.ConsoleColor.Green
        | Raise(n,f,v) ->   System.Console.ForegroundColor <- System.ConsoleColor.Green
        | Check(n,f) ->      System.Console.ForegroundColor <- System.ConsoleColor.Gray
        | Call(n,f,v) ->    System.Console.ForegroundColor <- System.ConsoleColor.Gray
        | Winner ->    System.Console.ForegroundColor <- System.ConsoleColor.Magenta
        | Show ->          System.Console.ForegroundColor <- System.ConsoleColor.DarkGray
        | _ ->          System.Console.ForegroundColor <- System.ConsoleColor.White
           
        printfn "%s" msg
        System.Console.ForegroundColor <- System.ConsoleColor.White

    member this.PrintGameMsg (msg, eventCode) =
        match eventCode with
        | Flop(_) -> System.Console.ForegroundColor <- System.ConsoleColor.Yellow
        | Turn(_) -> System.Console.ForegroundColor <- System.ConsoleColor.Yellow
        | River(_) -> System.Console.ForegroundColor <- System.ConsoleColor.Yellow
        | NonExistantStage -> System.Console.ForegroundColor <- System.ConsoleColor.Red
           
        printfn "%s" msg
        System.Console.ForegroundColor <- System.ConsoleColor.White


    (* defaults to normal *)
    member this.Rollout with get () = m_Rollout
                            and set(rollout) = m_Rollout <- rollout
       
    member this.ShuffleDeck () =
        m_Deck.Reset(m_Rollout)
        m_Deck.Shuffle()
        this.PrintGameMsg (sprintf "*** Dealer shuffles the deck", GameEvent.SystemCall)

    member this.AddPlayer (cash, name, phase) = 
        this.PrintGameMsg (sprintf "* Player added to the game.", GameEvent.SystemCall);
        m_noPlayers <- m_noPlayers+1
        m_playerManager.AddPlayer (cash, name, phase)

    member this.AddRolloutBot () = 
        this.PrintGameMsg (sprintf "* RolloutBot added to the game.", GameEvent.SystemCall);
        m_RolloutPlayer.Rollout <- true
        m_noPlayers <- m_noPlayers+1
        m_playerManager.AddPlayer(m_RolloutPlayer)

    member this.DealHoleCards () =            
        this.PrintGameMsg (sprintf "*** Dealing cards", GameEvent.SystemCall)
        
        (* collect blinds first and set dealer*)
        let small = m_playerManager.GetPlayer()
        match m_playerManager.Next(small, Blind(Small(10))) with
            | Blind(Small(n)) -> 
                m_Pot <- m_Pot + n
                this.PrintGameMsg (sprintf "* Player %s posts small blind (%i)" (small.Name.ToString()) n, GameEvent.Blind(Small(n)))
            | _ -> printfn "Player does something unexpected..."

        (* pay big blinds *)
        let big = m_playerManager.GetPlayer()
        match m_playerManager.Next(big, Blind(Big(20))) with
            | Blind(Big(n)) -> 
                m_Pot <- m_Pot + n;
                this.PrintGameMsg (sprintf "* Player %s posts big blind (%i)" (big.Name.ToString()) n, GameEvent.Blind(Big(n)))
            | _ -> printfn "Player does something unexpected..."

        for i in 1 .. m_noPlayers do
            let player = m_playerManager.GetPlayer()
            
            (* if player is rollout bot, we have to deal different cards *)
            if player.Rollout then
                let hand = match m_Rollout with
                | Rollout(hand) -> hand
                | _ -> Empty

                let response = m_playerManager.Next (player, Hole(hand))
                this.PrintGameMsg (sprintf "* RolloutBot was given: %s" (player.Hand.ToString ()), GameEvent.SystemCall)
            else
                let hand = m_Deck.DrawCards (2)
                let response = m_playerManager.Next (player, Hole(Hand(hand.Head, (hand.Tail).Head)))
                this.PrintGameMsg (sprintf "* Player %s was dealt: %s" (player.Name.ToString()) (player.Hand.ToString ()), GameEvent.SystemCall)

    member this.Deal (what) = 
        match what with 
        | Flop(history) ->
            this.PrintGameMsg (sprintf "*** Dealing flop", Flop(history));
            m_playerManager.ResetPot()
            let cards = m_Deck.DrawCards(3);
            m_Table.AddToTable(cards)
            this.PrintGameMsg(m_Table.ToString(), Flop(history))
            Turn(history)
        | Turn(history) ->
            this.PrintGameMsg (sprintf "*** Dealing turn", Turn(history))
            m_playerManager.ResetPot()
            let cards = m_Deck.DrawCards(1); 
            m_Table.AddToTable(cards)
            this.PrintGameMsg(m_Table.ToString(), Turn(history))
            River(history)
        | River(history) ->
            this.PrintGameMsg (sprintf "*** Dealing river", River(history))
            m_playerManager.ResetPot()
            let cards = m_Deck.DrawCards(1);
            m_Table.AddToTable(cards)
            this.PrintGameMsg(m_Table.ToString(), River(history))
            ShowDown(history)
        | _ -> NonExistantStage
    
    member this.Tell(player:Player, stakes, stage, live, followers, raises, his) =
            match m_playerManager.Next(player, Betting(stakes, m_Pot, stage, m_Table.ToCards(), m_playerManager.NumberOfPlayers, live, followers, raises, his)) with
            | Call(n,f,v) -> 
                m_Pot <- m_Pot + n
                this.PrintGameMsg (sprintf "* Player %s calls (%i)" (player.Name.ToString()) n, GameEvent.Call(n,f,v))
                (stakes, H_Event(HistoryStage.FromType(stage), player.Name.ToString(), H_Call, m_Table.ToCards(),f,v))
            | Bet(n,f,v) -> 
                m_Pot <- m_Pot + n
                this.PrintGameMsg (sprintf "* Player %s bets %i" (player.Name.ToString()) n, GameEvent.Bet(n,f,v))
                (n, H_Event(HistoryStage.FromType(stage), player.Name.ToString(), H_Bet, m_Table.ToCards(),f,v))
            | Raise(n,f,v) -> 
                m_Pot <- m_Pot + n
                this.PrintGameMsg (sprintf "* Player %s raises to %i" (player.Name.ToString()) n, GameEvent.Raise(n,f,v))
                (n, H_Event(HistoryStage.FromType(stage), player.Name.ToString(), H_Raise, m_Table.ToCards(),f,v))
            | Fold -> 
                this.PrintGameMsg (sprintf "* Player %s folds" (player.Name.ToString()), GameEvent.Fold)
                player.Folded <- true
                (stakes, H_Event(HistoryStage.FromType(stage), player.Name.ToString(), H_Fold,[],0.0,Context.None))
            | Check(f,v) -> 
                this.PrintGameMsg (sprintf "* Player %s checks" (player.Name.ToString()), GameEvent.Check(f,v))
                (stakes, H_Event(HistoryStage.FromType(stage), player.Name.ToString(), H_Check, m_Table.ToCards(),f,v))
            | Folded -> 
                (stakes, H_Event(HistoryStage.FromType(stage), player.Name.ToString(), H_Ignore,[],0.0,Context.None)) // player has already folded at some point in time, do nothing?
            | _ -> 
                this.PrintGameMsg (sprintf "* Player %s does something unexpected." (player.Name.ToString()), GameEvent.SystemCall)
                (stakes, H_Event(HistoryStage.FromType(stage), player.Name.ToString(), H_Ignore,[],0.0,Context.None))

    member this.Begin(newRound, stage) = 
        if newRound then 
            this.PrintGameMsg (sprintf "*** Beginning new round", GameEvent.SystemCall)

        let rec BeginRound (c_stakes, h:History list, from) =
            // start betting round (players will need to match the stakes if they want to play)
            
            (* control values *)
            let stakes = c_stakes
            let history = h
            let c_raises = (history |> List.fold (fun ac x -> match x with 
                                                                    | H_Event(_,_,H_Raise,_,_,_) -> ac+1
                                                                    | H_Event(_,_,H_Bet,_,_,_) -> ac+1
            
                                                                    | _ -> ac) 0)
            
            (* if someone raises, we need to re-arrange the order of traversal *)
            let splitList tail list = 
                let t1 =
                    m_playerManager.PlayerList
                    |> List.filter (fun x -> if tail |> List.exists (fun t -> if x = t then false else true) then true else false)
                tail |> List.append t1

            let LoopRound players =
                let followers = m_noPlayers - history.Length
                
                let live = this.Live()

                let temp = 
                    match stage with
                    | Flop(h) -> h
                    | Turn(h) -> h
                    | River(h) -> h
                    | _ -> []

                let rec loopPlayerList plist hasplayed stakes (player:Player list) (his:History list) =       
                    match plist with
                    | p::t -> 
                        let b = player |> List.exists (fun x -> if x = p then true else false)
                        if (b <> true) then
                            match this.Tell(p, stakes, stage, live, followers, this.NumberOfRaises(his), temp |> List.append(his)) with
                            | (st, ac) ->
                                match ac with
                                | H_Event(_,_,H_Raise,_,_,_) -> 
                                    let temp = List.append (hasplayed |> List.append(t)) [p]
                                    [ac] |> List.append (loopPlayerList temp [] st [p] (his |> List.append([ac])))

                                | H_Event(_,_,H_Bet,_,_,_) -> 
                                    let temp = List.append ((List.rev hasplayed) |> List.append(t)) [p]
                                    let temp2 = List.filter (fun (x:Player) -> if x.Folded <> true then true else false) temp
                                    [ac] |> List.append (loopPlayerList temp2 [] st [p] (his |> List.append([ac])))
                                | ac -> [ac] |> List.append (loopPlayerList t (hasplayed |> List.append([p])) st player (his |> List.append([ac])))
                        else
                            []
                    | [] -> []

                loopPlayerList players [] stakes [] history

            let history = LoopRound m_playerManager.PlayerList

            if stakes > c_stakes && this.Live(history) > 1 then
                // we aren't done betting yet, begin again
                let raised = this.LastPlayerToBetRaise(history)
                   
                BeginRound(stakes, history, raised) 
            else
                history

        let history = BeginRound (snd m_Blinds, [], m_playerManager.PlayerList.Head) // start new round with BB

        (* after initial betting we will need to move people around so that next betting round starts with SB *)
        if newRound = true then
            for i in 1 .. (m_playerManager.NumberOfPlayers-2) do
                m_playerManager.Advance()

        history

    member this.NumberOfRaises(history) = 
        (history |> List.fold (fun ac x -> match x with 
                                            | H_Event(_,_,H_Raise,_,_,_) -> ac+1
                                            | H_Event(_,_,H_Bet,_,_,_) -> ac+1
                                            | _ -> ac) 0)

    member this.LastPlayerToBetRaise(history) = 
        let raised = 
            history  
            |> List.rev   
            |> List.find(fun x -> 
                            match x with 
                            | H_Event(_,_,H_Raise,_,_,_) -> true
                            | H_Event(_,_,H_Bet,_,_,_) -> true
                            | _ -> false)

        match raised with
        | H_Event(_,p,_,_,_,_) -> m_playerManager.GetPlayer(p)
    (* gets number of players still playing *)
    member this.Live(history) = 
        m_noPlayers - (history |> List.fold (fun ac x -> 
                                                match x with 
                                                | H_Event(_,_,H_Fold,_,_,_) -> ac+1
                                                | _ -> ac) 0)
    member this.Live() =
        m_noPlayers - (m_playerManager.PlayerList
                        |> List.filter (fun x -> if x.Folded = true then true else false)
                        |> List.length)

    (* cleanup on isle 6..! *)
    member this.Clean() = 
        m_playerManager.Clean()
        m_Table.CleanTable()
        m_Pot <- 0

    (* resets everything! *)
    member this.Reset() = 
        m_playerManager.Reset()
        m_Table.CleanTable()
        m_Pot <- 0
        m_noPlayers <- 0
    
    member this.Showdown(history:History list,live) =
        (* it's time to get down to business! *)
        if m_Table.ToCards().Length > 0 && live > 1 then
            let powerList = 
                m_playerManager.PlayerList 
                |> List.filter (fun x -> if x.Folded <> true then true else false)
                |> List.zip [for player in m_playerManager.PlayerList do
                                if player.Folded <> true then yield m_Table.CalcPower (player.Hand)]

            let wlist = (List.sortBy (fun (x, y) -> x) powerList)
            let winner = wlist.Head

            let winners = 
                let rec findWinners list w wlist =
                    match list with
                    | (pow,pl)::t -> if PowerRating.op_LessThan(pow,w) then wlist else findWinners t w (wlist |> List.append ([(pow,pl)]))
                    | [] -> wlist

                findWinners (List.sortBy (fun (x, y) -> x) powerList) (fst (List.sortBy (fun (x, y) -> x) powerList).Head) []

            if winners.Length > 1 then
                let mutable split = ""
                for (pow, pl) in winners do
                    split <- split + sprintf "%s " pl.Name
                    m_playerManager.Wins(m_Pot / winners.Length, pl)

                for (pow, pl) in powerList do
                    this.PrintGameMsg(sprintf "*** Player %s showing hand: %s ***" (pl.Name.ToString()) (pow.ToString()) , GameEvent.Show)
                    m_playerManager.UpdateHistory(history, pl.Name)

                this.PrintGameMsg(sprintf "*** Players %ssplits the pot (%i) ***" split m_Pot, GameEvent.Winner)
            else
                for (pow, pl) in powerList do
                    this.PrintGameMsg(sprintf "*** Player %s showing hand: %s ***" (pl.Name.ToString()) (pow.ToString()) , GameEvent.Show)
                    m_playerManager.UpdateHistory(history, (pl.Name.ToString()))

                this.PrintGameMsg(sprintf "*** Player %s wins the pot (%i) ***" ((snd winner).Name.ToString()) m_Pot, GameEvent.Winner)

                (* distribute winnings and clean up the table *)
                m_playerManager.Wins (m_Pot, snd winner)

                

            m_playerManager.Advance()
            let i = m_playerManager.PlayerList |> List.sumBy (fun x -> x.Cash)
            //this.PrintGameMsg(sprintf "*** Table has: %i cash total ***" i, GameEvent.SystemCall)
            this.Clean()

            snd winner
        else
            let powerList = 
                m_playerManager.PlayerList 
                |> List.filter (fun x -> if x.Folded <> true then true else false)

            let winner = powerList.Head

            this.PrintGameMsg(sprintf "*** Player %s wins the pot (%i) ***" ((winner).Name.ToString()) m_Pot, GameEvent.Winner)
            m_playerManager.Wins (m_Pot, winner)
            m_playerManager.Advance()
            this.Clean()

            winner