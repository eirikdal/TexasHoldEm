module OpponentModeller

open GameEvent

type OpponentModel (player:string) = 
    let m_Name = player
    let mutable m_Events = []

    member this.Name with get() = m_Name

    member this.AddEvent(event, name) = 
        m_Events <- m_Events |> List.append([event])

    member this.Events with get () = m_Events

type OpponentModeller () =
    let mutable (m_Opp:OpponentModel list) = []

    member this.AddOpponent(player:string) =
        m_Opp <- m_Opp |> List.append([new OpponentModel(player)])

    member this.AddEvent(event, name) = 
        let rec findPlayer (list:OpponentModel list) =
            match list with
            | h::t -> if h.Name = name then h.AddEvent(event, name) else findPlayer t
            | [] -> ()

        findPlayer m_Opp

    member this.GetPlayerHistory(name) = 
        let rec findPlayer (list:OpponentModel list) =
            match list with
            | h::t -> if h.Name = name then [h] else findPlayer t
            | [] -> []
             
        (findPlayer m_Opp).Head

    member this.UpdateFromHistory(history, player) = 
        let rec update list = 
            match list with
            | h::t -> 
                match h with
                | H_Event(_,_,H_Ignore,_,_,_) -> update t
                | H_Event(H_PreFlop,_,_,__,_,_) -> update t
                | H_Event(_,name,_,_,_,_) as h_event -> if name = player then this.AddEvent(h_event, name); update t else update t
                | _ -> ()
            | [] -> ()

        update history

    member this.Get(name:string, context) : History list = 
        let rec get list nlist = 
            match list with
            | h::t -> 
                match h with
                | H_Event(_,pname,_,__,_,pc) -> if pname = name && pc = context then get t (nlist |> List.append [h]) else get t nlist
                | _ -> get t nlist
            | [] -> nlist

        get (this.GetPlayerHistory(name).Events) []