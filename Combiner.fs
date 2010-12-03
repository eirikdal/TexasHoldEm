module Combiner

(* a little helper class for doing combinatorics *)
type Combiner() =
    static member CombinationsOf(list, n) = 
        let rec choices = function
            | []      -> []
            | p::tail -> (p,tail) :: [ for (y,l) in choices tail -> (y,l) ]

        let rec combinations s k =
            [ if k=0 then yield [] else
                    for (e,r) in choices s do
                        for o in combinations r (k-1) do yield e::o  ]

        combinations list n