namespace TreeSetdotNet

module BalancedBinaryTree =
    type 'K KeyRecord =
        Datum of 'K * int
        //| NoDatum

    type 'K BalancedSearchTree =
        Tree of 'K KeyRecord * 'K BalancedSearchTree * 'K BalancedSearchTree
        | EmptyTree

    let leftEmpty t =
        match t with
        | Tree (_, EmptyTree, _) -> true
        | Tree (_, Tree _, _) -> false
        | EmptyTree -> false

    let lChild t =
        match t with
        | Tree(_,lc,_) -> lc
        // | Tree(_,(Tree(_,_,_) as lc),_) -> lc
        // | Tree(_,EmptyTree,_) -> EmptyTree
        | EmptyTree -> EmptyTree

    let rChild t =
        match t with
        | Tree(_,_,rc) -> rc
        //| Tree(_,_,(Tree(_,_,_) as rc)) -> rc
        //| Tree(_,_,EmptyTree) -> EmptyTree
        | EmptyTree -> EmptyTree

    let tHeight t =
        match t with
        | Tree(Datum(_,h),_,_) -> h
        //| Tree(NoDatum, lc, rc) -> -1 // Not supposed to happen...
        | EmptyTree -> -1

    let tDatum t =
        match t with
        | Tree(d, _, _) -> Some d
        | EmptyTree -> None

    let tKandH t = 
        match t with
        | Tree(Datum(k,h),_,_) -> Some (k,h)
        | EmptyTree -> None

    let leftRotate t =
        let rc = rChild t
        let lc = lChild t
        let lcOfRc = lChild rc
        let rcOfRc = rChild rc
        let heightOfLc = tHeight lc
        let heightOfLcOfRc = tHeight lcOfRc
        let rootData = 
            match tDatum t with
            | Some (Datum(x,_)) -> x
            | None -> None // not supposed to happen...
        let newLc =
            Tree(Datum(rootData,(max heightOfLc heightOfLcOfRc) + 1), lc, lcOfRc)
        let rcData =
            match tDatum rc with
            | Some (Datum(x,_)) -> x
            | None -> None // not supposed to happen...
        let newRc = rcOfRc
        Tree(Datum(rcData, (max (tHeight newLc) (tHeight newRc)) + 1), newLc, newRc)

(*
    let leftRotate t =
        match t with
        | Tree(Datum(rootDatum,h),EmptyTree,EmptyTree) -> t // failwith "can't lrotate!"
        | Tree(Datum(rootDatum,h),
               EmptyTree,
               Tree(Datum(rcDatum,h1),EmptyTree,EmptyTree)) -> 
          Tree(Datum(rcDatum,1),Tree(Datum(rootDatum,0),EmptyTree,EmptyTree),EmptyTree)
        | Tree(Datum(rootDatum,h),
               leftTree,
               EmptyTree) -> 
          t
        | Tree(Datum(rootData, rootHeight),
               Tree(Datum(lcDatum,heightOfLc),treeOne,treeTwo),
               Tree(Datum(rcDatum,heightOfRc),
                    Tree(Datum(lcOfRcDatum,heightOfLcOfRc),treeThreeL,treeThreeR),
                    Tree(Datum(rcOfRcDatum,heightOfRcOfRc),treeFourL,treeFourR)) ) ->
          let heightOfNewLc = (max heightOfLc heightOfLcOfRc) + 1
          let newLc =
            Tree(Datum(rootData, heightOfNewLc),
                 Tree(Datum(lcDatum,heightOfLc),treeOne,treeTwo),
                 Tree(Datum(lcOfRcDatum,heightOfLcOfRc), treeThreeL, treeThreeR))
          let heightOfNewRc = heightOfRcOfRc
          let newRc =
            Tree(Datum(rcOfRcDatum,heightOfNewRc), treeFourL, treeFourR)
          Tree(Datum(rcDatum, (max heightOfNewLc heightOfNewRc) + 1),
               newLc,
               newRc)
        | EmptyTree -> t
*)

(*
    let leftRotate2 t =
        match t with 
        | Tree (Datum(rootData, rootHeight), lChild, rChild) ->
            let lcOfRc, rcOfRc, heightOfLcOfRc = 
                match rChild with
                | Tree(_,lc,rc) -> 
                    lc, rc, 
                    match lc with
                    | Tree(Datum(_,h),_,_) -> h
                    | EmptyTree -> -1
                | EmptyTree -> EmptyTree, EmptyTree, -1
            let heightOfLc =
                match lChild with
                | Tree(Datum(_,h),_,_) -> h
                | EmptyTree -> -1
            let newLc =
                Tree(Datum(rootData, (max heightOfLc heightOfLcOfRc) + 1),
                     lChild,
                     lcOfRc)
            let newRc = 
                rcOfRc
            match newLc, newRc with
            | Tree(Datum(_,heightLc),_,_), Tree(Datum(_,heightRc),_,_) ->
                Tree(Datum)

        | Tree (Datum(rootData, rootHeight),
                Tree(Datum(lcData,lcHeight),lcOfLc,rcOfLc), 
                Tree(Datum(rcData,rcHeight),
                     Tree(Datum(lcOfRcData,lcOfRcHeight),lcOfLcOfRc,rcOfLcOfRc),
                     Tree(Datum(rcOfRcData,rcOfRcHeight),
                          Tree(Datum(_,_),_,_),
                          rcOfRc) ) ) ->

          // Most general case:  t has two children.  Otherwise, leftRotate doesn't make much sense!
*)            
    (* 
    Need several operations:
    + Create a treeset?
    + Insert into a treeset
    + delete from a treeset
    + concatenation of treesets
    + rotation primitives
    + rebalancing operation

    Then there are the set-theory operations:
    + Partition, or split, given a key value
    + set union
    + set intersection
    + set difference

    Others:
    + Turn the tree-set into a list of data?
    + 
    *)