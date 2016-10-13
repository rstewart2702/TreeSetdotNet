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

    // These rotation primitives are shockingly short and elegant.
    //
    // I was nearly driven towards this formulation by attempts
    // to avoid "incomplete pattern match" warnings from the F# compiler.
    // So, the act of trying to get the structure of the types to help
    // drive the structure of the function actually led us to a correct
    // function.
    // These are superfically different from the corresponding Scheme 
    // functions, but I suspect this may be due mainly to the 
    // abbreviation/shortening provided by pattern matching!
    let leftRotate t =
        match t with
        | EmptyTree -> EmptyTree
        | Tree(_,_,EmptyTree) -> t
        | Tree(Datum(rootData,_),
               lc,
               Tree(Datum(rcData,_), lcOfRc, rcOfRc)) ->
            let newLcHeight = 
                (max (tHeight lc) (tHeight lcOfRc)) + 1
            Tree(Datum(rcData, (max newLcHeight (tHeight rcOfRc)) + 1), 
                 (Tree(Datum(rootData, newLcHeight), lc, lcOfRc)),
                 rcOfRc)

    let rightRotate t =
        match t with
        | EmptyTree -> EmptyTree
        | Tree(_,EmptyTree,_) -> t
        | Tree(Datum(rootData,_),
               Tree(Datum(lcData,_),lcOfLc,rcOfLc),
               rc) ->
            let newRcHeight =
                (max (tHeight rcOfLc) (tHeight rc)) + 1
            Tree(Datum(lcData, (max (tHeight lcOfLc) newRcHeight) + 1),
                 lcOfLc,
                 Tree(Datum(rootData, newRcHeight), rcOfLc, rc))


    let rebalance t =
        match t with
        | EmptyTree -> EmptyTree
        | Tree(Datum(rootKey,_),lc,rc) ->
            if (abs (tHeight lc) - (tHeight rc)) > 1 then
                match lc, rc with
                | (Tree(_,lcOfLc,rcOfLc)),(Tree(_,lcOfRc,rcOfRc)) ->
                    if (tHeight lc) < (tHeight rc) then
                        if (tHeight lcOfRc) <= (tHeight rcOfRc) then 
                            leftRotate t
                        else
                            let newRc =
                                rightRotate rc
                            leftRotate (Tree(Datum(rootKey,(max (tHeight lc) (tHeight newRc))+1),
                                             lc,
                                             newRc))
                    else
                        if (tHeight rcOfLc) <= (tHeight lcOfLc) then
                            rightRotate t
                        else
                            let newLc =
                                leftRotate lc
                            rightRotate (Tree(Datum(rootKey,(max (tHeight newLc) (tHeight rc))+1),
                                              newLc,
                                              rc))
            else t

    let rebalance2 t =
        match t with
        | EmptyTree -> EmptyTree
        | Tree(_,EmptyTree,_) -> t
        | Tree(_,_,EmptyTree) -> t
        | Tree(Datum(rootKey,_),
               (Tree(Datum(_,heightLc),lcOfLc,rcOfLc) as lc),
               (Tree(Datum(_,heightRc),lcOfRc,rcOfRc) as rc)) ->
            if (abs heightLc - heightRc) > 1 then
                if heightLc < heightRc then
                    if (tHeight lcOfRc) <= (tHeight rcOfRc) then 
                        leftRotate t
                    else
                        let newRc =
                            rightRotate rc
                        leftRotate (Tree(Datum(rootKey,(max heightLc (tHeight newRc))+1),
                                            lc,
                                            newRc))
                else
                    if (tHeight rcOfLc) <= (tHeight lcOfLc) then
                        rightRotate t
                    else
                        let newLc =
                            leftRotate lc
                        rightRotate (Tree(Datum(rootKey,(max (tHeight newLc) heightRc)+1),
                                            newLc,
                                            rc))
            else t                    

    let rec btInsert t k =
        match t with
        | EmptyTree -> Tree(Datum(k,0),EmptyTree,EmptyTree)
        | Tree(Datum(rootKey,_),lc,rc) ->
            let ltNew =
                if rootKey < k then
                    btInsert lc k
                else
                    lc
            let rtNew = 
                if rootKey < k then
                    rc
                else
                    btInsert rc k
            rebalance ( Tree(Datum(rootKey, (max (tHeight ltNew) (tHeight rtNew))+1),ltNew,rtNew) )

//        | Tree(Datum(rootKey,_),lc,rc) when rootKey < k ->
//            let ltNew =
//                btInsert lc k
//            let rtNew = 
//                rc
//            rebalance Tree(Datum(rootKey, (max (tHeight ltNew) (tHeight rtNew))+1),ltNew,rtNew)
//        | Tree(Datum(rootKey,_),lc,rc) when k < rootKey ->
//            let ltNew =
//                lc
//            let rtNew =
//                btInsert rc k
//            rebalance Tree(Datum(rootKey, (max (tHeight ltNew) (tHeight rtNew))+1),ltNew,rtNew)
            
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