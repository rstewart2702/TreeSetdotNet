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
        | Tree(Datum(rootKey,_), lc, rc) ->
            if abs ((tHeight lc) - (tHeight rc)) > 1 then
                if (tHeight lc) > (tHeight rc) then
                    let heightOfLcOfLc = tHeight (lChild lc)
                    let heightOfRcOfLc = tHeight (rChild lc)
                    let newLc = 
                        if heightOfLcOfLc < heightOfRcOfLc then
                            leftRotate lc
                        else 
                            lc
                    rightRotate (Tree(Datum(rootKey,(max (tHeight newLc) (tHeight rc))+1),
                                      newLc,
                                      rc))
                else // if (theight lc) < (tHeight rc) then
                    let heightOfLcOfRc = tHeight (lChild rc)
                    let heightOfRcOfRc = tHeight (rChild rc)
                    let newRc =
                        if heightOfRcOfRc < heightOfLcOfRc then
                            rightRotate rc
                        else 
                            rc
                    leftRotate (Tree(Datum(rootKey,(max (tHeight lc) (tHeight newRc))+1),
                                     lc,
                                     newRc))
            else
                t
            

    let rec btInsert t k =
        match t with
        | EmptyTree -> Tree(Datum(k,0),EmptyTree,EmptyTree)
        | Tree(Datum(rootKey,_),lc,rc) ->
            let ltNew =
                if rootKey < k then
                    lc 
                else
                    btInsert lc k 
            let rtNew = 
                if rootKey < k then
                    btInsert rc k 
                else
                    rc 
            rebalance ( Tree(Datum(rootKey, (max (tHeight ltNew) (tHeight rtNew))+1),ltNew,rtNew) )

    let rec findMin t = 
        match t with 
        | EmptyTree -> None
        | Tree(Datum(k,_),EmptyTree,_) -> Some k
        | Tree(Datum(k,_),(Tree(_,_,_) as lc),_) -> findMin lc

    let rec btRemove t k =
        // I had tried to use pattern matching inappropriately, in previous versions of
        // this function.  Fundamentally, pattern-matching introduces new identifiers,
        // new scope, and so if the free variables you introduce in a match expression
        // have the same name as those in an enclosing scope, you lose access to those
        // from the enclosing scope!  I had introduced patterns which mentioned a 
        // variable k, under the mistaken assumption that the "k" used in the 
        // pattern-matching clauses would be identical to the "k" in the parameter list
        // of this btRemove function.  That's just not now it works in F#!
        //
        // So, each of the following patterns now contains an identifier called 
        //   rootKey
        // and we must explicitly compare rootKey to k in order to determine 
        // whether or not the root of the tree happens to be the item named by k, above.
        //
        // It may indeed be significantly simpler to reason about the tree 
        // in terms of the height field?
        match t with
        | EmptyTree -> EmptyTree
        | Tree(Datum(rootKey,0),EmptyTree,EmptyTree) ->
            // If the item appears in the root, and it is the only item in the tree,
            // then the result is an EmptyTree.
            // Otherwise, return the original tree, for the sought item, k,
            // simply is not present.
            // We thus take the philosophy that the result of trying to remove
            // a non-existent item from a tree leaves the tree unchanged.
            if rootKey = k then EmptyTree else t
        | Tree(Datum(rootKey,_),lc,rc) ->
            if k < rootKey then
                let newLc = btRemove lc k
                rebalance (
                    Tree(Datum(rootKey,(max (tHeight newLc) (tHeight rc))+1),
                         newLc,
                         rc)
                )
            else if rootKey < k then
                let newRc = btRemove rc k
                rebalance (
                    Tree(Datum(rootKey,(max (tHeight lc) (tHeight newRc))+1),
                         lc,
                         newRc)
                )
            else
                match (findMin rc) with
                | Some minKey ->
                    let newRc = btRemove rc minKey
                    rebalance (
                        Tree(Datum(minKey,(max (tHeight lc) (tHeight newRc))+1),
                             lc,
                             newRc)
                    )
                | None ->
                    // This means that the right-hand child rc was EmptyTree, 
                    // which implies that the only option left to us is to
                    // replace t with its left child.
                    // Former versions of btRemove function prevented this
                    // case from being evaluated with a longer, more complex
                    // list of matching expressions.  This is much simpler.
                    lc

    let rec rightConcat lt x rt =
        // Concatenation of rt onto lt, where rt is assumed to be shorter,
        // and the key ranges of the two trees DO *NOT* OVERLAP.
        // If the trees' key ranges do overlap, then this function
        // will not correctly concatenate the sets together!
        match lt, rt with
        | EmptyTree, EmptyTree ->   Tree(Datum(x,0),EmptyTree,EmptyTree)
        | EmptyTree, Tree(_,_,_) -> Tree(Datum(x,1),EmptyTree,rt)
        | Tree(_,_,_), EmptyTree -> Tree(Datum(x,1),lt,EmptyTree)
        | Tree(Datum(dl,hl),lcOfLt,rcOfLt), Tree(Datum(_,hr),_,_) ->
            if hl > hr then
                let newRt =
                    rightConcat rcOfLt x rt
                rebalance (
                    Tree(Datum(dl, (max (tHeight lcOfLt) (tHeight newRt))+1),lcOfLt,newRt)
                )
            else // if hl <= hr then
                // hr - hl <= 1, correct?  This HAS to be the case, correct?
                Tree(Datum(x,(max hl hr)+1),lt,rt)

    let rec leftConcat lt x rt =
        match lt, rt with
        | EmptyTree, EmptyTree ->   Tree(Datum(x,0),EmptyTree,EmptyTree)
        | EmptyTree, Tree(_,_,_) -> Tree(Datum(x,1),EmptyTree,rt)
        | Tree(_,_,_), EmptyTree -> Tree(Datum(x,1),lt,EmptyTree)
        | Tree(Datum(_,hl),_,_), Tree(Datum(dr,hr),lcOfRt,rcOfRt) ->
            if hl < hr then
                let newLt = 
                    leftConcat lt x lcOfRt
                rebalance (
                    Tree(Datum(dr,(max (tHeight newLt) (tHeight rcOfRt))+1),newLt,rcOfRt)
                )
            else
                Tree(Datum(x,(max hl hr)+1),lt,rt)

    let concatTrees lt x rt =
        if (tHeight lt) < (tHeight rt) then leftConcat lt x rt
        else rightConcat lt x rt

    let concatSets ls rs =
        let newKey = findMin rs
        match newKey with
        | None -> 
            ls
        | Some k ->
            btRemove rs k |> concatTrees ls k
            // concatTrees ls k (btRemove rs k)

    type BSTZipperDirection =
        Left
        | Right
        | NeitherDir

    type 'K BSTZipper =
        (BSTZipperDirection * 'K BalancedSearchTree) list

    let rec zipperTop z =
        match z with
        | [] -> []
        | headZ :: tailZ ->
            match headZ with
            | _, EmptyTree -> z
            | NeitherDir, Tree(_,_,_) -> z
            | (Left, _) | (Right, _) -> tailZ

    let rec zipTraverse z k =
        match z with
        | [] -> []
        | headZ :: tailZ ->
            match headZ with
            | _, t ->
                match t with
                | EmptyTree -> z
                | Tree(Datum(dk,_),lc,rc) ->
                    if k < dk then
                        zipTraverse ((Left,lc)::z) k
                    else if dk < k then 
                        zipTraverse ((Right,rc)::z) k
                    else z

    let rec zipperMoveTo z k =
        zipTraverse (zipperTop z) k

    let rec zipSplit z ls rs =
        match z with
        | [] -> ls, rs
        | headZ :: tailZ ->
            let lSplit =
                match headZ with
                | (NeitherDir|Left), t ->
                    concatSets t ls
                | Right, _ ->
                    ls
            let rSplit =
                match headZ with
                | (NeitherDir|Right), t ->
                    concatSets t rs
                | Left, _ ->
                    rs
            zipSplit tailZ lSplit rSplit

    let rec splitTree t k =
        let locZip = 
            zipTraverse [(NeitherDir,t)] k
        zipSplit locZip EmptyTree EmptyTree
    


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
    + Cursor/zipper
    *)