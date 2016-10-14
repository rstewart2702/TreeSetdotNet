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

    let rec btRemoveOld t k =
        match t with
        | EmptyTree -> EmptyTree
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
        // whether or not the root of the tree happens to be the item named by k, above...
        | Tree(Datum(rootKey,0),EmptyTree,EmptyTree) -> 
            // If k is the item in the root, then the result is EmptyTree; otherwise, regurgitate the tree.
            if rootKey = k then EmptyTree else t
        // 
        | Tree(Datum(rootKey,1),EmptyTree,rc) -> 
            // If k is the item in the root, then the result is the right-hand child, rc
            if rootKey = k then 
                rc 
            else
            // If k is NOT the item in the root, 
            // then the result is the tree derived from trying to remove the item from the right child
            // and then reassembling the new version of the tree:
                let newRc = (btRemoveOld rc k)
                Tree(Datum(rootKey,(max -1 (tHeight newRc))+1),EmptyTree, newRc )
        // 
        | Tree(Datum(rootKey,1),lc,EmptyTree) -> 
            // If k is the item in the root, then the result is the left-hand child, lc
            if rootKey = k then 
                lc 
            else 
            // If k is NOT the item in the root, 
            // then the result is the tree derived from trying to remove the item from the left child
            // and then reassembling the new version of the tree:
                let newLc = (btRemoveOld lc k)
                Tree(Datum(rootKey,(max (tHeight newLc) -1)+1), newLc, EmptyTree)
        // The most general case, in which the tree must be taller than 0 or 1:
        | Tree(Datum(rootKey,h),lc,rc) -> 
            if k < rootKey then
                // If the search key falls to the left of the root key:
                // Calculate a new left tree by removing the item from the left-hand
                // tree...
                let newLc =
                    btRemoveOld lc k
                // ...and calculate a new resulting tree by rebalancing the reassembled
                // tree
                rebalance (
                    Tree(Datum(rootKey,(max (tHeight newLc) (tHeight rc))+1),
                         newLc,
                         rc)
                )
            else if rootKey < k then
                // If the search key falls to the right of the root key:
                // Calculate a new right-hand tree by removing the searched-for item
                // from the right-hand tree...
                let newRc = 
                    btRemoveOld rc k
                // ...and calculate a new resulting tree by rebalancing the reassembled
                // tree:
                rebalance (
                    Tree(Datum(rootKey,(max (tHeight lc) (tHeight newRc))+1),
                         lc,
                         newRc)
                )
            else 
                // The rootKey = search key, k, so we must re-stitch the tree
                // back together by deriving a new root from the minimum element
                // of the right-hand tree, rc.
                //
                // findMin has to return an Option-typed value.
                // This means we must unpack it via the following match expression.
                match (findMin rc) with
                | Some k ->
                    let newRc = 
                        btRemoveOld rc k
                    rebalance (
                        Tree(Datum(k, (max (tHeight lc) (tHeight newRc))+1),
                             lc,
                             newRc)
                    )
                | None -> EmptyTree


    // 
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