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
        // 
        // Herein, the recursion is intended to reduce the height
        // of the lt, until the evaluation comes to a right-spine
        // subtree of the lt which has height equal to or less than
        // the height or the rt.
        match lt, rt with
        | EmptyTree,                       EmptyTree -> 
            Tree(Datum(x,0),lt,rt)
        | Tree(Datum(dl,0),_,_),           EmptyTree -> 
            Tree(Datum(dl,1),
                 EmptyTree,
                 Tree(Datum(x,0),EmptyTree,EmptyTree))
        | Tree(Datum(dl,h),lcOfLt,rcOfLt), _        ->
            if (h) > (tHeight rt) then
                let newRt = 
                    rightConcat rcOfLt x rt
                rebalance (
                    Tree(Datum(dl, (max (tHeight lcOfLt) (tHeight newRt))+1),
                         lcOfLt,
                         newRt)
                )
            else
                Tree(Datum(x,(max (tHeight lt) (tHeight rt))+1),lt,rt) 
        | EmptyTree, Tree(Datum(rk,0),EmptyTree,EmptyTree) ->
            // If the height of the left tree has gone down to -1,
            // then we must have fallen off the tree in the process
            // of trying to get to a shorter right-spine subtree.
            // This means that we have reached the shortest possible
            // right-spine subtree!
            Tree(Datum(x,1),EmptyTree,rt)
        | _, _ ->
            failwith "rightConcat:  pattern matching failed!!!"

    let rec leftConcat lt x rt =
        // Herein, the recursion is intended to reduce the height
        // of the rt, until the evaluation comes to a left-spine
        // subtree of the rt which has height equal to or less than
        // the height of the lt.
        match lt, rt with
        | EmptyTree,                       EmptyTree             -> 
            Tree(Datum(x,0),EmptyTree,EmptyTree)
        | EmptyTree,                       Tree(Datum(dr,0),_,_) ->
            Tree(Datum(dr,1),
                 Tree(Datum(x,0),EmptyTree,EmptyTree),
                 EmptyTree)
        | _,                               Tree(Datum(dr,hr),lcOfRt,rcOfRt) ->
            if (tHeight lt) < hr then
                let newLt =
                    leftConcat lt x lcOfRt
                rebalance (
                    Tree(Datum(dr,(max (tHeight newLt) (tHeight rcOfRt))+1),
                         newLt,
                         rcOfRt)
                )
            else
                Tree(Datum(x,(max (tHeight lt) (tHeight rt))+1),lt,rt)
        | Tree(Datum(lk,0),EmptyTree,EmptyTree), EmptyTree ->
            // height of the right tree has "gone negative,"
            // which means we must return the left tree:
            Tree(Datum(x,1),lt,EmptyTree)
        | _,  _ ->
            failwith "leftConcat:  pattern matching failed!!!"

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
        // | NeitherDir

    type 'K BSTZipPath =
        (BSTZipperDirection * 'K BalancedSearchTree) list

    type 'K BSTZipper =
        'K BSTZipPath * 'K BalancedSearchTree

    let rec zipperTop z =
        match z with
        | [], EmptyTree -> z
        | [], Tree(_,_,_) -> z
        | (pathHead :: pathTail) , t ->
            match pathHead with
            | (Left|Right), t ->
                zipperTop (pathTail, t)

    let rec zipTraverse z k =
        match z with
        | _, EmptyTree -> z
        | path, (Tree(Datum(rk,rh),lc,rc) as root) ->
            if k < rk then
                zipTraverse (((Left,root) :: path), lc) k
            elif rk < k then
                zipTraverse (((Right,root) :: path), rc) k
            else z

    let rec zipperMoveTo z k =
        zipTraverse (zipperTop z) k

    let rec zipSplitR ls rs z =
        match z with 
        | [], _ -> ls , rs
        | headZ :: tailZ, (Tree(Datum(fk,_),flc,frc) as focusTree) ->
            match headZ with
            | Left, (Tree(Datum(dk,_),dlc,drc) as ptOfDeparture) ->
                zipSplitR
                    ls
                    // (concatTrees drc dk rs)
                    (concatTrees rs dk drc)
                    (tailZ, ptOfDeparture)
            | Right, (Tree(Datum(dk,_),dlc,drc) as ptOfDeparture) ->
                zipSplitR
                    // (concatTrees ls dk dlc)
                    (concatTrees dlc dk ls)
                    rs
                    (tailZ, ptOfDeparture)
            | _,    EmptyTree ->
                failwith "zipSplitR:  impossible pattern in zipper head?"
        | _, EmptyTree -> 
            failwith "zipSplitR:  impossible pattern in zipper?"

    let zipSplit z =
        match z with
        | [], EmptyTree -> EmptyTree, EmptyTree
        | [], Tree(_,flc,frc) ->
            flc,frc
        //
        | headZ :: tailZ, EmptyTree ->
            // The zipper traversal fell off the tree, i.e., the sought-after key
            // did not exist in the set:
            match headZ with
            | Left, (Tree(Datum(dk,_),_,rc) as ptOfOrigin) ->
                // ordering of the subtrees:
                //   EmptyTree (aka "currentFocus")
                //   dk
                //   rc (which is to the right of the key which is in the origin tree)
                zipSplitR
                    EmptyTree  // i.e., we fell off, to left of, the dk key
                    // (concatTrees rc dk EmptyTree)
                    (concatTrees EmptyTree dk rc)
                    (tailZ, ptOfOrigin)
            | Right, (Tree(Datum(dk,_),lc,_) as ptOfOrigin) ->
                // ordering of the subtrees:
                //   lc (which is to the left of the key which is in the origin tree)
                //   dk
                //   EmptyTree (aka "currentFocus")
                zipSplitR
                    // (concatTrees EmptyTree dk lc)
                    (concatTrees lc dk EmptyTree)
                    EmptyTree
                    (tailZ, ptOfOrigin)
            | _, EmptyTree ->
                failwith "zipSplit:  impossible pattern in zipper head, left the tree."
        | headZ :: tailZ, (Tree(Datum(fk,_),flc,frc) as currentFocus) ->
            match headZ with
            | Left, (Tree(Datum(dk,_),_,rc) as ptOfOrigin) ->
                // ordering of the subtrees:
                //   frc (contribution of the currentFocus to the right-hand partition)
                //   dk
                //   rc (which is to the right of all of the keys in the focus)
                zipSplitR
                    flc
                    (concatTrees frc dk rc)
                    (tailZ, ptOfOrigin)
            | Right, (Tree(Datum(dk,_),lc,_) as ptOfOrigin) ->
                // ordering of the subtrees:
                //   lc (which is to the left of all of the keys in the focus)
                //   dk
                //   frc (contribution of the currentFocus to the left-hand partition)
                zipSplitR
                    (concatTrees lc dk flc)
                    frc
                    (tailZ, ptOfOrigin)
            | _, EmptyTree ->
                failwith "zipSplit:  impossible pattern in zipper head, stayed inside the tree."

    let rec splitTree t k =
        let locZip = 
            // zipTraverse [(NeitherDir,t)] k
            zipTraverse ([],t) k
        zipSplit locZip // EmptyTree EmptyTree
    
    // Quick-and-dirty functions to provide inorder traversal
    // of tree structures, so that we may convert a
    //   'a BalancedSearchTree
    // into
    //   'a list
    let flip f x y = f y x

    let rec treeInorderR listAcc t =    
        match t with
        | EmptyTree -> listAcc
        | Tree(Datum(dk,_),lc,rc) ->
            dk :: (treeInorderR listAcc lc) 
            |> (flip treeInorderR rc)

    let treeInorder t =
        treeInorderR [] t |> List.rev

    let splitSet s k =
        let locZipper =
            zipTraverse ([],s) k
        match locZipper with
        | [], t ->
            // the sought-after key is at the root of the tree!
            match t with
            | EmptyTree -> 
                // Shouldn't happen unless s is an EmptyTree to start with?
                EmptyTree, None, EmptyTree
            | Tree(Datum(k,_),lc,rc) ->
                lc, Some k, rc
        | z, t ->
            let splitKey =
                match t with
                | EmptyTree ->
                    None
                | Tree(Datum(k,_),_,_) ->
                    Some k
            match zipSplit locZipper with
            | l, r ->
                l, splitKey, r


    // Set union, via the split/partition function:
    let rec setUnion s1 s2 =
        match s1 with
        | EmptyTree -> s2
        | Tree(_,_,_) ->
            match s2 with
            | EmptyTree -> s1
            | Tree(Datum(k2,_),lc2,rc2) ->
                match splitSet s1 k2 with
                | partL1, _, partR1 ->
                    let unionL =
                        setUnion partL1 lc2
                    let unionR =
                        setUnion partR1 rc2
                    concatTrees unionL k2 unionR

    let rec setIntersection s1 s2 =
        match s1 with
        | EmptyTree -> EmptyTree
        | Tree(Datum(k1,_),_,_) ->
            match s2 with
            | EmptyTree -> EmptyTree
            | Tree(Datum(k2,_),lc2,rc2) ->
                match splitSet s1 k2 with
                | partL1, sKeyOption, partR1 ->
                    let intersectL =
                        setIntersection partL1 lc2
                    let intersectR = 
                        setIntersection partR1 rc2
                    match sKeyOption with
                    | None ->
                        concatSets intersectL intersectR
                    | Some k ->
                        concatTrees intersectL k intersectR

    let rec setDifference s1 s2 =
        match s1 with
        | EmptyTree -> EmptyTree
        | Tree(Datum(k1,_),lc1,rc1) ->
            match s2 with
            | EmptyTree -> s1
            | Tree(Datum(k2,_),lc2,rc2) ->
                match splitSet s1 k2 with
                | partL1, sKeyOption, partR1 ->
                    let diffL =
                        setDifference partL1 lc2
                    let diffR = 
                        setDifference partR1 rc2
                    concatSets diffL diffR
                    
    let rec zipAscend z dir =
        match z with
        | [], t -> z
        | (pDir, t) :: tailZ, cf ->
            if pDir = dir then
                tailZ, t
            else
                zipAscend (tailZ, t) dir
    
    let rec zipDown dir z =
        match z, dir with
        | (pathList, (Tree(_,(Tree(_,_,_) as lc),_) as cf)), Left ->
            zipDown dir ((Left,cf)::pathList, lc)
        | (pathList, Tree(_,EmptyTree,_)),                   Left ->
            z
        | (pathList, (Tree(_,_,(Tree(_,_,_) as rc)) as cf)), Right ->
            zipDown dir ((Right,cf)::pathList, rc) 
        | (pathList, Tree(_,_,EmptyTree)),                   Right ->
            z
        | _,_ ->
            failwith "zipDown:  impossible case reached?"

    let zipSuccessor z =
        match z with
        | pathList, (Tree(_,_,(Tree(_,_,_) as rc)) as cf) ->
            (((Right, cf) :: pathList), rc) |>
            zipDown Left 
        // The only way the following case arises is if the zipper
        // happens to have been calculated for a key that didn't 
        // exist in the tree in the first place:
        | _, EmptyTree ->
            zipAscend z Left |>
            zipDown Left
        | pathList, Tree(_,_,EmptyTree) ->
            zipAscend z Left 
        



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