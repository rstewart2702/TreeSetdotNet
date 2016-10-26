

namespace TreeSetdotNet
  module BalancedBinaryTree = begin
    type 'K KeyRecord = | Datum of 'K * int
    type 'K BalancedSearchTree =
      | Tree of 'K KeyRecord * 'K BalancedSearchTree * 'K BalancedSearchTree
      | EmptyTree
    val lChild : t:'a BalancedSearchTree -> 'a BalancedSearchTree
    val rChild : t:'a BalancedSearchTree -> 'a BalancedSearchTree
    val tHeight : t:'a BalancedSearchTree -> int
    val tDatum : t:'a BalancedSearchTree -> 'a KeyRecord option
    val tKandH : t:'a BalancedSearchTree -> ('a * int) option
    val leftRotate :
      t:'a BalancedSearchTree -> 'a BalancedSearchTree
    val rightRotate :
      t:'a BalancedSearchTree -> 'a BalancedSearchTree
    val rebalance :
      t: 'a BalancedSearchTree -> 'a BalancedSearchTree
    val findMin :
      t: 'a BalancedSearchTree -> 'a option
    // When we get to the insertion and deletion operators, 
    // the values provided into the tree must implement the IComparable
    // interface!  That is what 
    //   when 'a : comparison
    // means, fundamentally.
    val btInsert :
      t: 'a  BalancedSearchTree -> k: 'a -> 'a BalancedSearchTree
        when 'a : comparison
    val btRemove :
      t: 'a BalancedSearchTree -> k: 'a -> 'a BalancedSearchTree
        when 'a : comparison
    //
    val rightConcat :
      lt: 'a BalancedSearchTree -> x: 'a -> rt: 'a BalancedSearchTree -> 'a BalancedSearchTree
        when 'a : comparison
    val leftConcat :
      lt: 'a BalancedSearchTree -> x: 'a -> rt: 'a BalancedSearchTree -> 'a BalancedSearchTree
        when 'a : comparison
    //
    val concatTrees :
      lt: 'a BalancedSearchTree -> x: 'a -> rt: 'a BalancedSearchTree -> 'a BalancedSearchTree
        when 'a : comparison
    val concatSets: 
      ls: 'a BalancedSearchTree -> 'a BalancedSearchTree -> 'a BalancedSearchTree
        when 'a : comparison
    //
    type BSTZipperDirection =
        Left
        | Right
        // | NeitherDir

    type 'K BSTZipPath =
        (BSTZipperDirection * 'K BalancedSearchTree) list

    type 'K BSTZipper =
        'K BSTZipPath * 'K BalancedSearchTree
    //
    val zipperTop :
      z: 'a BSTZipper -> 'a BSTZipper
        when 'a : comparison
    val zipTraverse :
      z: 'a BSTZipper -> k: 'a -> 'a BSTZipper
        when 'a : comparison
    val zipperMoveTo :
      z: 'a BSTZipper -> k: 'a -> 'a BSTZipper
       when 'a : comparison
    val zipSplitR :
        ls: 'a BalancedSearchTree 
        -> rs: 'a BalancedSearchTree 
        -> z: 'a BSTZipper 
        -> ('a BalancedSearchTree * 'a BalancedSearchTree)
        when 'a : comparison
    val zipSplit :
      z: 'a BSTZipper -> ('a BalancedSearchTree * 'a BalancedSearchTree)
        when 'a : comparison
    val splitTree :
      t: 'a BalancedSearchTree -> k: 'a -> ('a BalancedSearchTree * 'a BalancedSearchTree)
        when 'a : comparison
    //
    // Miscellaneous:
    val flip : f: ('b -> 'a ->'c) -> x:'a -> y:'b -> 'c
    val treeInorder: t: ('a BalancedSearchTree) -> ('a list)
        when 'a : comparison
    //
    val splitSet :
      s: 'a BalancedSearchTree -> k: 'a -> ('a BalancedSearchTree * 'a option * 'a BalancedSearchTree) 
        when 'a: comparison
    val setUnion :
      s1: 'a BalancedSearchTree -> s2: 'a BalancedSearchTree -> 'a BalancedSearchTree
        when 'a: comparison
    val setIntersection :
      s1: 'a BalancedSearchTree -> s2: 'a BalancedSearchTree -> 'a BalancedSearchTree
        when 'a: comparison
    val setDifference :
      s1: 'a BalancedSearchTree -> s2: 'a BalancedSearchTree -> 'a BalancedSearchTree
        when 'a: comparison
    //
    val zipSuccessor :
      z: 'K BSTZipper -> 'K BSTZipper
    val zipPredecessor :
      z: 'K BSTZipper -> 'K BSTZipper
  end



