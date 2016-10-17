

namespace TreeSetdotNet
  module BalancedBinaryTree = begin
    type 'K KeyRecord = | Datum of 'K * int
    type 'K BalancedSearchTree =
      | Tree of 'K KeyRecord * 'K BalancedSearchTree * 'K BalancedSearchTree
      | EmptyTree
    val leftEmpty : t:'a BalancedSearchTree -> bool
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
        | NeitherDir

    type 'K BSTZipper =
        (BSTZipperDirection * 'K BalancedSearchTree) list
    //
    val zipperTop :
      z: 'a BSTZipper -> 'a BSTZipper
        when 'a : comparison
    val zipTraverse :
      z: 'a BSTZipper -> k: 'a -> 'a BSTZipper
        when 'a : comparison
    val zipperMoveTo :
      z: 'a BSTZipper -> k: 'a -> 'a BSTZipper
    val zipSplit :
      z: 'a BSTZipper -> ls: 'a BalancedSearchTree -> rs: 'a BalancedSearchTree -> ('a BalancedSearchTree * 'a BalancedSearchTree)
        when 'a : comparison
    val splitTree :
      t: 'a BalancedSearchTree -> k: 'a -> ('a BalancedSearchTree * 'a BalancedSearchTree)
        when 'a : comparison
  end



