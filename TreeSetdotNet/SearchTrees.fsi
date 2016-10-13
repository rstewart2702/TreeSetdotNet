

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
  end



