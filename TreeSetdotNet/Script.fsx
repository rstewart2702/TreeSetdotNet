// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

// #load "Library1.fs"
// open TreeSetdotNet

// Define your library scripting code here
#r @"bin/Debug/TreeSetdotNet.dll"

open TreeSetdotNet.BalancedBinaryTree ;;

let myT = 
    Tree(Datum("Stewart, Richard",1),EmptyTree,Tree(Datum("Thomas, Jack",0),EmptyTree,EmptyTree)) ;;

(leftRotate myT) = 
  Tree(Datum("Thomas, Jack",1),Tree(Datum("Stewart, Richard",0),EmptyTree,EmptyTree),EmptyTree) ;;

(rightRotate 
   ( Tree(Datum("Stewart, Richard",1),Tree(Datum("Alvin",0),EmptyTree,EmptyTree),EmptyTree) ) ) =
  Tree(Datum("Alvin",1),EmptyTree,Tree(Datum("Stewart, Richard",0),EmptyTree,EmptyTree)) ;;

