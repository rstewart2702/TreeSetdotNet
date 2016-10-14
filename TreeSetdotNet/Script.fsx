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

btInsert EmptyTree "Smith"

"Ockinga, Harold" |>
btInsert (
    "Lister, Hugh" |>
    btInsert (
      "James, Rick" |> 
      btInsert (btInsert (btInsert (btInsert (btInsert myT "Smith, Bob") "Jones, Richard") "Hicks, Alfred") "Jackson, Gary")))

[ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold";
  "Orvis, Bob"; "Ostend, Dean"; "Jannson, Tove" ] |> List.fold btInsert EmptyTree
(* 
val it : string BalancedSearchTree =
  Tree
    (Datum ("Jones, Richard",3),
     Tree
       (Datum ("Jackson, Gary",2),
        Tree (Datum ("Hicks, Alfred",0),EmptyTree,EmptyTree),
        Tree
          (Datum ("James, Rick",1),EmptyTree,
           Tree (Datum ("Jannson, Tove",0),EmptyTree,EmptyTree))),
     Tree
       (Datum ("Ockenga, Harold",2),
        Tree (Datum ("Lister, Hugh",0),EmptyTree,EmptyTree),
        Tree
          (Datum ("Ostend, Dean",1),
           Tree (Datum ("Orvis, Bob",0),EmptyTree,EmptyTree),
           Tree (Datum ("Smith, Bob",0),EmptyTree,EmptyTree))))
*)

// The following exposed a bug, probably with the rebalance function:

[ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold" ]
|> List.fold btInsert EmptyTree
(* 
val it : string BalancedSearchTree =
  Tree
    (Datum ("Jones, Richard",2),
     Tree
       (Datum ("Jackson, Gary",1),
        Tree (Datum ("Hicks, Alfred",0),EmptyTree,EmptyTree),
        Tree (Datum ("James, Rick",0),EmptyTree,EmptyTree)),
     Tree
       (Datum ("Ockenga, Harold",1),
        Tree (Datum ("Lister, Hugh",0),EmptyTree,EmptyTree),
        Tree (Datum ("Smith, Bob",0),EmptyTree,EmptyTree)))
*)

[ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred" ] |> List.fold btInsert EmptyTree