﻿// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
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

"Jannson, Tove" |>
btInsert (
    "Ockinga, Harold" |>
    btInsert (
        "Lister, Hugh" |>
        btInsert (
            "James, Rick" |> 
            btInsert (btInsert (btInsert (btInsert (btInsert myT "Smith, Bob") "Jones, Richard") "Hicks, Alfred") "Jackson, Gary"))) )


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

[ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold";
  "Orvis, Bob"; "Ostend, Dean"; "Jannson, Tove"; "Stewart, Richard" ] |> List.fold btInsert EmptyTree
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
       (Datum ("Ostend, Dean",2),
        Tree
          (Datum ("Ockenga, Harold",1),
           Tree (Datum ("Lister, Hugh",0),EmptyTree,EmptyTree),
           Tree (Datum ("Orvis, Bob",0),EmptyTree,EmptyTree)),
        Tree
          (Datum ("Smith, Bob",1),EmptyTree,
           Tree (Datum ("Stewart, Richard",0),EmptyTree,EmptyTree))))
*)  

btRemove 
    ([ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold";
         "Orvis, Bob"; "Ostend, Dean"; "Jannson, Tove"; "Stewart, Richard" ] |> List.fold btInsert EmptyTree)
    "Jones, Richard"

btRemove 
    ([ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold";
         "Orvis, Bob"; "Ostend, Dean"; "Jannson, Tove"; "Stewart, Richard" ] |> List.fold btInsert EmptyTree)
    "Stewart, Richard"
      
btRemove
    (btRemove 
        ([ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold";
           "Orvis, Bob"; "Ostend, Dean"; "Jannson, Tove"; "Stewart, Richard" ] |> List.fold btInsert EmptyTree)
        "Stewart, Richard"
    )
    "Jackson, Gary"

btRemove
    (btRemove 
        ([ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold";
           "Orvis, Bob"; "Ostend, Dean"; "Jannson, Tove"; "Stewart, Richard" ] |> List.fold btInsert EmptyTree)
        "Stewart, Richard"
    )
    "Ostend, Dean"

[ "Smith, Bob"; "Jones, Richard"; "Hicks, Alfred"; "Jackson, Gary"; "James, Rick"; "Lister, Hugh"; "Ockenga, Harold";
           "Orvis, Bob"; "Ostend, Dean"; "Jannson, Tove"; "Stewart, Richard" ; "Patrick, Chad"; "Quinn, Anthony" ] 
           |> List.fold btInsert EmptyTree




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


btRemove 
    (Tree(Datum("Smith, Bob",1),EmptyTree,Tree(Datum("Stewart, Richard",0),EmptyTree,EmptyTree))) "Stewart, Richard"

let lgTree =
    ["Stewart, Richard" ;
    "Bowers, Richard" ;
    "Franklin, Regena" ;
    "Dobbs, Richard" ;
    "Gaddis, Calvin" ;
    "Ford, Matthew" ;
    "Flannery, William" ;
    "Teukolsky, Saul" ;
    "Cotillard, Marion" ;
    "Holmes, Katie" ;
    "Bessemer, Henry" ;
    "Harkness, Jack" ;
    "De Vargas, Juan" ;
    "Martin, Chris" ;
    "Nolan, Christopher" ;
    "Thomas, Emma" ;
    "Scharansky, Natan" ;
    "Smith, Markus" ;
    "Marvinson, Gus" ;
    "Desmond, Charles" ;
    "Chisman, Kerry" ;
    "Jones, Kerri" ;
    "Yee, Soo Jean" ;
    "Pachelbel, Johann" ;
    "Gavrilovich, Pavel" ;
    "Hardy, Tom" ;
    "Mott, Nathan" ;
    "Casali, Roy" ;
    "Murphy, Elaine" ;
    "Bennett, Matthew" ;
    "Wunder, Ron" ;
    "Lewis, Holly" ;
    "May, Parker" ;
    "Bailey, Tom" ;
    "McClinton, Rebecca" ;
    "Selle, Bron" ;
    "Richardson, Keith" ;
    "Lewis, Clive" ;
    "Tolkien, John Ronald Reuel" ;
    "Austen, Jane" ;
    "Jackson, Peter" ;
    "Rai, Aishwarya" ;
    "Roshan, Hrithik" ;
    "Khan, Shah Rukh" ;
    "Wilson, Warren" ;
    "Leach, Nora" ;
    "Lin, Zoe" ;
    "Clancy, Scot" ;
    "McBride, Martina" ;
    "Ferrell, Will" ;
    "Plantinga, Alvin" ;
    "Kuyper, Abraham" ;
    "Mohler, R. Albert" ;
    "Walther, C.F.W." ;
    "Poe, Edgar Alan" ;
    "Barnett, Matthew" ;
    "O'Reilly, Dennis" ;
    "Camp, Colleen" ;
    "Skarsgaard, Stellan" ;
    "Skarsgaard, Alexander" ;
    "Dijkstra, Edsger" ;
    "Knuth, Donald" ;
    "Wikstrom, Cornelius" ;
    "Van Til, Cornelius" ;
    "Poythress, Vern Sheridan" ;
    "Frame, John" ;
    "Horton, Michael" ;
    "Pilkington, Paul" ;
    "Piller, Michael" ;
    "Orange, John" ;
    "Mott, Nathan" ;
    "Evers, David" ;
    "Oliphant, Scott" ;
    "Ellison, Harlan" ;
    "DeRama, Peter" ;
    "Okasaki, Chris" ;
    "Tarjan, Robert E." ;
    "Steele, Guy L" ;
    ] |> List.fold btInsert EmptyTree ;;



match (splitTree lgTree "Nolan, Christopher") with
| EmptyTree, EmptyTree -> [], []
| lt, rt -> (treeInorder lt, treeInorder rt)

let myZip = 
    zipTraverse ([],lgTree) "Wunder, Ron"

match myZip with
| [], _ -> []
| headZ :: tailZ, t ->
    (treeInorder t),
    match headZ with
    | _, EmptyTree -> []
    | Left, t -> 

match zipSplit (zipTraverse ([],lgTree) "Wunder, Ron") with
| EmptyTree, EmptyTree -> [], []
| lt, rt -> (treeInorder lt, treeInorder rt)