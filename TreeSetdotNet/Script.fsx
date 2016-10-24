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
//    "Mott, Nathan" ;
    "Evers, David" ;
    "Oliphant, Scott" ;
    "Ellison, Harlan" ;
    "DeRama, Peter" ;
    "Okasaki, Chris" ;
    "Tarjan, Robert E." ;
    "Steele, Guy L" ;
    ] |> List.fold btInsert EmptyTree ;;

let lgTree1 = 
    match splitTree lgTree "Fuss, David" with
    | lt, rt -> lt

match splitTree lgTree "Fuss, Dvaid" with
| ls, rs -> treeInorder ls, treeInorder rs



match (splitTree lgTree "Nolan, Christopher") with
| EmptyTree, EmptyTree -> [], []
| lt, rt -> (treeInorder lt, treeInorder rt)

let myZip = 
    zipTraverse ([],lgTree) "Wunder, Ron"

//match myZip with
//| [], _ -> []
//| headZ :: tailZ, t ->
//    (treeInorder t),
//    match headZ with
//    | _, EmptyTree -> []
//    | Left, t -> 

match zipSplit (zipTraverse ([],lgTree) "Wunder, Ron") with
| EmptyTree, EmptyTree -> [], []
| lt, rt -> (treeInorder lt, treeInorder rt)

zipSplit (zipTraverse ([],lgTree) "Wunder, Ron") ;;


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
    "Mott, Nathan";
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
//    "Mott, Nathan" ;
    "Evers, David" ;
    "Oliphant, Scott" ;
    "Ellison, Harlan" ;
    "DeRama, Peter" ;
    "Okasaki, Chris" ;
    "Tarjan, Robert E." ;
    "Steele, Guy L" ;
     ] |> List.fold  btInsert EmptyTree ;;

// concatSets 
Tree(Datum ("Walther, C.F.W.",1),
           Tree (Datum ("Van Til, Cornelius",0),EmptyTree,EmptyTree),EmptyTree)

match zipSplit (zipTraverse ([],lgTree) "Dobbs, Richard") with
| EmptyTree, EmptyTree -> [], []
| lt, rt -> (treeInorder lt, treeInorder rt)


match zipSplit (zipTraverse ([],lgTree) "Teukolsky, Saul") with
| EmptyTree, EmptyTree -> [], []
| lt, rt -> (treeInorder lt, treeInorder rt)

zipTraverse ([],lgTree) "Teukolsky, Saul"

match splitTree lgTree "Comstock, Anthony" with
| EmptyTree, EmptyTree -> [], []
| lt, rt -> (treeInorder lt, treeInorder rt)

splitSet lgTree "Teukolsky, Saul"

splitSet lgTree "Comstock, Anthony"

let myTree2 =
    [ "Johns, Harold"; "Michener, James"; "Okasaki, Chris"; 
     "Yee, Soo Jean";
    "Boswell, James"]
    |> List.fold btInsert EmptyTree ;;

setUnion lgTree myTree2 ;;

setUnion lgTree myTree2 |> treeInorder ;;

setIntersection lgTree myTree2 |> treeInorder ;;

setDifference myTree2 lgTree |> treeInorder ;;
setDifference lgTree  myTree2 |> treeInorder ;; 



setDifference
  (Tree(Datum("Boswell, James",0),EmptyTree,EmptyTree))
  (Tree
    (Datum ("Khan, Shah Rukh",3),
    Tree
        (Datum ("Jackson, Peter",1),
            Tree (Datum ("Horton, Michael",0),EmptyTree,EmptyTree),
            Tree (Datum ("Jones, Kerri",0),EmptyTree,EmptyTree)),
    Tree
        (Datum ("Lewis, Clive",2),
        Tree
            (Datum ("Kuyper, Abraham",1),
             Tree (Datum ("Knuth, Donald",0),EmptyTree,EmptyTree),
             Tree (Datum ("Leach, Nora",0),EmptyTree,EmptyTree)),
             Tree
               (Datum ("Lewis, Holly",1),EmptyTree,
                Tree (Datum ("Lin, Zoe",0),EmptyTree,EmptyTree))))) ;;

zipSplitR 
  EmptyTree EmptyTree
  ([],(Tree (Datum ("Okasaki, Chris",0),EmptyTree,EmptyTree))) ;;

setDifference 
  myTree2
  (Tree(Datum("Boswell, James",0),EmptyTree,EmptyTree)) ;;

setDifference 
  myTree2
  (Tree(Datum("Michener, James",0),EmptyTree,EmptyTree)) ;;
  
splitSet (Tree(Datum("Boswell, James",0),EmptyTree,EmptyTree)) "Michener, James" 

// THE BIG TEST, THUS FAR:
setDifference myTree2 lgTree |> treeInorder ;;

setDifference 
  (Tree
     (Datum ("Franklin, Regena",5),
      Tree
        (Datum ("Cotillard, Marion",4),
         Tree
           (Datum ("Bowers, Richard",3),
            Tree
              (Datum ("Bennett, Matthew",2),
               Tree
                 (Datum ("Bailey, Tom",1),
                  Tree (Datum ("Austen, Jane",0),EmptyTree,EmptyTree),
                  Tree (Datum ("Barnett, Matthew",0),EmptyTree,EmptyTree)),
               Tree (Datum ("Bessemer, Henry",0),EmptyTree,EmptyTree)),
            Tree
              (Datum ("Chisman, Kerry",2),
               Tree
                 (Datum ("Casali, Roy",1),
                  Tree (Datum ("Camp, Colleen",0),EmptyTree,EmptyTree),
                  EmptyTree),
               Tree (Datum ("Clancy, Scot",0),EmptyTree,EmptyTree))),
         Tree
           (Datum ("Dobbs, Richard",3),
            Tree
              (Datum ("Desmond, Charles",2),
               Tree
                 (Datum ("De Vargas, Juan",1),EmptyTree,
                  Tree (Datum ("DeRama, Peter",0),EmptyTree,EmptyTree)),
               Tree (Datum ("Dijkstra, Edsger",0),EmptyTree,EmptyTree)),
            Tree
              (Datum ("Flannery, William",2),
               Tree
                 (Datum ("Evers, David",1),
                  Tree (Datum ("Ellison, Harlan",0),EmptyTree,EmptyTree),
                  Tree (Datum ("Ferrell, Will",0),EmptyTree,EmptyTree)),
               Tree
                 (Datum ("Ford, Matthew",1),EmptyTree,
                  Tree (Datum ("Frame, John",0),EmptyTree,EmptyTree))))),
      Tree
        (Datum ("Holmes, Katie",4),
         Tree
           (Datum ("Gavrilovich, Pavel",2),
            Tree (Datum ("Gaddis, Calvin",0),EmptyTree,EmptyTree),
            Tree
              (Datum ("Harkness, Jack",1),
               Tree (Datum ("Hardy, Tom",0),EmptyTree,EmptyTree),EmptyTree)),
         Tree
           (Datum ("Lewis, Clive",3),
            Tree
              (Datum ("Khan, Shah Rukh",2),
               Tree
                 (Datum ("Jackson, Peter",1),
                  Tree (Datum ("Horton, Michael",0),EmptyTree,EmptyTree),
                  Tree (Datum ("Jones, Kerri",0),EmptyTree,EmptyTree)),
               Tree
                 (Datum ("Kuyper, Abraham",1),
                  Tree (Datum ("Knuth, Donald",0),EmptyTree,EmptyTree),
                  Tree (Datum ("Leach, Nora",0),EmptyTree,EmptyTree))),
            Tree
              (Datum ("Martin, Chris",2),
               Tree
                 (Datum ("Lewis, Holly",1),EmptyTree,
                  Tree (Datum ("Lin, Zoe",0),EmptyTree,EmptyTree)),
               Tree
                 (Datum ("McClinton, Rebecca",1),
                  Tree
                    (Datum ("May, Parker",1),
                     Tree (Datum ("Marvinson, Gus",0),EmptyTree,EmptyTree),
                     Tree (Datum ("McBride, Martina",0),EmptyTree,EmptyTree)),
                  EmptyTree))))) )
  (lChild myTree2)

let lgTreeL1 = lChild lgTree


match splitSet lgTree "Michener, James" with
| l, k, r -> l |> treeInorder, k, r |> treeInorder

splitSet 
      (Tree
        (Datum ("Cotillard, Marion",4),
         Tree
           (Datum ("Bowers, Richard",3),
            Tree
              (Datum ("Bennett, Matthew",2),
               Tree
                 (Datum ("Bailey, Tom",1),
                  Tree (Datum ("Austen, Jane",0),EmptyTree,EmptyTree),
                  Tree (Datum ("Barnett, Matthew",0),EmptyTree,EmptyTree)),
               Tree (Datum ("Bessemer, Henry",0),EmptyTree,EmptyTree)),
            Tree
              (Datum ("Chisman, Kerry",2),
               Tree
                 (Datum ("Casali, Roy",1),
                  Tree (Datum ("Camp, Colleen",0),EmptyTree,EmptyTree),
                  EmptyTree),
               Tree (Datum ("Clancy, Scot",0),EmptyTree,EmptyTree))),
         Tree
           (Datum ("Dobbs, Richard",3),
            Tree
              (Datum ("Desmond, Charles",2),
               Tree
                 (Datum ("De Vargas, Juan",1),EmptyTree,
                  Tree (Datum ("DeRama, Peter",0),EmptyTree,EmptyTree)),
               Tree (Datum ("Dijkstra, Edsger",0),EmptyTree,EmptyTree)),
            Tree
              (Datum ("Flannery, William",2),
               Tree
                 (Datum ("Evers, David",1),
                  Tree (Datum ("Ellison, Harlan",0),EmptyTree,EmptyTree),
                  Tree (Datum ("Ferrell, Will",0),EmptyTree,EmptyTree)),
               Tree
                 (Datum ("Ford, Matthew",1),EmptyTree,
                  Tree (Datum ("Frame, John",0),EmptyTree,EmptyTree))))) )
  "Johns, Harold"







setDifference myTree2 lgTree1
let lgTree2 =
  (lgTree1 |> rChild |> rChild)

lgTree2 |> (setDifference myTree2 )


splitSet myTree2 "Cotillard, Marion"

zipTraverse ([],myTree2) "Cotillard, Marion" 

(zipTraverse ([],myTree2) "Cotillard, Marion" ) |> zipSplit



setUnion lgTree2 myTree2 ;;
setUnion myTree2 lgTree2;;


zipTraverse ([],myTree2) "Flannery, William"

match zipTraverse ([],myTree2) "Flannery, William" with
| headZ :: tailZ, EmptyTree ->
    match headZ with
    | Right, (Tree(Datum(k,_),lc,_) as nft) ->
        lc, (tailZ, nft),

zipTraverse ([],myTree2) "Flannery, William" |> zipSplit


zipTraverse ([],myTree2) "Flannery, William" 

zipSplitR
    (Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree))
    EmptyTree
    ([(Left,
        Tree
         (Datum ("Johns, Harold",1),
          Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree));
      (Left,
       Tree
         (Datum ("Michener, James",2),
          Tree
           (Datum ("Johns, Harold",1),
            Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree),
          Tree (Datum ("Okasaki, Chris",0),EmptyTree,EmptyTree)))],
       (Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree))) ;;

zipSplitR 
    EmptyTree 
    EmptyTree
    ([(Left,
       Tree
        (Datum ("Johns, Harold",1),
         Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree));
      (Left,
       Tree
         (Datum ("Michener, James",2),
          Tree
            (Datum ("Johns, Harold",1),
             Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree),
           Tree (Datum ("Okasaki, Chris",0),EmptyTree,EmptyTree)))],
     Tree(Datum("Boswell, James",0),EmptyTree,EmptyTree) )

concatTrees 
    EmptyTree
    "Boswell, James"
    EmptyTree ;;

zipSplitR
    EmptyTree
    (Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree))
    ([
      (Left,
       Tree
         (Datum ("Michener, James",2),
          Tree
            (Datum ("Johns, Harold",1),
             Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree),
           Tree (Datum ("Okasaki, Chris",0),EmptyTree,EmptyTree)))],
      Tree
        (Datum ("Johns, Harold",1),
         Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree) )

zipSplitR'
    EmptyTree 
    EmptyTree
    ([(Left,
       Tree
        (Datum ("Johns, Harold",1),
         Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree));
      (Left,
       Tree
         (Datum ("Michener, James",2),
          Tree
            (Datum ("Johns, Harold",1),
             Tree (Datum ("Boswell, James",0),EmptyTree,EmptyTree),EmptyTree),
           Tree (Datum ("Okasaki, Chris",0),EmptyTree,EmptyTree)))],
     Tree(Datum("Boswell, James",0),EmptyTree,EmptyTree) )


let lgTree3 =
  lgTree |> rChild |> lChild ;;
lgTree3 |> treeInorder ;;
lgTree |> treeInorder ;;
setDifference lgTree lgTree3 |> treeInorder ;;

myTree2 |> setUnion lgTree3 |> setDifference lgTree |> treeInorder 
myTree2 |> treeInorder
lgTree3 |> treeInorder
myTree2 |> setUnion lgTree3 |> treeInorder

(* 
I wonder what it would take to integrate this thing into Linq,
and use it from C#?
*)