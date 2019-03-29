
open System

type Shape =
    | Oval
    | Squiggle
    | Diamond

type Color =
    | Red
    | Purple
    | Green

type Number =
    | One
    | Two
    | Three

type Shading =
    | Solid
    | Striped
    | Outlined

type Card =
    Card of Shape * Color * Number * Shading

let areSame a b c =
    a = b && b = c

let areAllDiff a b c =
    a <> b && a <> c && b <> c

let isSet (c1: Card) (c2: Card) (c3: Card): Boolean =
    let (Card (shp1, col1, num1, sha1)) = c1
    let (Card (shp2, col2, num2, sha2)) = c2
    let (Card (shp3, col3, num3, sha3)) = c3
    
    in 
        (areSame shp1 shp2 shp3 || areAllDiff shp1 shp2 shp3) &&
        (areSame col1 col2 col3 || areAllDiff col1 col2 col3) &&
        (areSame num1 num2 num3 || areAllDiff num1 num2 num3) &&
        (areSame sha1 sha2 sha3 || areAllDiff sha1 sha2 sha3)
        
let c1 = Card (Oval, Red, One, Solid)

let deck: Card list =
    [ for shp in [Oval; Squiggle; Diamond] do
        for col in [Red; Purple; Green] do
            for num in [One; Two; Three] do
                for sha in [Solid; Striped; Outlined] do
                    yield Card(shp, col, num, sha)
    ]

let cardCount = 81;

let rec pickPositions count (rnd: Random) positions =
    match count with
    | 0 -> positions
    | n when n > 0 && n <= cardCount -> 
        let pos = rnd.Next () % cardCount
        if List.contains pos positions then
            pickPositions count rnd positions
        else
            pickPositions (count - 1) rnd (pos :: positions)
    | otherwise -> failwith <| "count must be positive and lesser or equal to " + (string cardCount)

let pickCards count deck =
    let pickedPositions = pickPositions count (System.Random()) []
    let cards = List.toArray deck
    List.map (fun pos -> cards.[pos]) pickedPositions

let combinations size aList = 
    let rec pairHeadAndTail acc bList = 
        match bList with
        | [] -> acc
        | x::xs -> pairHeadAndTail (List.Cons ((x,xs),acc)) xs
    let remainderAfter = aList |> pairHeadAndTail [] |> Map.ofList
    let rec comboIter n acc = 
        match n with
        | 0 -> acc
        | _ -> 
            acc
            |> List.fold (fun acc alreadyChosenElems ->
                match alreadyChosenElems with
                | [] -> aList //Nothing chosen yet, therefore everything remains.
                | lastChoice::_ -> remainderAfter.[lastChoice]
                |> List.fold (fun acc elem ->
                    List.Cons (List.Cons (elem,alreadyChosenElems),acc)
                ) acc
            ) []
            |> comboIter (n-1)
    comboIter size [[]]

let findSets () =
    pickCards 81 deck
    |> combinations 3
    |> List.filter (fun cards -> isSet cards.[0] cards.[1] cards.[2])
    |> List.length
    


[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!!!"
    0
