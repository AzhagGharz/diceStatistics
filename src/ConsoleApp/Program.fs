open Types
open StaticDiceResults
open TestUtilities

let possibleDiceResults dice =
  match dice with
  | Dice.Boost -> diceBoost
  | Dice.Ability -> diceAbility
  | Dice.Proficiency -> diceProficiency
  | Dice.Setback -> diceSetback
  | Dice.Difficulty -> diceDifficulty
  | Dice.Challenge -> diceChallenge
  | _ -> failwithf "%s is not a known dice type" (dice.ToString())

let addDiceResults (results: RollResult list) = List.reduce (+) results

let traverseDiceSet diceResults =
  let multiplyPossibleResults (fst: RollResult seq) (snd: RollResult seq) =
    seq {
      for fstRes in fst do
        yield! seq { for sndRes in snd -> fstRes + sndRes }
    }
  // let multiplyPossibleResults (fst: RollResult list) (snd: RollResult list) : RollResult list =
  //   let partialResults =
  //     fst
  //     |> List.map (fun fstRes -> async { return List.map (fun sndRes -> fstRes + sndRes) snd })
  //     |> fun res -> Async.Parallel(res, 8)
  //     |> Async.RunSynchronously

  //   List.fold
  //     (fun acc elemArray -> List.fold (fun acc2 elem -> elem :: acc2) acc elemArray)
  //     []
  //     (Array.toList partialResults)

  let rec generate dices acc =
    match dices with
    | dice :: tail -> generate tail (multiplyPossibleResults acc dice)
    | [] -> acc

  generate diceResults [ emptyResult ]

let runTest dicePool =
  let testExample = dicePool |> List.map possibleDiceResults |> traverseDiceSet

  printfn "Got %d results" (Seq.length testExample)

for i in seq { 10..10 } do
  timeMeasuredFunction (fun _ -> runTest (List.init i (fun _ -> Dice.Ability)))
