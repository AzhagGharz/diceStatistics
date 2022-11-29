open System

type Dice =
  | Boost = 0
  | Ability = 1
  | Proficiency = 2
  | Setback = 3
  | Difficulty = 4
  | Challenge = 5
  | TestDice1 = 6
  | TestDice2 = 7

type RollResult =
  { Successes: int
    Failures: int
    Advantages: int
    Disadvantages: int
    Triumphs: int
    Despairs: int }

  static member (+)(addend1, addend2) =
    { Advantages = addend1.Advantages + addend2.Advantages
      Successes = addend1.Successes + addend2.Successes
      Triumphs = addend1.Triumphs + addend2.Triumphs
      Disadvantages = addend1.Disadvantages + addend2.Disadvantages
      Failures = addend1.Failures + addend2.Failures
      Despairs = addend1.Despairs + addend2.Despairs }

  member this.prettyPrint() =
    [ ("Advantage", this.Advantages)
      ("Successes", this.Successes)
      ("Triumphs", this.Triumphs)
      ("Disadvantages", this.Disadvantages)
      ("Failures", this.Failures)
      ("Despairs", this.Despairs) ]
    |> List.filter (fun (_, counter) -> counter <> 0)
    |> List.map (fun (name, value) -> sprintf "%s: %d" name value)
    |> String.concat Environment.NewLine

let emptyResult: RollResult =
  { Successes = 0
    Failures = 0
    Advantages = 0
    Disadvantages = 0
    Triumphs = 0
    Despairs = 0 }

type SimplifiedRollResult =
  { HasSucceded: bool
    SummedResult: RollResult
    OriginalResult: RollResult }

let listInitResult length result = List.init length (fun _ -> result)

let possibleDiceResults dice =
  match dice with
  | Dice.Boost ->
    (listInitResult 2 emptyResult)
    @ [ { emptyResult with Successes = 1 }
        { emptyResult with Advantages = 1 }
        { emptyResult with
            Advantages = 1
            Successes = 1 }
        { emptyResult with Advantages = 2 } ]
  | Dice.Ability ->
    (listInitResult 2 emptyResult)
    @ (listInitResult 2 { emptyResult with Advantages = 1 })
      @ (listInitResult 2 { emptyResult with Successes = 1 })
        @ [ { emptyResult with
                Advantages = 1
                Successes = 1 }
            { emptyResult with Successes = 2 }
            { emptyResult with Advantages = 2 } ]
  | Dice.Proficiency ->
    (listInitResult 1 emptyResult)
    @ (listInitResult 2 { emptyResult with Successes = 1 })
      @ (listInitResult 2 { emptyResult with Successes = 2 })
        @ (listInitResult 2 { emptyResult with Advantages = 2 })
          @ (listInitResult
               3
               { emptyResult with
                   Advantages = 1
                   Successes = 1 })
            @ [ { emptyResult with Advantages = 1 }; { emptyResult with Triumphs = 1 } ]
  | Dice.Setback ->
    (listInitResult 2 emptyResult)
    @ (listInitResult 2 { emptyResult with Disadvantages = 1 })
      @ (listInitResult 2 { emptyResult with Failures = 1 })
  | Dice.Difficulty ->
    (listInitResult 1 emptyResult)
    @ (listInitResult 2 { emptyResult with Disadvantages = 2 })
      @ (listInitResult 2 { emptyResult with Failures = 2 })
        @ (listInitResult 3 { emptyResult with Disadvantages = 1 })
          @ [ { emptyResult with
                  Disadvantages = 1
                  Successes = 1 }
              { emptyResult with Advantages = 2 } ]
  | Dice.Challenge ->
    (listInitResult 1 emptyResult)
    @ (listInitResult 2 { emptyResult with Disadvantages = 1 })
      @ (listInitResult 2 { emptyResult with Disadvantages = 2 })
        @ (listInitResult 2 { emptyResult with Failures = 1 })
          @ (listInitResult 2 { emptyResult with Failures = 2 })
            @ (listInitResult
                 2
                 { emptyResult with
                     Disadvantages = 1
                     Failures = 1 })
              @ [ { emptyResult with Despairs = 1 } ]
  // TODO: remove test implementation
  | Dice.TestDice1 ->
    [ emptyResult
      { emptyResult with Advantages = 100 }
      { emptyResult with Successes = 100 } ]
  | Dice.TestDice2 -> [ emptyResult; { emptyResult with Advantages = 10 } ]
  | _ -> failwithf "%s is not a known dice type" (dice.ToString())

let addDiceResults (results: RollResult list) = List.reduce (+) results
// { Advantages = List.sumBy (fun r -> r.Advantages) results
//   Successes = List.sumBy (fun r -> r.Successes) results
//   Triumphs = List.sumBy (fun r -> r.Triumphs) results
//   Disadvantages = List.sumBy (fun r -> r.Disadvantages) results
//   Failures = List.sumBy (fun r -> r.Failures) results
//   Despairs = List.sumBy (fun r -> r.Despairs) results }

let traverseDiceSet (dices: Dice list) =
  let multiplyPossibleResults (fst: RollResult seq) (snd: RollResult seq) =
    seq {
      for fstRes in fst do
        yield! seq { for sndRes in snd -> fstRes + sndRes }
    }

  let rec generate dices acc =
    match dices with
    | dice :: tail -> generate tail (multiplyPossibleResults acc (possibleDiceResults dice))
    | [] -> acc
  // let generate dices =
  //   seq {
  //     yield possibleDiceResults dices.Item
  //   }
  generate dices [ emptyResult ]

let testExample = traverseDiceSet [ Dice.TestDice1; Dice.TestDice2; Dice.TestDice2 ]
printfn "Got %d results" (Seq.length testExample)

for result in testExample do
  printfn "Result:"
  printfn "%s%s" (result.prettyPrint ()) Environment.NewLine

printfn "End"
