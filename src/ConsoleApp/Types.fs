module Types

type Dice =
  | Boost = 0
  | Ability = 1
  | Proficiency = 2
  | Setback = 3
  | Difficulty = 4
  | Challenge = 5

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
    |> String.concat System.Environment.NewLine

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
