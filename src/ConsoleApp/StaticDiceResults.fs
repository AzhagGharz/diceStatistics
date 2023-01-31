module StaticDiceResults

open Types

let listInitResult length result = List.init length (fun _ -> result)

let diceBoost =
  (listInitResult 2 emptyResult)
  @ [ { emptyResult with Successes = 1 }
      { emptyResult with Advantages = 1 }
      { emptyResult with
          Advantages = 1
          Successes = 1 }
      { emptyResult with Advantages = 2 } ]

let diceAbility =
  (listInitResult 1 emptyResult)
  @ (listInitResult 2 { emptyResult with Advantages = 1 })
    @ (listInitResult 2 { emptyResult with Successes = 1 })
      @ [ { emptyResult with
              Advantages = 1
              Successes = 1 }
          { emptyResult with Successes = 2 }
          { emptyResult with Advantages = 2 } ]

let diceProficiency =
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

let diceSetback =
  (listInitResult 2 emptyResult)
  @ (listInitResult 2 { emptyResult with Disadvantages = 1 })
    @ (listInitResult 2 { emptyResult with Failures = 1 })

let diceDifficulty =
  (listInitResult 1 emptyResult)
  @ (listInitResult 2 { emptyResult with Disadvantages = 2 })
    @ (listInitResult 2 { emptyResult with Failures = 2 })
      @ (listInitResult 3 { emptyResult with Disadvantages = 1 })
        @ [ { emptyResult with
                Disadvantages = 1
                Successes = 1 }
            { emptyResult with Advantages = 2 } ]

let diceChallenge =
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
