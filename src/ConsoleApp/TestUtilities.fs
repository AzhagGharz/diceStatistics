module TestUtilities

let timeMeasuredFunction f =
  let timer = System.Diagnostics.Stopwatch.StartNew()
  f ()
  timer.Stop()
  printfn "Took %fms" timer.Elapsed.TotalMilliseconds
