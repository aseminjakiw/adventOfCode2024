seq {
    for i in 1..9 do
        let divisor = (pown 10L i)
        let lower = (pown 10L ((i * 2) - 1))
        let upper = lower * 10L - 1L

        let divisorStr = divisor.ToString("N0")
        let lowerStr = lower.ToString("N0")
        let upperStr = upper.ToString("N0")
        yield lower, upper, divisor
}
|> Seq.toList

[ 2097446912
  14168
  4048
  2
  0
  2
  4
  40
  48
  2024
  40
  48
  80
  96
  2
  8
  6
  7
  6
  0
  3
  2 ]
|> List.sort
