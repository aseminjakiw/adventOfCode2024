seq{
    for i in 1..9 do
        let divisor = (pown 10L i)
        let lower = (pown 10L ((i*2)-1))
        let upper = lower * 10L - 1L
        
        let divisorStr =divisor.ToString("N0")
        let lowerStr = lower.ToString("N0")
        let upperStr = upper.ToString("N0")
        yield lower, upper, divisor
}
|> Seq.toList