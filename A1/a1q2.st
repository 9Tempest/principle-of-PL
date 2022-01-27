Object subclass: RunningSum [

    | sum sumSq samples |

    RunningSum class >> new [
        | r |
        r := super new.
        r init.
        ^ r
    ]

    init [
        sum := 0.
        sumSq := 0.
        samples := 0.
    ]

    updateWith: aNumber [
        | num |
        num := (aNumber asFloat).
        sum := sum + num.
        samples := samples + 1.
        sumSq := sumSq + (num raisedTo: 2).
    ]
    mean [
        ^ sum / samples.
    ]

    stdev [
        ^ (sumSq - (2 * (self mean) * sum) + (samples * ((self mean) raisedTo: 2)) / samples) sqrt.
    ]
]

