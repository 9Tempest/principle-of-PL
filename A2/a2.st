"Part A"
Object subclass: Lambda [
    |exp|

    Lambda class >> new: exp[
        | r |
        r := super new.
        r init: exp.
        ^r
    ]

    init: sexp[
        exp := sexp.
    ]

    toDeBruijn[
        ^ exp toDeBruijn: (Dictionary new).
    ]

    displayString [
        ^exp displayString.
    ]
]

LambdaVar extend [
    toDeBruijn: map [
        |hadKey|
        hadKey := map includesKey: name.
        hadKey ifTrue: [
            ^LambdaVar withName: ((map at: name) asString).
        ]
    ]
]

LambdaAbs extend [
    toDeBruijn: map [
        map keysAndValuesDo: [:key :value|
            map at: key put: (value+1).
        ].
        map at: var put: 1.
        ^LambdaAbs withVar: nil body: (body toDeBruijn: map).
    ]
]

LambdaApp extend [
    toDeBruijn: map [
        |newMap|
        newMap := (Dictionary new).
        map keysAndValuesDo: [:key :value|
            newMap at: key put: (value).
        ].
        ^LambdaApp withRator: (rator toDeBruijn: map) rand: (rand toDeBruijn: newMap)
    ]
]

"Part B"
Lambda extend [
    aoe [
        exp := exp aoe.
        ^ exp.
    ]
    aoe: steps [
        |f ssteps|
        ssteps := steps.
        f := [:e |
            e aoe.
        ].
        exp := exp reduceWith: f steps: ssteps.
        ^exp.
    ]
]

LambdaAbs extend [
    aoe [
        ^nil.
    ]

    substitute: exp name:sname [
        (var = sname) ifFalse:[
            |hasKey|
            hasKey := (exp freeVars) includesKey: var.
            hasKey ifFalse: [
                ^ LambdaAbs withVar: var body: (body substitute: exp name:sname).
            ]   ifTrue: [
                |nvar nbody nlamVar|
                nvar := var , (self hash asString).
                nlamVar := LambdaVar withName: nvar.
                nbody := body substitute: nlamVar name: var.
                ^ LambdaAbs withVar: nvar body: (nbody substitute: exp name:sname).
            ].
        ]   ifTrue: [
            ^self.
        ]
    ]
    
]

LambdaVar extend [
    aoe [
        ^nil.
    ]

    substitute: exp name:sname [
        name = sname ifTrue: [
            ^exp.
        ].
        ^self.
    ]
]

LambdaApp extend [
    aoe [
        |m1 m2|
        m1 := rator aoe.
        m1 isNil ifFalse: [                 "reduce left"
            ^LambdaApp withRator: m1 rand: rand.
        ]   ifTrue: [           
            m2 := rand aoe.
            m2 isNil ifFalse: [             "reduce right"
                ^LambdaApp withRator: rator rand: m2.
            ]   ifTrue: [
                (rator isAbs) ifTrue: [     "rator is abs"
                    ^((rator body) substitute: rand name: (rator var)).
                ]  ifFalse: [
                    ^nil.
                ].
            ].
        ].
    ]

    substitute: exp name:sname[
        ^LambdaApp withRator: (rator substitute: exp name: sname) rand: (rand substitute: exp name: sname).
    ]
]

"Part C"
Lambda extend [
    nor [
        exp := exp nor.
        ^exp.
    ]

    nor: steps [
        |f ssteps|
        ssteps := steps.
        f := [:e |
            e nor.
        ].
        exp := exp reduceWith: f steps: ssteps.
        ^exp.
    ]
]

LambdaVar extend [
    nor [
        ^nil.
    ]
]

LambdaAbs extend [
    nor [
        |e|
        e := body nor.
        e isNil ifTrue: [   "body cannot be reduced"
            ^nil.
        ]   ifFalse: [      "reduce body"
            ^LambdaAbs withVar: var body: e.
        ]
    ]
]

LambdaApp extend [
    nor [
        (rator isAbs) ifTrue: [             "If rator is abs; direct substitute"
            ^((rator body) substitute: rand name: (rator var)).
        ]   ifFalse: [
            |e1 e2|
            e1 := rator nor.
            e1 isNil ifFalse: [             "reduce left"
                ^LambdaApp withRator: e1 rand:rand.
            ]   ifTrue: [                   "reduce right"
                e2 := rand nor.
                e2 isNil ifFalse: [
                    ^LambdaApp withRator: rator rand:e2.
                ]   ifTrue: [
                    ^nil.   
                ].
            ].
        ].
    ]
]

"PartD"
Lambda extend [
    eta [
        exp := exp eta.
        ^exp.
    ]

    eta: steps [
        |f ssteps|
        ssteps := steps.
        f := [:e |
            e eta.
        ].
        exp := exp reduceWith: f steps: ssteps.
        ^exp.
    ]
]

LambdaAbs extend [
    eta [
        |inner|
        inner := body eta.
        inner isNil ifTrue:[
            body isApp ifTrue:[
                |x exp|
                x := body rand.
                exp := body rator.
                x isVar ifTrue:[                "app body is variable"
                    (x name) = var ifTrue:[     "app var matched lambad var"
                    |hasKey|
                     hasKey := (exp freeVars) includesKey: var.
                        hasKey ifFalse:[
                            ^exp.
                        ].
                    ].
                    ^nil.   "name not eq or have key"
                ] ifFalse: [    "x not var"
                    ^ nil.
                ]
            ]   ifFalse:[   "body not app"
                ^ nil.
            ].
        ] ifFalse:[
            ^LambdaAbs withVar:var body:inner.
        ].
    ]
]

LambdaVar extend [
    eta [
        ^ nil.
    ]
]

LambdaApp extend [
    eta [
        |e1 e2|
        e1 := rator eta.
        e1 isNil ifTrue: [      "reduce right"
            e2 := rand eta.
            e2 isNil ifTrue: [
                ^ nil.
            ]   ifFalse: [
                ^ LambdaApp withRator:rator rand:e2.
            ].
        ]   ifFalse: [          "reduce left"
            ^ LambdaApp withRator:e1 rand:rand.
        ].
    ]
]


