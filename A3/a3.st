


Object subclass: Hasntkell [
    |exp globals|
    Hasntkell class >> new: exp withGlobals: dict[
        | r |
        r := super new.
        r init: exp withGlobals: dict.
        ^r
    ]

    init: sexp withGlobals: dict[
        exp := sexp.
        globals := dict.
    ]

    eval [
        |res|
        res := self reduce.
        exp := nil.
        ^res.
    ]

    reduce [
        exp isValue ifFalse: [
            |block|
            block := [:e |
                |globalsVar|
                globalsVar := globals.
                e noeOneStep: globalsVar.
            ].
            exp := exp reduceWith: block.
        ].
        ^exp.
    ]


]

HasntkellIO extend [
    noeOneStep: globals [
        ^nil.
    ]
]

HasntkellError extend [
    noeOneStep: globals [
        ^nil.
    ]
]

HasntkellAbs extend [

    noeOneStep: globals [
        ^nil.
    ]

    "substitute var with exp"
    substitute: exp name:sname [
        (var = sname) ifFalse:[
            |hasKey|
            hasKey := (exp freeVars) includesKey: var.
            hasKey ifFalse: [
                ^ HasntkellAbs withVar: var body: (body substitute: exp name:sname).
            ]   ifTrue: [
                |nvar nbody nlamVar|
                nvar := var , (self hash asString).
                nlamVar := HasntkellVar withName: nvar.
                nbody := body substitute: nlamVar name: var.
                ^ HasntkellAbs withVar: nvar body: (nbody substitute: exp name:sname).
            ].
        ]   ifTrue: [
            ^self dup.
        ].
    ]

]

HasntkellApp extend [
    "substitute var with exp"
    substitute: exp name:sname[
        ^HasntkellApp withRator: (rator substitute: exp name: sname) rand: (rand substitute: exp name: sname).
    ]

    noeOneStep: globals [
        (rator isError) ifTrue: [   "two cases for error"
            ^rator.
        ].
        (rand isError) ifTrue: [
            ^rand.
        ].
        (rator isIOAbs) ifTrue:[
            ^(rator apply: rand).
        ].
        (rator isAbs) ifTrue: [             "If rator is abs; direct substitute"
            """
            |var nVar nExpr nVarExpr res hadKey| 
            var := rator var.
            nVar := var , (self hash asString).
            nVarExpr := HasntkellVar withName: nVar.    
            nExpr := rand substitute: nVarExpr name: var. 
            globals at: var put: nExpr.
            res := (rator body) noeOneStep: globals.    
            hadKey := globals includesKey: var.
            hadKey ifTrue: [
                globals removeKey: var.
            ].
            res isNil ifTrue:[
                 ^((rator body) substitute: rand name: var).
            ]   ifFalse:[
                ^HasntkellApp withRator: (HasntkellAbs withVar: var body: res) rand: rand.
            ].
            """
            ^((rator body) substitute: rand name: (rator var)).
        ]   ifFalse: [
            |e1 e2|
            e1 := rator noeOneStep: globals.
            e1 isNil ifFalse: [             "reduce left"
                ^HasntkellApp withRator: e1 rand:rand.
            ]   ifTrue: [                   "reduce right"
                e2 := rand noeOneStep: globals.
                e2 isNil ifFalse: [
                    ^HasntkellApp withRator: rator rand:e2.
                ]   ifTrue: [
                    ^nil.   
                ].
            ].
        ].
    ]
]

HasntkellNum extend [

    noeOneStep: globals [
        ^nil.
    ]
    "substitute var with exp"
    substitute: exp name:sname[
        ^self dup.
    ]
]

HasntkellBoolLiteral extend [
    noeOneStep: globals [
        ^nil.
    ]
    "substitute var with exp"
    substitute: exp name:sname[
        ^self dup.
    ]
]

HasntkellVar extend [

    "substitute var with exp"
    substitute: exp name:sname [
        name = sname ifTrue: [
            ^exp.
        ].
        ^self dup.
    ]

    noeOneStep: globals [
        |hadKey|
        hadKey := globals includesKey: name.
        hadKey ifTrue: [
            ^globals at: name.
        ].
        ^nil.
    ]
]

HasntkellIf extend [

    "substitute var with exp"
    substitute: exp name:sname [
        ^HasntkellIf withCondition: (condition substitute: exp name:sname) thenExp: (thenExp substitute: exp name:sname) elseExp: (elseExp substitute: exp name:sname).
    ]

    noeOneStep: globals [
        |condRes|
        condition isTrue ifTrue: [
            ^thenExp.
        ].
        condition isFalse ifTrue: [
            ^elseExp.
        ].
        condRes := condition noeOneStep: globals.
        condRes isNil ifFalse: [
            ^ HasntkellIf withCondition: condRes thenExp:thenExp elseExp: elseExp.
        ].
        ^ nil.
    ]
]

HasntkellNumExp extend [
    "substitute var with exp"
    substitute: exp name:sname [
        ^HasntkellNumExp withOp: op left: (left substitute: exp name:sname) right: (right substitute: exp name:sname).
    ]

    noeOneStep: globals [
        |res|
        left isNum ifTrue: [
            right isNum ifTrue: [
                (op = '+') ifTrue: [
                    ^HasntkellNum withValue: (left value) + (right value).
                ].
                (op = '-') ifTrue: [
                    |resVal|
                    resVal := (left value) - (right value).
                    (resVal < 0) ifTrue: [  "natural number"
                        resVal := 0.
                    ].
                    ^HasntkellNum withValue: resVal.
                ].
                (op = '*') ifTrue: [
                    ^HasntkellNum withValue: (left value) * (right value).
                ].
                (op = '=') ifTrue: [
                    (left value) = (right value) ifTrue: [
                        ^HasntkellTrue new.
                    ]   ifFalse: [
                        ^HasntkellFalse new.
                    ]
                ].
                ^ nil.
            ]   ifFalse: [
                res := right noeOneStep: globals.
                res isNil ifFalse: [
                    ^HasntkellNumExp withOp: op left: left right: res.
                ].
                ^nil.
            ].
        ]   ifFalse: [
                res := left noeOneStep: globals.
                res isNil ifFalse: [
                    ^HasntkellNumExp withOp: op left: res right: right.
                ].
                ^nil.
        ].
    ]
]

HasntkellError extend [
    "substitute var with exp"
    substitute: exp name:sname[
        ^self dup.
    ]
]

Object subclass: HasntkellRefLib [
    HasntkellRefLib class >> globals: dict [
        dict at: 'ref' put: (HasntkellIOAbs withConstructor: HasntkellRef).
        dict at: 'get' put: (HasntkellIOAbs withConstructor: HasntkellGet).
        dict at: 'put' put: (HasntkellIOAbs withConstructor: HasntkellCurryPut).
        ^dict
    ]
]


HasntkellIO subclass: HasntkellRef [
    |exp|
    HasntkellRef class >> new: sexp [
        | r |
        r := super new.
        r init: sexp.
        ^r.
    ]

    init: sexp [
        exp := sexp.
    ]

    dup [
        ^HasntkellRef new: exp.
    ]
    performIO: globals heap: heap [
        |e v l|
        e := Hasntkell new: exp dup withGlobals: globals.
        v := e eval.
        l := HasntkellLabel withValue: (self hash asString).
        heap at: l value put: v.
        ^l.
    ]
]

HasntkellIO subclass: HasntkellGet [
    |exp|
    HasntkellGet class >> new: sexp [
        | r |
        r := super new.
        r init: sexp.
        ^r.
    ]

    init: sexp [
        exp := sexp.
    ]

    dup [
        ^HasntkellGet new: exp.
    ]
    performIO: globals heap: heap [
        |e v l|
        e := Hasntkell new: exp dup withGlobals: globals.
        l := e eval.
        "assume l is a HasntkellLabel"
        ^(heap at: l value).
    ]
]

HasntkellIOAbs subclass: HasntkellCurryPut [
    |left|
    HasntkellCurryPut class >> new: sleft[
        | r |
        r := super new.
        r init: sleft.
        ^r.
    ]

    init: sleft[
        left := sleft.
    ]

    dup [
        ^HasntkellCurryPut withLeft: left
    ]
    apply: exp [
        ^HasntkellPut withLeft: left right: exp.
    ]

    substitute: exp name:sname[
        ^self dup.
    ]
]

HasntkellIO subclass: HasntkellPut [
    |left right|
    HasntkellPut class >> withLeft: sleft right: sright [
        | r |
        r := super new.
        r init: sleft right: sright.
        ^r.
    ]

    init: sleft right:sright [
        left := sleft.
        right := sright.
    ]

    dup [
        ^HasntkellPut withLeft: left right: right.
    ]

    performIO: globals heap: heap[
        |e1 e2 v l|
        e1 := Hasntkell new: left dup withGlobals: globals.
        e2 := Hasntkell new: right dup withGlobals: globals.
        l := e1 eval.
        v := e2 eval.
        heap at: l value put: v.
        ^l.
    ]

    substitute: exp name:sname[
        ^self dup.
    ]
]

HasntkellExp subclass: HasntkellLabel [
    | value |
    HasntkellLabel class >> withValue: value [
        | r |
        r := super new.
        r init: value.
        ^r
    ]

    init: svalue [
        value := svalue.
    ]

    dup [
        ^HasntkellLabel withValue: value
    ]

    isValue [ ^true ]

    value [ ^value ]

    displayString [
        ^value asString
    ]

    substitute: exp name:sname[
        ^self dup.
    ]
]


