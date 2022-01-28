
Object subclass: Node [
    |val left right|

    Node class >> new: aNumber [
        | r |
        r := super new.
        r init: aNumber.
        ^ r
    ]

    init: aNumber [
        val := (aNumber asFloat).
        left := nil.
        right := nil.
    ]

    getVal [
        ^val.
    ]

    getLeft [
        ^left.
    ]

    getRight [
        ^right.
    ]

    setLeft: leftNode [
        left = leftNode.
    ]

    setRight: rightNode [
        right = rightNode.
    ]

    add: aNumber [
        (aNumber > val) ifTrue: [
            right ~= nil ifTrue: [
                right add: aNumber.
            ] ifFalse: [
                right := Node new: aNumber.
            ].
        ].
        (aNumber < val)  ifTrue: [
            left ~= nil ifTrue: [
                left add: aNumber.
            ]   ifFalse: [
                left := Node new: aNumber.
            ].
        ].  
    ]

    displayString [
        | r |
        r := val displayString.
        left ~= nil ifTrue: [
            r := '(', left displayString, ') ', r.
        ].
        right ~= nil ifTrue: [
            r := r, ' (', right displayString, ')'.
        ].
        ^r.
    ]

    do: block [
        left ~= nil ifTrue: [ left do: block ].
        val := block value: val.
        right ~= nil ifTrue: [ right do: block ].
    ]

    reduce: block [
        |r lst i|
        left == nil ifTrue: [ r := val. ] ifFalse: [r := block value: (left reduce: block) value: val.].
        right == nil ifTrue: [^r].
        lst := OrderedCollection new.
        right do: [ :a | lst add: a ].
        i := 1.
        [ i <= lst size ] whileTrue: [
            r := block value: r value: (lst at: i).
            i := i+1.
        ].
        ^r.
    ]

]


Object subclass: SortedTree [
    | root |

    SortedTree class >> new: aNumber [
        | r |
        r := super new.
        r init: aNumber.
        ^ r.
    ]

    init: aNumber [
        root := Node new: aNumber.
    ]

    add: aNumber [
        root add: aNumber.
    ]

    displayString [
        ^root displayString.
    ]

    printString [
        ^self displayString.
    ]

    do: block [
        root do: block.
    ]

    reduce: block [
        ^root reduce: block.
    ]




    
]