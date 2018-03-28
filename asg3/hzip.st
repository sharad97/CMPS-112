#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

"Sharad Shrestha"
"sshrest3"

nlines := Character nl.
line := SortedCollection new.

"modified catFile.st provided by professor"
Character extend [
    egt:=8.
    isPrint [
        ^ (Character space < self) & (self <= $~)
    ]
    visible [
        self isPrint 
        ifTrue: [^ self asString]
        ifFalse: [^ self asInteger printStringRadix: egt]
    ]
]               

"modified catfile.st provided by professor"
FileStream subclass: streamfile [
    more:=1.
    zero:=0.
    fiv:=5.
    nlines := Character nl.
    catFile [
        |countnum txtline|
        countnum := zero.
        [self atEnd not] whileTrue: [
            countnum := countnum + more.
            txtline :=self nextLine.
            countnum asString size to: fiv do: [:skip| stdout << ' '].
            stdout << countnum << ': ' << txtline << nlines.
        ]
    ]
]

"modified tree.st provided by professor"
Object subclass: Leaf [
    |char countnum isleaf|
    
    char [ ^ char ]
    countnum [ ^ countnum ]
    isleaf [^ isleaf]
    
    Leaf class >> new [
        self shouldNotImplement
    ]
    
    Leaf class >> new: aChar countnum: aCount [
        |result|
        result := super new.
        result setChar: aChar andCount: aCount.
        ^result
    ]
    
    setChar: aChar andCount: aCount [
        char := aChar.
        countnum := aCount.
        isleaf:=true.
    ]
    
    <= other [
        ^ (countnum < other countnum)
        | ((countnum = other countnum) & (char <= other char))
    ]
    
    printBase: aStream [
        ^ aStream << self class << '(' << char << ',' << countnum
    ]
    
    printOn: aStream [
    (self printBase: aStream) << ')'.
    ]
    
    inorder: visitor prefix: string [
        visitor value: char value: string.
    ]
    
    postorder: visitor [
        visitor  value:char value: isleaf.
    ]
]

"modified tree.st provided by professor"
Leaf subclass: Tree [
    |left right|
    
    Tree class >> new: aChar countnum: aCount [
        self shouldNotImplement
    ]
    
    Tree class >> new: aChar countnum: aCount left: aLeft right: aRight [
        |result|
        result := super new: aChar countnum: aCount.
        result setLeft: aLeft andRight: aRight.
        ^ result
    ]
    
    setLeft: aLeft andRight: aRight [
        left := aLeft.
        right := aRight.
        isleaf:=false.
    ]
    
    left  [ ^ left]
    right [ ^ right]
    
    printOn: aStream [
        (self printBase: aStream) << ',' << left << ',' << right << ')'.
    ]

    inorder: visitor prefix: string [
        left inorder: visitor prefix: string, '0'.
        right inorder: visitor prefix: string, '1'.
    ]
    
    postorder: visitor[      
        left postorder: visitor .
        right postorder: visitor .
        visitor value: char value:isleaf.
    ]
]

"modified charfreq.st provided by professor"
Object subclass: ZeroArray [
    more:=1.
    zero:=0.
    |theArray|
    
    ZeroArray class >> new [
        self shouldNotImplement.
    ]
    ZeroArray class >> new: size [
        |result|
        result := super new.
        result init: size.
        ^ result
    ]
    init: size [
        theArray := Array new: size.
    ]            
    size [
        ^ theArray size.
    ]   
    at: index [
        ^ theArray at: index + more.
    ]
    at: index put: value [
        ^ theArray at: index + more put: value.
    ]
    incr: index [
        (self at: index) isNil
        ifTrue: [ self at: index put: zero].
        self at: index put: (self at: index) + more.
    ]
    KeyValues: aBlock [
        (zero to: (self size) - more) do: [:index |
        aBlock value: index value: (self at: index).
        ]
    ]
]

more:=1.
less:=-1.
egt:=8.
fiv:=5.
array1:=257.
array2:=256.
zero:=0.
nine:=9.
b1 := '000000000'.
b2 := '000000001'.
decsize:=-48.


frequecytbl := ZeroArray new: array1.
encodetable := ZeroArray new: array1.
frequecytbl at:array2 put:more.

oD := false.
oC := false.
oT := false.
oU := false.

"modified outbit.st provided by professor"
Object subclass: BitStream [
    more:=1.
    nine:=9.
    less:=-1.
    |bitIndex byte myStream|
    BitStream class >> new: streamfile [
        |result|
        result := super new.
        result init: streamfile.
        ^result
    ]
    init: streamfile [
        myStream := streamfile.
        bitIndex := more.
    ]
    nextBit [
        bitIndex = more
        ifTrue: [
        byte := myStream next.
        bitIndex := nine.
        ].
    
        bitIndex := bitIndex less.
        ^byte value bitAt: bitIndex
    ]
    atEnd [
        ^bitIndex = more and: [myStream atEnd]
    ]
]

"modified outbit.st provided by professor"
Object subclass: OutBits [
    less:=-1.
    egt:=8.
    zero:=0.
    |bitIndex currentByte myStream|
    OutBits class >> new [
        self shouldNotImplement.
    ]
    OutBits class >> new: streamfile [
        |result|
        result := super new.
        result init: streamfile.
        ^result
    ]
    clearByte [
        bitIndex := egt.
        currentByte := zero.
    ]
    init: streamfile [
        myStream := streamfile.
        self clearByte.
    ]
    goo [
        myStream nextPutByte: currentByte.
        self clearByte.
    ]
    
    writeBit: bit [
        currentByte := currentByte bitAt: bitIndex put: bit.
        bitIndex := bitIndex - 1.
        bitIndex = zero
        ifTrue: [
        self goo].
    ]
]

"for following code got help from TA and idea from professor"

|input1 input2 output outfile1 argnum|

argnum:=zero.

Smalltalk arguments: '-d -t -c -u' do: [
    :opt :arg |
    (opt = $d) 
    ifTrue: [
        oD := true.
    ].
    
    (opt = $t) 
    ifTrue: [
        oT := true.
    ].
    
    (opt = $c) 
    ifTrue: [
        oC := true.
    ].
    
    (opt = $u) 
    ifTrue: [
        oU := true.
    ].
    
    (arg = nil)
    ifFalse: [
        argnum = zero 
        ifFalse:[
            oD 
            ifTrue:[
                stdout <<'output: ' << arg <<nlines.
            ].
            output := streamfile open: arg
                    mode: streamfile write.
        ]
        ifTrue:[
            oD 
            ifTrue:[
                stdout <<'input: ' << arg <<nlines.
            ].
             
            input1 := streamfile open: arg 
                    mode: streamfile read.
            
            input2 := streamfile open: arg
                    mode: streamfile read.
        ].
        argnum := argnum + more.
    ].
].

(oU)
ifTrue:[
    |IsEOF transfer stack|
    IsEOF:=false.
    oD 
    ifTrue: [
        stdout<<'runing decompression'<<nlines.
    ].
    transfer := OutBits new: output.  
    stack := OrderedCollection new.
    fileBits := BitStream new: input1.
    treebuilt:=false.
    remaketree:=''.
    node:=''.
    [(fileBits atEnd not) & (IsEOF not)] whileTrue:[
        treenode:=fileBits nextBit.
        leafchar:=zero.
        treebuilt 
        ifTrue:[
            node isleaf 
            ifFalse:[
                treenode=zero 
                ifFalse:[
                    node:=node right.
                    node isleaf ifTrue:[
                        IsEOF not ifTrue:[
                            ((node char)=array2) ifTrue:[
                                IsEOF:=true.
                            ] 
                            ifFalse:[
                                output=nil ifTrue:[
                                    stdout nextPut: (Character value: (node char)).
                                ]
                                ifFalse:[
                                    egt to: more by: less do: [
                                        :index |
                                        bit:= (node char) bitAt: index.
                                        transfer writeBit: bit.
                                    ].
                                ].
                            node:=remaketree.

                            ].
                        ].
                    ]
                ]
                ifTrue:[
                    node:=node left.

                    node isleaf 
                    ifTrue:[
                        IsEOF not 
                        ifTrue:[  
                            ((node char)=array2) 
                            ifFalse:[
                                output=nil 
                                ifFalse:[
                                    egt to: more by: less do: [
                                        :index |
                                        bit:= (node char) bitAt: index.
                                        transfer writeBit: bit.
                                    ].
                                ]
                                ifTrue:[
                                stdout nextPut: (Character value: (node char)).
                                ].
                                node:=remaketree.
                            ]
                            ifTrue:[
                                IsEOF:=true.
                            ].
                        ].
                    ]
                ]. 
            ]
            ifTrue:[
                ((node char)=array2) 
                ifFalse:[
                    stdout nextPut: (Character value: (node char)).
                    node:=remaketree.
                ]
                ifTrue:[
                    IsEOF:=true.
                ].
            ]
        ]
        ifFalse:[
            treenode=zero 
            ifTrue:[
                egt to:more by:less do:[
                    :i|
                    bit:=fileBits nextBit.
                    leafchar:=leafchar bitAt:i put:bit.
                ].
                
                leafchar=zero 
                ifTrue:[
                    specialbit:=fileBits nextBit.
                    specialbit= more
                    ifTrue:[
                        leafchar:=leafchar bitAt:9 put:more.
                    ].
                ].
         
                stack add: (Leaf new: leafchar countnum: '').

            ].

            treenode=more
            ifTrue:[
                stack size>more 
                ifFalse:[
                    treebuilt:=true.
                    remaketree:=stack removeLast.
                    node:=remaketree.
                ]
                ifTrue:[
                    firstr:=stack removeLast.
                    firstl:=stack removeLast.
           
                    newtree := (Tree new: less
                                countnum: ''
                                left: firstl right: firstr).
                    stack add: newtree.
                ].
            ].
        ].
    ].
]. 

|dtree exits|
exits:=zero. 


(oU)
ifFalse:[
    [input1 atEnd not] whileTrue: [
        |char1|
        char1 := input1 next asInteger.
        frequecytbl incr: char1.
    ].
    
    frequecytbl KeyValues: [:key :countnum |
        (countnum notNil and: [countnum > zero])
        ifTrue: [
            line add: (Leaf new: key countnum: countnum).
        ]
    ].

    [line size > more ] whileTrue: [
        |firstl firstr|
        firstl := line removeFirst.
        firstr := line removeFirst.
        newtree := (Tree new: (firstl char)
                    countnum: (firstl countnum) + (firstr countnum)
                    left: firstl right: firstr).
        line add: newtree.
    ].
    
    dtree:=line removeFirst.
    
    dtree inorder: [:char :string |
        encodetable at: char put: string.
    ] prefix: ''.
    
    btree:=''.
    dtree postorder: [:char :isleaf |
        isleaf  
        ifTrue:[
            bin:=''.
            char = array2
            ifFalse:[
                char=zero
                ifFalse:[
                    egt to:more by:less do:[:i|
                        bin:=bin,((char bitAt:i)printString).
                    ]
                ]
                ifTrue:[bin:= b1.].                        
            ]
            ifTrue:[bin:= b2.].
            btree:=(btree,'0',bin).
        ]
        ifFalse:[btree:=btree,'1'.].
    ].

    oT 
    ifTrue:[
        stdout <<nlines<< 'tree decoder' << nlines.
        stdout << dtree << nlines<<nlines.
        stdout <<'decoding binary tree: '<<nlines<<btree << nlines.

        stdout <<nlines<< 'tree encoder' << nlines.
        encodetable KeyValues: [:key :countnum |
            (countnum notNil and: [countnum > zero]) 
            ifTrue: [
                stdout << key asCharacter visible << '     '
                << (frequecytbl at: key) <<'    '<<countnum
                << nlines.
            ].
        ].
    ].

    oC  
    ifTrue:[ 
        output~=nil 
        ifFalse:[
            oD 
            ifTrue:[
                stdout <<nlines<< 'oC = '<<oC
                <<'output file missing'<< nlines.
            ].
            stdout <<nlines<< 'input encoded in binary' << nlines.
            stdout <<btree<<more.
            [input2 atEnd not] whileTrue: [
                |char2|
                char2 := input2 next asInteger.
                code:= (encodetable at: char2).
                stdout <<code. 
            ].
            eof:= (encodetable at: array2).
            stdout<<eof.
        ]
        ifTrue:[
            oD 
            ifTrue: [
                stdout <<nlines<< 'oC = '<<oC 
                <<'output file exist'<< nlines.
            ].
            
            transfer := OutBits new: output.    
            more to: (btree size) by: more do: [:index |
                bit:= ((btree at: index) asInteger)- decsize.
                transfer writeBit: bit.
            ].

            transfer writeBit: more.

            [input2 atEnd not] whileTrue: [
                |char2|
                char2 := input2 next asInteger.
                code:= (encodetable at: char2). 
                more to: (code size) by: more do: [:index |
                    bit:= ((code at: index) asInteger)- decsize.
                transfer writeBit: bit.
                ].
            ].
        
            eof:= (encodetable at: array2).
            more to: (eof size) by: more do: [
                :index |
                bit:= ((eof at: index) asInteger)- decsize.
                transfer writeBit: bit.
            ].
            transfer goo.
        ].   
    ].
].


input1 close.
output~=nil 
ifTrue:[
    output close.
].

oD 
ifTrue: [
    stdout<<nlines<<'Quit: '<<exits<<nlines.
    stdout<<'Complete'<<nlines.
].