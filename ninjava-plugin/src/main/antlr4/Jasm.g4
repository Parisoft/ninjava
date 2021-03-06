grammar Jasm;

file
	: (header? EOL)+ dtClass EOL*
	;

header
    : comment
    | dtImport
    | iNesMap
    | iNesPrg
    | iNesChr
    | iNesMir
    | nes2ChrRAM
    | nes2PrgRAM
    | nes2Sub
    | nes2TV
    | nes2VS
    | nes2BRAM
    | nes2ChrBRAM
    ;

directive
    : dtImport
    | dtClass
    | dtField
    | dtVar
    | dtZp
    | dtDB
    | dtDW
    | dtPrg
    | dtChr
    | dtDmc
    | dtFunction
    | dtMethod
    | dtMacro
    | dtIncbin
    | dtInclude
    ;

dtImport
    : '.import' qname comment?
    ;

dtClass
    : '.class' dtFinal? dtSingleton? qname dtExtends? dtImplements? comment? (line? EOL)+ '.endclass' comment?
    ;

dtFinal
    : '.final'
    ;

dtSingleton
    : '.singleton'
    ;

dtExtends
    : '.extends' (qname | name)
    ;

dtImplements
    : '.implements' (qname | name) (',' (qname | name))*
    ;

dtField
    : '.field' name expr comment?
    ;

dtVar
    : '.var' name expr comment?
    ;

dtZp
    : '.zp' name expr comment?
    ;

dtDB
    : '.db' expr (',' expr)* comment?
    ;

dtDW
    : '.dw' expr (',' expr)* comment?
    ;

dtPrg
    : '.prg' expr comment?
    ;

dtChr
    : '.chr' expr comment?
    ;

dtDmc
    : '.dmc' expr comment?
    ;

dtMethod
    : '.met' fname comment? EOL body '.endmet' comment?
    ;

dtFunction
    : '.fun' fname comment? EOL body '.endfun' comment?
    ;

dtMacro
    : '.mac' name params comment? EOL body '.endmac' comment?
    ;

dtInclude
    : ('.include' | 'incsrc') string comment?
    ;

dtIncbin
    : '.incbin' string comment?
    ;

dtFree
    : '.free' (name | qname | pname) comment?
    ;

dtIf
    : '.if' expr comment? EOL body dtElse
    ;

dtElse
    : '.else' comment? EOL body '.endif' comment? #Else
    | '.else' dtIf #ElseIf
    | '.endif' comment? #EndIf
    ;

dtIfDef
    : '.ifdef' (name | qname | pname) comment? EOL body '.endif' comment?
    ;

dtIfNDef
    : '.ifndef' (name | qname | pname) comment? EOL body '.endif' comment?
    ;

dtRept
    : '.rept' expr comment? EOL body '.endrept' comment?
    ;

iNesMap // Mapper number of NES ROM.
    : '.inesmap' expr comment?
    ;

iNesPrg //  Number of 16KiB PRG ROM banks in a NES ROM.
    : '.inesprg' expr comment?
    ;

iNesChr // Number of 8KiB CHR ROM banks in a NES ROM.
    : '.ineschr' expr comment?
    ;

iNesMir // Mirroring mode of a NES ROM.
    : '.inesmir' expr comment?
    ;

nes2ChrRAM // Amount of CHR RAM used by a NES ROM.
    : '.nes2chrram' expr comment?
    ;

nes2PrgRAM // Amount of PRG RAM used by a NES ROM.
    : '.nes2prgram' expr comment?
    ;

nes2Sub // Submapper number of NES ROM.
    : '.nes2sub' expr comment?
    ;

nes2TV // TV mode of NES ROM: NTSC, PAL, or both (0, 1 or 2) ('N', 'P' or 'B')
    : '.nes2tv' expr comment?
    ;

nes2VS // Sets ROM to use the Vs. Unisystem.
    : '.nes2vs' expr comment?
    ;

nes2BRAM // Amount of battery-packed PRG RAM in NES ROM.
    : '.nes2bram' expr comment?
    ;

nes2ChrBRAM // Amount of battery-packed CHR RAM in NES ROM.
    : '.nes2chrbram' expr comment?
    ;

callMacro
    : (name | qname) (expr (',' expr)*)? comment?
    ;

label
    : lname comment?
    ;

symbol
    : name '=' expr comment?
    ;

line
	: comment
	| directive
    | label
    | symbol
	;

params
    : pname*
    ;

body
    : (stmt? EOL)+
    ;

stmt
    : comment
    | dtVar
    | dtZp
    | dtFree
    | dtIf
    | dtIfDef
    | dtIfNDef
    | dtRept
    | label
    | symbol
    | callMacro
    | instruction
    ;

instruction
    : label? opcode arg? comment?
    ;

arg
    : '#' expr #Immediate
    | '(' expr ',' ('x' | 'X') ')' #IndirectX
    | '(' expr ')' ',' ('y' | 'Y') #IndirectY
    | '(' expr ')' #Indirect
    | expr ',' ('x' | 'X') #AbsoluteX
    | expr ',' ('y' | 'Y') #AbsoluteY
    | expr #Absolute
    ;

expr
    : op=('-' | '+' | '~' | '!' | '<' | '>' | '.sizeof') right=expr #UnaryExpr
    | left=expr op=('*' | '/' | '%') right=expr #MultiplicativeExpr
    | left=expr op=('+' | '-') right=expr #AdditiveExpr
    | left=expr op=('<<' | '>>') right=expr #ShiftExpr
    | left=expr op=('<' | '>' | '<=' | '>=') right=expr #RelationalExpr
    | left=expr op=('==' | '!=') right=expr #EqualityExpr
    | left=expr '&' right=expr #BitAndExpr
    | left=expr '^' right=expr #BitXorExpr
    | left=expr '|' right=expr #BitOrExpr
    | '(' left=expr ')' #ParensExpr
    | number #NumLiteral
    | identifier #Reference
    | '$' #PC
    ;

identifier
    : lname
    | fname
    | pname
    | qname
    | name
    ;

lname
    : NAME ':'
    | '+'+
    | '-'+
    ;

fname
    : NAME '(' (NAME '[]'* (',' NAME '[]'*)*)? ')'
    ;

pname
    : PNAME
    ;

qname
 	: NAME ('.' NAME)+
 	;

name
	: NAME
	;

string
	: STRING
	;

number
	: HEX_NUM
	| BIN_NUM
	| DEC_NUM
	| CHA_NUM
	;

comment
	: COMMENT
	;

opcode
	: ADC
    | AND
    | ASL
    | BCC
    | BCS
    | BEQ
    | BIT
    | BMI
    | BNE
    | BPL
    | BVC
    | BVS
    | CLC
    | CLD
    | CLI
    | CLV
    | CMP
    | CPX
    | CPY
    | DEC
    | DEX
    | DEY
    | EOR
    | INC
    | INX
    | INY
    | JMP
    | JSR
    | JSS
    | JCC
    | JCS
    | JEQ
    | JMI
    | JNE
    | JPL
    | JVC
    | JVS
    | LDA
    | LDY
    | LDX
    | LSR
    | NOP
    | ORA
    | PHA
    | PHP
    | PLA
    | PLP
    | ROL
    | ROR
    | RTI
    | RTS
    | SBC
    | SEC
    | SED
    | SEI
    | STA
    | STX
    | STY
    | TAX
    | TAY
    | TSX
    | TXA
    | TXS
    | TYA
	;

fragment A
	: ('a' | 'A')
	;


fragment B
	: ('b' | 'B')
	;


fragment C
	: ('c' | 'C')
	;


fragment D
	: ('d' | 'D')
	;


fragment E
	: ('e' | 'E')
	;


fragment F
	: ('f' | 'F')
	;


fragment G
	: ('g' | 'G')
	;


fragment H
	: ('h' | 'H')
	;


fragment I
	: ('i' | 'I')
	;


fragment J
	: ('j' | 'J')
	;


fragment K
	: ('k' | 'K')
	;


fragment L
	: ('l' | 'L')
	;


fragment M
	: ('m' | 'M')
	;


fragment N
	: ('n' | 'N')
	;


fragment O
	: ('o' | 'O')
	;


fragment P
	: ('p' | 'P')
	;


fragment Q
	: ('q' | 'Q')
	;


fragment R
	: ('r' | 'R')
	;


fragment S
	: ('s' | 'S')
	;


fragment T
	: ('t' | 'T')
	;


fragment U
	: ('u' | 'U')
	;


fragment V
	: ('v' | 'V')
	;


fragment W
	: ('w' | 'W')
	;


fragment X
	: ('x' | 'X')
	;


fragment Y
	: ('y' | 'Y')
	;


fragment Z
	: ('z' | 'Z')
	;

ADC
	: A D C
	;

AND
	: A N D
	;

ASL
	: A S L
	;

BCC
	: B C C
	;

BCS
	: B C S
	;

BEQ
	: B E Q
	;

BIT
	: B I T
	;

BMI
	: B M I
	;

BNE
	: B N E
	;

BPL
	: B P L
	;

BVC
	: B V C
	;

BVS
	: B V S
	;

CLC
	: C L C
	;

CLD
	: C L D
	;

CLI
	: C L I
	;

CLV
	: C L V
	;

CMP
	: C M P
	;

CPX
	: C P X
	;

CPY
	: C P Y
	;

DEC
	: D E C
	;

DEX
	: D E X
	;

DEY
	: D E Y
	;

EOR
	: E O R
	;

INC
	: I N C
	;

INX
	: I N X
	;

INY
	: I N Y
	;

JMP
	: J M P
	;

JSR
	: J S R
	;

JSS
	: J S S
	;

JCC
	: J C C
	;

JCS
	: J C S
	;

JEQ
	: J E Q
	;

JMI
	: J M I
	;

JNE
	: J N E
	;

JPL
	: J P L
	;

JVC
	: J V C
	;

JVS
	: J V S
	;


LDA
	: L D A
	;

LDY
	: L D Y
	;

LDX
	: L D X
	;

LSR
	: L S R
	;

NOP
	: N O P
	;

ORA
	: O R A
	;

PHA
	: P H A
	;

PHP
	: P H P
	;

PLA
	: P L A
	;

PLP
	: P L P
	;

ROL
	: R O L
	;

ROR
	: R O R
	;

RTI
	: R T I
	;

RTS
	: R T S
	;

SBC
	: S B C
	;

SEC
	: S E C
	;

SED
	: S E D
	;

SEI
	: S E I
	;

STA
	: S T A
	;

STX
	: S T X
	;

STY
	: S T Y
	;

TAX
	: T A X
	;

TAY
	: T A Y
	;

TSX
	: T S X
	;

TXA
	: T X A
	;

TXS
	: T X S
	;

TYA
	: T Y A
	;


NAME
	: [a-zA-Z_] [a-zA-Z0-9_]*
	;

PNAME
    : '@' [a-zA-Z_] [a-zA-Z0-9_]*
    ;

HEX_NUM
	: '$' [0-9a-fA-F] +
	;

BIN_NUM
	: '%' [0-1]+
	;

DEC_NUM
	: [0-9]+
	;

CHA_NUM
	: '\''~ ["]'\''
	;

COMMENT
	: ';' ~ [\r\n]* -> skip
	;


STRING
	: '"' ~ ["]* '"'
	;


EOL
	: [\r\n] +
	;


WS
	: [ \t] -> skip
	;