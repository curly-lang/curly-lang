if exists("b:current_syntax")
	finish
endif

let b:current_syntax = "curly"

" Keywords
syntax keyword curlyKeyword let
syntax keyword curlyKeyword for some all where pass stop
syntax keyword curlyKeyword if then else
syntax keyword curlyKeyword type enum ptr class match to
syntax keyword curlyKeyword lambda
syntax keyword curlyKeyword and or xor
syntax keyword curlyKeyword in
syntax keyword curlyKeyword module import as extern
highlight link curlyKeyword Keyword

" Operators
syntax match curlyOperator "\v\*"
syntax match curlyOperator "\v/"
syntax match curlyOperator "\v\%"
syntax match curlyOperator "\v\+"
syntax match curlyOperator "\v\-"
syntax match curlyOperator "\v\<\=|\>\=|\=\=|!\=|\<|\>|\="
syntax match curlyOperator "\v\&"
syntax match curlyOperator "\v\|"
syntax match curlyOperator "\v\^"
syntax match curlyOperator "\v::?"
syntax match curlyOperator "\v\(|\)|\[|\]|\{|\}"
syntax match curlyOperator "\v\=\>|\-\>"
syntax match curlyOperator "\v\.|,|;"
syntax match curlyOperator "\v\$"
highlight link curlyOperator Operator

" Values
syntax region curlyString start=/\v"/ skip=/\v\\./ end=/\v"/
highlight link curlyString String
syntax match curlyConst "\v[0-9]+|0x[0-9a-fA-F]+|0b[01]+"
syntax match curlyConst "\v[0-9]+u|[0-9a-fA-F]+h|[01]+b"
syntax match curlyConst "\v[0-9]+(\.[0-9]*([eE][+-]?[0-9]+)?|[eE][+-]?[0-9]+)"
syntax match curlyConst "\vtrue|false"
syntax match curlyConst "\v'(\\.|[^\\'])'"
highlight link curlyConst Constant

" Identifiers
syntax match curlyIdent "\v((true|false)[a-zA-Z0-9_']@!)@![@a-z_][a-zA-Z0-9_']*"
highlight link curlyIdent Identifier
syntax match curlyType "\v[A-Z][a-zA-Z0-9_']*"
highlight link curlyType Type

" Comments
syntax match curlyComment "\v#.*$"
syn region curlyComment start="{-"    end="-}"
highlight link curlyComment Comment
