Red [title: "Gram"]

tree: []

; Gram grammar

pas*: [any space]
ren*: [pas* lf ws*]
ws*: [any [lf | space]]

upper: charset [#"A" - #"Z"]
lower: charset [#"a" - #"z"]
letter: union lower upper

comma: charset "," 
newline: ["\n"]

non-zero-digit: charset "123456789" 
digit: union charset "0" non-zero-digit

alphanum: union letter digit

nonterminal: [upper any lower]
terminal: [some lower]

grammar: [ws* collect into tree [header body] ws*]
header: [opt [keep (quote name:) keep grammar-name pas* #"=" ws*]]
grammar-name: [nonterminal]

body: [keep (quote rules:) collect [rule any [ren* rule]] | keep (quote conds:) collect [conds]]
rule: [collect [keep (quote name:) keep rule-name pas* #":" ws* keep (quote conds:) collect [conds]]]
rule-name: [nonterminal | terminal]

conds: [or-cond any [ren* keep (#"|") or-cond]]
or-cond: [and-cond any [some space and-cond]]
and-cond: [[group | cond] opt cardinality | keep range]

group: ["(" pas* collect [and-cond some [some space and-cond]] pas* ")"]
cond: [reference | literal]
cardinality: [keep [#"?" | #"*" | #"+"]]
range: ["[" pas* ["0-9" | "1-9" | "A-Z" | "a-z"] pas* "]"]

reference: [keep [rule-name not [pas* [#"=" | #":"]]]]
literal: [#"'" keep [number | comma | newline | string]]
string: [any letter]
number: ["0" | non-zero-digit any digit]

; Parse the gram file

gram-grammar: read %csv.gram

parse gram-grammar grammar

; Red parser

tree-name: tree/name

cardinality: charset "?*+"

stringify: func [chars] [
  rejoin [
    "^""
    to-string chars
    "^""
  ]
]

lookahead: function [conds index] [
  cond: pick at conds (index + 1) 1
  if (type? cond) = char! [
    if find cardinality to-string cond [
      rejoin [
        switch cond [
          #"?" ["opt"]
          #"*" ["any"]
          #"+" ["some"]
        ] " "
      ]
    ]
  ]
]

persist: function [conds] [
  index: 0
  foreach cond conds [
    index: index + 1
    write/append red-parser " "
    switch type?/word cond [
      char! [
        if not (find cardinality to-string cond) ; skip cardinality -> already processed in lookahead
          [write/append red-parser either cond = #"|" [either index < (length? conds) ["|"] [""]] [stringify cond]]
      ]
      string! [
        either cond/1 = #"[" [
          write/append red-parser rejoin [
             switch cond [
              "[0-9]" ["_digit"]
              "[1-9]" ["_nonzerodigit"]
              "[A-Z]" [either cond/2 == #"A" ["_upper"] ["_lower"]]
            ] " "
          ]
        ] [
          card: lookahead conds index
          if not none? card [write/append red-parser card]
          write/append red-parser rejoin [either cond = "\n" ["lf"] [cond]]
        ]
      ]
      block! [
        card: lookahead conds index
        if not none? card [write/append red-parser card]
        write/append red-parser "["
        persist cond
        write/append red-parser " ]"
      ]
    ]
  ]
]

red-parser: %parser.red

write red-parser {Red [Title "Red Parser"]

_upper: charset [#"A" - #"Z"]
_lower: charset [#"a" - #"z"]
_letter: union _lower _upper
_nonzerodigit: charset "123456789" 
_digit: union charset "0" _nonzerodigit
_alphanum: union _letter _digit

}

either not empty? tree/rules [
  foreach rule tree/rules [
    if not none? rule/name [
      write/append red-parser rejoin [
        rule/name ": [ " (either (to-string rule/name/1) == (uppercase copy to-string rule/name/1) [
          rejoin [
            "collect [ keep(quote " rule/name   ":)"
          ]
        ] [" keep [ "])
      ]
      conds: rule/conds
      persist conds
      write/append red-parser " ] ]^/"
    ]
  ]
] [
  not empty? tree/conds [
    persist tree/conds
    write/append red-parser "^/"
  ]
]

write/append red-parser {
text: trim/tail read %example.csv
tree: parse text csv

csv-tree: %csv.tree
write csv-tree ""

lookahead: func [node value] [
  not none? select node to-lit-word value
]

process: func [node] [
  within-block: yes
  index: 0
  foreach value node [
    index: index + 1
    switch/default type?/word value [
      set-word! [either lookahead node value [write/append csv-tree rejoin [to-string value "("]] [within-block: no]]
      block! [if index >= 3 [write/append csv-tree ","] process value if within-block [write/append csv-tree ")"]]
      char! [write/append csv-tree to-string value]
    ] [print rejoin ["Unknown type: " type?/word value]]
  ]
]

process tree
write/append csv-tree ")"
}

