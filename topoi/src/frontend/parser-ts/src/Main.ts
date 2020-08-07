import moo from 'moo'

let lexer = moo.compile({
  WS:      /[ \t]+/,
  comment: /\/\/.*?$/,
  number:  /0|[1-9][0-9]*/,
  string:  /"(?:\\["\\]|[^\n"\\])*"/,
  lparen:  '(',
  rparen:  ')',
  keyword: ['while', 'if', 'else', 'moo', 'cows'],
  NL:      { match: /\n/, lineBreaks: true },
})

lexer.reset('while (10) cows\nmoo')
// const a = lexer.next() // -> { type: 'keyword', value: 'while' }
const b = lexer.next() // -> { type: 'WS', value: ' ' }
const c = lexer.next() // -> { type: 'lparen', value: '(' }
const d = lexer.next() // -> { type: 'number', value: '10' }

console.log(lexer.next(), b, c, d)