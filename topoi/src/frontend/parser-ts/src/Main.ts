import moo from 'moo'

let lexer = moo.compile({
  whitespace:      /[ \t]+/,
  comment: /\/\/.*?$/,
  number:  /0|[1-9][0-9]*/,
  string:  /"(?:\\["\\]|[^\n"\\])*"/,
  lparen:  '(',
  rparen:  ')',
  dot: '.',
  identifier: /[A-Za-z_][\w]*/,
  keyword: ['while', 'if', 'else', 'let', 'where', 'in'],
  NL:      { match: /\n/, lineBreaks: true },
})

class ParserCombinator {
  private lexer: moo.Lexer
  // I am writing this simply because moo lexer does not support peek function
  private currToken: moo.Token | undefined
  // private errors: any[]

  constructor(lexer: moo.Lexer) {
    this.lexer = lexer
    this.currToken = lexer.next()
  }

  advance(): void { this.currToken = this.lexer.next() }

  allow(tokenType: string): boolean {
    if (this.currToken?.type === tokenType) {
      this.advance()
      return true
    } else return false
  }

  expect(tokenType: string): void {
    if (this.currToken?.type === tokenType) this.advance()
    else throw new Error(`expect the token ${tokenType}`)
  }

  expect_exact(tokenType: string, expected: string): void {
    if (this.currToken?.type === tokenType && this.currToken?.value === expected) this.advance()
    else throw new Error(`expect the token ${expected}`)
  }

  mark_start_then_advance(): number {
    const start_offset = this.currToken?.offset || 0
    this.advance()
    return start_offset
  }

  mark_end_then_advance(): number {
    const end_offset = (this.currToken?.offset || 0) + (this.currToken?.value.length || 1)
    this.advance()
    return end_offset
  }

  capture() : moo.Token {
    const curr_token = this.currToken
    if (!curr_token) throw new Error('expect some token, found EOF')
    return curr_token
  }

  parenPair(): number[] {
    const start_pos = this.mark_start_then_advance()
    const end_pos = this.mark_end_then_advance()
    return [start_pos, end_pos]
  }

  IdentifierOrMemberAccessParse(object: moo.Token): any {
    const start_pos = this.mark_start_then_advance()
    this.expect('dot')
    const member = this.capture()
    const end_pos = this.mark_end_then_advance()
    console.log({
      start: start_pos,
      end: end_pos,
      content: { object, member }
    })
  }
}

const parser = new ParserCombinator(lexer.reset('"aaa'))

console.log(parser.capture())
// parser.IdentifierOrMemberAccessParse(parser.capture())

// const a = lexer.next() // -> { type: 'keyword', value: 'while' }
// const b = lexer.next() // -> { type: 'WS', value: ' ' }
// const c = lexer.next() // -> { type: 'lparen', value: '(' }
// const d = lexer.next() // -> { type: 'number', value: '10' }
// const e = lexer.next()

// console.log(a, b, c, d, e)

// const parser = new ParserCombinator(lexer.reset('()(()'))
// console.log(parser.parenPair())
// console.log(parser.allow('lparen'))
// console.log(parser.parenPair())