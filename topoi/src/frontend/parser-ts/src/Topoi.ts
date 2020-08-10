import moo from 'moo'

type TokenResult = moo.Token

export function parse(currToken: moo.Token): TokenResult {
    switch (currToken.type) {
        case 'identifier': return currToken
        // case 'lparen': parseTupleExpression(currToken)
        case 'string': return currToken
        case 'number': return currToken
    }
}

function parseIdentifier(currToken: moo.Token) {}
function parseTupleExpression(currToken: moo.Token) {}
function parseString(currToken: moo.Token) {}
