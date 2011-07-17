###
TOKENS
###
TokenType = {}

registerToken = (name, fn) ->
    TokenType[name] = {name:name, text:fn}
    
    
registerToken "LEFT_PAREN",     -> '('
registerToken "RIGHT_PAREN",    -> ')'
registerToken "COMMA",          -> ','
registerToken "ASSIGN",         -> '='
registerToken "PLUS",           -> '+'
registerToken "MINUS",          -> '-'
registerToken "ASTERISK",       -> '*'
registerToken "SLASH",          -> '/'
registerToken "CARET",          -> '^'
registerToken "TILDE",          -> '~'
registerToken "BANG",           -> '!'
registerToken "QUESTION",       -> '?'
registerToken "COLON",          -> ':'
registerToken "NAME",           -> @text
registerToken "EOF",            -> null

class Token
    constructor: (@type, @text = "") ->

    toString: () -> @type.text.call this

###
PRECEDENCE
###
Precedence =
    # Ordered in increasing precedence
    ASSIGNMENT:     1
    CONDITIONAL:    2
    SUM:            3
    PRODUCT:        4
    EXPONENT:       5
    PREFIX:         6
    POSTFIX:        7
    CALL:           8

###
LEXER
###
class Lexer
    constructor: (@text) ->
        @index = 0
        @punctuators = {}
        @punctuators[type.text()] = type for _, type of TokenType when type.text()?

    next: () ->
        while @index < @text.length
            c = @text.charAt @index++
            
            return new Token(@punctuators[c]) if @punctuators[c]?
            
            start = @index - 1
            while c.match(/^[a-zA-Z]$/)? and @index < @text.length
                c = @text.charAt @index++
           
            if @index - 1 > start
                --@index
                return new Token TokenType.NAME, @text.substr(start, @index - start)

            # Otherwise, we'll just keep going, ignoring whitespace and other stuff we don't understand

        # We're at the end of the string, now just keep returning EOF until the Parser gives up
        new Token TokenType.EOF

###
PARSER
###
class Parser
    constructor: (@tokens) ->
        @prefixParselets = {}
        @infixParselets = {}
        @read = []

    register: (type, parselet) ->
        (if parselet instanceof PrefixParselet then @prefixParselets else @infixParselets)[type.name] = parselet

    parseExpression: (precedence = 0) ->
        token = @consume()
        prefix = @prefixParselets[token.type.name]
        throw "Could not parse \"#{token}\"." if not prefix?
        
        left = prefix.parse this, token
        while precedence < @getPrecedence()
            token = @consume()
            
            infix = @infixParselets[token.type.name]
            left = infix.parse this, left, token

        left
    
    match: (expected) ->
        token = @lookAhead()
        return false if token.type isnt expected
        @consume()
        true

    consume: (expected = null) ->
        # Make sure we actually read the token
        token = @lookAhead()
        throw "Expected #{expected.name} but got \"#{token}\"" if expected? and token.type isnt expected
        @read.pop()
        token

    lookAhead: (distance = 0) ->
        @read.push @tokens.next() while distance >= @read.length
        @read[distance]

    getPrecedence: ->
        parselet = @infixParselets[@lookAhead()?.type?.name]
        if parselet? then parselet.precedence else 0

class BantamParser extends Parser
    constructor: (lexer) ->
        super lexer

        @register TokenType.NAME,           new NameParselet()
        @register TokenType.ASSIGN,         new AssignParselet()
        @register TokenType.QUESTION,       new ConditionalParselet()
        @register TokenType.LEFT_PAREN,     new GroupParselet()
        @register TokenType.LEFT_PAREN,     new CallParselet()
        
        @prefix TokenType.PLUS,             Precedence.PREFIX
        @prefix TokenType.MINUS,            Precedence.PREFIX
        @prefix TokenType.TILDE,            Precedence.PREFIX
        @prefix TokenType.BANG,             Precedence.PREFIX

        @postfix TokenType.BANG,            Precedence.POSTFIX

        @infixLeft TokenType.PLUS,          Precedence.SUM
        @infixLeft TokenType.MINUS,         Precedence.SUM
        @infixLeft TokenType.ASTERISK,      Precedence.PRODUCT
        @infixLeft TokenType.SLASH,         Precedence.PRODUCT
        @infixRight TokenType.CARET,        Precedence.EXPONENT

    prefix:         (type, precedence) -> @register type, new PrefixOperatorParselet(type, precedence)
    postfix:        (type, precedence) -> @register type, new PostfixOperatorParselet(type, precedence)
    infixLeft:      (type, precedence) -> @register type, new BinaryOperatorParselet(type, precedence, no)
    infixRight:     (type, precedence) -> @register type, new BinaryOperatorParselet(type, precedence, yes)

###
PARSELETS
###
class InfixParselet
    constructor: ->
        @precedence = 0

    parse: (parser, left, token) ->

class PrefixParselet
    constructor: ->

    parse: (parser, token) ->

class NameParselet extends PrefixParselet
    constructor: ->

    parse: (parser, token) -> new NameExpression(token)

class AssignParselet extends InfixParselet
    constructor: ->
        @precedence = Precedence.ASSIGNMENT

    parse: (parser, left, token) ->
        throw "The left-hand side of an assignment must be a name." if not (left instanceof NameExpression)
        right = parser.parseExpression Precedence.ASSIGNMENT - 1

        new AssignExpression left.name, right

class ConditionalParselet extends InfixParselet
    constructor: ->
        @precedence = Precedence.CONDITIONAL

    parse: (parser, left, token) ->
        thenArm = parser.parseExpression()
        parser.consume TokenType.COLON
        elseArm = parser.parseExpression Precedence.CONDITIONAL - 1

        new ConditionalExpression left, thenArm, elseArm

class BinaryOperatorParselet extends InfixParselet
    constructor: (@type, @precedence, @isRight) ->

    parse: (parser, left, token) ->
        right = parser.parseExpression @precedence - (if @isRight then 1 else 0)
        
        new OperatorExpression(left, @type, right)

class PostfixOperatorParselet extends InfixParselet
    constructor: (@type, @precedence) ->

    parse: (parser, left, token) -> new PostfixExpression left, @type

class PrefixOperatorParselet extends PrefixParselet
    constructor: (@type, @precedence) ->

    parse: (parser, token) ->
        right = parser.parseExpression @precedence

        new PrefixExpression @type, right

class CallParselet extends InfixParselet
    constructor: ->
        @precedence = Precedence.CALL

    parse: (parser, left, token) ->
        args = []
        until parser.match TokenType.RIGHT_PAREN
            args.push parser.parseExpression()
            break unless parser.match TokenType.COMMA
        
        parser.match TokenType.RIGHT_PAREN
        
        new CallExpression left, args

class GroupParselet extends PrefixParselet
    constructor: ->

    parse: (parser, token) ->
        expression = parser.parseExpression()
        parser.consume TokenType.RIGHT_PAREN

        expression

###
CODE GENERATOR
###
class Writer
    constructor: ->
        @code = ""


###
AST NODES
###

class Expression

class NameExpression extends Expression
    constructor: (@name) ->

    compile: (writer) ->
        writer.code += "#{@name}"

class AssignExpression extends Expression
    constructor: (@name, @right) ->

    compile: (writer) ->
        writer.code += "(#{@name} = "
        @right.compile writer
        writer.code += ")"

class ConditionalExpression extends Expression
    constructor: (@condition, @thenArm, @elseArm) ->

    compile: (writer) ->
        writer.code += "("
        @condition.compile writer
        writer.code += " ? "
        @thenArm.compile writer
        writer.code += " : "
        @elseArm.compile writer

class OperatorExpression extends Expression
    constructor: (@left, @operator, @right) ->

    compile: (writer) ->
        writer.code += "("
        @left.compile writer
        writer.code += " #{@operator.text()} "
        @right.compile writer
        writer.code += ")"

class PostfixExpression extends Expression
    constructor: (@left, @operator) ->

    compile: (writer) ->
        writer.code += "("
        @left.compile writer
        writer.code += @operator.text()
        writer.code += ")"

class PrefixExpression extends Expression
    constructor: (@operator, @right) ->

    compile: (writer) ->
        writer.code += "("
        writer.code += @operator.text()
        @right.compile writer
        writer.code += ")"

class CallExpression extends Expression
    constructor: (@function, @args) ->

    compile: (writer) ->
        writer.code += @function.compile writer
        writer.code += "("
        for arg, i in @args
            arg.compile writer
            writer.code += ", " unless i == @args.length - 1
        writer.code += ")"

###
REPL
###

stdin = process.openStdin()
console.log "Type Ctrl+D or Ctrl+C to exit"
stdin.on 'data', (chunk) ->
    expr = new BantamParser(new Lexer(chunk.toString())).parseExpression()
    w = new Writer
    expr.compile w
    console.log w.code

