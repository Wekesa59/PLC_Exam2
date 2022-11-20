
Token_add = 'Token_add'
Token_sub = 'Token_sub'
Token_mul = 'Token_mul'
Token_div = 'Token_div'
Token_mod = 'Token_mod'
Token_less = 'Token_less'
Token_greater = 'Token_greater'
Token_lessEqual = 'Token_lessEqual'
Token_greatEqual = 'Token_greatEqual'
Token_equal = 'Token_equal'
Token_notEqual = 'Token_notEqual'
Token_Assign = 'Token_Assign'
Token_LP = 'Token_LP'
Token_RP = 'Token_RP'
Token_int = 'Token_int'
Token_float = 'Token_float'
Token_EOF = 'Token_EOF'

Digit = '0123456789'
Keywords = [
    ''
]

class Tokens:
    def __init__(self, type, value = None):
        self.type = type
        self.value = value
        
    def __repr__(self):
        if self.value: return f'{self.type}:{self.value}'
        return f'{self.type}'

#######################################################################################################################
class Lexer:
    def __init__(self, text):
        self.text = text
        self.pos = -1
        self.advance()
        
    def advance(self):
        self.pos +=1
        self.current_char = self.text[self.pos] if self.pos < len(self.text) else None

    
    def make_tokens(self):
        tokens = []
        while self.current_char != None:
            if self.current_char in '\t':
                self.advance()
                #Check for numbers
            elif self.current_char in Digit:
                tokens.append(self.make_num())
                #Check for others
            elif self.current_char == '+':
                tokens.append(Tokens(Token_add))
                self.advance()
            elif self.current_char == '-':
                tokens.append(Tokens(Token_sub))
                self.advance()
            elif self.current_char == '*':
                tokens.append(Tokens(Token_mul))
                self.advance()
            elif self.current_char == '/':
                tokens.append(Tokens(Token_div))
                self.advance()
            elif self.current_char == '%':
                tokens.append(Tokens(Token_mod))
                self.advance()
            elif self.current_char == '<':
                tokens.append(Tokens(Token_less))
                self.advance()
            elif self.current_char == '>':
                tokens.append(Tokens(Token_greater))
                self.advance()
            elif self.current_char == '<=':
                tokens.append(Tokens(Token_lessEqual))
                self.advance()
            elif self.current_char == '>=':
                tokens.append(Tokens(Token_greatEqual))
                self.advance()
            elif self.current_char == '==':
                tokens.append(Tokens(Token_equal))
                self.advance()
            elif self.current_char == '!=':
                tokens.append(Tokens(Token_notEqual))
                self.advance()
            elif self.current_char == '=':
                tokens.append(Tokens(Token_Assign))
                self.advance()
            elif self.current_char == '(':
                tokens.append(Tokens(Token_LP))
                self.advance()
            elif self.current_char == ')':
                tokens.append(Tokens(Token_RP))
                self.advance()
            else:
                char = self.current_char
                self.advance()
                return [], IllegalCharError("'"+ char + "'")

        return tokens, None

    def make_num(self):
        num_str = ''
        dots = 0
        while self.current_char != None and self.current_char in Digit + '.':
            if self.current_char == '.':
                if dots == 1: break
                dots += 1
                num_str += '.'
            else:
                num_str += self.current_char
            self.advance()

        if dots == 0:
            return Tokens(Token_int, int(num_str))
        else:
            return Tokens(Token_float, float(num_str))
    def run(text):
        lexer = Lexer(text)
        tokens, error = lexer.make_tokens()
        return tokens, error

######################################################################
class Number:
        def __init__(self, tok):
            self.tok = tok

            self.pos_start = self.tok.pos_start
            self.pos_end = self.tok.pos_end

        def __repr__(self):
            return f'{self.tok}'
###########################################################################
class Binary_node:
    def __init__(self, left_node, op_tok, right_node):
        self.left_node = left_node
        self.op_tok = op_tok
        self.right_node = right_node

        self.pos_start = self.left_node.pos_start
        self.pos_end = self.right_node.pos_end

class Num_node:
    def __init__(self, tokens):
        self.tokens = tokens
    def __repr__(self):
	    return f'{self.tok}'

class InParenthesis:
    def __init__(self, op_tok, node):
        self.op_tok = op_tok
        self.node = node
    def __repr__(self):
        return f'({self.op_tok}, {self.node})'
# Parsing ###########################################################
class Parse:
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.advance()

    def advance(self):
        self.token_index += 1
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]
        return self.current_token
    def factor(self):
        token = self.current_token
        result = ParseResult()
        if token.type in (Token_add, Token_sub):
            factor = result.register(self.factor())
            if result.error: return result
            return result.success(InParenthesis(token, factor))
        elif token.type in (Token_int, Token_float):
            self.advance()
            return Num_node
        elif tok.type == TT_LPAREN:
            result.register(self.advance())
            expr = res.register(self.expr())
            if result.error: return res
            if self.current_tok.type == TT_RPAREN:
                result.register(self.advance())
                return result.success(expr)
            else:
                return result.failure(InvalidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ')'"
				))
    def parse(self):
        result = self.expression()
        if not result and self.current_token != Token_EOF:
            return result.failure(InValidSyntaxError("Expected '+, '-', '*', '/' "))
        return result

    def term(self):
        return self.binary_op(self.factor, (Token_mul, Token_div))
    def expression(self):
        return self.binary_op(self.term, (Token_add, Token_sub))
    def binary_op(self, function, op):
        left = self.factor()
        while self.current_token.type in (Token_mul, Token_div):
            operation = self.current_token
            self.advance()
            right = self.factor()
            left = Binary_node(left, operation, right)
        return left
#Error messages
class Error:
    def __init__(self, error, details):
        self.error = error
        self.details = details

class IllegalCharError(Error):
    def __init__(self, details):
        super().__init__('Illegal Character', details)

def run(fn, text):
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error: return None, error
    parser = Parse(tokens)
    ast = parser.parse()

    return ast.node, ast.error

class InValidSyntaxError(Error):
    def __init__(self, pos_start, pos_end, details=''):
        super().__init__(pos_start, pos_end, 'Invalid Syntax', details)

