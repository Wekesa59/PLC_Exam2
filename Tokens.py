import string

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
Token_Identify = 'Token_Identify'
Token_Key = "Token_Key"


Digit = '0123456789'
Letter = string.ascii_letters
lettter_digit =  Letter + Digit
Keywords = [
    'SENSATIONAL', #Variable name
    'Wallabee', #For loop (Numba 4 from KND)
    'Major_L', #while loop plus what I'm getting for a grade
    'STOOL', #step
    'DOS', #To
    'NOW' #Then
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
                #check for identifier
            elif self.current_char in Letter:
                tokens.append(self.make_ident())
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

    def make_ident(self):
        string_id = ''
        while self.current_char != None and self.current_char in lettter_digit + '_':
            string_id += self.current_char
            self.advance()

        tok_type = Token_Key if string_id in Keywords else Token_Identify
        return Tokens(tok_type, string_id, self.pos)

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
        elif token.type == Token_LP:
            result.register(self.advance())
            expr = res.register(self.expr())
            if result.error: return res
            if self.current_tok.type == TT_RPAREN:
                result.register(self.advance())
                return result.success(expr)
            else:
                return result.failure(InValidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected ')'"
				))

    def for_loop(self):
        result = ParseResult()

        if not self.current_tok.matches(Token_Key, 'Wallabee'):
            return result.failure(InValidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'Wallabee'"
			))

        result.register_advancement()
        self.advance()

        if self.current_tok.type != Token_Identify:
            return result.failure(InValidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected identifier"
			))

        var_name = self.current_tok
        result.register_advancement()
        self.advance()

        if self.current_tok.type != Token_equal:
            return result.failure(InValidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected '='"
			))
		
        result.register_advancement()
        self.advance()

        start_value = res.register(self.expr())
        if result.error: return res

        if not self.current_tok.matches(Token_Key, 'DOS'):
            return result.failure(InValidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'DOS'"
			))
		
        result.register_advancement()
        self.advance()

        end_value = result.register(self.expr())
        if result.error: return result

        if self.current_tok.matches(Token_Key, 'STOOL'):
            result.register_advancement()
            self.advance()

            step_value = result.register(self.expr())
            if result.error: return result
        else:
            stool_value = None

        if not self.current_tok.matches(Token_Key, 'NOW'):
            return result.failure(InValidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				f"Expected 'NOW'"
			))

        result.register_advancement()
        self.advance()

        body = result.register(self.expr())
        if res.error: return res

        return result.success(ForNode(var_name, start_value, end_value, step_value, body))
    def parse(self):
        result = self.expression()
        if not result and self.current_token != Token_EOF:
            return result.failure(InValidSyntaxError("Expected '+, '-', '*', '/' "))
        return result

    def term(self):
        return self.binary_op(self.factor, (Token_mul, Token_div))
    def expression(self):
        result = ParseResult()
        if self.current_token.matches(Token_Key, 'SENSATIONAL'):
                result.register(self.advance())
                if self.current_tok.type != Token_Identify:
                    return result.failure(InValidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected identifier"
				))

        variable = self.current_token
        result.register_advancement()
        self.advance()

        if self.current_tok.type != Token_equal:
            return result.failure(InValidSyntaxError(
					self.current_tok.pos_start, self.current_tok.pos_end,
					"Expected '='"
				))

        result.register_advancement()
        self.advance()
        expr = result.register(self.expr())
        if result.error: return result
        return result.success(VarAssignNode(variable, expr))

        node = result.register(self.binary_op(self.term, (Token_add, Token_sub)))

        if result.error:
            return result.failure(InValidSyntaxError(
				self.current_tok.pos_start, self.current_tok.pos_end,
				"Expected 'VAR', int, float, identifier, '+', '-' or '('"
			))

        return result.success(node)

class For:
    def __init__(self, var_name_tok, start_value_node, end_value_node, step_value_node, body_node):
        self.var_name_tok = var_name_tok
        self.start_value_node = start_value_node
        self.end_value_node = end_value_node
        self.step_value_node = step_value_node
        self.body_node = body_node

        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.body_node.pos_end
class while_loop:
    def __init__(self, condition_node, body_node):
        self.condition_node = condition_node
        self.body_node = body_node

        self.pos_start = self.condition_node.pos_start
        self.pos_end = self.body_node.pos_end

    def binary_op(self, function, op):
        left = self.factor()
        while self.current_token.type in (Token_mul, Token_div):
            operation = self.current_token
            self.advance()
            right = self.factor()
            left = Binary_node(left, operation, right)
        return left

class Table:
    def __init__(self):
        self.symbol = {}
        self.parent = None

    def get(self, name):
        value = self.symbol.get(name, None)
        if value == None and self.parent:
            return parent.get(name)
        return value
    def set(self, name, value):
        self.symbol[name] = value
    def remove(self, name):
        del self.symbbol[name]
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

class ParseResult:
	def __init__(self):
		self.error = None
		self.node = None
		self.advance_count = 0

	def register_advancement(self):
		self.advance_count += 1

	def register(self, res):
		self.advance_count += res.advance_count
		if res.error: self.error = res.error
		return res.node

	def success(self, node):
		self.node = node
		return self

	def failure(self, error):
		if not self.error or self.advance_count == 0:
			self.error = error
		return self