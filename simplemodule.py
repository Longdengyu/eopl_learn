
# lexer lib
# ref: https://www.jayconrod.com/posts/37/a-simple-interpreter-from-scratch-in-python--part-1-

def debug(msg):
    pass
    print("[+] DEBUG: {}".format(msg))

def backtrace():
    import traceback
    # traceback.print_stack()
import sys
import re

def lex(characters, token_exprs):
    pos = 0
    tokens = []
    while pos < len(characters):
        match = None
        for token_expr in token_exprs:
            pattern, tag = token_expr
            regex = re.compile(pattern)
            match = regex.match(characters, pos)
            if match:
                text = match.group(0)
                if tag:
                    token = (text, tag)
                    tokens.append(token)
                break
        if not match:
            sys.stderr.write('Illegal character: %s\n' % characters[pos])
            sys.exit(1)
        else:
            pos = match.end(0)
    return tokens

# combinator
# ref: https://www.jayconrod.com/posts/38/a-simple-interpreter-from-scratch-in-python--part-2-

class Result:
    def __init__(self, value, pos):
        self.value = value
        self.pos = pos
        debug(self)

    def __repr__(self):
        return 'Result(%s, %d)' % (self.value, self.pos)

class Parser:
    def __add__(self, other):
        return Concat(self, other)

    def __mul__(self, other):
        return Exp(self, other)

    def __or__(self, other):
        return Alternate(self, other)

    def __xor__(self, function):
        return Process(self, function)

class Tag(Parser):
    def __init__(self, tag):
        self.tag = tag

    def __call__(self, tokens, pos):
        debug("call Tag: {}".format(self.tag))
        backtrace()
        if pos < len(tokens) and tokens[pos][1] == self.tag:
            return Result(tokens[pos][0], pos + 1)
        else:
            return None

class Reserved(Parser):
    def __init__(self, value, tag):
        debug("build Reserved: {}".format(value))
        self.value = value
        self.tag = tag

    def __call__(self, tokens, pos):
        debug("call Reserved: {}".format(self.value))
        backtrace()
        if pos < len(tokens) and \
           tokens[pos][0] == self.value and \
           tokens[pos][1] == self.tag:

            return Result(tokens[pos][0], pos + 1)
        else:
            backtrace()
            return None

class Concat(Parser):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __call__(self, tokens, pos):
        left_result = self.left(tokens, pos)
        if left_result:
            right_result = self.right(tokens, left_result.pos)
            if right_result:
                combined_value = (left_result.value, right_result.value)
                return Result(combined_value, right_result.pos)
        return None

class Exp(Parser):
    def __init__(self, parser, separator):
        self.parser = parser
        self.separator = separator

    def __call__(self, tokens, pos):
        result = self.parser(tokens, pos)

        def process_next(parsed):
            (sepfunc, right) = parsed
            return sepfunc(result.value, right)
        next_parser = self.separator + self.parser ^ process_next

        next_result = result
        while next_result:
            next_result = next_parser(tokens, result.pos)
            if next_result:
                result = next_result
        return result            

class Alternate(Parser):
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __call__(self, tokens, pos):
        left_result = self.left(tokens, pos)
        if left_result:
            return left_result
        else:
            right_result = self.right(tokens, pos)
            return right_result

class Opt(Parser):
    def __init__(self, parser):
        self.parser = parser

    def __call__(self, tokens, pos):
        result = self.parser(tokens, pos)
        if result:
            return result
        else:
            return Result(None, pos)

class Rep(Parser):
    def __init__(self, parser):
        self.parser = parser

    def __call__(self, tokens, pos):
        results = []
        result = self.parser(tokens, pos)
        while result:
            results.append(result.value)
            pos = result.pos
            result = self.parser(tokens, pos)
        return Result(results, pos)

class Process(Parser):
    def __init__(self, parser, function):
        self.parser = parser
        self.function = function

    def __call__(self, tokens, pos):
        result = self.parser(tokens, pos)
        if result:
            result.value = self.function(result.value)
            return result

class Lazy(Parser):
    def __init__(self, parser_func):
        self.parser = None
        self.parser_func = parser_func

    def __call__(self, tokens, pos):
        if not self.parser:
            self.parser = self.parser_func()
        return self.parser(tokens, pos)

class Phrase(Parser):
    def __init__(self, parser):
        self.parser = parser

    def __call__(self, tokens, pos):
        result = self.parser(tokens, pos)
        if result and result.pos == len(tokens):
            return result
        else:
            return None


# lexer for LET
RESERVED = 'RESERVED'
INT      = 'INT'
ID       = 'ID'

token_exprs = [
    (r'[ \n\t]+',              None),
    (r'#[^\n]*',               None),
    # (r'\:=',                   RESERVED),
    (r'\(',                    RESERVED),
    (r'\)',                    RESERVED),
    # (r';',                     RESERVED),
    # (r'\+',                    RESERVED),
    # (r'-',                     RESERVED),
    # (r'\*',                    RESERVED),
    # (r'/',                     RESERVED),
    # (r'<=',                    RESERVED),
    # (r'<',                     RESERVED),
    # (r'>=',                    RESERVED),
    # (r'>',                     RESERVED),
    # (r'!=',                    RESERVED),
    (r'=',                     RESERVED),
    # (r'and',                   RESERVED),
    # (r'or',                    RESERVED),
    # (r'not',                   RESERVED),
    (r'zero\?',                    RESERVED),
    (r'if',                    RESERVED),
    (r'then',                  RESERVED),
    (r'else',                  RESERVED),
    (r'let',                  RESERVED),
    (r'in',                  RESERVED),
    # (r'while',                 RESERVED),
    # (r'do',                    RESERVED),
    # (r'end',                   RESERVED),
    (r'[0-9]+',                INT),
    (r'[A-Za-z][A-Za-z0-9_]*', ID),
]

def let_lex(characters):
    return lex(characters, token_exprs)

# let parser

def keyword(kw):
    return Reserved(kw, RESERVED)

num = Tag(INT) ^ (lambda i: int(i))
id = Tag(ID)

# Ast
class AbsExp:pass 

class VarExp(AbsExp):
    def __init__(self, name):
        self.name = name 
        
    # def __str__(self):
    #     return """
    # VarExp: {}
    # """.format(self.name)
    
class LetExp(AbsExp):
    def __init__(self, bound_var, bind_exp: AbsExp, body: AbsExp):
        self.bound_var = bound_var 
        self.bind_exp = bind_exp
        self.body = body 

    # def __str__(self):
    #     return """
    # LetExp:
    #     bound_var: {}
    #     bind_exp: {}
    #     body: {}
    # """.format(self.bound_var, str(self.bind_exp), str(self.body))
    
class ConstExp(AbsExp):
    def __init__(self, num):
        self.num = num 

    # def __str__(self):
    #     return "ConstExp: {}".format(self.num)


class ZeroExp(AbsExp):
    def __init__(self, exp1):
        self.exp1 = exp1
        

class IfExp(AbsExp):
    def __init__(self, pred, true_body, false_body):
        self.pred = pred 
        self.true_body = true_body
        self.false_body = false_body
        

def dumpAst(ast: AbsExp):
    def dumpAst_(ast: AbsExp, align_length):
        if isinstance(ast, ConstExp):
            return  dumpConstExp(ast, align_length)
        elif isinstance(ast, VarExp):
            return  dumpVarExp(ast, align_length)
        elif isinstance(ast, LetExp):
            return  dumpLetExp(ast, align_length)
        elif isinstance(ast, ZeroExp):
            return  dumpZeroExp(ast, align_length)
        elif isinstance(ast, IfExp):
            return dumpIfExp(ast, align_length)
        else:
            raise Exception("not expected")
        
    def dumpConstExp(expr: ConstExp, align_length):
      return """ConstExp: {}""".format(expr.num)
    
    def dumpVarExp(expr: VarExp, align_length):
        return """VarExp: {}""".format(expr.name)

    def dumpZeroExp(expr: ZeroExp, align_length):
        return """
    {0}ZeroExp: {1}""".format(align_length * '\t', dumpAst_(expr.exp1, align_length + 1))
    
    def dumpLetExp(expr: LetExp, align_length):
        return """
    {0}LetExp:
    {0}  bound_var: {1}
    {0}  bind_exp: {2}
    {0}  body: {3}
    """.format(align_length * '\t', 
               expr.bound_var, 
               dumpAst_(expr.bind_exp, align_length + 1), 
               dumpAst_(expr.body, align_length + 1))
    
    def dumpIfExp(expr: IfExp, align_length):
        return """
    {0}IfExp:
    {0}pred:{1}
    {0}true_body:{2}
    {0}false_body:{3}
    """.format(align_length * '\t',
               dumpAst_(expr.pred, align_length + 1),
               dumpAst_(expr.true_body, align_length + 1),
               dumpAst_(expr.false_body, align_length + 1))
    return dumpAst_(ast, 0)
   
# Top level parser
def let_parse(tokens):
    result = parser()(tokens, 0)
    ast = result.value
    return ast

def parser():
    return Phrase(exprP())    

def exprP():
    return constP() | varP() | letP() | zeroP() | ifP()

def varP():
    debug("var")
    return id ^ (lambda x : VarExp(x))

def constP():
    debug("const")
    return num ^ (lambda num : ConstExp(num))

def letP():
    debug("let")
    def process(parsed):
        debug("let")
        debug(parsed)
        (((((let,name),_),bind_exp),in_),body) = parsed
        return LetExp(name, bind_exp, body)
    
    return keyword("let") + id + keyword("=") + Lazy(exprP) + keyword("in") + Lazy(exprP) ^ process

def zeroP():
    debug("zero")
    def process(parsed):
        (((_,_),expr),_) = parsed
        return ZeroExp(expr)
    
    return keyword("zero?")+ keyword("(") + Lazy(exprP) + keyword(")") ^ process

def ifP():
    debug("if")
    def process(parsed):
        (((((_,a),_),b),_),c) = parsed
        return IfExp(a, b, c)
    return keyword("if") + Lazy(exprP) + keyword("then") + Lazy(exprP) + keyword("else") + Lazy(exprP) ^ process

def test():
    tokens = let_lex("let a = if 1 then 2 else 3 in let b = 3 in zero?(1)")
    # tokens = let_lex("zero?(1)")
    # tokens = let_lex("if a then b else c")
    ast = let_parse(tokens)
    print(dumpAst(ast))
    # print(ast)

if __name__ == "__main__":
    test()
