"""
    syntax:
    Program ::= Expression
            a-program(exp1)

    Expression ::= Number
                const-exp(num)

    Expression ::= - (Expression, Expression)
                diff-exp(exp1, exp2)

    Expression ::= zero? (Expression)
                zero?-exp(exp1)

    Expression ::= if Expression then Expression else Expression
                if-exp(exp1, exp2, exp3)

    Expression ::= Identifier
                var-exp(var)

    Expression ::= let Identifier = Expression in Expression
                let-exp (var, exp1, body)
"""



class Token:
    def __init__(self, token_type, token_value):
        self.type = token_type
        self.value = token_value

    def __str__(self):
        return "{0}:{1}".format(self.type, self.value)




class TokenizerException(Exception): pass


def tokenize(text):
    import re
    token_pattern = r"""
    (?P<ID>[a-zA-Z_][a-zA-Z0-9_?]*)
    |(?P<NUMBER>-?[0-9]+)
    |(?P<MINUS>-)
    |(?P<EQUAL>=)
    |(?P<LEFT_P>[(])
    |(?P<RIGHT_P>[)])
    |(?P<COMMA>[,])
    |(?P<SPACE>\s+)
    """
    token_re = re.compile(token_pattern, re.VERBOSE)
    keywords = ['let', 'in', 'if', "then", "else", "zero?"]
    pos = 0
    while True:
        m = token_re.match(text, pos)
        if not m: break
        pos = m.end()
        tokname = m.lastgroup
        tokvalue = m.group(tokname)

        if tokname == "ID" and tokvalue in keywords:
            yield Token(tokvalue.upper(), tokvalue)
        elif tokname != "SPACE":
            yield Token(tokname, tokvalue)

    if pos != len(text):
        raise TokenizerException('tokenizer stopped at pos %r of %r' % (
            pos, len(text)))


class Lexer:
    def __init__(self, s):
        self.tokens = list(tokenize(s))
        self.tokens.append(Token("EOF", "_"))
        self.pos = 0

    def current_token(self):
        return self.tokens[self.pos]

    def next_token(self):
        token = self.tokens[self.pos]
        self.pos += 1
        return token


class ParseException(BaseException):
    def __init__(self, msg):
        BaseException.__init__(self, msg)


class Exp:
    pass


class ConstExp(Exp):
    def __init__(self, number):
        self.number = number


class DiffExp(Exp):
    def __init__(self, exp1, exp2):
        self.exp1 = exp1
        self.exp2 = exp2


class IsZeroExp(Exp):
    def __init__(self, exp):
        self.exp = exp


class IFExp(Exp):
    def __init__(self, exp1, exp2, exp3):
        self.exp1 = exp1
        self.exp2 = exp2
        self.exp3 = exp3


class VarExp(Exp):
    def __init__(self, name):
        self.name = name


class LetExp(Exp):
    def __init__(self, name, exp1, body):
        self.name = name
        self.exp1 = exp1
        self.body = body


class Parser:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer

    def require(self, token_type):
        token = self.lexer.next_token()
        if token_type != token.type:
            raise ParseException("require")
        else:
            return token

    def parse(self):
        token = self.lexer.current_token()
        if token.type == "NUMBER":
            return self.parse_number_exp()
        elif token.type == "MINUS":
            return self.parse_diff_exp()
        elif token.type == "ZERO?":
            return self.parse_zero_exp()
        elif token.type == "IF":
            return self.parse_if_exp()
        elif token.type == "ID":
            return self.parse_var_exp()
        elif token.type == "LET":
            return self.parse_let_exp()

    def parse_number_exp(self):
        token = self.require("NUMBER")
        return ConstExp(int(token.value))

    def parse_diff_exp(self):
        self.require("MINUS")
        self.require("LEFT_P")
        exp1 = self.parse()
        self.require("COMMA")
        exp2 = self.parse()
        self.require("RIGHT_P")
        return DiffExp(exp1, exp2)

    def parse_zero_exp(self):
        self.require("ZERO?")
        self.require("LEFT_P")
        exp = self.parse()
        self.require("RIGHT_P")
        return IsZeroExp(exp)

    def parse_if_exp(self):
        self.require("IF")
        exp1 = self.parse()
        self.require("THEN")
        exp2 = self.parse()
        self.require("ELSE")
        exp3 = self.parse()
        return IFExp(exp1, exp2, exp3)

    def parse_var_exp(self):
        token = self.require("ID")
        return VarExp(token.value)

    def parse_let_exp(self):
        self.require("LET")
        id_token = self.require("ID")
        self.require("EQUAL")
        exp1 = self.parse()
        self.require("IN")
        body = self.parse()
        return LetExp(id_token.value, exp1, body)


class LookupFailedException(BaseException):
    pass


class Env:
    def apply(self, key):
        pass


class EmptyEnv(Env):
    def apply(self, key):
        raise LookupFailedException()


class ExtendEnv(Env):
    def __init__(self, key, value, old_env: Env):
        self.old_env = old_env
        self.key = key
        self.value = value

    def apply(self, key):
        if key == self.key:
            return self.value
        else:
            return self.old_env.apply(key)


def init_env():
    # return ExtendEnv("xx", NumVal(1), EmptyEnv())
    return EmptyEnv()


class ExpVal:
    def to_num(self):
        raise Exception("not num")

    def to_bool(self):
        raise Exception("not bool")


class NumVal(ExpVal):
    def __init__(self, value):
        self.value = value

    def to_num(self):
        return self.value

    def __str__(self):
        return str(self.value)


class BoolVal(ExpVal):
    def __init__(self, value):
        self.value = value

    def to_bool(self):
        return self.value


def evaluate(exp: Exp, env: Env) -> ExpVal:
    if isinstance(exp, ConstExp):
        return NumVal(exp.number)
    if isinstance(exp, VarExp):
        return env.apply(exp.name)
    if isinstance(exp, DiffExp):
        v1 = evaluate(exp.exp1, env)
        v2 = evaluate(exp.exp2, env)
        return NumVal(v1.to_num() - v2.to_num())
    if isinstance(exp, IsZeroExp):
        v1 = evaluate(exp.exp, env)
        return BoolVal(v1.to_num() == 0)
    if isinstance(exp, IFExp):
        v1 = evaluate(exp.exp1, env)
        if v1.to_bool():
            v2 = evaluate(exp.exp2, env)
            return v2
        else:
            v3 = evaluate(exp.exp3, env)
            return v3
    if isinstance(exp, LetExp):
        v1 = evaluate(exp.exp1, env)
        return evaluate(exp.body, ExtendEnv(exp.name, v1, env))


def run(prog):
    parser = Parser(Lexer(prog))
    exp = parser.parse()

    text = """
the program:
---
{0}
---
evaluated to:
{1}
    """.format(prog, evaluate(exp, init_env()))
    print(text)

# run("let n = 1 in n")
# run("let n = 9 in -(n, -1)")
run("""
let n = 9 
in 
    if zero?(n) 
    then -(n, 1)
    else -(n, 2)
""")
