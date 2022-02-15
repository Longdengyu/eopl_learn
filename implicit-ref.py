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

    Expression ::= proc (Identifier) Expression
                proc-exp (var, body)

    Expression ::= (rator, rand)
                call-exp(rator, rand)

    Expression ::= letrec Identifier (Identifier) = Expression in Expression
                letrec-exp (p-name b-var p-body letrec-body)

    Expression ::= set Identifier = Expression
                assign-exp (var exp1)
"""

import re


class Token:
    def __init__(self, token_type, token_value):
        self.type = token_type
        self.value = token_value

    def __str__(self):
        return "{0}:{1}".format(self.type, self.value)


token_pattern = r"""
(?P<ID>[a-zA-Z_][a-zA-Z0-9_?]*)
|(?P<NUMBER>-?[0-9]+)
|(?P<MINUS>-)
|(?P<EQUAL>=)
|(?P<LEFT_P>[(])
|(?P<RIGHT_P>[)])
|(?P<COMMA>[,])
|(?P<SPACE>[\s\n]+)
|(?P<SEMICOLON>;)
"""

token_re = re.compile(token_pattern, re.VERBOSE)


class TokenizerException(Exception): pass


def tokenize(text):
    keywords = ['let', 'in', 'if', "then", "else", "zero?", "proc", "letrec", "set", "begin", "end"]
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


class ProcExp(Exp):
    def __init__(self, var, body):
        self.var = var
        self.body = body


class CallExp(Exp):
    def __init__(self, rator, rand):
        self.rator = rator
        self.rand = rand


class LetRecExp(Exp):
    def __init__(self, p_name, b_var, p_body, letrec_body):
        self.p_name = p_name
        self.b_var = b_var
        self.p_body = p_body
        self.letrec_body = letrec_body


class AssignExp(Exp):
    def __init__(self, var, exp1):
        self.var = var
        self.exp1 = exp1


class BeginExp(Exp):
    def __init__(self, expressions):
        self.expressions = expressions


class Parser:
    def __init__(self, lexer: Lexer):
        self.lexer = lexer

    def require(self, token_type):
        token = self.lexer.next_token()
        if token_type != token.type:
            raise ParseException("require")
        else:
            return token

    def parse_exp(self):
        exp = self.parse()
        self.require("EOF")
        return exp

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
        elif token.type == "PROC":
            return self.parse_proc_exp()
        elif token.type == "LEFT_P":
            return self.parse_call_exp()
        elif token.type == "LETREC":
            return self.parse_letrec_exp()
        elif token.type == "SET":
            return self.parse_assign_exp()
        elif token.type == "BEGIN":
            return self.parse_begin_exp()

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

    def parse_proc_exp(self):
        self.require("PROC")
        self.require("LEFT_P")
        id_token = self.require("ID")
        self.require("RIGHT_P")
        exp = self.parse()
        return ProcExp(id_token.value, exp)

    def parse_call_exp(self):
        self.require("LEFT_P")
        rator_exp = self.parse()
        self.require("COMMA")
        rand_exp = self.parse()
        self.require("RIGHT_P")
        return CallExp(rator_exp, rand_exp)

    def parse_letrec_exp(self):
        self.require("LETREC")
        p_name_token = self.require("ID")
        self.require("LEFT_P")
        b_var_token = self.require("ID")
        self.require("RIGHT_P")
        self.require("EQUAL")
        p_body = self.parse()
        self.require("IN")
        letrec_body = self.parse()
        return LetRecExp(p_name_token.value, b_var_token.value, p_body, letrec_body)

    def parse_assign_exp(self):
        self.require("SET")
        var = self.require("ID").value
        self.require("EQUAL")
        exp1 = self.parse()
        return AssignExp(var, exp1)

    def parse_begin_exp(self):
        expressions = []
        self.require("BEGIN")
        exp1 = self.parse()
        expressions.append(exp1)
        while True:
            token = self.lexer.current_token()
            if token.type == "SEMICOLON":
                self.require("SEMICOLON")
                exp2 = self.parse()
                expressions.append(exp2)
            else:
                break
        self.require("END")
        return BeginExp(expressions)


class LookupFailedException(BaseException):
    pass


class Env:
    def apply(self, var):
        pass


class EmptyEnv(Env):
    def apply(self, var):
        raise LookupFailedException()


class ExtendEnv(Env):
    def __init__(self, key, value, old_env: Env):
        self.old_env = old_env
        self.key = key
        self.value = value

    def apply(self, var):
        if var == self.key:
            return self.value
        else:
            return self.old_env.apply(var)


class LetRecEnv(Env):
    def __init__(self, p_name, b_var, p_body, saved_env: Env):
        self.p_name = p_name
        self.b_var = b_var
        self.p_body = p_body
        self.saved_env = saved_env

    def apply(self, var):
        if var == self.p_name:
            ref = new_ref(ProcVal(self.b_var, self.p_body, self))
            return ref
        else:
            return self.saved_env.apply(var)


def init_env():
    return EmptyEnv()


class ExpVal:
    def to_bool(self):
        raise Exception("not bool")

    def to_proc(self):
        raise Exception("not proc")

    def to_num(self):
        raise Exception("not num")

    def to_ref(self):
        raise Exception("not ref")


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


class Procedure:
    def __init__(self, var, body:Exp, env:Env):
        self.var = var
        self.body = body
        self.env = env


class ProcVal(ExpVal):
    def __init__(self, var, body:Exp, env:Env):
        self.procedure = Procedure(var, body, env)

    def to_proc(self):
        return self.procedure


class RefVal(ExpVal):
    def __init__(self, index):
        self.index = index

    def to_ref(self):
        return self.index


g_store = []


def get_store():
    global g_store
    return g_store


def new_ref(value) -> int:
    the_store = get_store()
    next_index = len(the_store)
    the_store.append(value)
    return RefVal(next_index)


def de_ref(ref: RefVal):
    the_store = get_store()
    return the_store[ref.to_ref()]


def set_ref(ref: RefVal, value):
    the_store = get_store()
    if ref.to_ref() + 1 > len(the_store):
        raise Exception("invalid reference")
    else:
        the_store[ref.to_ref()] = value


def evaluate(exp: Exp, env: Env) -> ExpVal:
    if isinstance(exp, ConstExp):
        return NumVal(exp.number)
    if isinstance(exp, VarExp):
        ref: RefVal = env.apply(exp.name)
        return de_ref(ref)
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
        ref = new_ref(v1)
        return evaluate(exp.body, ExtendEnv(exp.name, ref, env))
    if isinstance(exp, ProcExp):
        return ProcVal(exp.var, exp.body, env)
    if isinstance(exp, CallExp):
        proc_val = evaluate(exp.rator, env)
        procedure = proc_val.to_proc()
        rand_val = evaluate(exp.rand, env)
        ref = new_ref(rand_val)
        return evaluate(procedure.body, ExtendEnv(procedure.var, ref, procedure.env))
    if isinstance(exp, LetRecExp):
        return evaluate(exp.letrec_body, LetRecEnv(exp.p_name, exp.b_var, exp.p_body, env))
    if isinstance(exp, BeginExp):
        ret_val = None
        for the_exp in exp.expressions:
            ret_val = evaluate(the_exp, env)
        return ret_val
    if isinstance(exp, AssignExp):
        ref: RefVal = env.apply(exp.var)
        v1 = evaluate(exp.exp1, env)
        set_ref(ref, v1)


# tokens = list(tokenize("proc"))
# print(tokens)

parser = Parser(Lexer("""
let num = 0 in
letrec f(n) =    if zero?(n) 
                    then num
                    else 
                        begin
                            set num = -(num, -2);
                            (f, -(n, 1))
                        end    
in (f, 10)    
"""))

exp = parser.parse_exp()
# print(exp)
print(evaluate(exp, init_env()))
