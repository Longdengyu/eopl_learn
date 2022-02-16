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
"""

token_re = re.compile(token_pattern, re.VERBOSE)


class TokenizerException(Exception): pass


def tokenize(text):
    keywords = ['let', 'in', 'if', "then", "else", "zero?", "proc", "letrec"]
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
        # self.require("COMMA")
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
            return ProcVal(self.b_var, self.p_body, self)
        else:
            return self.saved_env.apply(var)


def init_env():
    # return ExtendEnv("xx", NumVal(1), EmptyEnv())
    return EmptyEnv()


class ExpVal:
    def to_bool(self):
        raise Exception("not bool")

    def to_proc(self):
        raise Exception("not proc")

    def to_num(self):
        raise Exception("not num")


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


def value_of(exp: Exp, env: Env) -> ExpVal:
    if isinstance(exp, ConstExp):
        return NumVal(exp.number)
    if isinstance(exp, VarExp):
        return env.apply(exp.name)
    if isinstance(exp, DiffExp):
        v1 = value_of(exp.exp1, env)
        v2 = value_of(exp.exp2, env)
        return NumVal(v1.to_num() - v2.to_num())
    if isinstance(exp, IsZeroExp):
        v1 = value_of(exp.exp, env)
        return BoolVal(v1.to_num() == 0)
    if isinstance(exp, IFExp):
        v1 = value_of(exp.exp1, env)
        if v1.to_bool():
            v2 = value_of(exp.exp2, env)
            return v2
        else:
            v3 = value_of(exp.exp3, env)
            return v3
    if isinstance(exp, LetExp):
        v1 = value_of(exp.exp1, env)
        return value_of(exp.body, ExtendEnv(exp.name, v1, env))

    if isinstance(exp, ProcExp):
        return ProcVal(exp.var, exp.body, env)

    if isinstance(exp, CallExp):
        proc_val = value_of(exp.rator, env)
        procedure = proc_val.to_proc()

        rand_val = value_of(exp.rand, env)
        return value_of(procedure.body, ExtendEnv(procedure.var, rand_val, procedure.env))

    if isinstance(exp, LetRecExp):
        return value_of(exp.letrec_body, LetRecEnv(exp.p_name, exp.b_var, exp.p_body, env))


class Type:pass
class IntType(Type):pass
class BoolType(Type):pass


class ProcType(Type):
    def __init__(self, arg_type, result_type):
        self.arg_type = arg_type
        self.result_type = result_type

class VarType:
    def __init__(self, sn):
        self.sn = sn



def apply_one_subst(ty0: VarType, tvar: VarType, ty1: Type) -> Type:
    if isinstance(ty0, IntType):
        return IntType()
    elif isinstance(ty0, BoolType):
        return BoolType()
    elif isinstance(ty0, ProcType):
        return ProcType(apply_one_subst(ty0.arg_type, tvar, ty1),
                 apply_one_subst(ty0.result_type, tvar, ty1))
    elif isinstance(ty0, VarType):
        if ty0.sn == tvar.sn:
            return ty1
        else:
            return ty0
    else:
        raise Exception("forbiden")


Subst = dict


def empty_subst():
    return {}


def extend_subst(orig_subst: Subst, tvar: VarType, ty: Type) -> Subst:
    if tvar.sn in orig_subst:
        raise Exception("forbidden")

    subst = orig_subst.copy()
    for tvar_item in subst:
        ty_item = subst[tvar_item]
        sub_result = apply_one_subst(ty_item, tvar, ty)
        subst[tvar_item] = sub_result

    subst[tvar.sn] = ty
    return subst


def apply_subst_to_type(ty: Type, subst: Subst) -> Type:
    if isinstance(ty, IntType):
        return IntType()
    elif isinstance(ty, BoolType):
        return BoolType()
    elif isinstance(ty, ProcType):
        return ProcType(apply_subst_to_type(ty.arg_type, subst),
                        apply_subst_to_type(ty.result_type, subst))
    elif isinstance(ty, VarType):
        if ty.sn in subst:
            return subst[ty.sn]
        else:
            return ty


def same_type(ty1: Type, ty2: Type):
    if isinstance(ty1, IntType) and isinstance(ty2, IntType):
        return True
    elif isinstance(ty1, BoolType) and isinstance(ty2, BoolType):
        return True
    elif isinstance(ty1, ProcType) and isinstance(ty2, ProcType):
        return same_type(ty1.arg_type, ty2.arg_type) and same_type(ty1.result_type, ty2.result_type)
    elif isinstance(ty1, VarType) and isinstance(ty2, VarType):
        return ty1.sn == ty2.sn
    else:
        return False


def is_no_occurrence(tvar: VarType, ty: Type):
    if isinstance(ty, IntType) or isinstance(ty, BoolType):
        return True
    elif isinstance(ty, ProcType):
        return is_no_occurrence(tvar, ty.arg_type) and is_no_occurrence(tvar, ty.result_type)
    elif isinstance(ty, VarType):
        return tvar.sn != ty.sn


def unifier(ty1: Type, ty2: Type, subst: Subst, exp: Exp) -> Subst:
    ty1 = apply_subst_to_type(ty1, subst)
    ty2 = apply_subst_to_type(ty2, subst)
    if same_type(ty1, ty2):
        return subst
    elif isinstance(ty1, VarType):
        if is_no_occurrence(ty1, ty2):
            return extend_subst(subst, ty1, ty2)
        else:
            raise Exception("occurrence-violation")
    elif isinstance(ty2, VarType):
        if is_no_occurrence(ty2, ty1):
            return extend_subst(subst, ty2, ty1)
        else:
            raise Exception("occurrence-violation")
    elif isinstance(ty1, ProcType) and isinstance(ty2, ProcType):
        subst = unifier(ty1.arg_type, ty2.arg_type, subst, exp)
        subst = unifier(ty1.result_type, ty2.result_type, subst, exp)
        return subst
    else:
        raise Exception("unification-failure")

g_sn = 0
def fresh_var_type():
    global g_sn
    g_sn += 1
    return VarType(g_sn)

class TEnv:
    def apply(self, var):
        pass


class TEmptyEnv(TEnv):
    def apply(self, var):
        raise LookupFailedException()


class TExtendEnv(TEnv):
    def __init__(self, key, value, old_env: TEnv):
        self.old_env = old_env
        self.key = key
        self.value = value

    def apply(self, var):
        if var == self.key:
            return self.value
        else:
            return self.old_env.apply(var)


def type_of(exp: Exp, tenv: TEnv, subst: Subst) -> (Type, Subst):
    if isinstance(exp, ConstExp):
        return IntType(), subst
    if isinstance(exp, VarExp):
        return tenv.apply(exp.name), subst
    if isinstance(exp, DiffExp):
        t1, s1 = type_of(exp.exp1, tenv, subst)
        s1 = unifier(t1, IntType(), s1, exp.exp1)
        t2, s2 = type_of(exp.exp2, tenv, s1)
        s2 = unifier(t1, IntType(), s2, exp.exp2)
        return IntType, s2
    if isinstance(exp, IsZeroExp):
        t1, s1 = type_of(exp.exp, tenv, subst)
        s2 = unifier(t1, IntType(), s1, exp)
        return BoolType(), s2
    if isinstance(exp, IFExp):
        t1, subst = type_of(exp.exp1, tenv, subst)
        subst = unifier(t1, BoolType(), subst, exp)
        t2, subst = type_of(exp.exp2, tenv, subst)
        t3, subst = type_of(exp.exp3, tenv, subst)
        subst = unifier(t2, t3, subst, exp)
        return t3, subst
    if isinstance(exp, LetExp):
        t_var, subst1 = type_of(exp.exp1, tenv, subst)
        return type_of(exp.body, TExtendEnv(exp.name, t_var, tenv), subst)
    if isinstance(exp, ProcExp):
        var_type = fresh_var_type()
        body_type, subst = type_of(exp.body, TExtendEnv(exp.var, var_type, tenv), subst)
        return ProcType(var_type, body_type), subst
    if isinstance(exp, CallExp):
        result_type = fresh_var_type()
        rator_type, subst = type_of(exp.rator, tenv, subst)
        rand_type, subst = type_of(exp.rand, tenv, subst)
        subst = unifier(rator_type, ProcType(rand_type, result_type), subst, exp)
        return result_type, subst
    if isinstance(exp, LetRecExp):
        return type_of(exp.letrec_body, LetRecEnv(exp.p_name, exp.b_var, exp.p_body, tenv))
    if isinstance(exp, LetRecExp):
        # TODO
        raise Exception("TODO:")

    
def run(prog):
    parser = Parser(Lexer(prog))
    exp = parser.parse_exp()

    text = """
the program:
---
{0}
---
evaluated to:
{1}
    """.format(prog, value_of(exp, init_env()))
    print(text)


# run("""
# letrec f(n) =
#         if zero?(n)
#         then 0
#         else -((f -(n, 1)), -(0, n))
# in (f 10)
# """)


def check_type(prog):
    parser = Parser(Lexer(prog))
    exp = parser.parse_exp()
    t, s = type_of(exp, TEmptyEnv(), {})
    print(t)


check_type("""
let f = proc (z) z in proc (x) -((f x), 1)
""")