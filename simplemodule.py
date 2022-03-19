# TODO: rewrite the evaluator with visitor pattern
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
    (r'\[',                    RESERVED),
    (r'\]',                    RESERVED),
    (r'\{',                    RESERVED),
    (r'\}',                    RESERVED),
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
    (r'interface',                  RESERVED),
    (r'in',                  RESERVED),
    (r'module',                  RESERVED),
    (r'body',                  RESERVED),
    (r'from',                  RESERVED),
    (r'take',                  RESERVED),
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

class Program:
    def __init__(self, modules, body):
        self.modules = modules
        self.body = body 

class Module:
    def __init__(self, name, interface, body):
        self.name = name 
        self.interface = interface 
        self.body = body 

class ModuleInterface:
    def __init__(self,names):
        self.names = names

class ModuleBody:
    def __init__(self, defns):
        self.defns = defns 

class Defn:
    def __init__(self, name, exp):
        self.name = name 
        self.exp = exp

# helper function for ast
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
        elif isinstance(ast, Program):
            return dumpProgram(ast, align_length)
        elif isinstance(ast, Module):
            return dumpModule(ast, align_length)
        elif isinstance(ast, ModuleBody):
            return dumpModuleBody(ast, align_length)
        elif isinstance(ast, Defn):
            return dumpDefn(ast, align_length)
        elif isinstance(ast, FromExp):
            return dumpFromExp(ast, align_length)
        else:
            raise Exception("not expected")
    def dumpFromExp(ast: FromExp, align_length):
        return "FromExp:({}, {})".format(ast.m_name, ast.m_var)
    
    def dumpConstExp(expr: ConstExp, align_length):
      return """ConstExp: {}""".format(expr.num)
    
    def dumpVarExp(expr: VarExp, align_length):
        return """VarExp: {}""".format(expr.name)

    def dumpZeroExp(expr: ZeroExp, align_length):
        return """
    {0}ZeroExp: {1}""".format(align_length * '  ', dumpAst_(expr.exp1, align_length + 1))
    
    def dumpLetExp(expr: LetExp, align_length):
        return """
    {0}LetExp:
    {0}  bound_var: {1}
    {0}  bind_exp: {2}
    {0}  body: {3}
    """.format(align_length * '  ', 
               expr.bound_var, 
               dumpAst_(expr.bind_exp, align_length + 1), 
               dumpAst_(expr.body, align_length + 1))
    
    def dumpProgram(prog, align_length):
        str_modules = map((lambda m: dumpAst_(m, align_length + 3)), prog.modules)
        return """
    {0}Program:
    {0}    modules:{1}
    {0}    program_body:{2}
    """.format(align_length * '  ', '\n'.join(str_modules), dumpAst_(prog.body, align_length + 1)
            )
    def dumpModule(module: Module, align_length):
        return """
    {0}Module:
    {0} name: {1}
    {0} interfaces: {2}
    {0} module_body: {3}
    """.format(align_length * '  ', module.name, ",".join(module.interface.names), dumpAst_(module.body, align_length + 1))
    
    def dumpModuleBody(moduleBody, align_length):
        str_defns = map((lambda defn: dumpAst_(defn, align_length + 2)), moduleBody.defns)
        return "\n".join(str_defns)
    
    def dumpDefn(defn, align_length):
        return """
    {0}name:{1} 
    {0}exp:{2}
    """.format(align_length * '  ', defn.name, dumpAst_(defn.exp, align_length + 1))
    
    def dumpIfExp(expr: IfExp, align_length):
        return """
    {0}IfExp:
    {0}pred:{1}
    {0}true_body:{2}
    {0}false_body:{3}
    """.format(align_length * '  ',
               dumpAst_(expr.pred, align_length + 1),
               dumpAst_(expr.true_body, align_length + 1),
               dumpAst_(expr.false_body, align_length + 1))
    return dumpAst_(ast, 0)


# build parser using the combinator
# Top level parser
def let_parse(tokens):
    result = parser()(tokens, 0)
    ast = result.value
    return ast

def parser():
    # return Phrase(exprP())    
    return Phrase(programP())    

def exprP():
    return constP() | varP() | letP() | zeroP() | ifP() | fromP()

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

class FromExp(AbsExp):
    def __init__(self, m_name, m_var):
        self.m_name = m_name
        self.m_var = m_var
        
def fromP():
    def process(parsed):
        (((m1, module_name), m3), export_varname) = parsed
        return FromExp(module_name, export_varname)         

    return keyword("from") + id + keyword("take") + id ^ process
    

def moduleP():
    def process(parsed):
        debug("process moduleP")
        (((((((((m1,module_name),m3),m4),interface_names),m6),m7),m8),module_body),m10) = parsed
        return Module(module_name, ModuleInterface(interface_names), module_body)

    return keyword("module") + id + \
        keyword("interface") + keyword("[") + Lazy(moduleInterfaceP) + keyword("]") + \
            keyword("body") + keyword("[") + Lazy(moduleBodyP) + keyword("]") ^ process

def moduleInterfaceP():
    return Rep(id) 


def moduleBodyP():
    def process(parsed):
        debug("process moduleBodyP")
        defns = parsed
        return ModuleBody(defns)
    
    return Rep(defnP()) ^ process


def defnP():
    def process(parsed):
        ((name,m2),exp) = parsed 
        return Defn(name, exp)
    return id + keyword("=") + Lazy(exprP) ^ process

def programP():
    def process(parsed):
        debug("process programP")
        (modules, body) = parsed
        return Program(modules, body)
        
    return  Rep(moduleP()) + Lazy(exprP) ^ process

# evaluator 
class AbsEnv: 
    def apply(self, name):
        raise Exception("not implented")

class EmptyEnv(AbsEnv):
    def apply(self, name):
        raise Exception("not found: {}".format(name))

class ExtendEnv(AbsEnv):
    def __init__(self, name, value, old_env: AbsEnv):
        self.name = name 
        self.value = value 
        self.old_env = old_env 

    def apply(self, name):
        if self.name == name:
            return self.value 
        else:
            return self.old_env.apply(name)

class ExpValue:
    def __str__(self):
        raise Exception("not implemented") 

class IntValue(ExpValue):
    def __init__(self, value):
        self.value = value 

    def __str__(self):
        return str(self.value)

class BoolValue(ExpValue):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return str(self.value)


class ModuleValue(ExpValue):
    def __init__(self, name, exports):
        self.name = name
        self.exports = exports 
    
    def apply(self, name):
        if not name in self.exports:
            raise Exception("not found in Module")
        return self.exports[name]


def value_of(expr: AbsExp, env: AbsEnv) -> ExpValue:
    if isinstance(expr, ConstExp):
        return IntValue(expr.num)
    elif isinstance(expr, VarExp):
        return env.apply(expr.name)
    elif isinstance(expr, ZeroExp):
        num_val = value_of(expr.exp1, env)
        if isinstance(num_val, IntValue) and num_val.value == 0:
            return BoolValue(True)
        if isinstance(num_val, IntValue) and num_val.value != 0:
            return BoolValue(False)
        else:
            raise Exception("ZeroExp not expected")
    elif isinstance(expr, LetExp):
        bind_val = value_of(expr.bind_exp, env)
        return value_of(expr.body, ExtendEnv(expr.bound_var, bind_val, env))
    elif isinstance(expr, IfExp):
        pred_val = value_of(expr.pred, env)
        if not isinstance(pred_val, BoolValue):
            raise Exception("pred_val must be BoolValue")
        if pred_val.value:
            return value_of(expr.true_body, env)
        else:
            return value_of(expr.false_body, env)
    elif isinstance(expr, Program):
        current_env = env
        for m_exp in expr.modules:
            m_v = value_of(m_exp, current_env)
            current_env = ExtendEnv(m_v.name, m_v, current_env)
        
        return value_of(expr.body, current_env)
    elif isinstance(expr, Module):
        defn_table = {}
        current_env = env 
        for defn in expr.body.defns:
            (name, defn_body) = defn.name, defn.exp
            defn_val = value_of(defn_body, current_env)
            defn_table[name] = defn_val
            current_env = ExtendEnv(name, defn_val, current_env)
        
        exports = {}
        for export_name in expr.interface.names:
            if export_name not in defn_table:
                raise Exception("export name {} not defined".format(export_name))
            else:
                exports[export_name] = defn_table[export_name]
        return ModuleValue(expr.name, exports)
            
    elif isinstance(expr, FromExp):
        m_v = env.apply(expr.m_name)
        return m_v.apply(expr.m_var)
    else:
        raise Exception("not expected")

    """
    value_of
        program
        module
        body
        from
    
    define the value representation of a module
    
    name: module_value
    usage:
        from m take n
        1: lookup module value from env 
        2: lookup value from m
            -> apply m_v name
            
    describe the behavior of let* pattern
    module body:
        0: init_env
        a = 1  env1
        b = a + 1 env2 (a)
        c = a + b env3(b a)
    
    module defins:
        0: init_env
        m1 = xxx env1(init_env)
        m2 = xxx env2(m1, env1)
    
    TODO:
    | collect the usages
        | apply m var -> exprval
        | apply: ModuleValue -> String -> ExpValue
        
    | collect the function
        apply of ModuleValue
        
    | define the module value representation
        class ModuleValue
            def __init__(self, ...)
                lookup_table # the keys is interface names
            def apply(self, name):
                xxx

    | list the function signature
        ModuleVale
            __init__(self, dict)
            apply(self, name)
        
    | make some stub function
        | lookup of module
        | lookup in module
        
    | write the small test case for every aspect of the evaluation
        | case1: value_of(from m1 take name)
            | false case
                module_value1
            | trueth case
                module_value2
        | case2: value_of(module_defn)
            | case 1: empty module 
                module m1 interface [] body []
            | case 2: none empty module 
                module m1 interface [a] body [a = 1]
            | case 3: none empty module
                module m1 interface [a b] body [a = 1 b = a c = a + b]
            | case 4: many module
                case41:
                    module m1 
                    interface [a]
                    body [a = 1]
                    
                    module m2
                    interface [b]
                    body [b = a]
                case41:
                    module m1
                        interface [a]
                        body [a = 1]
                    module m2
                        interface [a]
                        body [a = zero?(from m1 take a)]
        
    | check it
    """

# some test
def test():
    # tokens = let_lex("let a = if 1 then 2 else 3 in let b = 3 in zero?(1)")
    # tokens = let_lex("zero?(1)")
    # tokens = let_lex("if a then b else c")
    # tokens = let_lex('''
    #                  module m1
    #                  interface [a b c]
    #                  body [
    #                      x = zero?(let a = 1 in b)
    #                      y = 2
    #                  ]
    #                  module m2
    #                  interface [x y z]
    #                  body [
    #                      a = b
    #                      c = d
    #                  ]
    #                  1
    #                  ''')
    
    tokens = let_lex("""
                     module m1 
                     interface [
                         num
                     ]
                     body [
                         num = 1
                     ]
                     from m1 take num
                     """)
    # tokens = let_lex('''
    #                 1
    #                 ''')
    # tokens = let_lex("""
    #                  in in in 1
    #                  """)
    print(tokens)         
    ast = let_parse(tokens)
    print(dumpAst(ast))
    # print(ast)

def test_value_of():
    # tokens = let_lex("let a = let a = 8 in a in a")
    # tokens = let_lex("zero?(1)")
    tokens = let_lex("if zero?(1) then 2 else 3")
    ast = let_parse(tokens)
    print(dumpAst(ast))
    print("value_of: ")
    print(value_of(ast, EmptyEnv()))

def test_case1():
    """
            | case1: value_of(from m1 take name)
            | false case
                module_value1
            | trueth case
                module_value2
    """
    program = """
        module m1
            interface [
                a 
                b
            ]
            body [
                a = 1
                b = zero?(a)
            ]
        module m2 
            interface [
                a
                b
            ]
            body [
                a = 1
                b = if zero?(100) then from m1 take a else from m1 take b
            ]
        from m2 take b
    """
    
    tokens = let_lex(program)
    ast = let_parse(tokens)
    print(dumpAst(ast))
    print("value_of: ")
    print(value_of(ast, EmptyEnv()))

if __name__ == "__main__":
    # test()
    test_case1()
    # test_value_of()
