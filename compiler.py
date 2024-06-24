import sys
from sly import Lexer as L, Parser as P

class Lexer(L):
    tokens = {PROGRAM,PROCEDURE,IS,IN,END,IF,
              THEN,ELSE,WHILE,ENDWHILE,ENDIF,DO,UNTIL,REPEAT,READ,WRITE,T,NUM,PIDENTIFIER,
              ASSIGN,NEQ,LE,GE,EQ,LT,GT,PLUS,MINUS,MOD,TIMES,DIV,LP1,RP1,RP2,LP2,COMMA,SEMIC}

    ignore = ' \t'

    @_(r'#.*')
    def ignore_comment(self, t):
        self.lineno += t.value.count('\n')

    @_(r'\n+')
    def ignore_newline(self, t):
        self.lineno += t.value.count('\n')    

    PROGRAM = r'PROGRAM'
    PROCEDURE = r'PROCEDURE'
    IS = r'IS'
    IN = r'IN'
    ENDIF = r'ENDIF'  
    WHILE = r'WHILE'
    ENDWHILE = r'ENDWHILE'  
    END = r'END'
    IF = r'IF'
    THEN = r'THEN'
    ELSE = r'ELSE'
    DO = r'DO'
    UNTIL = r'UNTIL'
    REPEAT = r'REPEAT'
    READ = r'READ'
    WRITE = r'WRITE'
    T = r'T'
    PIDENTIFIER = r'[_a-z]+'
    ASSIGN = r':='
    NEQ = r'!='
    LE = r'<='
    GE = r'>='
    LT = r'<'
    GT = r'>'
    EQ = r'='
    PLUS = r'\+'
    MINUS = r'-'
    TIMES = r'\*'
    MOD = r'%'
    DIV = r'/'
    LP1 = r'\('
    RP1 = r'\)'
    LP2 = r'\['
    RP2 = r'\]'
    COMMA = r','
    SEMIC = r';'

    @_(r'\d+')
    def NUM(self, t):
        t.value = int(t.value)
        return t
    
    def error(self, t):
        print("Illegal character '%s'" % t.value[0])
        self.index += 1

class Parser(P):
    tokens = Lexer.tokens 

    def error(self, token):
        print(f"Syntax Error at line {token.lineno}")
        sys.exit(1)
    
    @_('procedures main')
    def program_all(self,p):
        return (p.procedures,p.main)

    @_('procedures PROCEDURE proc_head IS declarations IN commands END')
    def procedures(self,p):
        p.procedures.append((p.proc_head,('declarations',p.declarations),('commands',p.commands),p.lineno))
        return p.procedures
    
    @_('procedures PROCEDURE proc_head IS IN commands END')
    def procedures(self,p):
        p.procedures.append((p.proc_head,('commands',p.commands),p.lineno))
        return p.procedures
    
    @_('')
    def procedures(self, p):
        return []

    @_('PROGRAM IS declarations IN commands END')
    def main(self,p):
        return (('declarations',p.declarations),('commands',p.commands))
    
    @_('PROGRAM IS IN commands END')
    def main(self,p):
        return (('commands',p.commands))
    
    @_('commands command')
    def commands(self,p):
        p.commands.append(p.command)
        return p.commands
    
    @_('command')
    def commands(self,p):
        return [p.command]
    
    @_('identifier ASSIGN expression SEMIC')
    def command(self,p):
        return ('ASSIGN',p.identifier,p.expression,p.lineno)
    
    @_('IF condition THEN commands ELSE commands ENDIF')
    def command(self,p):
        return ('IFELSE', p.condition,('if_commands',p.commands0),('else_commands',p.commands1),p.lineno)
    
    @_('IF condition THEN commands ENDIF')
    def command(self,p):
        return ('IF', p.condition,('commands',p.commands),p.lineno)

    @_('WHILE condition DO commands ENDWHILE')
    def command(self,p):
        return ('WHILE',p.condition,('commands',p.commands),p.lineno)
    
    @_('REPEAT commands UNTIL condition SEMIC')
    def command(self,p):
        return ('REPEAT',p.condition,('commands',p.commands),p.lineno)
    
    @_('proc_call SEMIC')
    def command(self,p):
        return  p.proc_call
    
    @_('READ identifier SEMIC')
    def command(self,p):
        return ('READ',p.identifier,p.lineno)
    
    @_('WRITE value SEMIC')
    def command(self,p):
        return ('WRITE',p.value,p.lineno)
    
    @_('PIDENTIFIER LP1 args_decl RP1')
    def proc_head(self,p):
        return (p.PIDENTIFIER,p.args_decl)

    @_('PIDENTIFIER LP1 args RP1')
    def proc_call(self,p):
        return ('proc_call',p.PIDENTIFIER,p.args,p.lineno)  
    
    @_( 'declarations COMMA PIDENTIFIER')
    def declarations(self,p):
        p.declarations.append(('var',p.PIDENTIFIER,p.lineno))
        return p.declarations
    
    @_('declarations COMMA PIDENTIFIER LP2 NUM RP2')
    def declarations(self,p):
        p.declarations.append(('arr',p.PIDENTIFIER,p.NUM,p.lineno))
        return  p.declarations
    
    @_('PIDENTIFIER')
    def declarations(self,p):
        return [('var',p.PIDENTIFIER,p.lineno)]
    
    @_('PIDENTIFIER LP2 NUM RP2')
    def declarations(self,p):
        return [('arr',p.PIDENTIFIER,p.NUM,p.lineno)]

    @_('args_decl COMMA PIDENTIFIER')
    def args_decl(self,p):
        p.args_decl.append(('var',p.PIDENTIFIER))
        return p.args_decl
    
    @_('args_decl COMMA T PIDENTIFIER')
    def args_decl(self,p):
        p.args_decl.append(('arr' ,p.PIDENTIFIER))
        return p.args_decl
    
    @_('PIDENTIFIER')
    def args_decl(self,p):
        return [('var',p.PIDENTIFIER)]
    
    @_('T PIDENTIFIER')
    def args_decl(self,p):
        return [('arr',p.PIDENTIFIER)]
    
    @_('args COMMA PIDENTIFIER')
    def args(self,p):
        p.args.append(p.PIDENTIFIER)
        return p.args
    
    @_('PIDENTIFIER')
    def args(self,p):
        return [p.PIDENTIFIER]
    
    @_('value')
    def expression(self,p):
        return ('val',p.value)
    
    @_('value PLUS value')
    def expression(self,p):
        return ('ADD',p.value0,p.value1)
    
    @_('value MINUS value')
    def expression(self,p):
        return ('SUB',p.value0,p.value1)
    
    @_('value TIMES value')
    def expression(self,p):
        return ('MUL',p.value0,p.value1)
    
    @_('value DIV value')
    def expression(self,p):
        return ('DIV',p.value0,p.value1)
    
    @_('value MOD value')
    def expression(self,p):
        return ('MOD',p.value0,p.value1)

    @_('value EQ value')
    def condition(self,p):
        return ('EQ',p.value0,p.value1)
    
    @_('value NEQ value')
    def condition(self,p):
        return ('NEQ',p.value0,p.value1)
    
    @_('value LT value')
    def condition(self,p):
        return ('LT',p.value0,p.value1)
    
    @_('value GT value')
    def condition(self,p):
        return ('GT',p.value0,p.value1)

    @_('value LE value')
    def condition(self,p):
        return ('LE',p.value0,p.value1)

    @_('value GE value')
    def condition(self,p):
        return ('GE',p.value0,p.value1)

    @_('NUM')
    def value(self,p):
        return ('number',p.NUM)
    
    @_('identifier')
    def value(self,p):
        return ('iden',p.identifier)
    
    @_('PIDENTIFIER')
    def identifier(self,p):
        return ('var',p.PIDENTIFIER)
    
    @_('PIDENTIFIER LP2 NUM RP2')
    def identifier(self,p):
        return ('arr2',p.PIDENTIFIER,p.NUM)
    
    @_('PIDENTIFIER LP2 PIDENTIFIER RP2')
    def identifier(self,p):
        return ('arr', p[0],p[2])
    
class Variable:
    def __init__(self,name,type,scope,size = 1):
        self.name = name
        self.type = type
        self.is_assigned = False
        self.in_reg = False 
        self.reg = None
        self.mem = None
        self.scope = scope
        self.size = size

    def set_register(self,reg):
        self.in_reg = True
        self.reg = reg
    
    def set_mem(self,mem):
        self.mem = mem

class Procedure:
    def __init__(self,name):
        self.name = name
        self.declarations = dict() 
        self.arg_map = dict()
        self.arg_list = []
        self.commands = [] 
        self.proc_head = ""
        self.call_num = 0

    def set_commands(self,commands):
        self.commands = commands    

    def set_arguments(self,var_list):
        for i,name in enumerate(var_list):
            self.arg_list[i].set_var(name)
    
    def inc_call_num(self):
        self.call_num += 1    

class Argument:
    def __init__(self, name, type, proc_name):
        self.name = name
        self.type = type
        self.proc_name = proc_name
        self.mem = None
        self.assigning = False

    def set_var(self, var):
        self.var = var
 
class Analyzer: 
    def __init__(self, ast):
        self.used_registers = {"a","h"}  # zawsze uzyte
        self.free_registers = {"b","c","d","e","f","g"}
        self.k = 0 
        self.main_declarations = dict()
        self.proc_declarations = dict()
        self.data = self.analyze(ast)

    def analyze(self,ast):
        for procedure in ast[0]:
            self.analyze_procedure(procedure)
        self.analyze_main(ast[1])

    def reg_delete(self,reg):
        self.used_registers.add(reg)
        if reg in self.free_registers:
            self.free_registers.remove(reg)

    def analyze_procedure(self, procedure):    
        proc_head = procedure[0]
        proc_name = proc_head[0]
        line_number = procedure[-1] 
        arguments = proc_head[1]
        proc = Procedure(proc_name)
        proc.proc_head = self.gen_fun_head(proc_head)

        if proc_name in self.proc_declarations.keys():
            error(f"{line_number}: Procedure name '{proc_name}' already used")
        else:
            self.proc_declarations[proc_name] = proc

        for argument in arguments:
            t = argument[0]
            arr_name = argument[1]
            if t == 'var':
                arr = Argument(arr_name,'var',proc_name)
                proc.arg_map[arr_name] = arr
                proc.arg_list.append(arr)
            else :
                arr = Argument(arr_name,'arr',proc_name)
                proc.arg_map[arr_name] = arr
                proc.arg_list.append(arr)

        for node in procedure:
            if type(node) is int:
                continue
            if node[0] == 'declarations':
                for declaration in node[1]:
                    var_name = declaration[1]
                    line_number = declaration[-1]
                    dec = proc.declarations
                    if (var_name in dec.keys()) or (var_name in proc.arg_map.keys()): 
                        error(f"{line_number}: name '{var_name}' already used")
                    else:
                        if declaration[0]  == 'var':
                            var = Variable(var_name,'var',proc_name)
                            dec[var_name] = var
                        else: 
                            size = declaration[2]
                            var = Variable(var_name,'arr',proc_name,size=size)
                            dec[var_name] = var

        for node in procedure:
            if type(node) is int:
                continue
            if node[0] == 'commands':
                for command in node[1]:
                    self.analyze_command(command,proc.declarations,True,proc_name,proc.arg_map,proc.arg_list)                        

    def analyze_main(self, main):
        for node in main:
            if node[0] == 'declarations':
                for declaration in node[1]:
                    line_number = declaration[-1]
                    var_name = declaration[1]
                    if var_name in self.main_declarations.keys():
                        error(f"{line_number}: name '{var_name}' already used")
                    if declaration[0] == 'arr':
                        size = declaration[2]
                        var = Variable(var_name,'arr','main',size=size)
                        self.main_declarations[var_name] = var
                    else:
                        var = Variable(var_name,'var','main')
                        self.main_declarations[var_name] = var

        for node in main:
            if node[0] == 'commands':
                for command in node[1]:
                    self.analyze_command(command,self.main_declarations)
                    
    def analyze_command(self,command,declarations,is_proc = False,proc_name = None, arg_map = None,arg_list = None):
        line_number = command[-1]
        com_type = command[0]
        if com_type == 'ASSIGN':
            iden =  command[1]
            if iden[1] not in declarations.keys():
                if is_proc and (iden[1] not in arg_map.keys()):
                    error(f"{line_number}: '{iden[1]}' not declared")
                if not is_proc:
                    error(f"{line_number}: '{iden[1]}' not declared")
            if (iden[1] in declarations.keys()) and declarations[iden[1]].type == 'var':
                declarations[iden[1]].is_assigned = True
            if is_proc and (iden[1] in arg_map.keys()):
                arg_map[iden[1]].assigning = True
    
            self.check_iden_syntax(line_number,iden,declarations,is_proc,arg_map)    
            exp = command[2]
 
            if exp[0] == 'val':
                val = exp[1]
                self.check_val_syntax(line_number,val,declarations,is_proc,arg_map)
                        
            operator = exp[0]         
            if operator == 'ADD' or operator == 'SUB' or operator == 'MUL' or operator == 'DIV' or operator == 'MOD':
                val1 = exp[1]
                val2 = exp[2]
                self.check_val_syntax(line_number,val1,declarations,is_proc,arg_map)
                self.check_val_syntax(line_number,val2,declarations,is_proc,arg_map)

            if operator == 'MUL' or operator == 'DIV' :
                self.reg_delete("rg")
                self.reg_delete("re")

        if com_type in {'IF', 'IFELSE', 'WHILE', 'REPEAT'}:
            
            condition = command[1]
            val1 = condition[1]
            val2 = condition[2]

            if com_type != 'REPEAT':
                self.check_val_syntax(line_number,val1,declarations,is_proc,arg_map)
                self.check_val_syntax(line_number,val2,declarations,is_proc,arg_map)

            
            if condition[1] == 'EQ' or condition[1] == 'NEQ' :
                self.reg_delete("rg")
                self.reg_delete("re")
        
            if com_type == 'IF':
                if_commands = command[2][1]
            
                for com in if_commands:
                    self.analyze_command(com,declarations,is_proc,proc_name,arg_map,arg_list) 
            
            if com_type == 'IFELSE':
                if_commands = command[2][1]
                else_commands = command[3][1]
            
                for com in if_commands:
                    self.analyze_command(com,declarations,is_proc,proc_name,arg_map,arg_list)
                
                for com in else_commands:
                    self.analyze_command(com,declarations,is_proc,proc_name,arg_map,arg_list)
            
            if com_type == 'WHILE':
                while_commands = command[2][1]
            
                for com in while_commands:
                    self.analyze_command(com,declarations,is_proc,proc_name,arg_map,arg_list)
            
            if com_type == 'REPEAT':
                repeat_commands = command[2][1]
            
                for com in repeat_commands:
                    self.analyze_command(com,declarations,is_proc,proc_name,arg_map,arg_list)

                self.check_val_syntax(line_number,val1,declarations,is_proc,arg_map)
                self.check_val_syntax(line_number,val2,declarations,is_proc,arg_map)

        if com_type  == 'READ':
            iden = command[1]

            if iden[0] == 'var':
                if (iden[1] in declarations.keys()) and declarations[iden[1]].type == 'var':
                    declarations[iden[1]].is_assigned = True

            self.check_iden_syntax(line_number,iden,declarations,is_proc,arg_map)

        if com_type == 'WRITE':
            val = command[1]
            self.check_val_syntax(line_number,val,declarations,is_proc,arg_map)

        if com_type == 'proc_call':
            c_proc_name = command[1]
            args = command[2]

            if c_proc_name not in self.proc_declarations.keys():
                error(f"{line_number}: Procedure '{c_proc_name}' not declared")
            if c_proc_name == proc_name and is_proc:
                error(f"{line_number}: Recursive call in function '{proc_name}'")

            called_proc_args = self.proc_declarations[c_proc_name].arg_list

            if c_proc_name in self.proc_declarations.keys():
                self.proc_declarations[c_proc_name].inc_call_num()

            if len(args) != len(called_proc_args):
                error(f"{line_number}: Wrong number of arguments, expected {len(called_proc_args)}")

            for i,arg in enumerate(args): 
                if called_proc_args[i].assigning == True:
                    if arg in declarations.keys():
                        declarations[arg].is_assigned = True
                    else: 
                        arg_map[arg].assigning = True

            for arg in args:
                if (arg not in declarations.keys()):
                    if is_proc and arg not in arg_map.keys():
                        error(f"{line_number}: '{arg}' not declared")
                    if not is_proc:
                        error(f"{line_number}: '{arg}' not declared")

            called_proc = self.proc_declarations[c_proc_name]
            head = called_proc.proc_head
            for i,arg in enumerate(args):
                if arg in declarations.keys() and declarations[arg].type != self.proc_declarations[c_proc_name].arg_list[i].type:
                    if declarations[arg].type == 'var':
                         error(f"{line_number}: {arg} is not an array, {head}")  
                    else:
                        error(f"{line_number}: {arg} is not a variable {head}")
                
                if is_proc and arg in arg_map.keys():
                    if arg_map[arg].type != self.proc_declarations[c_proc_name].arg_list[i].type:
                        if arg_map[arg].type == 'var':
                            error(f"{line_number}: {arg} is not an array, {head}")
                        else:
                            error(f"{line_number}: {arg} is not a variable, {head}")
    
    def check_val_syntax(self,line_num,val,declarations,is_proc = False,arguments = None):
        if val[0] == 'iden':
            iden = val[1]
            self.check_iden_syntax(line_num,iden,declarations,is_proc,arguments)

    def check_iden_syntax(self,line_num,iden,declarations,is_proc = False,arguments = None):
        if iden[1] not in declarations.keys():
            if is_proc and (iden[1] not in arguments.keys()):
                error(f"{line_num}: {iden[1]} not declared") 
            if not is_proc:
                error(f"{line_num}: {iden[1]} not declared")
         
        if iden[0] == 'var':
            if (iden[1] in declarations.keys()) and declarations[iden[1]].type != 'var':
                error(f"{line_num}: {iden[1]} is array")

            if is_proc and (iden[1] in arguments.keys()) and arguments[iden[1]].type != 'var':
                 error(f"{line_num}: {iden[1]} is array")

            if iden[1] in declarations.keys():
                if not declarations[iden[1]].is_assigned:
                    error(f"{line_num}: {iden[1]} is not assigned")
         
        if iden[0] == 'arr' or iden[0] == 'arr2':
            if  (iden[1] in declarations.keys()) and declarations[iden[1]].type != 'arr':
                error(f"{line_num}: {iden[1]} is not an array")

            if is_proc and (iden[1] in arguments.keys()) and arguments[iden[1]].type != 'arr':
                error(f"{line_num}: {iden[1]} is not an array")

            if (iden[2] not in declarations.keys()) and iden[0] != 'arr2':
                if is_proc and (iden[2] not in arguments.keys()):
                    error(f"{line_num}: {iden[1]} not declared")
                   
                if not is_proc:
                    error(f"{line_num}: {iden[1]} not declared")
                          
            if (iden[2] in declarations.keys()) and not declarations[iden[2]].is_assigned:
                error(f"{line_num}: {iden[2]} is not assigned")
                
    def assign_mem_to_vars(self):
        for var_name in self.main_declarations.keys():
            var = self.main_declarations[var_name]
            var.set_mem(self.k)
            self.k = self.k + var.size
            
        for proc_name in self.proc_declarations.keys():
            for var_name in self.proc_declarations[proc_name].declarations.keys():
                var = self.proc_declarations[proc_name].declarations[var_name]
                var.set_mem(self.k)
                self.k = self.k + var.size
           
    def get_data(self):
        self.assign_mem_to_vars()
        return (self.proc_declarations,self.main_declarations)
                   
    def gen_fun_head(self,head):
        name = head[0]
        args = head[1]
        st = f"{name}(" 
        for arg in args:
            if arg[0] == 'var':
                st += f"{arg[1]},"
            else:
                st += f"T {arg[1]},"
        
        st = st[:-1] + ")"
        return st

class MachineCodeGenerator:
    def __init__(self,data):
        self.proc_declarations = data[0]
        self.main_declarations = data[1]
        self.asm = []

    def translate(self,ast):   
        for procedure in ast[0]:
            proc_name = procedure[0][0]
            proc = self.proc_declarations[proc_name]
            for node in procedure:
                if type(node) is int:
                    continue
                if node[0] == 'commands':
                    proc.set_commands(node[1])

        self.translate_main(ast[1])
        self.asm.append('HALT')
        return self.asm

    def translate_main(self,main):
        for node in main:
            if node[0] == 'commands':
                for command in node[1]:
                    self.translate_command(command,'main')

    def translate_command(self,com,scope):
        if com[0] == 'READ':
            self.translate_read(com,scope)
        if com[0] == 'WRITE':
            self.translate_write(com,scope)
        if com[0] == 'ASSIGN':
            self.translate_assign(com,scope)
        if com[0] == 'IF':
            k = self.translate_if(com,scope)
            self.add_jump_to_if(com[1][0],k,len(self.asm))
        if com[0] == 'IFELSE':
            self.translate_ifelse(com,scope)
        if com[0] == 'WHILE':
            self.translate_while(com,scope)
        if com[0] == 'REPEAT':
            self.translate_repeat(com,scope)
        if com[0] == 'proc_call':
            self.translate_proc_call(com,scope)

    def translate_proc_call(self,com,scope):
        proc_name = com[1]
        proc = self.proc_declarations[proc_name]
        args = []
        for var_name in com[2]:
            var = self.find_var_by_name(scope,var_name)
            args.append(var)
        proc.set_arguments(args)
        self.asm.append("RST c")
        for command in proc.commands:
            self.translate_command(command,proc_name)
        self.asm.append("RST c")

    def translate_assign(self,com,scope):
        iden = com[1]
        exp = com[2]

        if exp[0] == 'val':
            val = exp[1]
            if val[0] == 'number':
                self.asm += load_num(val[1],'a')
            if val[0] == 'iden': 
                self.load_from_iden(scope,val[1],'a')
            self.save_to_iden(scope,iden,'a')

        if exp[0] == 'ADD':
            val1 = exp[1]
            val2 = exp[2]
            self.load_from_val(scope,val1,'b')
            self.load_from_val(scope,val2,'h')
            self.asm.append("GET b")
            self.asm += gen_expression('+')
            self.save_to_iden(scope,iden,'a')
        
        if exp[0] == 'SUB':
            val1 = exp[1]
            val2 = exp[2]
            self.load_from_val(scope,val1,'b')
            self.load_from_val(scope,val2,'h')
            self.asm.append("GET b")
            self.asm += gen_expression('-')
            self.save_to_iden(scope,iden,'a')
        
        if exp[0] == 'MUL':
            val1 = exp[1]
            val2 = exp[2]
    
            if val2[0] == 'number' and val2[1] == 2:
                self.load_from_val(scope,val1,'a')
                self.asm.append("SHL a")
                self.save_to_iden(scope,iden,'a')
            else:
                self.load_from_val(scope,val1,'b')  
                self.load_from_val(scope,val2,'h') 
                self.asm.append("GET b")   
                self.asm += gen_expression('*',len(self.asm))
                self.save_to_iden(scope,iden,'a')    

        if exp[0] == 'DIV':
            val1 = exp[1]
            val2 = exp[2]
            if val2[0] == 'number' and val2[1] == 2:
                self.load_from_val(scope,val1,'a')
                self.asm.append("SHR a")
                self.save_to_iden(scope,iden,'a')
            else:
                self.load_from_val(scope,val1,'b')
                self.load_from_val(scope,val2,'h')
                self.asm.append("GET b")
                self.asm += gen_expression('/',len(self.asm))
                self.save_to_iden(scope,iden,'a')

        if exp[0] == 'MOD':
            val1 = exp[1]
            val2 = exp[2]
            self.load_from_val(scope,val1,'b')
            self.load_from_val(scope,val2,'h')
            self.asm.append("GET b")
            self.asm += gen_expression('%',len(self.asm))
            self.save_to_iden(scope,iden,'a')


    def translate_read(self,com,scope):
        iden = com[1]
        self.asm.append("READ")
        self.save_to_iden(scope,iden,'a')

    def translate_write(self,com,scope):
        val = com[1]
        self.load_from_val(scope,val,'a')
        self.asm.append("WRITE")

    def translate_ifelse(self,com,scope):
        self.asm.append("RST c")
        k = self.translate_if(com,scope)
        l = len(self.asm)
        self.asm.append(" ")
        elsecommands = com[3][1]
        for command in elsecommands:
            self.translate_command(command,scope)
        m = len(self.asm)
        self.add_jump_to_if(com[1][0],k,l+1)
        self.asm[l] = f"JUMP {m}"
        self.asm.append("RST c")
        
    def translate_if(self,com,scope):
        cond = com[1]
        commands = com[2][1]
        op = cond[0]
        val1 = cond[1]
        val2 = cond[2]
        self.translate_cond(op,val1,val2,scope)        
        k  = len(self.asm)
        self.asm.append(" ")
        for command in commands:
            self.translate_command(command,scope)    
        return k

    def add_jump_to_if(self,op,k,l):
        if op == 'LE':
            self.asm[k] = f"JPOS {l}"
        if op == 'GE':
            self.asm[k] = f"JPOS {l}"
        if op == 'LT':
            self.asm[k] = f"JZERO {l}" 
        if op == 'GT':
            self.asm[k] = f"JZERO {l}" 
        if op == 'EQ':
            self.asm[k] = f"JPOS  {l}" 
        if op == 'NEQ':
            self.asm[k] = f"JZERO {l}" 

    def translate_repeat(self,com,scope):
        self.asm.append("RST c") 
        commands = com[2][1]
        cond = com[1]
        op = cond[0]
        k = len(self.asm)
        for command in commands:
            self.translate_command(command,scope)
        val1 = cond[1]
        val2 = cond[2]
        self.translate_cond(self.neg_op(op),val1,val2,scope)
        l = len(self.asm)
        self.asm.append(" ") 
        self.asm.append(f"JUMP {k}") 
        self.add_jump_to_if(self.neg_op(op),l,len(self.asm))    

    def neg_op(self,op):
        if op == 'NEQ':
            return 'EQ'
        if op == 'LE':
            return 'GE'
        if op == 'GE':
            return 'LE'
        if op == 'LT':
            return 'GT'
        if op == 'GT':
            return 'LT'
        if op == 'EQ':
            return 'NEQ'
        
    def translate_while(self,com,scope):
        self.asm.append("RST c")
        self.asm.append("RST b")
        commands = com[2][1]
        cond = com[1]
        op = cond[0]
        val1 = cond[1]
        val2 = cond[2]
        k = len(self.asm)
        self.translate_cond(op,val1,val2,scope)
        l = len(self.asm)
        self.asm.append(" ") 
        for command in commands:
            self.translate_command(command,scope)
        self.asm.append(f"JUMP {k}") 
        self.add_jump_to_if(op,l,len(self.asm))
        self.asm.append("RST c")
        
    def translate_cond(self, op, val1, val2, scope):
        if op == 'LE':
            self.load_from_val(scope,val1,'b')  
            self.load_from_val(scope,val2,'h') 
            self.asm.append("GET b")   
            self.asm.append("SUB h")
        if op == 'GE':
            self.translate_cond('LE',val2,val1,scope)

        if op == 'GT':
            self.load_from_val(scope,val1,'b')  
            self.load_from_val(scope,val2,'h') 
            self.asm.append("GET b")   
            self.asm.append("SUB h")
        if op == 'LT':
            self.translate_cond('GT',val2,val1,scope)

        if op == 'EQ' or op == 'NEQ':
            self.load_from_val(scope,val1,'b')  
            self.load_from_val(scope,val2,'h') 
            self.asm.append("GET b")   
            self.asm.append("PUT g")
            self.asm.append("SUB h")
            self.asm.append("PUT f")
            self.asm.append("GET h")
            self.asm.append("SUB g")
            self.asm.append("ADD f")   

    def save_to_iden(self, scope, iden, reg):
        if iden[0] == 'var':
            var = self.find_var_by_name(scope,iden[1])
            self.asm += save_to_mem(reg,var,0)
        
        elif iden[0] == 'arr' or iden[0] == 'arr2':
            var_name = iden[1]
            var = self.find_var_by_name(scope,var_name)

            if iden[0] == 'arr':
                var2_name = iden[2]
                var2 = self.find_var_by_name(scope,var2_name)
                self.asm += save_arr(reg,var,var2)    
                
            if iden[0] == 'arr2':
                num = iden[2]
                self.asm += save_to_mem(reg,var,num)
        
    def load_from_val(self, scope, val, reg):
        if val[0] == 'number':
            self.asm += load_num(val[1],reg)
        if val[0] == 'iden':
            iden = val[1]
            self.load_from_iden(scope,iden,reg)
        return

    def load_from_iden(self, scope, iden, reg):
        if iden[0] == 'var':
            var = self.find_var_by_name(scope,iden[1])
            self.asm += load_var(var,reg)
        
        elif iden[0] == 'arr' or iden[0] == 'arr2':
            var_name = iden[1]
            var = self.find_var_by_name(scope,var_name)

            if iden[0] == 'arr':
                var2_name = iden[2]
                var2 = self.find_var_by_name(scope,var2_name)
                self.asm += load_arr(var,var2,reg)    
                
            if iden[0] == 'arr2':
                num = iden[2]
                self.asm += load_from_mem(var,num,reg)

    def find_var_by_name(self, scope, var_name):
        var = None
        if scope == 'main':
            var = self.main_declarations[var_name]
        else:
            if var_name in self.proc_declarations[scope].declarations:
                var = self.proc_declarations[scope].declarations[var_name]
            else:
                var = self.proc_declarations[scope].arg_map[var_name].var
        return var


def error(error_message):
    print("ERROR",error_message)
    sys.exit(1)

def load_num(num, reg):
    asm = []
    asm.append(f"RST {reg}")
    bin_r = bin(num)[2:]
    for i,num in enumerate(bin_r):
        if i != 0:
            asm.append(f"SHL {reg}")
        if num == '1':
            asm.append(f"INC {reg}")
    return asm

def load_var(var, reg):
    asm = []
    if var.in_reg:
        if reg == var.register:
            return asm
        else:
            asm.append(f"GET {var.reg}")
            if reg != 'a':
                asm.append(f"PUT {reg}")
    else:
        asm += load_from_mem(var,0,reg)
        
    return asm

def save_to_mem(reg, var, of): 
    asm = []
    if reg != 'a':
        asm.append(f"PUT {reg}")
    asm += load_num(var.mem + of,'h')
    asm.append("STORE h")
    return asm

def load_from_mem(var, of, reg):
    asm = []
    asm += load_num(var.mem + of,reg)
    asm.append(f"LOAD {reg}")
    if reg != 'a':
        asm.append(f"PUT {reg}")
    return asm

def load_arr(var1, var2, reg):
    asm = []
    asm += load_var(var2,'h')
    asm += load_num(var1.mem,'a')
    
    asm.append("ADD h")
    asm.append("LOAD a")
    if reg != 'a':
        asm.append(f"PUT {reg}")
    return asm

def save_arr(reg, var1, var2): 
    asm = []
    if reg != 'a' and reg != 'g':
        asm.append(f"GET {reg}")
    if reg != 'g':
        asm.append("PUT g")
    asm += load_var(var2,'h')
    asm += load_num(var1.mem,'a')
    asm.append("ADD h")
    asm.append("PUT h")
    asm.append("GET g")
    asm.append("STORE h")
    return asm

def gen_expression(operator, k = 0):
    asm = []
    if operator == '+':
        asm.append("ADD h")
    elif operator == '-':
        asm.append("SUB h")
    elif operator == '*':
        asm += log_mult(k)
    elif operator == '/':
        asm += log_div(k)
    elif operator == '%': 
        asm += log_mod(k)
    return asm

def log_mult(k):
    asm = []   
    asm.append("PUT g")
    asm.append("SUB h") 
    asm.append(f"JPOS {k+6}")
    asm.append("GET h")     
    asm.append("PUT f")  
    asm.append(f"JPOS {k+10}")  
    asm.append("GET g")  
    asm.append("PUT f")
    asm.append("GET h")
    asm.append("PUT g")
    asm.append("RST h")  
    asm.append("GET g")  
    asm.append(f"JZERO {k+28}")
    asm.append("SHR a")  
    asm.append("SHL a")     
    asm.append("PUT e")  
    asm.append("SUB g")  
    asm.append("PUT d")  
    asm.append("GET g")
    asm.append("SUB e")   
    asm.append("ADD d")  
    asm.append(f"JZERO {k+25}") 
    asm.append("GET h")
    asm.append("ADD f")
    asm.append("PUT h")
    asm.append("SHL f")
    asm.append("SHR g")
    asm.append(f"JUMP {k+11}")
    asm.append("GET h")
    asm.append("RST h")
    return asm
  
def log_div(k):
    asm = []
    asm.append("PUT f")
    asm.append("GET h")
    asm.append("PUT g")
    asm.append("RST h")
    asm.append("GET g")
    asm.append("SUB f")
    asm.append(f"JPOS {k + 25}")
    asm.append("RST e")
    asm.append("INC e")
    asm.append("GET g")
    asm.append("PUT d")
    asm.append("GET d")
    asm.append("SHL a")
    asm.append("SUB f")
    asm.append(f"JPOS {k + 18}")
    asm.append("SHL d")
    asm.append("SHL e")
    asm.append(f"JUMP {k + 11}")
    asm.append("GET f")
    asm.append("SUB d")
    asm.append("PUT f")
    asm.append("GET h")
    asm.append("ADD e")
    asm.append("PUT h")
    asm.append(f"JUMP {k + 4}")
    asm.append("GET h")
    asm.append("RST h")
    return asm

def log_mod(k):
    asm = []
    asm.append("PUT f")
    asm.append("GET h")
    asm.append("PUT g")
    asm.append("GET g")
    asm.append("SUB f")
    asm.append(f"JPOS {k + 21}")
    asm.append("RST e")
    asm.append("INC e")
    asm.append("GET g")
    asm.append("PUT d")
    asm.append("GET d")
    asm.append("SHL a")
    asm.append("SUB f")
    asm.append(f"JPOS {k + 17}")
    asm.append("SHL d")
    asm.append("SHL e")
    asm.append(f"JUMP {k + 10}")
    asm.append("GET f")
    asm.append("SUB d")
    asm.append("PUT f")   
    asm.append(f"JUMP {k + 3}")
    asm.append("GET f")
    asm.append("RST f")
    return asm


def main():
    if len(sys.argv) != 3:
        print('python3 compiler.py <path_to_input_file> <path_to_output_file>')
        sys.exit(1)

    input_file= sys.argv[1]

    with open(input_file, 'r') as file:
        input_text = file.read()

    lexer = Lexer()
    parser = Parser()

    tokens = lexer.tokenize(input_text)
    ast = parser.parse(tokens)

    analizer = Analyzer(ast)
    code_generator = MachineCodeGenerator(analizer.get_data())
    code_mr = code_generator.translate(ast)
    
    output_file = sys.argv[2]
    with open(output_file, 'w') as file:
        for line in code_mr:
            file.write(line + '\n')
    
main()
