import ply.yacc as yacc

from lexico import tokens

# This boolean tracks whether we are inside a function call
# It's used in a function call to make sure there are no nested calls inside the arguments
global in_function_call
in_function_call = False

# Our semantic consideration table (semantic cube)
global sc
sc = {
    'int': {
        'int': {
            'mul': 'int', 
            'div': 'float', 
            'sum': 'int', 
            'sub': 'int', 
            'lt': 'int', 
            'gt': 'int',
            'diff': 'int',
            'eq': 'int',
            'and': 'int',
            'or': 'int',
            'assign': 'int'
        },
        'float': {
            'mul': 'float', 
            'div': 'float', 
            'sum': 'float', 
            'sub': 'float', 
            'lt': 'int', 
            'gt': 'int',
            'diff': 'int',
            'eq': 'int',
            'and': 'int',
            'or': 'int',
            'assign': 'error'
        }
    },
    'float': {
        'int': {
            'mul': 'float', 
            'div': 'float', 
            'sum': 'float', 
            'sub': 'float', 
            'lt': 'int', 
            'gt': 'int',
            'diff': 'int',
            'eq': 'int',
            'and': 'int',
            'or': 'int',
            'assign': 'error'
        },
        'float': {
            'mul': 'float', 
            'div': 'float', 
            'sum': 'float', 
            'sub': 'float', 
            'lt': 'int', 
            'gt': 'int',
            'diff': 'int',
            'eq': 'int',
            'and': 'int',
            'or': 'int',
            'assign': 'float'
        }
    }
}

def p_program(p):
    '''program : PROGRAM programNP1 ID programNP2 SEMICOLON programA programB main'''
    p[0] = 1

# Neuralgic point (NP) in which we create the procedure directory (PD)
def p_programNP1(p):
    '''programNP1 :'''
    global pd 
    pd = {}

# NP in which we add the program ID to the PD and assign the type 'void' to it.
#
# We also set current_func_name to the program ID in order to be 
# able to link the global variable table (VT) to it.
#
# We also save the program name to a variable so we can easily access the global VT later.
#
# Finally, we create an empty global VT so even if the program doesn't declare global variables,
# the table is still there and we don't get KeyErrors later when trying to find it
def p_programNP2(p):
    '''programNP2 :'''
    pd[p[-1]] = {'type': 'void'}
    global current_func_name
    current_func_name = p[-1]
    global program_name
    program_name = p[-1]
    pd[program_name]['vt'] = {}

def p_programA(p):
    '''programA : vars
                    | empty'''

def p_programB(p):
    '''programB : funcion programB
                    | empty'''

def p_vars(p):
    '''vars : varsNP1 VAR varsA varsNP7'''

# NP in which we link the VT with the function it belongs to
def p_varsNP7(p):
    '''varsNP7 :'''
    global current_func_name
    pd[current_func_name]['vt'] = vt

# NP in which we create the VT
def p_varsNP1(p):
    '''varsNP1 :'''
    global vt
    vt = {}

def p_varsA(p):
    '''varsA : varsB SEMICOLON varsF'''

def p_varsB(p):
    '''varsB : FILE varsNP2 ID varsNP3 varsNP6 varsE
                | tipoSimple varsNP2 ID varsNP3 varsC varsNP6'''

# NP in which we save the variable's type in a variable
def p_varsNP2(p):
    '''varsNP2 :'''
    global current_var_type
    current_var_type = p[-1]

# NP in which we check if there already exists a variable
# with the same name in the VT, and if not, we add the variable.
def p_varsNP6(p):
    '''varsNP6 :'''
    global current_var_id
    if current_var_id in vt:
        raise Exception('This variable already exists: ' + current_var_id)
    else:
        vt[current_var_id] = {'type': current_var_type}

def p_varsC(p):
    '''varsC : LEFTBRACKET CTI varsNP4 RIGHTBRACKET varsD
                | empty'''

# NP in which we check if the 1D array is between the size of 1 and 100.
# If it is we update the variable's type
def p_varsNP4(p):
    '''varsNP4 :'''
    global current_var_type
    arrSize = int(p[-1])
    if 1 <= arrSize <= 100:
        current_var_type = '1d_arr'
    else:
        raise Exception('Array size must be between 1 and 100')

def p_varsD(p):
    '''varsD : LEFTBRACKET CTI varsNP5 RIGHTBRACKET
                | empty'''

# NP in which we check if the 2D array is between the size of 1 and 100.
# If it is we update the variable's type
def p_varsNP5(p):
    '''varsNP5 :'''
    global current_var_type
    arrSize = int(p[-1])
    if 1 <= arrSize <= 100:
        current_var_type = '2d_arr'
    else:
        raise Exception('Array size must be between 1 and 100')

def p_varsE(p):
    '''varsE : COMMA ID varsNP3 varsE
                | empty'''

# NP in which we save the variable's ID in a variable
def p_varsNP3(p):
    '''varsNP3 :'''
    global current_var_id
    current_var_id = p[-1]

def p_varsF(p):
    '''varsF : varsA
                | empty'''

def p_funcion(p):
    '''funcion : FUNC funcionA funcionNP1 ID funcionNP2 LEFTPAR funcionB RIGHTPAR SEMICOLON funcionC bloque'''

# NP in which we save the function's type in a variable
def p_funcionNP1(p):
    '''funcionNP1 :'''
    global current_func_type
    current_func_type = p[-1]

# NP in which we check if there already exists a procedure
# with the same name in the PD, and if not, we add the function.
# If the function gets added, we also save its name in a variable
# so that we can link it to its variable table (VT) later
# We also create an empty VT for the function just in case the function doesn't
# declare any variables; this way we avoid getting KeyErrors
def p_funcionNP2(p):
    '''funcionNP2 :'''
    global current_func_name
    if p[-1] in pd:
        raise Exception('This function already exists: ' + current_func_name)
    else:
        pd[p[-1]] = {'type': current_func_type, 'params': []}
        current_func_name = p[-1]
        pd[current_func_name]['vt'] = {}

def p_funcionA(p):
    '''funcionA : tipoSimple 
                    | VOID'''

def p_funcionB(p):
    '''funcionB : params
                    | empty'''

def p_funcionC(p):
    '''funcionC : vars
                    | empty'''

def p_main(p):
    '''main : MAINSTART mainNP1 bloque'''

# NP in which we assign the type 'void' and an empty VT to main
# We also change the current_func_name variable
def p_mainNP1(p):
    '''mainNP1 :'''
    pd[p[-1]] = {'type': 'void', 'vt': {}}
    global current_func_name
    current_func_name = 'main'

def p_tipoSimple(p):
    '''tipoSimple : INT
                    | FLOAT'''

def p_params(p):
    '''params : tipoSimple paramsNP1 ID paramsA'''

def p_paramsA(p):
    '''paramsA : COMMA tipoSimple paramsNP1 ID paramsA
                | empty'''

# NP in which we add each parameter type to the 
# parameter list for the function that's being declared
def p_paramsNP1(p):
    '''paramsNP1 :'''
    pd[current_func_name]['params'].append(p[-1])

def p_bloque(p):
    '''bloque : LEFTCURLY bloqueA RIGHTCURLY'''

def p_bloqueA(p):
    '''bloqueA : estatuto bloqueA
                | empty'''

def p_estatuto(p):
    '''estatuto : asignacion
                | llamada
                | read
                | escritura
                | condicion
                | ciclo'''

def p_asignacion(p):
    '''asignacion : variable ASSIGNOP exp'''

def p_llamada(p):
    '''llamada : ID llamadaNP1 LEFTPAR llamadaNP2 exp llamadaNP3 llamadaA RIGHTPAR llamadaNP4'''
    in_function_call = False

def p_llamadaA(p):
    '''llamadaA : COMMA exp llamadaNP2 llamadaA
                | empty'''

# NP in which first we check if we are already inside of a call (which would mean
# this call is nested). If that is so we throw an error. 
# If everything's OK, then we save the function's name in a variable in order
# to be able to access its params list later, in NP4
# We also change the in_function_call boolean to true because we will start analyzing the function call
def p_llamadaNP1(p):
    '''llamadaNP1 :'''
    if in_function_call:
        raise Exception('Nesting function calls is not supported')
    global current_call_name
    current_call_name = p[-1]
    in_function_call = True

# NP in which we define (and reset) a temporary list.
# We will add all the call's arguments' types to this list and then, in llamadaNP4, compare it
# with the params list that the function has in the PD to see if the arguments' types are correct
def p_llamadaNP2(p):
    '''llamadaNP2 :'''
    global arg_list
    arg_list = []

# NP in which we add the call's arguments' types to the temporary list.
def p_llamadaNP3(p):
    '''llamadaNP3 :'''
    global arg_list
    arg_list.append(p[-1])

# NP in which we compare the temporary list's contents with the function's params list.
# If they're not identical (meaning the order or types of the 
# parameters are different) then we throw an error
def p_llamadaNP4(p):
    '''llamadaNP4 :'''
    global arg_list
    if arg_list != pd[current_call_name]['params']:
        raise Exception('The order or types of the parameters in a ' + current_call_name + ' call are incorrect')

def p_read(p):
    '''read : READ ID'''

def p_escritura(p):
    '''escritura : WRITE LEFTPAR escrituraA escrituraB RIGHTPAR'''

def p_escrituraA(p):
    '''escrituraA : exp
                    | CTSTRING'''

def p_escrituraB(p):
    '''escrituraB : COMMA escrituraA escrituraB
                    | empty'''

def p_condicion(p):
    '''condicion : IF LEFTPAR exp RIGHTPAR bloque condicionA'''

def p_condicionA(p):
    '''condicionA : ELSE bloque
                    | empty'''

def p_ciclo(p):
    '''ciclo : FROM exp TO exp DO bloque'''

def p_variable(p):
    '''variable : ID variableNP1 variableA'''

# NP in which we verify if the variable exists, either in the local or global VT.
# The local VT has priority over the global VT
def p_variableNP1(p):
    '''variableNP1 :'''
    if p[-1] not in pd[current_func_name]['vt']:
        if p[-1] not in pd[program_name]['vt']:
            raise Exception('Variable ' + p[-1] + ' does not exist on either the local or global scopes')

def p_variableA(p):
    '''variableA : LEFTBRACKET exp RIGHTBRACKET variableB
                    | empty'''

def p_variableB(p):
    '''variableB : LEFTBRACKET exp RIGHTBRACKET
                    | empty'''

def p_exp(p):
    '''exp : exp1 expA'''

def p_expA(p):
    '''expA : OROP exp1 expA
                | empty'''

def p_exp1(p):
    '''exp1 : exp2 exp1A'''

def p_exp1A(p):
    '''exp1A : ANDOP exp2 exp1A
                | empty'''

def p_exp2(p):
    '''exp2 : exp3 exp2A'''

def p_exp2A(p):
    '''exp2A : exp2B exp3
                | empty'''

def p_exp2B(p):
    '''exp2B : LESSTHANOP
                | GREATERTHANOP
                | DIFFERENTOP
                | EQUALOP'''

def p_exp3(p):
    '''exp3 : termino exp3A'''

def p_exp3A(p):
    '''exp3A : exp3B termino exp3A
                | empty'''

def p_exp3B(p):
    '''exp3B : SUMOP
                | SUBOP'''

def p_termino(p):
    '''termino : terminoC terminoA'''

def p_terminoA(p):
    '''terminoA : terminoB terminoC terminoA
                    | empty'''

def p_terminoB(p):
    '''terminoB : MULOP
                    | DIVOP'''

def p_terminoC(p):
    '''terminoC : factor
                    | llamada'''

def p_factor(p):
    '''factor : LEFTPAR exp RIGHTPAR
                | CTI
                | CTF
                | variable'''

# For empty / epsilon
def p_empty(p):
    '''empty : '''

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input at:")
    print(p)
 
# Build the parser
parser = yacc.yacc()

# Test file 
print("\nTest file:")
try:
    file = open("./avance2_test1.txt", "r")
    input = file.read()
except EOFError:
    pass
result = parser.parse(input)
print("------------")
if (result == 1):
    print("Pass")
else:
    print("Fail")