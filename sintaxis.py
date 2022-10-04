import ply.yacc as yacc

from lexico import tokens

def p_program(p):
    '''program : PROGRAM programNP1 ID programNP2 SEMICOLON programA programB main'''
    p[0] = 1

# Neuralgic point (NP) in which we create the procedure directory (PD)
def p_programNP1(p):
    '''programNP1 :'''
    global pd 
    pd = {}

# NP in which we add the program ID to the PD and assign the type 'void' to it
# We also set current_func_name to the program ID in order to be
# able to link the global variable table (if there is one) to it
# We also save the program name to a variable so we can easily
# access the global variable table (VT) later
def p_programNP2(p):
    '''programNP2 :'''
    pd[p[-1]] = {'type': 'void'}
    global current_func_name
    current_func_name = p[-1]
    global program_name
    program_name = p[-1]

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
def p_funcionNP2(p):
    '''funcionNP2 :'''
    global current_func_name
    if p[-1] in pd:
        raise Exception('This function already exists: ' + current_func_name)
    else:
        pd[p[-1]] = {'type': current_func_type}
        current_func_name = p[-1]

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

def p_mainNP1(p):
    '''mainNP1 :'''
    pd[p[-1]] = {'type': 'void'}

def p_tipoSimple(p):
    '''tipoSimple : INT
                    | FLOAT'''

def p_params(p):
    '''params : tipoSimple ID paramsA'''

def p_paramsA(p):
    '''paramsA : COMMA tipoSimple ID paramsA
                | empty'''

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
    '''llamada : ID LEFTPAR exp llamadaA RIGHTPAR'''

def p_llamadaA(p):
    '''llamadaA : COMMA exp llamadaA
                | empty'''

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
    '''variable : ID variableA'''

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
    '''termino : factor terminoA'''

def p_terminoA(p):
    '''terminoA : terminoB factor terminoA
                    | empty'''

def p_terminoB(p):
    '''terminoB : MULOP
                    | DIVOP'''

def p_factor(p):
    '''factor : LEFTPAR exp RIGHTPAR
                | CTI
                | CTF
                | variable
                | llamada'''

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
    file = open("./avance2_test6.txt", "r")
    input = file.read()
except EOFError:
    pass
result = parser.parse(input)
print("------------")
if (result == 1):
    print("Pass")
else:
    print("Fail")