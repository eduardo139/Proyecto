import ply.yacc as yacc

from Avance1_Lexico import tokens

def p_program(p):
    '''program : PROGRAM ID SEMICOLON programA programB main'''
    p[0] = 1

def p_programA(p):
    '''programA : vars 
                    | empty'''

def p_programB(p):
    '''programB : funcion programB
                    | empty'''

def p_vars(p):
    '''vars : VAR varsA'''

def p_varsA(p):
    '''varsA : varsB SEMICOLON varsF'''

def p_varsB(p):
    '''varsB : FILE ID varsE
                | tipoSimple ID varsC'''

def p_varsC(p):
    '''varsC : LEFTBRACKET CTI RIGHTBRACKET varsD
                | empty'''

def p_varsD(p):
    '''varsD : LEFTBRACKET CTI RIGHTBRACKET
                | empty'''

def p_varsE(p):
    '''varsE : COMMA ID varsE
                | empty'''

def p_varsF(p):
    '''varsF : varsA
                | empty'''

def p_funcion(p):
    '''funcion : FUNC funcionA ID LEFTPAR funcionB RIGHTPAR SEMICOLON funcionC bloque'''

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
    '''main : MAINSTART bloque'''

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

# Test file 1
print("\nTest file 1:")
try:
    file = open("./avance1_test1.txt", "r")
    input = file.read()
except EOFError:
    pass
result = parser.parse(input)
print("------------")
if (result == 1):
    print("Pass")
else:
    print("Fail")
print("\n")

# Test file 2
print("\nTest file 2:")
try:
    file = open("./avance1_test2.txt", "r")
    input = file.read()
except EOFError:
    pass
result = parser.parse(input)
print("------------")
if (result == 1):
    print("Pass")
else:
    print("Fail")
print("\n")