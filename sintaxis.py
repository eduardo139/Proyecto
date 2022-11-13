import ply.yacc as yacc
import json

from lexico import tokens

# This boolean tracks whether we are inside a function call
# It's used in a function call to make sure there are no nested calls inside the arguments
global in_function_call
in_function_call = False

# In a quad, in the part where the result would go, we have an int
# that represents the index of the avail list where the result will be stored
global avail_list_current_index
avail_list_current_index = 0

global quad_list
quad_list = []

global quad_number
quad_number = 0

global avail_list
avail_list = []

global poper
poper = []

global pilao
pilao = []

global ptypes
ptypes = []

global piladim
piladim = []

global psaltos
psaltos = []

global opcodes
opcodes = {'=': 1, '<': 2, '>': 3, '<>': 4, '==': 5, '+': 6, '-': 7, '*': 8, '/': 9,
           '&': 10, '|': 11, 'read': 12, 'write': 13, 'goto': 14, 'gtf': 15, 'call': 16,
           'gosub': 17, 'era': 18, 'parameter': 19, 'endfunc': 20, 'verify': 21}

# Our semantic consideration table (semantic cube)
global sc
sc = {
    'int': {
        'int': {
            '*': 'int', 
            '/': 'float', 
            '+': 'int', 
            '-': 'int', 
            '<': 'int', 
            '>': 'int',
            '<>': 'int',
            '==': 'int',
            '&': 'int',
            '|': 'int',
            '=': 'int'
        },
        'float': {
            '*': 'float', 
            '/': 'float', 
            '+': 'float', 
            '-': 'float', 
            '<': 'int', 
            '>': 'int',
            '<>': 'int',
            '==': 'int',
            '&': 'int',
            '|': 'int',
            '=': 'error'
        }
    },
    'float': {
        'int': {
            '*': 'float', 
            '/': 'float', 
            '+': 'float', 
            '-': 'float', 
            '<': 'int', 
            '>': 'int',
            '<>': 'int',
            '==': 'int',
            '&': 'int',
            '|': 'int',
            '=': 'error'
        },
        'float': {
            '*': 'float', 
            '/': 'float', 
            '+': 'float', 
            '-': 'float', 
            '<': 'int', 
            '>': 'int',
            '<>': 'int',
            '==': 'int',
            '&': 'int',
            '|': 'int',
            '=': 'float'
        }
    }
}

def compute_function_memory_requirements(function_name):
    memory_requirements = {'var': {'int': 0, 'float': 0, 'file': 0}, 'temp': {'int': 0, 'float': 0, 'pointer' : 0}}
    for variable in pd[function_name]['vt']:
        variable_type = pd[function_name]['vt'][variable]['type']
        # Temp variables
        # dont make any other variable start with 'temp' or you will break this
        if variable[:4] == 'temp':
            if variable_type == 'int':
                memory_requirements['temp']['int'] += 1
            elif variable_type == 'float':
                memory_requirements['temp']['float'] += 1
            elif variable_type == 'pointer':
                memory_requirements['temp']['pointer'] += 1
        else:
            # Non-atomic vars
            if 'dim' in  pd[function_name]['vt'][variable]:
                # Matrices
                if 'm1' in pd[function_name]['vt'][variable]['dim']:
                    if variable_type == 'int':
                        memory_requirements['var']['int'] += pd[function_name]['vt'][variable]['dim']['lsup'] * pd[function_name]['vt'][variable]['dim']['m1']
                    elif variable_type == 'float':
                        memory_requirements['var']['float'] += pd[function_name]['vt'][variable]['dim']['lsup'] * pd[function_name]['vt'][variable]['dim']['m1']
                # Arrays
                else:
                    if variable_type == 'int':
                        memory_requirements['var']['int'] += pd[function_name]['vt'][variable]['dim']['lsup']
                    elif variable_type == 'float':
                        memory_requirements['var']['float'] += pd[function_name]['vt'][variable]['dim']['lsup']
            # Atomic vars
            else:
                if variable_type == 'int':
                    memory_requirements['var']['int'] += 1
                elif variable_type == 'float':
                    memory_requirements['var']['float'] += 1
                elif variable_type == 'file':
                    memory_requirements['var']['file'] += 1
    pd[function_name]['mem'] = memory_requirements

def assign_virtual_address(type, context, spaces_needed):
    if context == 'null':
        if current_func_name == program_name:
            context = 'global'
        else:
            context = 'local'
    global virtual_addresses
    virtual_address = virtual_addresses[context][type]
    virtual_addresses[context][type] += spaces_needed
    return virtual_address

def reset_virtual_address_counters():
    global virtual_addresses
    virtual_addresses['local'] = {'int': 2000, 'float': 2333, 'file': 2666}
    virtual_addresses['temp'] = {'int': 3000, 'float': 3333, 'pointer': 3666}

def find_virtual_address(operand):
    if operand not in pd[current_func_name]['vt']:
        if operand not in pd[program_name]['vt']:
            if operand not in const_table:
                raise Exception(str(operand) + ' does not exist on either the local or global scopes')
            else:
                return const_table[operand]['va']
        else:
            return pd[program_name]['vt'][operand]['va']
    else:
        return pd[current_func_name]['vt'][operand]['va']

def generate_quad(oper, l_op, r_op, result_type):
    global quad_list
    global avail_list
    global avail_list_current_index
    global pilao
    global ptypes

    if oper == 'read' or oper == 'write' or oper == '=' or oper == 'goto' or oper == 'endfunc' or oper == 'era' or oper == 'gosub' or oper == 'parameter':
        result = 'null'
    elif oper == 'gtf':
        # The result of evaluating the if expression
        l_op = pilao.pop()
        # Getting rid of the result type
        ptypes.pop()
        result = 'null'
    # Else it's an expression or a verify
    else:
        result = 'temp' + str(avail_list_current_index)
        avail_list_current_index += 1
        pilao.append(result)
        ptypes.append(result_type)
        pd[current_func_name]['vt'][result] = {'type': '', 'va': ''}
        pd[current_func_name]['vt'][result]['type'] = result_type
        pd[current_func_name]['vt'][result]['va'] = assign_virtual_address(result_type, 'temp', 1)
        l_op = find_virtual_address(l_op)
        r_op = find_virtual_address(r_op)
        result = find_virtual_address(result)
    if oper == '=':
        if result_type == 'return':
            return_type = l_op
            l_op = 'return' + str(avail_list_current_index)
            avail_list_current_index += 1
            pilao.append(l_op)
            ptypes.append(return_type)
            pd[program_name]['vt'][l_op] = {'type': '', 'va': ''}
            pd[program_name]['vt'][l_op]['type'] = return_type
            pd[program_name]['vt'][l_op]['va'] = assign_virtual_address(return_type, 'global', 1)
        l_op = find_virtual_address(l_op)
        r_op = find_virtual_address(r_op)
    if oper == 'read' or oper == 'gtf' or oper == 'parameter':
        l_op = find_virtual_address(l_op)
    if oper == 'write':
        for index, item in enumerate(l_op):
            l_op[index] = find_virtual_address(l_op[index])

    oper = opcodes[oper]
    quad_list.append([oper, l_op, r_op, result])
    
    global quad_number
    quad_number += 1
    return quad_number - 1

def process_statement():
    global poper
    global pilao
    global ptypes

    operator = poper.pop()

    if operator == '=':
        r_op = pilao.pop()
        l_op = pilao.pop()
        r_type = ptypes.pop()
        l_type = ptypes.pop()
        if (l_type == r_type) or (l_type == 'pointer' and r_type == 'int') or (l_type == 'pointer' and r_type == 'float') or (l_type == 'int' and r_type == 'pointer') or (l_type == 'float' and r_type == 'pointer'):
            generate_quad(operator, l_op, r_op, 'null')
        else:
            raise Exception('Cannot assign an element (' + r_type + ') to another that is not of the same type (' + l_type + ')')
    elif operator == 'call':
        # Getting rid of arg_types
        pilao.pop()
        arg_list = pilao.pop()
        # getting rid of the 'list's in ptypes
        ptypes.pop()
        ptypes.pop()

        func_id = pilao[-1]

        i = 1
        for arg in arg_list:
            generate_quad('parameter', arg, i, 'null')
            i += 1
        generate_quad('gosub', func_id, pd[func_id]['quadstart'], 'null')
    elif operator == 'read':
        file_to_read_from = pilao.pop()
        file_type = ptypes.pop()
        generate_quad(operator, file_to_read_from, 'null', 'null')
    elif operator == 'write':
        # Getting rid of the types list
        pilao.pop()
        ptypes.pop()
        arg_list = pilao.pop()
        # getting rid of the 'list' in ptypes
        ptypes.pop()
        generate_quad(operator, arg_list, 'null', 'null')

def process_exp():
    global poper
    global pilao
    global ptypes

    oper = poper.pop()
    r_op = pilao.pop()
    l_op = pilao.pop()
    r_type = ptypes.pop()
    l_type = ptypes.pop()
    result_type = sc[l_type][r_type][oper]
    if result_type != 'error':
        generate_quad(oper, l_op, r_op, result_type)
    else:
        raise Exception('Type mismatch at expression ' + r_op + ' ' + oper + ' ' + l_op)

def p_program(p):
    '''program : PROGRAM programNP1 ID programNP2 SEMICOLON programA programB main'''
    p[0] = 1

def p_programNP1(p):
    '''programNP1 :'''
    global pd 
    pd = {}

    global virtual_addresses
    virtual_addresses = {'global': {'int': 1000, 'float': 1333, 'file': 1666}, 
                        'local': {'int': 2000, 'float': 2333, 'file': 2666},
                        'temp': {'int': 3000, 'float': 3333, 'pointer': 3666},
                        'constant': {'int': 4000, 'float': 4333, 'string': 4666}}
    
    global const_table
    const_table = {}

    quad_num = generate_quad('goto', 'waiting for quad number of main', 'null', 'null')
    global psaltos
    psaltos.append(quad_num)

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
    '''vars : VAR varsA'''

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

    global isArray
    global isMatrix
    isArray = False
    isMatrix = False

# NP in which we check if there already exists a variable
# with the same name in the VT, and if not, we add the variable.
def p_varsNP6(p):
    '''varsNP6 :'''
    global current_var_id
    global current_var_type
    if current_var_id in pd[current_func_name]['vt']:
        raise Exception('This variable already exists: ' + current_var_id)
    else:
        pd[current_func_name]['vt'][current_var_id] = {'type': '', 'va': ''}
        pd[current_func_name]['vt'][current_var_id]['type'] = current_var_type

    global arrSize
    global matSize
    if isArray:
        pd[current_func_name]['vt'][current_var_id]['dim'] = {'lsup': arrSize}
        needed_spaces = arrSize
    elif isMatrix:
        pd[current_func_name]['vt'][current_var_id]['dim'] = {'lsup': arrSize, 'm1': matSize, 'next_dim': {'lsup': matSize}}
        needed_spaces = arrSize * matSize
    else:
        needed_spaces = 1
    pd[current_func_name]['vt'][current_var_id]['va'] = assign_virtual_address(current_var_type, 'null', needed_spaces)


def p_varsC(p):
    '''varsC : LEFTBRACKET varIsArray CTI varsNP4 RIGHTBRACKET varsD
                | empty'''

def p_varIsArray(p):
    '''varIsArray :'''
    global isArray
    isArray = True

# NP in which we check if the 1D array is between the size of 1 and 100.
# If it is we update the variable's type
# We also add the constant to the const table
def p_varsNP4(p):
    '''varsNP4 :'''
    global const_table
    global arrSize
    arrSize = int(p[-1])
    if arrSize < 1 or arrSize > 100:
        raise Exception('Array size must be between 1 and 100')
    if p[-1] not in const_table:
        const_table[p[-1]] = {'type': '', 'va': ''}
        const_table[p[-1]]['type'] = 'int'
        const_table[p[-1]]['va'] = assign_virtual_address('int', 'constant', 1)

def p_varsD(p):
    '''varsD : LEFTBRACKET varIsMatrix CTI varsNP5 RIGHTBRACKET
                | empty'''

def p_varIsMatrix(p):
    '''varIsMatrix :'''
    global isArray
    isArray = False
    global isMatrix
    isMatrix = True

# NP in which we check if the 2D array is between the size of 1 and 100.
# If it is we update the variable's type
# We also add the constant to the const table
def p_varsNP5(p):
    '''varsNP5 :'''
    global const_table
    global matSize
    matSize = int(p[-1])
    if matSize < 1 or matSize > 100:
        raise Exception('Array size must be between 1 and 100')
    if p[-1] not in const_table:
        const_table[p[-1]] = {'type': '', 'va': ''}
        const_table[p[-1]]['type'] = 'int'
        const_table[p[-1]]['va'] = assign_virtual_address('int', 'constant', 1)

def p_varsE(p):
    '''varsE : COMMA ID varsNP3 varsNP6 varsE
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
    '''funcion : FUNC funcionA funcionNP1 ID funcionNP2 LEFTPAR funcionB RIGHTPAR SEMICOLON funcionC quadStart bloque genMemReqs'''

def p_quadStart(p):
    '''quadStart :'''
    pd[current_func_name]['quadstart'] = quad_number

def p_genMemReqs(p):
    '''genMemReqs :'''
    compute_function_memory_requirements(current_func_name)
    generate_quad('endfunc', 'null', 'null', 'null')

    global has_a_return
    if not has_a_return and current_func_type != 'void':
        raise Exception('Non-void function ' + current_func_name + ' has no return statement')

# NP in which we save the function's type in a variable
# We also reset the virtual address counters
def p_funcionNP1(p):
    '''funcionNP1 :'''
    global current_func_type
    current_func_type = p[-1]
    reset_virtual_address_counters()
    global has_a_return
    has_a_return = False

# NP in which we check if there already exists a procedure
# with the same name in the PD, and if not, we add the function.
# If the function gets added, we also save its name in a variable
# so that we can link it to its variable table (VT) later
# We also create an empty VT for the function just in case the function doesn't
# declare any variables; this way we avoid getting KeyErrors
def p_funcionNP2(p):
    '''funcionNP2 :'''
    global current_func_name
    global current_func_type
    if p[-1] in pd:
        raise Exception('This function already exists: ' + current_func_name)
    else:
        pd[p[-1]] = {'type': current_func_type, 'params': []}
        current_func_name = p[-1]
        pd[current_func_name]['vt'] = {}

def p_funcionA(p):
    '''funcionA : tipoSimple funcionANP1
                    | VOID funcionANP1'''
    p[0] = p[2]

def p_funcionANP1(p):
    '''funcionANP1 :'''
    tipo = p[-1]
    p[0] = tipo

def p_funcionB(p):
    '''funcionB : params
                    | empty'''

def p_funcionC(p):
    '''funcionC : vars
                    | empty'''

def p_main(p):
    '''main : MAINSTART mainNP1 bloque getGlobalMemReqs'''

def p_getGlobalMemReqs(p):
    '''getGlobalMemReqs :'''
    compute_function_memory_requirements(program_name)

# NP in which we assign the type 'void' and an empty VT to main
# We also change the current_func_name variable
# We also reset the virtual address counets
def p_mainNP1(p):
    '''mainNP1 :'''
    global current_func_name
    current_func_name = program_name
    reset_virtual_address_counters()

    global quad_number
    quad_to_jump_to = quad_number
    global psaltos
    quad_to_fill = psaltos.pop()
    quad_list[quad_to_fill][1] = quad_to_jump_to

def p_tipoSimple(p):
    '''tipoSimple : INT tipoSimpleNP1
                    | FLOAT tipoSimpleNP1'''
    p[0] = p[2]

def p_tipoSimpleNP1(p):
    '''tipoSimpleNP1 :'''
    tipo = p[-1]
    p[0] = tipo

def p_params(p):
    '''params : tipoSimple paramsNP1 ID paramsNP2 paramsA'''

def p_paramsA(p):
    '''paramsA : COMMA tipoSimple paramsNP1 ID paramsNP2 paramsA
                | empty'''

# NP in which we add each parameter type to the 
# parameter list for the function that's being declared
def p_paramsNP1(p):
    '''paramsNP1 :'''
    global current_param_type
    current_param_type = p[-1]
    pd[current_func_name]['params'].append(p[-1])

def p_paramsNP2(p):
    '''paramsNP2 :'''
    global current_param_id
    global current_param_type
    current_param_id = p[-1]
    if current_param_id in pd[current_func_name]['vt']:
        raise Exception('This variable already exists: ' + current_param_id)
    else:
        pd[current_func_name]['vt'][current_param_id] = {'type': '', 'va': ''}
        pd[current_func_name]['vt'][current_param_id]['type'] = current_param_type
        pd[current_func_name]['vt'][current_param_id]['va'] = assign_virtual_address(current_param_type, 'local', 1)

def p_bloque(p):
    '''bloque : LEFTCURLY bloqueA RIGHTCURLY'''

def p_bloqueA(p):
    '''bloqueA : estatuto bloqueA
                | empty'''

def p_estatuto(p):
    '''estatuto : estatutoNP1 estatutoA'''

# This rule only exists so I could put the NP before every one of these options
# without having to repeat it over and over in the ORs.
# It is not in the design document
def p_estatutoA(p):
    '''estatutoA : asignacion 
                | llamada checkIfVoid
                | read
                | escritura
                | return
                | condicion
                | ciclo'''

def p_checkIfVoid(p):
    '''checkIfVoid :'''
    if p[-1] != 'void':
        raise Exception('Non-void function is being called as a statement')

# NP in which we create the stacks needed to generate quadruples
def p_estatutoNP1(p):
    '''estatutoNP1 : '''
    global poper
    global pilao
    global ptypes
    poper = []
    pilao = []
    ptypes = []

def p_asignacion(p):
    '''asignacion : variable ASSIGNOP asignacionNP1 exp asignacionNP2'''

# NP in which we push = into POper
def p_asignacionNP1(p):
    '''asignacionNP1 : '''
    global poper
    poper.append(p[-1])

def p_asignacionNP2(p):
    '''asignacionNP2 : '''
    process_statement()

def p_llamada(p):
    '''llamada : ID llamadaNP1 LEFTPAR loeNP2 primerparam llamadaA RIGHTPAR llamadaNP3'''
    global in_function_call
    in_function_call = False
    p[0] = p[2]

def p_primerparam(p):
    '''primerparam : exp loeNP1
                    | empty'''

def p_llamadaA(p):
    '''llamadaA : COMMA exp loeNP1 llamadaA
                | empty'''

# NP in which first we check if we are already inside of a call (which would mean
# this call is nested). If that is so we throw an error. 
# If everything's OK, then we save the function's name in a variable in order
# to be able to access its params list later, in NP4
# We also change the in_function_call boolean to true because we will start analyzing the function call
# We also push 'call' to poper, the id to pilao and 'func' to ptypess
# We also generate the ERA quad
def p_llamadaNP1(p):
    '''llamadaNP1 :'''
    global in_function_call
    if in_function_call:
        raise Exception('Nesting function calls is not supported')
    id = p[-1]
    if id not in pd:
        raise Exception('Function ' + id + ' was not declared but is being called')
    global current_call_name
    current_call_name = p[-1]
    in_function_call = True
    poper.append('call')
    pilao.append(p[-1])
    ptypes.append(pd[p[-1]]['type'])

    generate_quad('era', pd[id]['mem'], 'null', 'null')
    p[0] = pd[id]['type']

# NP in which we compare the arg_list's contents with the function's params list.
# If they're not identical (meaning the order or types of the parameters are different) 
# then we throw an error
def p_llamadaNP3(p):
    '''llamadaNP3 :'''
    arg_types = pilao[-1]
    arg_list = pilao[-2]
    func_id = pilao[-3]
    if arg_types == pd[func_id]['params']:
        process_statement()
    else:
        raise Exception('The order or types of the parameters in a ' + current_call_name + ' call are incorrect')


def p_read(p):
    '''read : READ ID readNP1'''

def p_readNP1(p):
    '''readNP1 :'''
    global current_func_name
    global program_name
    poper.append('read')
    if p[-1] not in pd[current_func_name]['vt']:
        if p[-1] not in pd[program_name]['vt']:
            raise Exception('Variable ' + p[-1] + ' does not exist on either the local or global scopes')
        else:
            ptypes.append(pd[program_name]['vt'][p[-1]]['type'])
    else:
        ptypes.append(pd[current_func_name]['vt'][p[-1]]['type'])
    pilao.append(p[-1])
    process_statement()

def p_escritura(p):
    '''escritura : WRITE escrituraNP1 LEFTPAR loeNP2 escrituraA escrituraB RIGHTPAR escrituraNP3'''

# NP in which we push 'write' to poper
# We also create/reset an argument list that will have all the arguments
def p_escrituraNP1(p):
    '''escrituraNP1 :'''
    poper.append('write')

def p_escrituraNP3(p):
    '''escrituraNP3 :'''
    process_statement()

# loe = Llamada o escritura
# NP in which we define (and reset) lists for arguments and their types
# We will add all the call/write's arguments' types to this list and then, 
# (in the case of a call) in llamadaNP3, compare it with the params list 
# that the function has in the PD to see if the arguments' types are correct
def p_loeNP2(p):
    '''loeNP2 :'''
    arg_list = []
    arg_types = []
    pilao.append(arg_list)
    ptypes.append('list')
    pilao.append(arg_types)
    ptypes.append('list')

def p_escrituraA(p):
    '''escrituraA : exp loeNP1
                    | CTSTRING escrituraNP2'''

# loe = Llamada o escritura
# NP in which we add the call's or write's arguments to the argument list
# and its types to the arg_type list
# Only EXP arguments, CTSTRING arguments to write have their own NP
def p_loeNP1(p):
    '''loeNP1 :'''
    global poper
    global pilao
    global ptypes

    exp_value = pilao.pop()
    exp_type = ptypes.pop()
    arg_types = pilao.pop()
    arg_list = pilao.pop()
    arg_list.append(exp_value)
    arg_types.append(exp_type)
    pilao.append(arg_list)
    pilao.append(arg_types)

# NP in which we append the CTSTRING to the argument list
# We also add it to the constants table if it's not there
def p_escrituraNP2(p):
    '''escrituraNP2 :'''
    global const_table
    arg_types = pilao.pop()
    arg_list = pilao.pop()
    arg_list.append(p[-1])
    arg_types.append('ctstring')
    pilao.append(arg_list)
    pilao.append(arg_types)
    if p[-1] not in const_table:
        const_table[p[-1]] = {'type': '', 'va': ''}
        const_table[p[-1]]['type'] = 'string'
        const_table[p[-1]]['va'] = assign_virtual_address('string', 'constant', 1)

def p_escrituraB(p):
    '''escrituraB : COMMA escrituraA escrituraB
                    | empty'''

def p_return(p):
    '''return : RETURN exp returnNP1'''

def p_returnNP1(p):
    '''returnNP1 :'''
    if current_func_name == program_name:
        raise Exception('Return statement in main')
    elif current_func_type == 'void':
        raise Exception('Return statement in void function')
    else:
        global has_a_return
        has_a_return = True
        exp_value = pilao.pop()
        exp_type = ptypes.pop()
        generate_quad('=', exp_type, exp_value, 'return')

def p_condicion(p):
    '''condicion : IF LEFTPAR exp condicionNP1 RIGHTPAR bloque condicionA condicionANP2'''

def p_condicionNP1(p):
    '''condicionNP1 :'''
    quad_num = generate_quad('gtf', 'null', 'waiting for quad number to jump to', 'null')
    global psaltos
    psaltos.append(quad_num)

def p_condicionA(p):
    '''condicionA : ELSE condicionANP1 bloque
                    | empty'''

def p_condicionANP1(p):
    '''condicionANP1 :'''
    global quad_number
    quad_to_jump_to = quad_number + 1 
    global psaltos
    quad_to_fill = psaltos.pop()
    quad_list[quad_to_fill][2] = quad_to_jump_to
    quad_num = generate_quad('goto', 'waiting for quad number to jump to', 'null', 'null')
    psaltos.append(quad_num)

def p_condicionANP2(p):
    '''condicionANP2 :'''
    global quad_number
    quad_to_jump_to = quad_number
    global psaltos
    quad_to_fill = psaltos.pop()
    if (quad_list[quad_to_fill][0] == 14):
        quad_list[quad_to_fill][1] = quad_to_jump_to
    else:
        quad_list[quad_to_fill][2] = quad_to_jump_to

def p_ciclo(p):
    '''ciclo : FROM exp TO exp DO cicloNP1 bloque cicloNP2'''

def p_cicloNP1(p):
    '''cicloNP1 :''' 
    global pilao
    global psaltos
    global cont_avail_list_index

    # We generate a quad where we substract the start from the end
    # We are substracting the end from the start, so cont should be less than 0
    # And we check if cont is less than 0, if not, the loop ends
    poper.append('-')
    process_exp()

    cont_avail_list_index = pilao.pop()
    cont_avail_list_type = ptypes.pop()

    global const_table
    if '0' not in const_table:
        const_table['0'] = {'type': '', 'va': ''}
        const_table['0']['type'] = 'int'
        const_table['0']['va'] = assign_virtual_address('int', 'constant', 1)
        
    quad_num = generate_quad('<', cont_avail_list_index, '0', 'int')
    quad_num_gtf = generate_quad('gtf', 'null', 'waiting for quad num', 'null')
    psaltos.append(quad_num)
    psaltos.append(quad_num_gtf)

def p_cicloNP2(p):
    '''cicloNP2 :''' 
    global cont_avail_list_index
    global quad_number
    
    # Para generar el cuadruplo (+, cont_avail_list_index, 1, int)
    global const_table
    if '1' not in const_table:
        const_table['1'] = {'type': '', 'va': ''}
        const_table['1']['type'] = 'int'
        const_table['1']['va'] = assign_virtual_address('int', 'constant', 1)
    generate_quad('+', cont_avail_list_index, '1', 'int')

    # Para generar el cuadruplo (=, cont_avail_list_index, [resultado de anterior cuadruplo], 'null')
    result_of_cont_plus_1 = pilao.pop()
    result_of_cont_plus_1_type = ptypes.pop()
    generate_quad('=', cont_avail_list_index, result_of_cont_plus_1, 'int')

    quad_to_fill = psaltos.pop()
    quad_to_jump_to = psaltos.pop()
    generate_quad('goto', quad_to_jump_to, 'null', 'null')
    quad_list[quad_to_fill][2] = quad_number

def p_variable(p):
    '''variable : ID variableNP1 variableA'''

# NP in which we verify if the variable exists, either in the local or global VT.
# The local VT has priority over the global VT
# If the variable does exist in one of these tables, then it appends its ID and type
# to the pilaO and pTypes stacks
def p_variableNP1(p):
    '''variableNP1 :'''
    global current_func_name
    global program_name
    global current_var_id
    current_var_id = p[-1]
    if p[-1] not in pd[current_func_name]['vt']:
        if p[-1] not in pd[program_name]['vt']:
            raise Exception('Variable ' + p[-1] + ' does not exist on either the local or global scopes')
        else:
            ptypes.append(pd[program_name]['vt'][p[-1]]['type'])
    else:
        ptypes.append(pd[current_func_name]['vt'][p[-1]]['type'])
    pilao.append(p[-1])

def p_variableA(p):
    '''variableA : LEFTBRACKET check_if_non_atomic exp verify RIGHTBRACKET variableB
                    | empty'''

def p_check_if_non_atomic(p):
    '''check_if_non_atomic :'''
    global current_var_id
    global current_nonatomic_id
    global current_var_type
    global piladim
    global on_matrix
    on_matrix = False
    current_var_id = pilao.pop()
    current_nonatomic_id = current_var_id
    current_var_type = ptypes.pop()
    if not 'dim' in pd[current_func_name]['vt'][current_var_id]:
        raise Exception('Trying to access an index on atomic variable ' + current_var_id)
    else:
        piladim.append(current_var_id)

def p_verify(p):
    '''verify :'''
    arr_id = piladim[-1]
    lsup = str(pd[current_func_name]['vt'][arr_id]['dim']['lsup'])
    generate_quad('verify', pilao[-1], lsup, 'int')
    # Getting rid of the verification's result and its type
    pilao.pop()
    ptypes.pop()
    aux = pilao.pop()
    ptypes.pop()

    if 'next_dim' in pd[current_func_name]['vt'][arr_id]['dim']:
        m1 = pd[current_func_name]['vt'][arr_id]['dim']['m1']

        generate_quad('*', aux, str(m1), 'int')
        global on_matrix
        on_matrix = True
    else:
        piladim.pop()
        generate_quad('+', aux, arr_id, 'pointer')

def p_variableB(p):
    '''variableB : LEFTBRACKET check_if_matrix exp verify_matrix RIGHTBRACKET
                    | empty on_matrix_check'''

def p_check_if_matrix(p):
    '''check_if_matrix :'''
    global piladim
    mat_id = piladim[-1]
    if not 'next_dim' in pd[current_func_name]['vt'][mat_id]['dim'] or mat_id != current_nonatomic_id:
        raise Exception('Trying to access a 2nd dimension in array' + mat_id)

def p_verify_matrix(p):
    '''verify_matrix :'''
    mat_id = piladim.pop()
    lsup = str(pd[current_func_name]['vt'][mat_id]['dim']['next_dim']['lsup'])
    generate_quad('verify', pilao[-1], lsup, 'int')
    # Getting rid of the verification's result and its type
    pilao.pop()
    ptypes.pop()
    aux = pilao.pop()
    ptypes.pop()
    s1m1 = pilao.pop()
    ptypes.pop()
    generate_quad('+', aux, s1m1, 'int')
    result = pilao.pop()
    ptypes.pop()
    generate_quad('+', result, mat_id, 'pointer')

def p_on_matrix_check(p):
    '''on_matrix_check :'''
    global on_matrix
    if on_matrix:
        raise Exception('Tried to access matrix with only one index')

def p_exp(p):
    '''exp : exp1 expA'''

def p_expNP1(p):
    '''expNP1 :'''
    if poper[-1] == '|':
        process_exp()

def p_expA(p):
    '''expA : OROP operNP1 exp1 expNP1 expA
                | empty'''

def p_exp1(p):
    '''exp1 : exp2 exp1A'''

def p_exp1NP1(p):
    '''exp1NP1 :'''
    if poper[-1] == '&':
        process_exp()

def p_exp1A(p):
    '''exp1A : ANDOP operNP1 exp2 exp1NP1 exp1A
                | empty'''

def p_exp2(p):
    '''exp2 : exp3 exp2A'''

def p_exp2A(p):
    '''exp2A : exp2B operNP1 exp3 exp2ANP1
                | empty'''

def p_exp2ANP1(p):
    '''exp2ANP1 :'''
    process_exp()

def p_exp2B(p):
    '''exp2B : LESSTHANOP
                | GREATERTHANOP
                | DIFFERENTOP
                | EQUALOP'''
    p[0] = p[1]

def p_exp3(p):
    '''exp3 : termino exp3A'''

def p_exp3NP1(p):
    '''exp3NP1 :'''
    if poper[-1] == '+' or poper[-1] == '-':
        process_exp()

def p_exp3A(p):
    '''exp3A : exp3B operNP1 termino exp3NP1 exp3A
                | empty'''

def p_exp3B(p):
    '''exp3B : SUMOP
                | SUBOP'''
    p[0] = p[1]

def p_termino(p):
    '''termino : terminoC terminoA'''

def p_terminoA(p):
    '''terminoA : terminoB operNP1 terminoC terminoANP1 terminoA
                    | empty'''

def p_terminoANP1(p):
    '''terminoANP1 :'''
    if poper[-1] == '*' or poper[-1] == '/':
        process_exp()

def p_terminoB(p):
    '''terminoB : MULOP
                    | DIVOP'''
    p[0] = p[1]

def p_terminoC(p):
    '''terminoC : factor
                    | llamada checkIfNotVoid'''

def p_checkIfNotVoid(p):
    '''checkIfNotVoid :'''
    if p[-1] == 'void':
        raise Exception('Void function is being called as part of an expression')

def p_factor(p):
    '''factor : LEFTPAR operNP1 exp RIGHTPAR factorNP1
                | CTI factorNP2
                | CTF factorNP3
                | variable'''

# NP in which we add an operator to pOper
def p_operNP1(p):
    '''operNP1 :'''
    global poper
    poper.append(p[-1])

# NP in which we pop the ( from pOper, erasing the 'fake bottom' from the stack
def p_factorNP1(p):
    '''factorNP1 :'''
    poper.pop()

def p_factorNP2(p):
    '''factorNP2 :'''
    global const_table
    pilao.append(p[-1])
    ptypes.append('int')
    if p[-1] not in const_table:
        const_table[p[-1]] = {'type': '', 'va': ''}
        const_table[p[-1]]['type'] = 'int'
        const_table[p[-1]]['va'] = assign_virtual_address('int', 'constant', 1)

def p_factorNP3(p):
    '''factorNP3 :'''
    global const_table
    pilao.append(p[-1])
    ptypes.append('float')
    if p[-1] not in const_table:
        const_table[p[-1]] = {'type': '', 'va': ''}
        const_table[p[-1]]['type'] = 'float'
        const_table[p[-1]]['va'] = assign_virtual_address('float', 'constant', 1)

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
    file = open("./avance3_test3.txt", "r")
    input = file.read()
except EOFError:
    pass
result = parser.parse(input)
print("------------")
if (result == 1):
    num = 0
    for i in quad_list:
        print(num, i)
        num += 1
    # print (json.dumps(pd, indent=2))
    print("Pass")
    json.dump(pd, open('proc_dir.txt', 'w'))
    json.dump(const_table, open('const_table.txt', 'w'))
    json.dump(quad_list, open('quad_list.txt', 'w'))
else:
    print("Fail")