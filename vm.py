import json

program_file = 'quad_list.txt'
proc_dir_file = 'proc_dir.txt'
const_table_file = 'const_table.txt'

quad_list = json.load(open(program_file))
procedure_directory = json.load(open(proc_dir_file))
constants_table = json.load(open(const_table_file))

class Memory:
    def __init__(self, type, memory_requirements):
        if type == 'global' or type == 'local':
            self.ints = [0] * memory_requirements['var']['int']
            self.floats = [0] * memory_requirements['var']['float']
            self.files = [0] * memory_requirements['var']['file']
            self.temp_ints = [0] * memory_requirements['temp']['int']
            self.temp_floats = [0] * memory_requirements['temp']['float']
            self.pointers = [0] * memory_requirements['temp']['pointer']
        elif type == 'constants':
            self.ints = [] 
            self.floats = []
            self.strings = []

constants_memory = Memory('constants', {})
for constant in constants_table:
    if constants_table[constant]['type'] == 'int':
        constants_memory.ints.append(int(constant))
    elif constants_table[constant]['type'] == 'float':
        constants_memory.floats.append(float(constant))
    elif constants_table[constant]['type'] == 'string':
        constants_memory.strings.append(constant)

program_name = list(procedure_directory)[0]
global_memory = Memory('global', procedure_directory[program_name]['mem'])

instruction_pointer = 0

def get_operand_type(virtual_address):
    if (virtual_address >= 1000 and virtual_address < 1333) or (virtual_address >= 2000 and virtual_address < 2333): 
        return 'int'
    elif (virtual_address >= 1333 and virtual_address < 1666) or (virtual_address >= 2333 and virtual_address < 2666):
        return 'float'
    elif (virtual_address >= 1666 and virtual_address < 2000) or (virtual_address >= 2666 and virtual_address < 3000):
        return 'file'
    elif virtual_address >= 4666:
        return 'string'
    elif virtual_address >= 3000 and virtual_address < 3333:
        return 'temp_int'
    elif virtual_address >= 3333 and virtual_address < 3666:
        return 'temp_float'
    elif virtual_address >= 3666 and virtual_address < 4000:
        return 'pointer'
    elif virtual_address >= 4000 and virtual_address < 4333:
        return 'const_int'
    elif virtual_address >= 4333 and virtual_address < 4666:
        return 'const_float'

def get_operand_value(type, real_address, memory, const_mem):
    value = -9999
    match type:
        case 'int':
            value = memory.ints[real_address]
        case 'float':
            value = memory.floats[real_address]
        case 'file':
            value = memory.files[real_address]
        case 'pointer':
            value = memory.pointers[real_address]
        case 'string':
            value = const_mem.strings[real_address]          
        case 'temp_int':
            value = memory.temp_ints[real_address]
        case 'temp_float':
            value = memory.temp_floats[real_address]
        case 'const_int':
            value = const_mem.ints[real_address]
        case 'const_float':
            value = const_mem.floats[real_address]
    return value

def get_real_memory_address(virtual_address, current_function):
    # Globals
    if virtual_address >= 1000 and virtual_address < 1333:
        return virtual_address - 1000
    elif virtual_address >= 1333 and virtual_address < 1666:
        return virtual_address - 1333 
    elif virtual_address >= 1666 and virtual_address < 2000:
        return virtual_address - 1666 
    # Locals
    if virtual_address >= 2000 and virtual_address < 2333:
        return virtual_address - 2000
    elif virtual_address >= 2333 and virtual_address < 2666:
        return virtual_address - 2333
    elif virtual_address >= 2666 and virtual_address < 3000:
        return virtual_address - 2666
    # Temps
    if virtual_address >= 3000 and virtual_address < 3333:
        return virtual_address - 3000
    elif virtual_address >= 3333 and virtual_address < 3666:
        return virtual_address - 3333
    elif virtual_address >= 3666 and virtual_address < 4000:
        return virtual_address - 3666 
    # What if it´s a constant? still got to do that??????????
    elif virtual_address >= 4000 and virtual_address < 4333:
        return virtual_address - 4000
    elif virtual_address >= 4333 and virtual_address < 4666:
        return virtual_address - 4333
    elif virtual_address >= 4666:
        return virtual_address - 4666

# opcodes = {'=': 1, '<': 2, '>': 3, '<>': 4, '==': 5, '+': 6, '-': 7, '*': 8, '/': 9,
#           '&': 10, '|': 11, 'read': 12, 'write': 13, 'goto': 14, 'gtf': 15, 'call': 16,
#           'gosub': 17, 'era': 18, 'parameter': 19, 'endfunc': 20}

current_function = program_name
current_memory = global_memory

while instruction_pointer < len(quad_list):
    opcode = quad_list[instruction_pointer][0]
    left_operand_virtual_address = quad_list[instruction_pointer][1]
    right_operand_virtual_address = quad_list[instruction_pointer][2]
    result_virtual_address = quad_list[instruction_pointer][3]
    print(instruction_pointer)
    print(quad_list[instruction_pointer])

    if opcode not in [16, 17, 18, 19, 20, 21]:
        if result_virtual_address != 'null':
            result_type = get_operand_type(result_virtual_address)
            result_real_address = get_real_memory_address(result_virtual_address, current_function)

        if left_operand_virtual_address != 'null' and type(left_operand_virtual_address) is not list:
            left_operand_type = get_operand_type(left_operand_virtual_address)
            left_operand_real_address = get_real_memory_address(left_operand_virtual_address, current_function)
            left_operand_value = get_operand_value(left_operand_type, left_operand_real_address, current_memory, constants_memory)

            if left_operand_type == 'pointer':
                left_operand_type = get_operand_type(left_operand_value)
                left_operand_real_address = get_real_memory_address(left_operand_value, current_function)
                left_operand_value = get_operand_value(left_operand_type, left_operand_real_address, current_memory, constants_memory)
        
        elif type(left_operand_virtual_address) is list:
            arg_list = []
            for virtual_address in left_operand_virtual_address:
                operand_type = get_operand_type(virtual_address)
                operand_real_address = get_real_memory_address(virtual_address, current_function)
                operand_value = get_operand_value(operand_type, operand_real_address, current_memory, constants_memory)
                
                if operand_type == 'pointer':
                    operand_type = get_operand_type(operand_value)
                    operand_real_address = get_real_memory_address(operand_value, current_function)
                    operand_value = get_operand_value(operand_type, operand_real_address, current_memory, constants_memory)
                
                arg_list.append(operand_value)

        if right_operand_virtual_address != 'null':
            right_operand_type = get_operand_type(right_operand_virtual_address)
            right_operand_real_address = get_real_memory_address(right_operand_virtual_address, current_function)
            right_operand_value = get_operand_value(right_operand_type, right_operand_real_address, current_memory, constants_memory)

            if right_operand_type == 'pointer':
                right_operand_type = get_operand_type(right_operand_value)
                right_operand_real_address = get_real_memory_address(right_operand_value, current_function)
                right_operand_value = get_operand_value(right_operand_type, right_operand_real_address, current_memory, constants_memory)

        
        match opcode:
            case 1:
                if left_operand_type in ['int', 'temp_int', 'const_int']:
                    # This if only exists so the case where we
                    # add 1 to cont (in a loop) works
                    if left_operand_virtual_address >= 3000 and left_operand_virtual_address < 3666:
                        current_memory.temp_ints[left_operand_real_address] = right_operand_value
                    else:
                        current_memory.ints[left_operand_real_address] = right_operand_value
                else:
                    current_memory.floats[left_operand_real_address] = right_operand_value
            case 2:
                if left_operand_value < right_operand_value:
                    current_memory.temp_ints[result_real_address] = 1
                else:
                    current_memory.temp_ints[result_real_address] = 0
            case 3:
                if left_operand_value > right_operand_value:
                    current_memory.temp_ints[result_real_address] = 1
                else:
                    current_memory.temp_ints[result_real_address] = 0
            case 4:
                if left_operand_value != right_operand_value:
                    current_memory.temp_ints[result_real_address] = 1
                else:
                    current_memory.temp_ints[result_real_address] = 0
            case 5:
                if left_operand_value == right_operand_value:
                    current_memory.temp_ints[result_real_address] = 1
                else:
                    current_memory.temp_ints[result_real_address] = 0
            case 6:
                if result_type == 'pointer':
                    right_operand_value = right_operand_virtual_address
                    current_memory.pointers[result_real_address] = left_operand_value + right_operand_value
                elif left_operand_type in ['int', 'temp_int', 'const_int'] and right_operand_type in ['int', 'temp_int', 'const_int']:
                    current_memory.temp_ints[result_real_address] = left_operand_value + right_operand_value
                else:
                    current_memory.temp_floats[result_real_address] = left_operand_value + right_operand_value
            case 7:
                if left_operand_type in ['int', 'temp_int', 'const_int'] and right_operand_type in ['int', 'temp_int', 'const_int']:
                    current_memory.temp_ints[result_real_address] = left_operand_value - right_operand_value
                else:
                    current_memory.temp_floats[result_real_address] = left_operand_value - right_operand_value
            case 8:
                if left_operand_type in ['int', 'temp_int', 'const_int'] and right_operand_type in ['int', 'temp_int', 'const_int']:
                    current_memory.temp_ints[result_real_address] = left_operand_value * right_operand_value
                else:
                    current_memory.temp_floats[result_real_address] = left_operand_value * right_operand_value
            case 9:
                print(result_real_address)
                if left_operand_type in ['int', 'temp_int', 'const_int'] and right_operand_type in ['int', 'temp_int', 'const_int']:
                    current_memory.temp_ints[result_real_address] = left_operand_value / right_operand_value
                else:
                    current_memory.temp_floats[result_real_address] = left_operand_value / right_operand_value
            case 10:
                if left_operand_value != 0 and right_operand_value != 0:
                    current_memory.temp_ints[result_real_address] = 1
                else:
                    current_memory.temp_ints[result_real_address] = 0
            case 11:
                if left_operand_value != 0 or right_operand_value != 0:
                    current_memory.temp_ints[result_real_address] = 1
                else:
                    current_memory.temp_ints[result_real_address] = 0
            case 12:
                pass
                # file = open(left_operand_value)
                # data_dict = json.load(file)
                # print(data_dict)
            case 13:
                for arg in arg_list:
                    print(arg)
            case 14:
                instruction_pointer = quad_list[instruction_pointer][1] - 1
            case 15:
                if left_operand_value == 0:
                    instruction_pointer = quad_list[instruction_pointer][1] - 1
            case 16:
                pass
            case 17:
                pass
            case 18:
                pass
            case 19:
                pass
            case 20:
                pass
            case _:
                pass
    instruction_pointer += 1