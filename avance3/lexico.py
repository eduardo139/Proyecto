import ply.lex as lex
 
# Reserved words
reserved = {
    'program' : 'PROGRAM',
    'var' : 'VAR',
    'int' : 'INT',
    'float' : 'FLOAT',
    'file': 'FILE',
    'if' : 'IF',
    'else' : 'ELSE',
    'from' : 'FROM',
    'to' : 'TO',
    'do' : 'DO',
    'main' : 'MAINSTART',
    'func' : 'FUNC',
    'void' : 'VOID',
    'read' : 'READ',
    'write' : 'WRITE'
}

# List of token names
tokens = [
    'SEMICOLON',
    'COMMA',
    'LEFTCURLY',
    'RIGHTCURLY',
    'LEFTPAR',
    'RIGHTPAR',
    'LEFTBRACKET',
    'RIGHTBRACKET',
    'ASSIGNOP',
    'LESSTHANOP',
    'GREATERTHANOP',
    'DIFFERENTOP',
    'EQUALOP',
    'SUMOP',
    'SUBOP',
    'MULOP',
    'DIVOP',
    'ANDOP',
    'OROP',
    'ID',
    'CTI',
    'CTF',
    'CTSTRING'
] + list(reserved.values())

# Regular expression rules
t_SEMICOLON = r';'
t_COMMA = r','
t_LEFTCURLY = r'{'
t_RIGHTCURLY = r'}'
t_LEFTPAR = r'\('
t_RIGHTPAR = r'\)'
t_LEFTBRACKET = r'\['
t_RIGHTBRACKET = r'\]'
t_ASSIGNOP = r'='
t_LESSTHANOP = r'<'
t_GREATERTHANOP = r'>'
t_DIFFERENTOP = r'<>'
t_EQUALOP = r'=='
t_SUMOP = r'\+'
t_SUBOP = r'-'
t_MULOP = r'\*'
t_DIVOP = r'/'
t_ANDOP = r'&'
t_OROP = r'\|'
def t_ID(t):
    r'[A-Za-z]([A-Za-z] | [0-9])*'
    t.type = reserved.get(t.value, 'ID')    # Checking for reserved words
    return t
t_CTI       = r'[0-9]+'
t_CTF       = r'[0-9]+ \. [0-9]+'
t_CTSTRING  = r'".+"'

# A string containing ignored characters (spaces, tabs and newlines)
t_ignore  = ' \t\n'
 
# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
 
# Build the lexer
lexer = lex.lex()