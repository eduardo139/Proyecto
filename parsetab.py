
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'ANDOP ASSIGNOP COMMA CTF CTI CTSTRING DIFFERENTOP DIVOP DO ELSE EQUALOP FILE FLOAT FROM FUNC GREATERTHANOP ID IF INT LEFTBRACKET LEFTCURLY LEFTPAR LESSTHANOP MAINSTART MULOP OROP PROGRAM READ RETURN RIGHTBRACKET RIGHTCURLY RIGHTPAR SEMICOLON SUBOP SUMOP TO VAR VOID WRITEprogram : PROGRAM programNP1 ID programNP2 SEMICOLON programA programB mainprogramNP1 :programNP2 :programA : vars\n                    | emptyprogramB : funcion programB\n                    | emptyvars : VAR varsAvarsA : varsB SEMICOLON varsFvarsB : FILE varsNP2 ID varsNP3 varsNP6 varsE\n                | tipoSimple varsNP2 ID varsNP3 varsC varsNP6varsNP2 :varsNP6 :varsC : LEFTBRACKET CTI varsNP4 RIGHTBRACKET varsD\n                | emptyvarsNP4 :varsD : LEFTBRACKET CTI varsNP5 RIGHTBRACKET\n                | emptyvarsNP5 :varsE : COMMA ID varsNP3 varsNP6 varsE\n                | emptyvarsNP3 :varsF : varsA\n                | emptyfuncion : FUNC funcionA funcionNP1 ID funcionNP2 LEFTPAR funcionB RIGHTPAR SEMICOLON funcionC quadStart bloque genMemReqsquadStart :genMemReqs :funcionNP1 :funcionNP2 :funcionA : tipoSimple funcionANP1\n                    | VOID funcionANP1funcionANP1 :funcionB : params\n                    | emptyfuncionC : vars\n                    | emptymain : MAINSTART mainNP1 bloque getGlobalMemReqsgetGlobalMemReqs :mainNP1 :tipoSimple : INT tipoSimpleNP1\n                    | FLOAT tipoSimpleNP1tipoSimpleNP1 :params : tipoSimple paramsNP1 ID paramsNP2 paramsAparamsA : COMMA tipoSimple paramsNP1 ID paramsNP2 paramsA\n                | emptyparamsNP1 :paramsNP2 :bloque : LEFTCURLY bloqueA RIGHTCURLYbloqueA : estatuto bloqueA\n                | emptyestatuto : estatutoNP1 estatutoAestatutoA : asignacion \n                | llamada checkIfVoid\n                | read\n                | escritura\n                | return\n                | condicion\n                | ciclocheckIfVoid :estatutoNP1 : asignacion : variable ASSIGNOP asignacionNP1 exp asignacionNP2asignacionNP1 : asignacionNP2 : llamada : ID llamadaNP1 LEFTPAR loeNP2 primerparam llamadaA RIGHTPAR llamadaNP3primerparam : exp loeNP1\n                    | emptyllamadaA : COMMA exp loeNP1 llamadaA\n                | emptyllamadaNP1 :llamadaNP3 :read : READ ID readNP1readNP1 :escritura : WRITE escrituraNP1 LEFTPAR loeNP2 escrituraA escrituraB RIGHTPAR escrituraNP3escrituraNP1 :escrituraNP3 :loeNP2 :escrituraA : exp loeNP1\n                    | CTSTRING escrituraNP2loeNP1 :escrituraNP2 :escrituraB : COMMA escrituraA escrituraB\n                    | emptyreturn : RETURN exp returnNP1returnNP1 :condicion : IF LEFTPAR exp condicionNP1 RIGHTPAR bloque condicionA condicionANP2condicionNP1 :condicionA : ELSE condicionANP1 bloque\n                    | emptycondicionANP1 :condicionANP2 :ciclo : FROM exp TO exp DO cicloNP1 bloque cicloNP2cicloNP1 :cicloNP2 :variable : ID variableNP1 variableAvariableNP1 :variableA : LEFTBRACKET exp RIGHTBRACKET variableB\n                    | emptyvariableB : LEFTBRACKET exp RIGHTBRACKET\n                    | emptyexp : exp1 expAexpNP1 :expA : OROP operNP1 exp1 expNP1 expA\n                | emptyexp1 : exp2 exp1Aexp1NP1 :exp1A : ANDOP operNP1 exp2 exp1NP1 exp1A\n                | emptyexp2 : exp3 exp2Aexp2A : exp2B operNP1 exp3 exp2ANP1\n                | emptyexp2ANP1 :exp2B : LESSTHANOP\n                | GREATERTHANOP\n                | DIFFERENTOP\n                | EQUALOPexp3 : termino exp3Aexp3NP1 :exp3A : exp3B operNP1 termino exp3NP1 exp3A\n                | emptyexp3B : SUMOP\n                | SUBOPtermino : terminoC terminoAterminoA : terminoB operNP1 terminoC terminoANP1 terminoA\n                    | emptyterminoANP1 :terminoB : MULOP\n                    | DIVOPterminoC : factor\n                    | llamada checkIfNotVoidcheckIfNotVoid :factor : LEFTPAR operNP1 exp RIGHTPAR factorNP1\n                | CTI factorNP2\n                | CTF factorNP3\n                | variableoperNP1 :factorNP1 :factorNP2 :factorNP3 :empty : '
    
_lr_action_items = {'PROGRAM':([0,],[2,]),'$end':([1,21,41,46,56,],[0,-1,-38,-37,-48,]),'ID':([2,3,17,18,19,20,24,25,26,28,29,30,31,33,34,35,42,48,50,56,58,59,60,61,62,63,64,65,67,68,70,72,75,79,80,82,83,85,86,87,88,89,90,91,92,93,94,95,96,97,102,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,141,143,146,147,149,150,151,152,153,154,164,168,172,173,174,175,176,177,187,190,191,192,194,198,199,200,201,202,203,204,211,214,216,217,218,219,220,222,223,225,227,229,230,232,234,236,238,],[-2,4,-12,-12,-42,-42,-28,-32,-32,39,40,-40,-41,43,-30,-31,-60,-60,67,-48,-51,-52,-59,-54,-55,-56,-57,-58,-95,83,67,67,103,-53,-62,-139,-72,-84,-139,-139,-139,-139,-139,-128,-130,-135,-137,-138,-134,67,-46,67,-76,-94,67,-97,-71,-76,-83,-100,-135,-103,-104,-135,-107,-108,-135,-110,-112,-113,-114,-115,-116,-135,-119,-120,-121,-122,-135,-124,-126,-127,-129,67,-132,-133,67,159,-63,67,67,67,67,67,67,67,-61,-139,-101,-105,-111,-117,-125,-136,67,67,-96,-99,67,-139,-139,-109,-139,-139,-131,-139,-70,-75,-102,-106,-118,-123,-90,-88,-93,-46,-64,-98,-73,-85,-91,239,-87,]),'SEMICOLON':([4,5,16,39,40,44,45,52,53,55,74,76,77,103,142,144,145,160,162,163,184,226,],[-3,6,27,-22,-22,-13,-139,-139,-13,-15,-10,-21,-11,-22,158,-13,-139,-139,-14,-18,-20,-17,]),'VAR':([6,158,],[10,10,]),'FUNC':([6,7,8,9,12,15,27,36,37,38,56,224,235,],[-139,14,-4,-5,14,-8,-139,-9,-23,-24,-48,-27,-25,]),'MAINSTART':([6,7,8,9,11,12,13,15,23,27,36,37,38,56,224,235,],[-139,-139,-4,-5,22,-139,-7,-8,-6,-139,-9,-23,-24,-48,-27,-25,]),'FILE':([10,27,],[17,17,]),'INT':([10,14,27,73,208,],[19,19,19,19,19,]),'FLOAT':([10,14,27,73,208,],[20,20,20,20,20,]),'VOID':([14,],[26,]),'LEFTCURLY':([15,22,27,32,36,37,38,158,178,179,180,181,182,205,206,221,233,],[-8,-39,-139,42,-9,-23,-24,-139,42,-92,-26,-35,-36,42,42,-89,42,]),'COMMA':([39,44,52,67,82,86,87,88,89,90,91,92,94,95,96,103,106,107,109,113,115,116,118,119,121,126,128,131,133,136,138,139,144,147,159,160,165,166,167,168,169,170,171,172,173,174,175,176,177,183,189,191,192,196,197,198,199,200,201,202,203,211,212,215,216,217,218,219,227,228,229,239,240,],[-22,-13,75,-95,-139,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-22,-76,-94,-97,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-13,-139,-47,75,187,-79,-66,-139,194,-79,-80,-101,-105,-111,-117,-125,-136,208,-65,-96,-99,-77,-78,-139,-139,-109,-139,-139,-131,-70,-79,194,-102,-106,-118,-123,-64,187,-98,-47,208,]),'LEFTBRACKET':([40,45,67,82,145,168,],[-22,54,-95,108,161,190,]),'RIGHTCURLY':([42,47,48,49,56,57,58,59,60,61,62,63,64,65,67,79,82,83,85,86,87,88,89,90,91,92,94,95,96,107,109,110,112,113,115,116,118,119,121,126,128,131,133,136,138,139,146,164,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,204,211,214,216,217,218,219,220,222,223,227,229,230,232,234,238,],[-139,56,-139,-50,-48,-49,-51,-52,-59,-54,-55,-56,-57,-58,-95,-53,-139,-72,-84,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-71,-83,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-63,-61,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-139,-70,-75,-102,-106,-118,-123,-90,-88,-93,-64,-98,-73,-85,-91,-87,]),'READ':([42,48,50,56,58,59,60,61,62,63,64,65,67,79,82,83,85,86,87,88,89,90,91,92,94,95,96,107,109,110,112,113,115,116,118,119,121,126,128,131,133,136,138,139,146,164,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,204,211,214,216,217,218,219,220,222,223,227,229,230,232,234,238,],[-60,-60,68,-48,-51,-52,-59,-54,-55,-56,-57,-58,-95,-53,-139,-72,-84,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-71,-83,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-63,-61,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-139,-70,-75,-102,-106,-118,-123,-90,-88,-93,-64,-98,-73,-85,-91,-87,]),'WRITE':([42,48,50,56,58,59,60,61,62,63,64,65,67,79,82,83,85,86,87,88,89,90,91,92,94,95,96,107,109,110,112,113,115,116,118,119,121,126,128,131,133,136,138,139,146,164,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,204,211,214,216,217,218,219,220,222,223,227,229,230,232,234,238,],[-60,-60,69,-48,-51,-52,-59,-54,-55,-56,-57,-58,-95,-53,-139,-72,-84,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-71,-83,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-63,-61,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-139,-70,-75,-102,-106,-118,-123,-90,-88,-93,-64,-98,-73,-85,-91,-87,]),'RETURN':([42,48,50,56,58,59,60,61,62,63,64,65,67,79,82,83,85,86,87,88,89,90,91,92,94,95,96,107,109,110,112,113,115,116,118,119,121,126,128,131,133,136,138,139,146,164,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,204,211,214,216,217,218,219,220,222,223,227,229,230,232,234,238,],[-60,-60,70,-48,-51,-52,-59,-54,-55,-56,-57,-58,-95,-53,-139,-72,-84,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-71,-83,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-63,-61,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-139,-70,-75,-102,-106,-118,-123,-90,-88,-93,-64,-98,-73,-85,-91,-87,]),'IF':([42,48,50,56,58,59,60,61,62,63,64,65,67,79,82,83,85,86,87,88,89,90,91,92,94,95,96,107,109,110,112,113,115,116,118,119,121,126,128,131,133,136,138,139,146,164,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,204,211,214,216,217,218,219,220,222,223,227,229,230,232,234,238,],[-60,-60,71,-48,-51,-52,-59,-54,-55,-56,-57,-58,-95,-53,-139,-72,-84,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-71,-83,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-63,-61,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-139,-70,-75,-102,-106,-118,-123,-90,-88,-93,-64,-98,-73,-85,-91,-87,]),'FROM':([42,48,50,56,58,59,60,61,62,63,64,65,67,79,82,83,85,86,87,88,89,90,91,92,94,95,96,107,109,110,112,113,115,116,118,119,121,126,128,131,133,136,138,139,146,164,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,204,211,214,216,217,218,219,220,222,223,227,229,230,232,234,238,],[-60,-60,72,-48,-51,-52,-59,-54,-55,-56,-57,-58,-95,-53,-139,-72,-84,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-71,-83,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-63,-61,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-139,-70,-75,-102,-106,-118,-123,-90,-88,-93,-64,-98,-73,-85,-91,-87,]),'LEFTPAR':([43,51,67,69,70,71,72,80,81,84,93,97,105,106,108,111,114,117,120,122,123,124,125,127,129,130,132,134,135,137,141,147,149,150,151,152,153,154,187,190,194,],[-29,73,-69,-74,93,97,93,-62,106,111,-135,93,93,-76,93,-76,-135,-135,-135,-112,-113,-114,-115,-135,-120,-121,-135,-126,-127,93,93,93,93,93,93,93,93,93,93,93,93,]),'CTI':([54,70,72,80,93,97,105,106,108,111,114,117,120,122,123,124,125,127,129,130,132,134,135,137,141,147,149,150,151,152,153,154,161,187,190,194,],[78,94,94,-62,-135,94,94,-76,94,-76,-135,-135,-135,-112,-113,-114,-115,-135,-120,-121,-135,-126,-127,94,94,94,94,94,94,94,94,94,185,94,94,94,]),'ELSE':([56,204,],[-48,221,]),'ASSIGNOP':([66,67,82,107,109,168,191,192,229,],[80,-95,-139,-94,-97,-139,-96,-99,-98,]),'MULOP':([67,82,90,91,92,94,95,96,107,109,136,138,139,168,176,177,191,192,202,203,211,227,229,],[-95,-139,134,-128,-130,-137,-138,-134,-94,-97,-129,-132,-133,-139,-125,-136,-96,-99,134,-131,-70,-64,-98,]),'DIVOP':([67,82,90,91,92,94,95,96,107,109,136,138,139,168,176,177,191,192,202,203,211,227,229,],[-95,-139,135,-128,-130,-137,-138,-134,-94,-97,-129,-132,-133,-139,-125,-136,-96,-99,135,-131,-70,-64,-98,]),'SUMOP':([67,82,89,90,91,92,94,95,96,107,109,131,133,136,138,139,168,175,176,177,191,192,201,202,203,211,219,227,229,],[-95,-139,129,-139,-128,-130,-137,-138,-134,-94,-97,-122,-124,-129,-132,-133,-139,-117,-125,-136,-96,-99,129,-139,-131,-70,-123,-64,-98,]),'SUBOP':([67,82,89,90,91,92,94,95,96,107,109,131,133,136,138,139,168,175,176,177,191,192,201,202,203,211,219,227,229,],[-95,-139,130,-139,-128,-130,-137,-138,-134,-94,-97,-122,-124,-129,-132,-133,-139,-117,-125,-136,-96,-99,130,-139,-131,-70,-123,-64,-98,]),'LESSTHANOP':([67,82,88,89,90,91,92,94,95,96,107,109,126,128,131,133,136,138,139,168,175,176,177,191,192,201,202,203,211,218,219,227,229,],[-95,-139,122,-139,-139,-128,-130,-137,-138,-134,-94,-97,-116,-119,-122,-124,-129,-132,-133,-139,-117,-125,-136,-96,-99,-139,-139,-131,-70,-118,-123,-64,-98,]),'GREATERTHANOP':([67,82,88,89,90,91,92,94,95,96,107,109,126,128,131,133,136,138,139,168,175,176,177,191,192,201,202,203,211,218,219,227,229,],[-95,-139,123,-139,-139,-128,-130,-137,-138,-134,-94,-97,-116,-119,-122,-124,-129,-132,-133,-139,-117,-125,-136,-96,-99,-139,-139,-131,-70,-118,-123,-64,-98,]),'DIFFERENTOP':([67,82,88,89,90,91,92,94,95,96,107,109,126,128,131,133,136,138,139,168,175,176,177,191,192,201,202,203,211,218,219,227,229,],[-95,-139,124,-139,-139,-128,-130,-137,-138,-134,-94,-97,-116,-119,-122,-124,-129,-132,-133,-139,-117,-125,-136,-96,-99,-139,-139,-131,-70,-118,-123,-64,-98,]),'EQUALOP':([67,82,88,89,90,91,92,94,95,96,107,109,126,128,131,133,136,138,139,168,175,176,177,191,192,201,202,203,211,218,219,227,229,],[-95,-139,125,-139,-139,-128,-130,-137,-138,-134,-94,-97,-116,-119,-122,-124,-129,-132,-133,-139,-117,-125,-136,-96,-99,-139,-139,-131,-70,-118,-123,-64,-98,]),'ANDOP':([67,82,87,88,89,90,91,92,94,95,96,107,109,119,121,126,128,131,133,136,138,139,168,173,174,175,176,177,191,192,199,200,201,202,203,211,218,219,227,229,],[-95,-139,117,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-108,-110,-116,-119,-122,-124,-129,-132,-133,-139,-105,-111,-117,-125,-136,-96,-99,117,-109,-139,-139,-131,-70,-118,-123,-64,-98,]),'OROP':([67,82,86,87,88,89,90,91,92,94,95,96,107,109,116,118,119,121,126,128,131,133,136,138,139,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,211,217,218,219,227,229,],[-95,-139,114,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-139,-101,-105,-111,-117,-125,-136,-96,-99,114,-139,-109,-139,-139,-131,-70,-106,-118,-123,-64,-98,]),'TO':([67,82,86,87,88,89,90,91,92,94,95,96,98,107,109,113,115,116,118,119,121,126,128,131,133,136,138,139,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,211,216,217,218,219,227,229,],[-95,-139,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,141,-94,-97,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-70,-102,-106,-118,-123,-64,-98,]),'RIGHTPAR':([67,73,82,86,87,88,89,90,91,92,94,95,96,99,100,101,106,107,109,113,115,116,118,119,121,126,128,131,133,136,138,139,140,147,155,156,159,165,166,167,168,169,170,171,172,173,174,175,176,177,183,186,188,189,191,192,193,195,196,197,198,199,200,201,202,203,207,209,211,212,215,216,217,218,219,227,228,229,231,237,239,240,241,],[-95,-139,-139,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,142,-33,-34,-76,-94,-97,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,-86,-139,177,178,-47,-139,-79,-66,-139,-139,-79,-80,-101,-105,-111,-117,-125,-136,-139,211,-68,-65,-96,-99,214,-82,-77,-78,-139,-139,-109,-139,-139,-131,-43,-45,-70,-79,-139,-102,-106,-118,-123,-64,-139,-98,-81,-67,-47,-139,-44,]),'RIGHTBRACKET':([67,78,82,86,87,88,89,90,91,92,94,95,96,104,107,109,113,115,116,118,119,121,126,128,131,133,136,138,139,148,168,172,173,174,175,176,177,185,191,192,198,199,200,201,202,203,210,211,213,216,217,218,219,227,229,],[-95,-16,-139,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,145,-94,-97,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,168,-139,-101,-105,-111,-117,-125,-136,-19,-96,-99,-139,-139,-109,-139,-139,-131,226,-70,229,-102,-106,-118,-123,-64,-98,]),'DO':([67,82,86,87,88,89,90,91,92,94,95,96,107,109,113,115,116,118,119,121,126,128,131,133,136,138,139,157,168,172,173,174,175,176,177,191,192,198,199,200,201,202,203,211,216,217,218,219,227,229,],[-95,-139,-139,-139,-139,-139,-139,-128,-130,-137,-138,-134,-94,-97,-100,-103,-104,-107,-108,-110,-116,-119,-122,-124,-129,-132,-133,179,-139,-101,-105,-111,-117,-125,-136,-96,-99,-139,-139,-109,-139,-139,-131,-70,-102,-106,-118,-123,-64,-98,]),'CTF':([70,72,80,93,97,105,106,108,111,114,117,120,122,123,124,125,127,129,130,132,134,135,137,141,147,149,150,151,152,153,154,187,190,194,],[95,95,-62,-135,95,95,-76,95,-76,-135,-135,-135,-112,-113,-114,-115,-135,-120,-121,-135,-126,-127,95,95,95,95,95,95,95,95,95,95,95,95,]),'CTSTRING':([111,149,194,],[-76,171,171,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'programNP1':([2,],[3,]),'programNP2':([4,],[5,]),'programA':([6,],[7,]),'vars':([6,158,],[8,181,]),'empty':([6,7,12,27,42,45,48,52,73,82,86,87,88,89,90,145,147,158,160,165,168,169,183,198,199,201,202,204,215,228,240,],[9,13,13,38,49,55,49,76,101,109,115,118,121,128,133,163,167,182,76,188,192,195,209,115,118,128,133,222,195,188,209,]),'programB':([7,12,],[11,23,]),'funcion':([7,12,],[12,12,]),'varsA':([10,27,],[15,37,]),'varsB':([10,27,],[16,16,]),'tipoSimple':([10,14,27,73,208,],[18,25,18,102,225,]),'main':([11,],[21,]),'funcionA':([14,],[24,]),'varsNP2':([17,18,],[28,29,]),'tipoSimpleNP1':([19,20,],[30,31,]),'mainNP1':([22,],[32,]),'funcionNP1':([24,],[33,]),'funcionANP1':([25,26,],[34,35,]),'varsF':([27,],[36,]),'bloque':([32,178,205,206,233,],[41,204,223,224,238,]),'varsNP3':([39,40,103,],[44,45,144,]),'getGlobalMemReqs':([41,],[46,]),'bloqueA':([42,48,],[47,57,]),'estatuto':([42,48,],[48,48,]),'estatutoNP1':([42,48,],[50,50,]),'funcionNP2':([43,],[51,]),'varsNP6':([44,53,144,],[52,77,160,]),'varsC':([45,],[53,]),'estatutoA':([50,],[58,]),'asignacion':([50,],[59,]),'llamada':([50,70,72,97,105,108,137,141,147,149,150,151,152,153,154,187,190,194,],[60,92,92,92,92,92,92,92,92,92,92,92,92,92,92,92,92,92,]),'read':([50,],[61,]),'escritura':([50,],[62,]),'return':([50,],[63,]),'condicion':([50,],[64,]),'ciclo':([50,],[65,]),'variable':([50,70,72,97,105,108,137,141,147,149,150,151,152,153,154,187,190,194,],[66,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,]),'varsE':([52,160,],[74,184,]),'checkIfVoid':([60,],[79,]),'llamadaNP1':([67,],[81,]),'variableNP1':([67,],[82,]),'escrituraNP1':([69,],[84,]),'exp':([70,72,97,105,108,137,141,147,149,187,190,194,],[85,98,140,146,148,155,157,166,170,212,213,170,]),'exp1':([70,72,97,105,108,137,141,147,149,150,187,190,194,],[86,86,86,86,86,86,86,86,86,172,86,86,86,]),'exp2':([70,72,97,105,108,137,141,147,149,150,151,187,190,194,],[87,87,87,87,87,87,87,87,87,87,173,87,87,87,]),'exp3':([70,72,97,105,108,137,141,147,149,150,151,152,187,190,194,],[88,88,88,88,88,88,88,88,88,88,88,174,88,88,88,]),'termino':([70,72,97,105,108,137,141,147,149,150,151,152,153,187,190,194,],[89,89,89,89,89,89,89,89,89,89,89,89,175,89,89,89,]),'terminoC':([70,72,97,105,108,137,141,147,149,150,151,152,153,154,187,190,194,],[90,90,90,90,90,90,90,90,90,90,90,90,90,176,90,90,90,]),'factor':([70,72,97,105,108,137,141,147,149,150,151,152,153,154,187,190,194,],[91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,91,]),'funcionB':([73,],[99,]),'params':([73,],[100,]),'varsNP4':([78,],[104,]),'asignacionNP1':([80,],[105,]),'variableA':([82,],[107,]),'readNP1':([83,],[110,]),'returnNP1':([85,],[112,]),'expA':([86,198,],[113,216,]),'exp1A':([87,199,],[116,217,]),'exp2A':([88,],[119,]),'exp2B':([88,],[120,]),'exp3A':([89,201,],[126,218,]),'exp3B':([89,201,],[127,127,]),'terminoA':([90,202,],[131,219,]),'terminoB':([90,202,],[132,132,]),'checkIfNotVoid':([92,],[136,]),'operNP1':([93,114,117,120,127,132,],[137,150,151,152,153,154,]),'factorNP2':([94,],[138,]),'factorNP3':([95,],[139,]),'paramsNP1':([102,225,],[143,236,]),'loeNP2':([106,111,],[147,149,]),'condicionNP1':([140,],[156,]),'varsD':([145,],[162,]),'asignacionNP2':([146,],[164,]),'primerparam':([147,],[165,]),'escrituraA':([149,194,],[169,215,]),'funcionC':([158,],[180,]),'paramsNP2':([159,239,],[183,240,]),'llamadaA':([165,228,],[186,237,]),'loeNP1':([166,170,212,],[189,196,228,]),'variableB':([168,],[191,]),'escrituraB':([169,215,],[193,231,]),'escrituraNP2':([171,],[197,]),'expNP1':([172,],[198,]),'exp1NP1':([173,],[199,]),'exp2ANP1':([174,],[200,]),'exp3NP1':([175,],[201,]),'terminoANP1':([176,],[202,]),'factorNP1':([177,],[203,]),'cicloNP1':([179,],[205,]),'quadStart':([180,],[206,]),'paramsA':([183,240,],[207,241,]),'varsNP5':([185,],[210,]),'condicionA':([204,],[220,]),'llamadaNP3':([211,],[227,]),'escrituraNP3':([214,],[230,]),'condicionANP2':([220,],[232,]),'condicionANP1':([221,],[233,]),'cicloNP2':([223,],[234,]),'genMemReqs':([224,],[235,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> PROGRAM programNP1 ID programNP2 SEMICOLON programA programB main','program',8,'p_program','sintaxis.py',266),
  ('programNP1 -> <empty>','programNP1',0,'p_programNP1','sintaxis.py',270),
  ('programNP2 -> <empty>','programNP2',0,'p_programNP2','sintaxis.py',297),
  ('programA -> vars','programA',1,'p_programA','sintaxis.py',306),
  ('programA -> empty','programA',1,'p_programA','sintaxis.py',307),
  ('programB -> funcion programB','programB',2,'p_programB','sintaxis.py',310),
  ('programB -> empty','programB',1,'p_programB','sintaxis.py',311),
  ('vars -> VAR varsA','vars',2,'p_vars','sintaxis.py',314),
  ('varsA -> varsB SEMICOLON varsF','varsA',3,'p_varsA','sintaxis.py',317),
  ('varsB -> FILE varsNP2 ID varsNP3 varsNP6 varsE','varsB',6,'p_varsB','sintaxis.py',320),
  ('varsB -> tipoSimple varsNP2 ID varsNP3 varsC varsNP6','varsB',6,'p_varsB','sintaxis.py',321),
  ('varsNP2 -> <empty>','varsNP2',0,'p_varsNP2','sintaxis.py',325),
  ('varsNP6 -> <empty>','varsNP6',0,'p_varsNP6','sintaxis.py',332),
  ('varsC -> LEFTBRACKET CTI varsNP4 RIGHTBRACKET varsD','varsC',5,'p_varsC','sintaxis.py',343),
  ('varsC -> empty','varsC',1,'p_varsC','sintaxis.py',344),
  ('varsNP4 -> <empty>','varsNP4',0,'p_varsNP4','sintaxis.py',350),
  ('varsD -> LEFTBRACKET CTI varsNP5 RIGHTBRACKET','varsD',4,'p_varsD','sintaxis.py',364),
  ('varsD -> empty','varsD',1,'p_varsD','sintaxis.py',365),
  ('varsNP5 -> <empty>','varsNP5',0,'p_varsNP5','sintaxis.py',371),
  ('varsE -> COMMA ID varsNP3 varsNP6 varsE','varsE',5,'p_varsE','sintaxis.py',385),
  ('varsE -> empty','varsE',1,'p_varsE','sintaxis.py',386),
  ('varsNP3 -> <empty>','varsNP3',0,'p_varsNP3','sintaxis.py',390),
  ('varsF -> varsA','varsF',1,'p_varsF','sintaxis.py',395),
  ('varsF -> empty','varsF',1,'p_varsF','sintaxis.py',396),
  ('funcion -> FUNC funcionA funcionNP1 ID funcionNP2 LEFTPAR funcionB RIGHTPAR SEMICOLON funcionC quadStart bloque genMemReqs','funcion',13,'p_funcion','sintaxis.py',399),
  ('quadStart -> <empty>','quadStart',0,'p_quadStart','sintaxis.py',402),
  ('genMemReqs -> <empty>','genMemReqs',0,'p_genMemReqs','sintaxis.py',406),
  ('funcionNP1 -> <empty>','funcionNP1',0,'p_funcionNP1','sintaxis.py',417),
  ('funcionNP2 -> <empty>','funcionNP2',0,'p_funcionNP2','sintaxis.py',431),
  ('funcionA -> tipoSimple funcionANP1','funcionA',2,'p_funcionA','sintaxis.py',448),
  ('funcionA -> VOID funcionANP1','funcionA',2,'p_funcionA','sintaxis.py',449),
  ('funcionANP1 -> <empty>','funcionANP1',0,'p_funcionANP1','sintaxis.py',453),
  ('funcionB -> params','funcionB',1,'p_funcionB','sintaxis.py',458),
  ('funcionB -> empty','funcionB',1,'p_funcionB','sintaxis.py',459),
  ('funcionC -> vars','funcionC',1,'p_funcionC','sintaxis.py',462),
  ('funcionC -> empty','funcionC',1,'p_funcionC','sintaxis.py',463),
  ('main -> MAINSTART mainNP1 bloque getGlobalMemReqs','main',4,'p_main','sintaxis.py',466),
  ('getGlobalMemReqs -> <empty>','getGlobalMemReqs',0,'p_getGlobalMemReqs','sintaxis.py',469),
  ('mainNP1 -> <empty>','mainNP1',0,'p_mainNP1','sintaxis.py',476),
  ('tipoSimple -> INT tipoSimpleNP1','tipoSimple',2,'p_tipoSimple','sintaxis.py',488),
  ('tipoSimple -> FLOAT tipoSimpleNP1','tipoSimple',2,'p_tipoSimple','sintaxis.py',489),
  ('tipoSimpleNP1 -> <empty>','tipoSimpleNP1',0,'p_tipoSimpleNP1','sintaxis.py',493),
  ('params -> tipoSimple paramsNP1 ID paramsNP2 paramsA','params',5,'p_params','sintaxis.py',498),
  ('paramsA -> COMMA tipoSimple paramsNP1 ID paramsNP2 paramsA','paramsA',6,'p_paramsA','sintaxis.py',501),
  ('paramsA -> empty','paramsA',1,'p_paramsA','sintaxis.py',502),
  ('paramsNP1 -> <empty>','paramsNP1',0,'p_paramsNP1','sintaxis.py',507),
  ('paramsNP2 -> <empty>','paramsNP2',0,'p_paramsNP2','sintaxis.py',513),
  ('bloque -> LEFTCURLY bloqueA RIGHTCURLY','bloque',3,'p_bloque','sintaxis.py',525),
  ('bloqueA -> estatuto bloqueA','bloqueA',2,'p_bloqueA','sintaxis.py',528),
  ('bloqueA -> empty','bloqueA',1,'p_bloqueA','sintaxis.py',529),
  ('estatuto -> estatutoNP1 estatutoA','estatuto',2,'p_estatuto','sintaxis.py',532),
  ('estatutoA -> asignacion','estatutoA',1,'p_estatutoA','sintaxis.py',538),
  ('estatutoA -> llamada checkIfVoid','estatutoA',2,'p_estatutoA','sintaxis.py',539),
  ('estatutoA -> read','estatutoA',1,'p_estatutoA','sintaxis.py',540),
  ('estatutoA -> escritura','estatutoA',1,'p_estatutoA','sintaxis.py',541),
  ('estatutoA -> return','estatutoA',1,'p_estatutoA','sintaxis.py',542),
  ('estatutoA -> condicion','estatutoA',1,'p_estatutoA','sintaxis.py',543),
  ('estatutoA -> ciclo','estatutoA',1,'p_estatutoA','sintaxis.py',544),
  ('checkIfVoid -> <empty>','checkIfVoid',0,'p_checkIfVoid','sintaxis.py',547),
  ('estatutoNP1 -> <empty>','estatutoNP1',0,'p_estatutoNP1','sintaxis.py',553),
  ('asignacion -> variable ASSIGNOP asignacionNP1 exp asignacionNP2','asignacion',5,'p_asignacion','sintaxis.py',562),
  ('asignacionNP1 -> <empty>','asignacionNP1',0,'p_asignacionNP1','sintaxis.py',566),
  ('asignacionNP2 -> <empty>','asignacionNP2',0,'p_asignacionNP2','sintaxis.py',571),
  ('llamada -> ID llamadaNP1 LEFTPAR loeNP2 primerparam llamadaA RIGHTPAR llamadaNP3','llamada',8,'p_llamada','sintaxis.py',575),
  ('primerparam -> exp loeNP1','primerparam',2,'p_primerparam','sintaxis.py',581),
  ('primerparam -> empty','primerparam',1,'p_primerparam','sintaxis.py',582),
  ('llamadaA -> COMMA exp loeNP1 llamadaA','llamadaA',4,'p_llamadaA','sintaxis.py',585),
  ('llamadaA -> empty','llamadaA',1,'p_llamadaA','sintaxis.py',586),
  ('llamadaNP1 -> <empty>','llamadaNP1',0,'p_llamadaNP1','sintaxis.py',596),
  ('llamadaNP3 -> <empty>','llamadaNP3',0,'p_llamadaNP3','sintaxis.py',617),
  ('read -> READ ID readNP1','read',3,'p_read','sintaxis.py',628),
  ('readNP1 -> <empty>','readNP1',0,'p_readNP1','sintaxis.py',631),
  ('escritura -> WRITE escrituraNP1 LEFTPAR loeNP2 escrituraA escrituraB RIGHTPAR escrituraNP3','escritura',8,'p_escritura','sintaxis.py',646),
  ('escrituraNP1 -> <empty>','escrituraNP1',0,'p_escrituraNP1','sintaxis.py',651),
  ('escrituraNP3 -> <empty>','escrituraNP3',0,'p_escrituraNP3','sintaxis.py',655),
  ('loeNP2 -> <empty>','loeNP2',0,'p_loeNP2','sintaxis.py',664),
  ('escrituraA -> exp loeNP1','escrituraA',2,'p_escrituraA','sintaxis.py',673),
  ('escrituraA -> CTSTRING escrituraNP2','escrituraA',2,'p_escrituraA','sintaxis.py',674),
  ('loeNP1 -> <empty>','loeNP1',0,'p_loeNP1','sintaxis.py',681),
  ('escrituraNP2 -> <empty>','escrituraNP2',0,'p_escrituraNP2','sintaxis.py',698),
  ('escrituraB -> COMMA escrituraA escrituraB','escrituraB',3,'p_escrituraB','sintaxis.py',712),
  ('escrituraB -> empty','escrituraB',1,'p_escrituraB','sintaxis.py',713),
  ('return -> RETURN exp returnNP1','return',3,'p_return','sintaxis.py',716),
  ('returnNP1 -> <empty>','returnNP1',0,'p_returnNP1','sintaxis.py',719),
  ('condicion -> IF LEFTPAR exp condicionNP1 RIGHTPAR bloque condicionA condicionANP2','condicion',8,'p_condicion','sintaxis.py',737),
  ('condicionNP1 -> <empty>','condicionNP1',0,'p_condicionNP1','sintaxis.py',740),
  ('condicionA -> ELSE condicionANP1 bloque','condicionA',3,'p_condicionA','sintaxis.py',746),
  ('condicionA -> empty','condicionA',1,'p_condicionA','sintaxis.py',747),
  ('condicionANP1 -> <empty>','condicionANP1',0,'p_condicionANP1','sintaxis.py',750),
  ('condicionANP2 -> <empty>','condicionANP2',0,'p_condicionANP2','sintaxis.py',760),
  ('ciclo -> FROM exp TO exp DO cicloNP1 bloque cicloNP2','ciclo',8,'p_ciclo','sintaxis.py',771),
  ('cicloNP1 -> <empty>','cicloNP1',0,'p_cicloNP1','sintaxis.py',774),
  ('cicloNP2 -> <empty>','cicloNP2',0,'p_cicloNP2','sintaxis.py',793),
  ('variable -> ID variableNP1 variableA','variable',3,'p_variable','sintaxis.py',803),
  ('variableNP1 -> <empty>','variableNP1',0,'p_variableNP1','sintaxis.py',810),
  ('variableA -> LEFTBRACKET exp RIGHTBRACKET variableB','variableA',4,'p_variableA','sintaxis.py',823),
  ('variableA -> empty','variableA',1,'p_variableA','sintaxis.py',824),
  ('variableB -> LEFTBRACKET exp RIGHTBRACKET','variableB',3,'p_variableB','sintaxis.py',827),
  ('variableB -> empty','variableB',1,'p_variableB','sintaxis.py',828),
  ('exp -> exp1 expA','exp',2,'p_exp','sintaxis.py',831),
  ('expNP1 -> <empty>','expNP1',0,'p_expNP1','sintaxis.py',834),
  ('expA -> OROP operNP1 exp1 expNP1 expA','expA',5,'p_expA','sintaxis.py',839),
  ('expA -> empty','expA',1,'p_expA','sintaxis.py',840),
  ('exp1 -> exp2 exp1A','exp1',2,'p_exp1','sintaxis.py',843),
  ('exp1NP1 -> <empty>','exp1NP1',0,'p_exp1NP1','sintaxis.py',846),
  ('exp1A -> ANDOP operNP1 exp2 exp1NP1 exp1A','exp1A',5,'p_exp1A','sintaxis.py',851),
  ('exp1A -> empty','exp1A',1,'p_exp1A','sintaxis.py',852),
  ('exp2 -> exp3 exp2A','exp2',2,'p_exp2','sintaxis.py',855),
  ('exp2A -> exp2B operNP1 exp3 exp2ANP1','exp2A',4,'p_exp2A','sintaxis.py',858),
  ('exp2A -> empty','exp2A',1,'p_exp2A','sintaxis.py',859),
  ('exp2ANP1 -> <empty>','exp2ANP1',0,'p_exp2ANP1','sintaxis.py',862),
  ('exp2B -> LESSTHANOP','exp2B',1,'p_exp2B','sintaxis.py',866),
  ('exp2B -> GREATERTHANOP','exp2B',1,'p_exp2B','sintaxis.py',867),
  ('exp2B -> DIFFERENTOP','exp2B',1,'p_exp2B','sintaxis.py',868),
  ('exp2B -> EQUALOP','exp2B',1,'p_exp2B','sintaxis.py',869),
  ('exp3 -> termino exp3A','exp3',2,'p_exp3','sintaxis.py',873),
  ('exp3NP1 -> <empty>','exp3NP1',0,'p_exp3NP1','sintaxis.py',876),
  ('exp3A -> exp3B operNP1 termino exp3NP1 exp3A','exp3A',5,'p_exp3A','sintaxis.py',881),
  ('exp3A -> empty','exp3A',1,'p_exp3A','sintaxis.py',882),
  ('exp3B -> SUMOP','exp3B',1,'p_exp3B','sintaxis.py',885),
  ('exp3B -> SUBOP','exp3B',1,'p_exp3B','sintaxis.py',886),
  ('termino -> terminoC terminoA','termino',2,'p_termino','sintaxis.py',890),
  ('terminoA -> terminoB operNP1 terminoC terminoANP1 terminoA','terminoA',5,'p_terminoA','sintaxis.py',893),
  ('terminoA -> empty','terminoA',1,'p_terminoA','sintaxis.py',894),
  ('terminoANP1 -> <empty>','terminoANP1',0,'p_terminoANP1','sintaxis.py',897),
  ('terminoB -> MULOP','terminoB',1,'p_terminoB','sintaxis.py',902),
  ('terminoB -> DIVOP','terminoB',1,'p_terminoB','sintaxis.py',903),
  ('terminoC -> factor','terminoC',1,'p_terminoC','sintaxis.py',907),
  ('terminoC -> llamada checkIfNotVoid','terminoC',2,'p_terminoC','sintaxis.py',908),
  ('checkIfNotVoid -> <empty>','checkIfNotVoid',0,'p_checkIfNotVoid','sintaxis.py',911),
  ('factor -> LEFTPAR operNP1 exp RIGHTPAR factorNP1','factor',5,'p_factor','sintaxis.py',916),
  ('factor -> CTI factorNP2','factor',2,'p_factor','sintaxis.py',917),
  ('factor -> CTF factorNP3','factor',2,'p_factor','sintaxis.py',918),
  ('factor -> variable','factor',1,'p_factor','sintaxis.py',919),
  ('operNP1 -> <empty>','operNP1',0,'p_operNP1','sintaxis.py',923),
  ('factorNP1 -> <empty>','factorNP1',0,'p_factorNP1','sintaxis.py',929),
  ('factorNP2 -> <empty>','factorNP2',0,'p_factorNP2','sintaxis.py',933),
  ('factorNP3 -> <empty>','factorNP3',0,'p_factorNP3','sintaxis.py',943),
  ('empty -> <empty>','empty',0,'p_empty','sintaxis.py',954),
]
