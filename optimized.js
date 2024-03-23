function main() {
    let var_35 = 3;/* Value */
    let var_36 = 2;/* Value */
    let var_37 = 1;/* Value */
    let var_38 = var_36 + var_37; /* ExpPrim int*/
    let var_39 = var_35 * var_38; /* ExpPrim int*/
    let var_40 = 3;/* Value */
    let var_41 = 2;/* Value */
    let var_42 = 1;/* Value */
    let var_43 = var_41 + var_42; /* ExpPrim int*/
    let var_44 = var_40 == var_43; /* ExpPrim bool*/

    let var_47;
    if (var_44) {
        let var_45 = "\"OK\"";/* Value */
        var_47 = var_45; /* Phi Node */
    } else {
        let var_46 = "\"NG\"";/* Value */
        var_47 = var_46; /* Phi Node */
    } let var_48 = 123;/* Value */
    let var_49 = false;/* Value */
    let var_50 = {
        tuple_1: var_48,
        tuple_2: var_49
    }; /* Record {tuple_1:int,tuple_2:bool} */
    let var_51 = 123;/* Value */
    let var_52 = true;/* Value */
    let var_53 = {
        tuple_1: var_51,
        tuple_2: var_52
    }; /* Record {tuple_1:int,tuple_2:bool} */
    let var_55 = (var_50).tuple_2; /* {tuple_1:int,tuple_2:bool} */

    let var_64;
    if (var_55) {
        let var_57 = (var_50).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_58 = 2;/* Value */
        let var_59 = var_57 * var_58; /* ExpPrim int*/
        var_64 = var_59; /* Phi Node */
    } else {
        let var_61 = (var_50).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_62 = 2;/* Value */
        let var_63 = var_61 / var_62; /* ExpPrim int*/
        var_64 = var_63; /* Phi Node */
    } let var_66 = (var_53).tuple_2; /* {tuple_1:int,tuple_2:bool} */

    let var_75;
    if (var_66) {
        let var_68 = (var_53).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_69 = 2;/* Value */
        let var_70 = var_68 * var_69; /* ExpPrim int*/
        var_75 = var_70; /* Phi Node */
    } else {
        let var_72 = (var_53).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_73 = 2;/* Value */
        let var_74 = var_72 / var_73; /* ExpPrim int*/
        var_75 = var_74; /* Phi Node */
    } let var_76 = {}; /* Record {} */
    let var_77 = lambda_0 /* Function Pointer (int -> string) */;/* Value */
    let var_78 = {
        env: var_76,
        f_ptr: var_77
    }; /* Record (int -> string) */
    let var_79 = 0;/* Value */
    let var_80 = lambda_0(var_79);/* ExpCall string*/
    let var_81 = 123;/* Value */
    let var_82 = lambda_0(var_81);/* ExpCall string*/
    let var_83 = {
        tuple_1: var_80,
        tuple_2: var_82
    }; /* Record {tuple_1:string,tuple_2:string} */
    let var_84 = {}; /* Record {} */
    let var_85 = lambda_1 /* Function Pointer ((int -> int) -> (int -> int)) */;/* Value */
    let var_86 = {
        env: var_84,
        f_ptr: var_85
    }; /* Record ((int -> int) -> (int -> int)) */
    let var_87 = 456;/* Value */
    let var_88 = {}; /* Record {} */
    let var_89 = lambda_3 /* Function Pointer (int -> int) */;/* Value */
    let var_90 = {
        env: var_88,
        f_ptr: var_89
    }; /* Record (int -> int) */
    let var_91 = lambda_1(var_90);/* ExpCall (int -> int)*/
    let var_92 = (var_91).env; /* (int -> int) */
    let var_93 = {}; /* Record {} */
    let var_94 = lambda_3 /* Function Pointer (int -> int) */;/* Value */
    let var_95 = {
        env: var_93,
        f_ptr: var_94
    }; /* Record (int -> int) */
    let var_96 = lambda_1(var_95);/* ExpCall (int -> int)*/
    let var_97 = (var_96).f_ptr; /* (int -> int) */
    let var_98 = var_97(var_92, var_87);/* ExpApp int */
    let var_99 = {}; /* Record {} */
    let var_100 = lambda_4 /* Function Pointer (int -> int) */;/* Value */
    let var_101 = {
        env: var_99,
        f_ptr: var_100
    }; /* Record (int -> int) */
    let var_102 = 3;/* Value */
    let var_103 = lambda_4(var_102);/* ExpCall int*/
    let var_105 = { var_16: var_103 }; /* Record int */
    let var_106 = lambda_5 /* Function Pointer (int -> int) */;/* Value */
    let var_107 = {
        env: var_105,
        f_ptr: var_106
    }; /* Record (int -> int) */
    let var_108 = {}; /* Record {} */
    let var_109 = lambda_6 /* Function Pointer (AD -> {tuple_1:AD,tuple_2:AD}) */;/* Value */
    let var_110 = {
        env: var_108,
        f_ptr: var_109
    }; /* Record (AD -> {tuple_1:AD,tuple_2:AD}) */
    let var_111 = {}; /* Record {} */
    let var_112 = lambda_7 /* Function Pointer ({tuple_1:AG,tuple_2:AH} -> AH) */;/* Value */
    let var_113 = {
        env: var_111,
        f_ptr: var_112
    }; /* Record ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_114 = 123;/* Value */
    let var_115 = {
        tuple_1: var_113,
        tuple_2: var_114
    }; /* Record {tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int} */
    let var_116 = 456;/* Value */
    let var_117 = lambda_6(var_116);/* ExpCall {tuple_1:int,tuple_2:int}*/
    let var_119 = (var_115).tuple_1; /* [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_120 = (var_119).env; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_122 = (var_115).tuple_1; /* [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_123 = (var_122).f_ptr; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_124 = var_123(var_120, var_117);/* ExpApp int */
    let var_125 = "\"abc\"";/* Value */
    let var_126 = lambda_6(var_125);/* ExpCall {tuple_1:string,tuple_2:string}*/
    let var_128 = (var_115).tuple_1; /* [AG,AH.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_129 = (var_128).env; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_131 = (var_115).tuple_1; /* [AG,AH.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_132 = (var_131).f_ptr; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_133 = var_132(var_129, var_126);/* ExpApp string */
    let var_134 = {
        tuple_1: var_124,
        tuple_2: var_133
    }; /* Record {tuple_1:int,tuple_2:string} */
    let var_135 = {}; /* Record {} */
    let var_136 = lambda_8 /* Function Pointer (int -> int) */;/* Value */
    let var_137 = {
        env: var_135,
        f_ptr: var_136
    }; /* Record (int -> int) */
    let var_155 = {
        a: var_39,
        b: var_47,
        c: var_50,
        c2: var_53,
        d: var_64,
        d2: var_75,
        e: var_78,
        f: var_83,
        fib: var_137,
        g: var_86,
        i: var_98,
        j: var_101,
        k: var_103,
        l: var_110,
        m: var_115,
        n: var_134,
        o: var_107
    }; /* Record {a:int,b:string,c:{tuple_1:int,tuple_2:bool},c2:{tuple_1:int,tuple_2:bool},d:int,d2:int,e:(int -> string),f:{tuple_1:string,tuple_2:string},fib:(int -> int),g:((int -> int) -> (int -> int)),i:int,j:(int -> int),k:int,l:(AD -> {tuple_1:AD,tuple_2:AD}),m:{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int},n:{tuple_1:int,tuple_2:string},o:(int -> int)} */
    return var_155; /* Return {a:int,b:string,c:{tuple_1:int,tuple_2:bool},c2:{tuple_1:int,tuple_2:bool},d:int,d2:int,e:(int -> string),f:{tuple_1:string,tuple_2:string},fib:(int -> int),g:((int -> int) -> (int -> int)),i:int,j:(int -> int),k:int,l:(AD -> {tuple_1:AD,tuple_2:AD}),m:{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int},n:{tuple_1:int,tuple_2:string},o:(int -> int)} */
}
function lambda_0(var_7 /* x */) {
    let var_157 = 0;/* Value */
    let var_158 = var_7 == var_157; /* ExpPrim bool*/

    let var_161;
    if (var_158) {
        let var_159 = "\"zero\"";/* Value */
        var_161 = var_159; /* Phi Node */
    } else {
        let var_160 = "\"nonzero\"";/* Value */
        var_161 = var_160; /* Phi Node */
    } return var_161; /* Return (int -> string) */
}
function lambda_1(var_10 /* h */) {
    let var_163 = { var_10: var_10 }; /* Record int */
    let var_164 = lambda_2 /* Function Pointer (int -> int) */;/* Value */
    let var_165 = {
        env: var_163,
        f_ptr: var_164
    }; /* Record (int -> int) */
    return var_165; /* Return ((int -> int) -> (int -> int)) */
}
function lambda_2(var_28 /* CC_ENV */, var_11 /* x */) {
    let var_167 = (var_28).var_10; /* (int -> int) */
    let var_170 = (var_167).env; /* (int -> int) */
    let var_172 = (var_167).f_ptr; /* (int -> int) */
    let var_173 = var_172(var_170, var_11);/* ExpApp int */
    let var_174 = 123;/* Value */
    let var_176 = (var_167).env; /* (int -> int) */
    let var_178 = (var_167).f_ptr; /* (int -> int) */
    let var_179 = var_178(var_176, var_174);/* ExpApp int */
    let var_180 = var_173 + var_179; /* ExpPrim int*/
    return var_180; /* Return (int -> int) */
}
function lambda_3(var_12 /* z */) {
    let var_182 = 2;/* Value */
    let var_183 = var_12 * var_182; /* ExpPrim int*/
    return var_183; /* Return (int -> int) */
}
function lambda_4(var_15 /* x */) {
    let var_185 = 0;/* Value */
    let var_186 = var_15 == var_185; /* ExpPrim bool*/

    let var_194;
    if (var_186) {
        let var_187 = 1;/* Value */
        var_194 = var_187; /* Phi Node */
    } else {
        let var_189 = 1;/* Value */
        let var_190 = var_15 - var_189; /* ExpPrim int*/
        let var_191 = lambda_4(var_190);/* ExpCall int*/
        let var_193 = var_191 * var_15; /* ExpPrim int*/
        var_194 = var_193; /* Phi Node */
    } return var_194; /* Return (int -> int) */
}
function lambda_5(var_31 /* CC_ENV */, var_17 /* x */) {
    let var_196 = (var_31).var_16; /* int */
    let var_199 = var_196 + var_17; /* ExpPrim int*/
    return var_199; /* Return (int -> int) */
}
function lambda_6(var_20 /* x */) {
    let var_202 = {
        tuple_1: var_20,
        tuple_2: var_20
    }; /* Record {tuple_1:AD,tuple_2:AD} */
    return var_202; /* Return (AD -> {tuple_1:AD,tuple_2:AD}) */
}
function lambda_7(var_21 /* x */) {
    let var_204 = (var_21).tuple_2; /* {tuple_1:AG,tuple_2:AH} */
    return var_204; /* Return ({tuple_1:AG,tuple_2:AH} -> AH) */
}
function lambda_8(var_25 /* x */) {
    let var_206 = 1;/* Value */
    let var_207 = var_25 == var_206; /* ExpPrim bool*/

    let var_223;
    if (var_207) {
        let var_208 = 1;/* Value */
        var_223 = var_208; /* Phi Node */
    } else {
        let var_210 = 2;/* Value */
        let var_211 = var_25 == var_210; /* ExpPrim bool*/

        let var_222;
        if (var_211) {
            let var_212 = 1;/* Value */
            var_222 = var_212; /* Phi Node */
        } else {
            let var_214 = 1;/* Value */
            let var_215 = var_25 - var_214; /* ExpPrim int*/
            let var_216 = lambda_8(var_215);/* ExpCall int*/
            let var_218 = 2;/* Value */
            let var_219 = var_25 - var_218; /* ExpPrim int*/
            let var_220 = lambda_8(var_219);/* ExpCall int*/
            let var_221 = var_216 + var_220; /* ExpPrim int*/
            var_222 = var_221; /* Phi Node */
        } var_223 = var_222; /* Phi Node */
    } return var_223; /* Return (int -> int) */
}
