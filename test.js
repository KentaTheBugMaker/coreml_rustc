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
    let var_54 = var_50 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
    let var_55 = (var_54).tuple_2; /* {tuple_1:int,tuple_2:bool} */

    let var_64;
    if (var_55) {
        let var_56 = var_50 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
        let var_57 = (var_56).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_58 = 2;/* Value */
        let var_59 = var_57 * var_58; /* ExpPrim int*/
        var_64 = var_59; /* Phi Node */
    } else {
        let var_60 = var_50 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
        let var_61 = (var_60).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_62 = 2;/* Value */
        let var_63 = var_61 / var_62; /* ExpPrim int*/
        var_64 = var_63; /* Phi Node */
    } let var_65 = var_53 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
    let var_66 = (var_65).tuple_2; /* {tuple_1:int,tuple_2:bool} */

    let var_75;
    if (var_66) {
        let var_67 = var_53 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
        let var_68 = (var_67).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_69 = 2;/* Value */
        let var_70 = var_68 * var_69; /* ExpPrim int*/
        var_75 = var_70; /* Phi Node */
    } else {
        let var_71 = var_53 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
        let var_72 = (var_71).tuple_1; /* {tuple_1:int,tuple_2:bool} */
        let var_73 = 2;/* Value */
        let var_74 = var_72 / var_73; /* ExpPrim int*/
        var_75 = var_74; /* Phi Node */
    } let var_76 = lambda_0 /* Function Pointer (int -> string) */;/* Value */
    let var_77 = 0;/* Value */
    let var_78 = lambda_0(var_77);/* ExpCall string*/
    let var_79 = 123;/* Value */
    let var_80 = lambda_0(var_79);/* ExpCall string*/
    let var_81 = {
        tuple_1: var_78,
        tuple_2: var_80
    }; /* Record {tuple_1:string,tuple_2:string} */
    let var_82 = lambda_1 /* Function Pointer ((int -> int) -> (int -> int)) */;/* Value */
    let var_83 = 456;/* Value */
    let var_84 = {}; /* Record {} */
    let var_85 = lambda_3 /* Function Pointer (int -> int) */;/* Value */
    let var_86 = {
        env: var_84,
        f_ptr: var_85
    }; /* Record {env:{},f_ptr:(int -> int)} */
    let var_87 = lambda_1(var_86);/* ExpCall (int -> int)*/
    let var_88 = (var_87).env; /* (int -> int) */
    let var_89 = {}; /* Record {} */
    let var_90 = lambda_3 /* Function Pointer (int -> int) */;/* Value */
    let var_91 = {
        env: var_89,
        f_ptr: var_90
    }; /* Record {env:{},f_ptr:(int -> int)} */
    let var_92 = lambda_1(var_91);/* ExpCall (int -> int)*/
    let var_93 = (var_92).f_ptr; /* (int -> int) */
    let var_94 = var_93(var_88, var_83);/* ExpApp int */
    let var_95 = lambda_4 /* Function Pointer (int -> int) */;/* Value */
    let var_96 = 3;/* Value */
    let var_97 = lambda_4(var_96);/* ExpCall int*/
    let var_98 = var_97 /* ExpVar int */;/* Value */
    let var_99 = { var_16: var_98 }; /* Record {var_16:int} */
    let var_100 = lambda_5 /* Function Pointer (int -> int) */;/* Value */
    let var_101 = {
        env: var_99,
        f_ptr: var_100
    }; /* Record (int -> int) */
    let var_102 = lambda_6 /* Function Pointer (AD -> {tuple_1:AD,tuple_2:AD}) */;/* Value */
    let var_103 = lambda_7 /* Function Pointer ({tuple_1:AG,tuple_2:AH} -> AH) */;/* Value */
    let var_104 = 123;/* Value */
    let var_105 = {
        tuple_1: var_103,
        tuple_2: var_104
    }; /* Record {tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int} */
    let var_106 = 456;/* Value */
    let var_107 = lambda_6(var_106);/* ExpCall {tuple_1:int,tuple_2:int}*/
    let var_108 = var_105 /* ExpVar [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */;/* Value */
    let var_109 = (var_108).tuple_1; /* [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_110 = (var_109).env; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_111 = var_105 /* ExpVar [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */;/* Value */
    let var_112 = (var_111).tuple_1; /* [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_113 = (var_112).f_ptr; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_114 = var_113(var_110, var_107);/* ExpApp int */
    let var_115 = "\"abc\"";/* Value */
    let var_116 = lambda_6(var_115);/* ExpCall {tuple_1:string,tuple_2:string}*/
    let var_117 = var_105 /* ExpVar [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */;/* Value */
    let var_118 = (var_117).tuple_1; /* [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_119 = (var_118).env; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_120 = var_105 /* ExpVar [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */;/* Value */
    let var_121 = (var_120).tuple_1; /* [AH,AG.{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int}] */
    let var_122 = (var_121).f_ptr; /* ({tuple_1:AG,tuple_2:AH} -> AH) */
    let var_123 = var_122(var_119, var_116);/* ExpApp string */
    let var_124 = {
        tuple_1: var_114,
        tuple_2: var_123
    }; /* Record {tuple_1:int,tuple_2:string} */
    let var_125 = lambda_8 /* Function Pointer (int -> int) */;/* Value */
    let var_126 = var_39 /* ExpVar int */;/* Value */
    let var_127 = var_47 /* ExpVar string */;/* Value */
    let var_128 = var_50 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
    let var_129 = var_53 /* ExpVar {tuple_1:int,tuple_2:bool} */;/* Value */
    let var_130 = var_64 /* ExpVar int */;/* Value */
    let var_131 = var_75 /* ExpVar int */;/* Value */
    let var_132 = var_76 /* ExpVar (int -> string) */;/* Value */
    let var_133 = var_81 /* ExpVar {tuple_1:string,tuple_2:string} */;/* Value */
    let var_134 = var_125 /* ExpVar (int -> int) */;/* Value */
    let var_135 = var_82 /* ExpVar ((int -> int) -> (int -> int)) */;/* Value */
    let var_136 = var_94 /* ExpVar int */;/* Value */
    let var_137 = var_95 /* ExpVar (int -> int) */;/* Value */
    let var_138 = var_97 /* ExpVar int */;/* Value */
    let var_139 = var_102 /* ExpVar (AD -> {tuple_1:AD,tuple_2:AD}) */;/* Value */
    let var_140 = var_105 /* ExpVar {tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int} */;/* Value */
    let var_141 = var_124 /* ExpVar {tuple_1:int,tuple_2:string} */;/* Value */
    let var_142 = var_101 /* ExpVar (int -> int) */;/* Value */
    let var_143 = {
        a: var_126,
        b: var_127,
        c: var_128,
        c2: var_129,
        d: var_130,
        d2: var_131,
        e: var_132,
        f: var_133,
        fib: var_134,
        g: var_135,
        i: var_136,
        j: var_137,
        k: var_138,
        l: var_139,
        m: var_140,
        n: var_141,
        o: var_142
    }; /* Record {a:int,b:string,c:{tuple_1:int,tuple_2:bool},c2:{tuple_1:int,tuple_2:bool},d:int,d2:int,e:(int -> string),f:{tuple_1:string,tuple_2:string},fib:(int -> int),g:((int -> int) -> (int -> int)),i:int,j:(int -> int),k:int,l:(AD -> {tuple_1:AD,tuple_2:AD}),m:{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int},n:{tuple_1:int,tuple_2:string},o:(int -> int)} */
    return var_143; /* Return {a:int,b:string,c:{tuple_1:int,tuple_2:bool},c2:{tuple_1:int,tuple_2:bool},d:int,d2:int,e:(int -> string),f:{tuple_1:string,tuple_2:string},fib:(int -> int),g:((int -> int) -> (int -> int)),i:int,j:(int -> int),k:int,l:(AD -> {tuple_1:AD,tuple_2:AD}),m:{tuple_1:({tuple_1:AG,tuple_2:AH} -> AH),tuple_2:int},n:{tuple_1:int,tuple_2:string},o:(int -> int)} */
}
function lambda_0(var_7 /* x */) {
    let var_144 = var_7 /* ExpVar int */;/* Value */
    let var_145 = 0;/* Value */
    let var_146 = var_144 == var_145; /* ExpPrim bool*/

    let var_149;
    if (var_146) {
        let var_147 = "\"zero\"";/* Value */
        var_149 = var_147; /* Phi Node */
    } else {
        let var_148 = "\"nonzero\"";/* Value */
        var_149 = var_148; /* Phi Node */
    } return var_149; /* Return (int -> string) */
}
function lambda_1(var_10 /* h */) {
    let var_150 = var_10 /* ExpVar (int -> int) */;/* Value */
    let var_151 = { var_10: var_150 }; /* Record {var_10:(int -> int)} */
    let var_152 = lambda_2 /* Function Pointer (int -> int) */;/* Value */
    let var_153 = {
        env: var_151,
        f_ptr: var_152
    }; /* Record (int -> int) */
    return var_153; /* Return ((int -> int) -> (int -> int)) */
}
function lambda_2(var_11 /* x */, var_28 /* CC_ENV */) {
    let var_154 = var_28 /* ExpVar {var_10:(int -> int)} */;/* Value */
    let var_155 = (var_154).var_10; /* (int -> int) */
    let var_156 = var_11 /* ExpVar int */;/* Value */
    let var_157 = var_155 /* ExpVar (int -> int) */;/* Value */
    let var_158 = (var_157).env; /* (int -> int) */
    let var_159 = var_155 /* ExpVar (int -> int) */;/* Value */
    let var_160 = (var_159).f_ptr; /* (int -> int) */
    let var_161 = var_160(var_158, var_156);/* ExpApp int */
    let var_162 = 123;/* Value */
    let var_163 = var_155 /* ExpVar (int -> int) */;/* Value */
    let var_164 = (var_163).env; /* (int -> int) */
    let var_165 = var_155 /* ExpVar (int -> int) */;/* Value */
    let var_166 = (var_165).f_ptr; /* (int -> int) */
    let var_167 = var_166(var_164, var_162);/* ExpApp int */
    let var_168 = var_161 + var_167; /* ExpPrim int*/
    return var_168; /* Return (int -> int) */
}
function lambda_3(var_12 /* z */) {
    let var_169 = var_12 /* ExpVar int */;/* Value */
    let var_170 = 2;/* Value */
    let var_171 = var_169 * var_170; /* ExpPrim int*/
    return var_171; /* Return (int -> int) */
}
function lambda_4(var_15 /* x */) {
    let var_172 = var_15 /* ExpVar int */;/* Value */
    let var_173 = 0;/* Value */
    let var_174 = var_172 == var_173; /* ExpPrim bool*/

    let var_182;
    if (var_174) {
        let var_175 = 1;/* Value */
        var_182 = var_175; /* Phi Node */
    } else {
        let var_176 = var_15 /* ExpVar int */;/* Value */
        let var_177 = 1;/* Value */
        let var_178 = var_176 - var_177; /* ExpPrim int*/
        let var_179 = lambda_4(var_178);/* ExpCall int*/
        let var_180 = var_15 /* ExpVar int */;/* Value */
        let var_181 = var_179 * var_180; /* ExpPrim int*/
        var_182 = var_181; /* Phi Node */
    } return var_182; /* Return (int -> int) */
}
function lambda_5(var_17 /* x */, var_31 /* CC_ENV */) {
    let var_183 = var_31 /* ExpVar {var_16:int} */;/* Value */
    let var_184 = (var_183).var_16; /* int */
    let var_185 = var_184 /* ExpVar int */;/* Value */
    let var_186 = var_17 /* ExpVar int */;/* Value */
    let var_187 = var_185 + var_186; /* ExpPrim int*/
    return var_187; /* Return (int -> int) */
}
function lambda_6(var_20 /* x */) {
    let var_188 = var_20 /* ExpVar AD */;/* Value */
    let var_189 = var_20 /* ExpVar AD */;/* Value */
    let var_190 = {
        tuple_1: var_188,
        tuple_2: var_189
    }; /* Record {tuple_1:AD,tuple_2:AD} */
    return var_190; /* Return (AD -> {tuple_1:AD,tuple_2:AD}) */
}
function lambda_7(var_21 /* x */) {
    let var_191 = var_21 /* ExpVar {tuple_1:AG,tuple_2:AH} */;/* Value */
    let var_192 = (var_191).tuple_2; /* {tuple_1:AG,tuple_2:AH} */
    return var_192; /* Return ({tuple_1:AG,tuple_2:AH} -> AH) */
}
function lambda_8(var_25 /* x */) {
    let var_193 = var_25 /* ExpVar int */;/* Value */
    let var_194 = 1;/* Value */
    let var_195 = var_193 == var_194; /* ExpPrim bool*/

    let var_211;
    if (var_195) {
        let var_196 = 1;/* Value */
        var_211 = var_196; /* Phi Node */
    } else {
        let var_197 = var_25 /* ExpVar int */;/* Value */
        let var_198 = 2;/* Value */
        let var_199 = var_197 == var_198; /* ExpPrim bool*/

        let var_210;
        if (var_199) {
            let var_200 = 1;/* Value */
            var_210 = var_200; /* Phi Node */
        } else {
            let var_201 = var_25 /* ExpVar int */;/* Value */
            let var_202 = 1;/* Value */
            let var_203 = var_201 - var_202; /* ExpPrim int*/
            let var_204 = lambda_8(var_203);/* ExpCall int*/
            let var_205 = var_25 /* ExpVar int */;/* Value */
            let var_206 = 2;/* Value */
            let var_207 = var_205 - var_206; /* ExpPrim int*/
            let var_208 = lambda_8(var_207);/* ExpCall int*/
            let var_209 = var_204 + var_208; /* ExpPrim int*/
            var_210 = var_209; /* Phi Node */
        } var_211 = var_210; /* Phi Node */
    } return var_211; /* Return (int -> int) */
}
