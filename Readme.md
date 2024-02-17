# Toy ML like language implementation using Rust

## Features

* polymorphic type inference

## compile Phases

* Tokenize
* Parse
* (Not needed) Elaboration
* Type Inference
* Alpha Conversion
* (Optional) Pritty Printer Gen(not yet)(planned)
* Closure Conversion(App for real closure )(call for non real closure)
* ANormalize (not yet)(planed)
* Pointer Operations (not yet)(planed)
* Machine CodeGen (not yet)(planed)
* Wasm LLVM Gen (not yet )(planed)

## See also

* コンパイラ ー 原理と構造 大堀　淳（2021年9月）共立出版  ISBN 9784320124783
* [CoreML implementation in SML#][CoreMLReferenceImpl]
* [SML# project][SMLSharp]
* [MinCaml][MinCaml]
* [My CoreML to Wasm Compiler][CoreMLWasm]
  
[CoreMLReferenceImpl]: https://github.com/AtsushiOhori/compiler-text
[SMLSharp]: https://github.com/smlsharp/smlsharp
[MinCaml]: https://github.com/esumii/min-caml
[CoreMLWasm]: https://github.com/KentaTheBugMaker/compiler-text
