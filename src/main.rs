mod flat_syntax;
mod parser;
mod syntax_tree;
mod typeinf;
use nom_recursive::RecursiveInfo;
use parser::Span;

use crate::syntax_tree::Top;
fn main() {
    let mini_program = "val x = fn x => #1(x,\"Hello CoreML\" )";
    let syn = parser::parse(Span::new_extra(mini_program, RecursiveInfo::new()));
    println!("{:#?}", syn);
    let Top::Dec(dec) = syn.unwrap().1;

    let flat_syn: flat_syntax::Dec = dec.try_into().unwrap();
    println!("{:#?}", flat_syn);
    let tyenv =
        crate::typeinf::type_inf(&crate::typeinf::TypeEnvironment::new(), flat_syn).unwrap();
    println!("{:#?}", tyenv)
}
