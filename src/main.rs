mod parser;
mod syntax_tree;
use nom_recursive::RecursiveInfo;
use parser::Span;
fn main() {
    let mini_program = "val x = fn x =>#1 (123,true)";
    println!(
        "{:#?}",
        parser::parse(Span::new_extra(mini_program, RecursiveInfo::new()))
    )
}
