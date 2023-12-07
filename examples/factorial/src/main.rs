fn main() {
    let fact = "
    fun fact n = 
     if eq(n,0)then
        1 
     else
        if eq(n,1) then 
            1 
        else
            mul(
                (fact
                    (sub
                        (n,1)
                    )
                ),
                n
            )
    val it = fact 5 
    val it = add(it, 10)
    ";
    coreml_rustc::top::compile(
        coreml_rustc::top::StopAt::ClosureConversion,
        coreml_rustc::top::Control {
            print_syntax: true,
            print_typeinf: true,
            print_alpha_conversion: true,
            print_closure_conversion: true,
        },
        fact.to_owned(),
        "interactive".to_owned(),
    );
}
