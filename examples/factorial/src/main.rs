mod generated_source;
fn main() {
    let test_suite = "
    val a = mul(3, add(2, 1))
val b = if eq(3, add(2, 1)) then \"OK\" else \"NG\"
val c = (123, false)
val c2 = (123, true)
val d = if #2 c then mul(#1 c, 2) else div(#1 c, 2)
val d2 = if #2 c2 then mul(#1 c2, 2) else div(#1 c2, 2)
fun e x = if eq(x, 0) then \"zero\" else \"nonzero\"
val f = (e 0, e 123)
fun g h = fn x => add(h x, h 123)
val i = g (fn z => mul(z, 2)) 456
fun j x = if eq(x, 0) then 1 else mul(j sub(x, 1), x)
val k = j 3
fun l x = (x, x)
val m = (fn x => #2 x, 123)
val n = (#1 m (l 456), #1 m (l \"abc\"))
fun fib x = if eq(x,1 )then 1 else if eq(x,2)then 1 else add(fib sub(x,1),fib sub(x,2))";
    let minimal_test = "
    fun g h = fn x => add(h x, h 123)
val i = g (fn z => mul(z, 2)) 456
    ";

    coreml_rustc::top::compile(
        coreml_rustc::top::StopAt::KNormalize,
        coreml_rustc::top::Control {
            print_syntax: true,
            print_typeinf: true,
            print_alpha_conversion: true,
            print_closure_conversion: true,
            print_knormalize: true,
        },
        minimal_test.to_owned(),
        "test_suite".to_owned(),
    );
}
