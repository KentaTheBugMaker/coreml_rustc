use coreml_rustc::top::Control;

fn main() {
    let mut source_code_buffer = String::new();
    let control = std::env::args().into_iter().fold(
        Control {
            print_syntax: false,
            print_typeinf: false,
            print_alpha_conversion: false,
            print_closure_conversion: false,
            print_knormalize: false,
            remove_dead_code: true,
        },
        |mut control, value| {
            if let Some((k, v)) = value.split_once('=') {
                let yes = match v {
                    "yes" => true,
                    "no" => false,
                    _ => false,
                };
                match k {
                    "-dprintTypeInf" => control.print_typeinf = yes,
                    "-dprintKNormal" => control.print_knormalize = yes,
                    "-dprintAlphaConversion" => control.print_alpha_conversion = yes,
                    "-dprintSyntax" => control.print_syntax = yes,
                    _ => (),
                };
                control
            } else {
                control
            }
        },
    );
    println!("{control:#?}");
    loop {
        let mut buffer = source_code_buffer.clone();
        if let Ok(_) = std::io::stdin().read_line(&mut buffer) {
            let success = coreml_rustc::top::compile(
                coreml_rustc::top::StopAt::KNormalize,
                control,
                buffer.clone(),
                "(Interactive)".to_owned(),
            );
            if success {
                source_code_buffer += &buffer;
            }
        } else {
            break;
        }
    }
}
