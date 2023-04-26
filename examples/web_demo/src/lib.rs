use wasm_bindgen::prelude::*;
#[wasm_bindgen(start)]
fn main() ->Result<(),JsValue>{
    // Use `web_sys`'s global `window` function to get a handle on the global
    // window object.


    Ok(())
}

#[wasm_bindgen]
pub fn transpile()->Result<String,JsValue>{
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let code_area:web_sys::HtmlTextAreaElement = document.get_element_by_id("code").expect("no code pain").dyn_into::<web_sys::HtmlTextAreaElement>().unwrap();
    

}