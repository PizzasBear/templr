use quote::quote;
use templr_parser::NodeBody;

#[proc_macro]
pub fn templ(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let body = syn::parse_macro_input!(tokens as NodeBody);

    dbg!(&body);

    quote! {()}.into()
}
