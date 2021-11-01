use proc_macro::TokenStream;
use syn::{self, Attribute, spanned::Spanned};
use quote::{ToTokens, quote};


#[proc_macro_derive(CustomDebug ,attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
// type StructFields = syn::punctuated::Punctuated<syn::Field,syn::Token!(,)>;
struct CusFieldStruct<'a>{
    ident: &'a syn::Ident,
    ident_name: String,
    format: Option<String>,
}
impl<'a> CusFieldStruct<'a> {
    fn new(field: &'a syn::Field) -> CusFieldStruct<'a> {
        let ident = field.ident.as_ref().unwrap();
        CusFieldStruct{
            ident,
            ident_name: ident.to_string(),
            format: get_custom_format_of_field(field)
        }
    }
}
fn get_custom_format_of_field(field: &syn::Field) -> Option<String> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::NameValue(syn::MetaNameValue {
            ref path,
            ref lit,
            ..
        })) = attr.parse_meta()
        {
            if path.is_ident("debug") {
                if let syn::Lit::Str(ref ident_str) =lit {
                    return Some(
                        ident_str.value()
                    );
                }
            }
        }
    }
    None
}
fn read_cus_field(d: &syn::DeriveInput) -> syn::Result<Vec<CusFieldStruct>> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = d.data{
        return Ok(named.iter().map(CusFieldStruct::new).collect())
    }
    Err(syn::Error::new_spanned(d, "Must define on a Struct, not Enum".to_string()))
}
fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let fields = read_cus_field(st)?;
    let struct_name_ident = &st.ident;
    let struct_name_literal = struct_name_ident.to_string();

    let mut fmt_body_stream = proc_macro2::TokenStream::new();

    fmt_body_stream.extend(quote!{
        fmt.debug_struct(#struct_name_literal)
    });

    for field in fields {
        let ident = field.ident;
        let ident_name = field.ident_name;
        let value = if let Some(format) = field.format {
            quote!(&format_args!(#format, &self.#ident))
        } else {
            quote!(&self.#ident)
        };
        fmt_body_stream.extend(quote!{
            .field(#ident_name, #value)
        });
        
    }
    fmt_body_stream.extend(quote!(
        .finish()
    ));
    return Ok(quote!(
        impl std::fmt::Debug for #struct_name_ident {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                #fmt_body_stream
            }
        }
    ));
}