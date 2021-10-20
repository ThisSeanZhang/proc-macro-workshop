use quote::quote;
use syn::{parse_macro_input, spanned::Spanned};

type StructFields = syn::punctuated::Punctuated<syn::Field,syn::Token!(,)>;
#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // let input = parse_macro_input!(input as DeriveInput);
    // let source_struct_name = input.ident.to_string();
    // let source_builder = Ident::new(&format!("{}Builder", source_struct_name), input.span());
    
    // let expanded = quote! {
    //     struct #source_builder {
    //     }

    //     impl #source_struct_name {
    //         pub builder () -> #source_builder {
                
    //         }
    //     }
    // };

    // // Hand the output tokens back to the compiler
    // TokenStream::from(expanded)
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    
    let source_ident = &st.ident;
    eprintln!("{:#?}", st.data);
    let builder_ident = syn::Ident::new(&format!("{}Builder", source_ident), st.span());
    let ret = quote! {     
        pub struct #builder_ident {
            
        }    
        impl #source_ident {
            pub fn builder() -> #builder_ident {
                
            }
        }
    };                     

    return Ok(ret);
}


fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = d.data{
        return Ok(named)
    }
    Err(syn::Error::new_spanned(d, "Must define on a Struct, not Enum".to_string()))
}