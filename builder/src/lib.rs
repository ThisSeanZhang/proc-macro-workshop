use proc_macro::TokenStream;
use syn::{self, DeriveInput, Fields, spanned::Spanned};
use quote::{ToTokens, quote};

 // 注意，这里和第一篇文章里的 #[proc_macro_attribute]不同
 #[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let st = syn::parse_macro_input!(input as syn::DeriveInput);
    match do_expand(&st) {
        Ok(token_stream) => token_stream.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    
    let source_ident = &st.ident;
    let builder_ident = syn::Ident::new(&format!("{}Builder", source_ident.to_string()), st.span());

    let fields = get_fields_from_derive_input(st)?;
    let builder_fields = generate_builder_struct_fields_def(fields)?;

    let builder_fields_default = generate_builder_struct_factory_init_clauses(fields)?;
    let builder_setter_clauses = builder_setter_clauses(fields)?;
    let ret = quote! {
        pub struct #builder_ident {
            #builder_fields
        }   
        impl #source_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#builder_fields_default),*
                }
            }
        }
        impl #builder_ident {
            #(#builder_setter_clauses)*
        }
    };

    Ok(ret)
}
type StructFields = syn::punctuated::Punctuated<syn::Field,syn::Token!(,)>;
fn get_fields_from_derive_input(input: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed {
            ref named,
            ..
        }),
        ..
    }) = input.data {
        return Ok(named);
    }
    Err(syn::Error::new_spanned(input, "Must define on a Struct, not Enum".to_string()))
}
fn generate_builder_struct_fields_def(fields: &StructFields) -> syn::Result<proc_macro2::TokenStream>{
    let idents: Vec<_> = fields.iter().map(|f| &f.ident ).collect();
    let types: Vec<_> = fields.iter().map(|f| &f.ty ).collect();

    let attributes = quote! {
        #(#idents: std::option::Option<#types>),*
    };

    Ok(attributes)
}

fn generate_builder_struct_factory_init_clauses(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>>{
    let init_clauses: Vec<_> = fields.iter().map(|f| {
        let ident = &f.ident;
        quote!{
            #ident: std::option::Option::None
        }
    }).collect();

    Ok(init_clauses)
}

fn builder_setter_clauses(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>>{
    let init_clauses: Vec<_> = fields.iter().map(|f| {
        let ident = &f.ident;
        let ty = &f.ty;
        quote!{
            fn #ident(&mut self, #ident: #ty)->&mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            } 
        }
    }).collect();

    Ok(init_clauses)
}