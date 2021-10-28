use proc_macro::TokenStream;
use syn::{spanned::Spanned};
use quote::{quote};

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

    let generate_build_function = generate_build_function(fields, source_ident)?;
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

            #generate_build_function
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
    let types: Vec<_> = fields
        .iter()
        .map(|f| {
            // 针对是否为`Option`类型字段，产生不同的结果
            if let Some(inner_ty) = get_optional_inner_type(&f.ty) {
                quote!(std::option::Option<#inner_ty>)
            } else {
                let origin_ty = &f.ty;
                quote!(std::option::Option<#origin_ty>)
            }
        })
        .collect();
    Ok(quote! {
        // 下面这一行，也做了修改
        #(#idents: #types),*
    })
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
        let ty = get_optional_inner_type(&f.ty).unwrap_or(&f.ty);
        quote!{
            fn #ident(&mut self, #ident: #ty)->&mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            } 
        }
    }).collect();

    Ok(init_clauses)
}

fn generate_build_function(fields: &StructFields, source_ident: &syn::Ident) -> syn::Result<proc_macro2::TokenStream>{
    
    let judge_fields = fields.iter()
    .filter(|f| {
        get_optional_inner_type(&f.ty).is_none()
    })
    .map(|f| &f.ident)
    .map(|ident| {
        quote! {
            if self.#ident.is_none() {
                let err = format!("{} field missing", stringify!(#ident));
                return std::result::Result::Err(err.into())
            }
        }
    })
    .fold(proc_macro2::TokenStream::new(), |mut all, span| { all.extend(span); all});

    let fill_in_source = fields.iter()
    .map(|f| {
        let ident = &f.ident;
        if get_optional_inner_type(&f.ty).is_none() {
            quote! {
                #ident: self.#ident.take().unwrap(),
            }
        } else {
            quote! {
                #ident: self.#ident.take(),
            }
        }
    })
    .fold(proc_macro2::TokenStream::new(), |mut all, span| { all.extend(span); all});

    Ok(quote! {
        pub fn build(&mut self) -> std::result::Result<#source_ident, std::boxed::Box<dyn std::error::Error>> {
            #judge_fields

            std::result::Result::Ok(#source_ident{
                #fill_in_source
            })
        }
    })
}

fn get_optional_inner_type(ty: &syn::Type) -> Option<&syn::Type> {
    if let syn::Type::Path(syn::TypePath{ ref path,..}) = ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == "Option" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{
                    ref args, ..
                }) = seg.arguments {
                    if let Some(syn::GenericArgument::Type(ref inner_type)) = args.first() {
                        return Some(inner_type);
                    }
                }
            }
        }
    }
    None
}