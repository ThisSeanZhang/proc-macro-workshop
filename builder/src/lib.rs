use proc_macro::TokenStream;
use syn::{spanned::Spanned};
use quote::{quote};

 // 注意，这里和第一篇文章里的 #[proc_macro_attribute]不同
 #[proc_macro_derive(Builder,attributes(builder))]
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
    let types: syn::Result<Vec<proc_macro2::TokenStream>> = fields
        .iter()
        .map(|f| {
            // 针对是否为`Option`类型字段，产生不同的结果
            if let Some(inner_ty) = get_optional_inner_type(&f.ty,"Option") {
                Ok(quote!(std::option::Option<#inner_ty>))
            } else if get_user_specified_ident_for_vec(f)?.is_some() {
                let origin_ty = &f.ty;
                Ok(quote!(#origin_ty))  // 题目中设定，如果用户指定了each属性，我们就可以认为它一定是作用在一个Vec字段上

            } else {
                let origin_ty = &f.ty;
                Ok(quote!(std::option::Option<#origin_ty>))
            }
        })
        .collect();
    let types = types?;
    let token_stream = quote! {
        #(#idents: #types),*
    };
    Ok(token_stream)
}

fn generate_builder_struct_factory_init_clauses(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>>{
    let init_clauses: syn::Result<Vec<proc_macro2::TokenStream>> = fields.iter().map(|f| {
        let ident = &f.ident;
        if get_user_specified_ident_for_vec(f)?.is_some() {
           Ok(quote!{
                #ident: std::vec::Vec::new()
            })
        } else {
            Ok(quote!{
                #ident: std::option::Option::None
            })
        }
    }).collect();

    Ok(init_clauses?)
}

fn builder_setter_clauses(fields: &StructFields) -> syn::Result<Vec<proc_macro2::TokenStream>>{
    let init_clauses: syn::Result<Vec<proc_macro2::TokenStream>> = fields.iter().map(|f| {
        let ident = &f.ident;
        let type_ = &f.ty;
        let specified_ident_for_vec = get_user_specified_ident_for_vec(f)?;
        if let Some(ref user_specified_ident) = specified_ident_for_vec {
            let inner_ty = get_optional_inner_type(type_,"Vec").ok_or(syn::Error::new(f.span(),"each field must be specified with Vec field")).unwrap();
            let other_setter = if user_specified_ident != ident.as_ref().unwrap() {
                quote! {
                    fn #ident(&mut self, #ident: #type_) -> &mut Self {
                        self.#ident = #ident;
                        self
                    }
                }
            } else {
                quote!()
            };
            return Ok(quote! {
                fn #user_specified_ident(&mut self, #user_specified_ident: #inner_ty) -> &mut Self {
                    self.#ident.push(#user_specified_ident);
                    self
                }

                #other_setter
            });
        }
        let ty = get_optional_inner_type(&f.ty, "Option").unwrap_or(&f.ty);
        return Ok(quote!{
            fn #ident(&mut self, #ident: #ty)->&mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            } 
        });
    }).collect();

    Ok(init_clauses?)
}

fn generate_build_function(fields: &StructFields, source_ident: &syn::Ident) -> syn::Result<proc_macro2::TokenStream>{
    
    let judge_fields = fields.iter()
    .filter(|f| {
        get_optional_inner_type(&f.ty, "Option").is_none()
    })
    .filter(|f| {
        if let Ok(inner) = get_user_specified_ident_for_vec(&f) {
            inner.is_none()
        } else {
            false
        }
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
        if let Ok(Some(_)) = get_user_specified_ident_for_vec(f) {
            quote! {
                #ident: self.#ident.drain(..).collect(),
            }
        } else if get_optional_inner_type(&f.ty, "Option").is_none() {
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

fn get_optional_inner_type<'a>(ty: &'a syn::Type, outer_ident_name: &'a str) -> Option<&'a syn::Type> {
    if let syn::Type::Path(syn::TypePath{ ref path,..}) = ty {
        if let Some(seg) = path.segments.last() {
            if seg.ident == outer_ident_name  {
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

fn get_user_specified_ident_for_vec(field: &syn::Field) -> syn::Result<Option<syn::Ident>> {
    for attr in &field.attrs {
        if let Ok(syn::Meta::List(syn::MetaList {
            ref path,
            ref nested,
            ..
        })) = attr.parse_meta() {
            if let Some(p) = path.segments.first() {
                if p.ident == "builder" {
                    if let Some(syn::NestedMeta::Meta(syn::Meta::NameValue(kv))) = nested.first() {
                        if kv.path.is_ident("each") {
                            if let syn::Lit::Str(ref ident_str) = kv.lit {
                                return Ok(Some(syn::Ident::new(
                                    ident_str.value().as_str(),
                                    attr.span(),
                                )));
                            }
                        } else {
                            // 第八关加入，注意这里new_spanned函数的参数，我们需要在语法树中找到一个合适的节点来获取它的span，如果这个语法树节点找的不对，产生出的错误信息就会不一样
                            if let Ok(syn::Meta::List(ref list)) = attr.parse_meta() {
                                return Err(syn::Error::new_spanned(list, r#"expected `builder(each = "...")`"#))
                            }
                        }
                    } 
                }
            }
        }
    }
    Ok(None)
}