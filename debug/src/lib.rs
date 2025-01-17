use std::collections::HashMap;

use proc_macro::TokenStream;
use syn::{self, Attribute, parse_quote, spanned::Spanned};
use quote::{ToTokens, quote};
use syn::visit::{self, Visit};

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
                if let syn::Lit::Str(ref ident_str) = lit {
                    return Some( ident_str.value());
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
fn generate_debug_trait_core(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    
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
    Ok(fmt_body_stream)
}
fn get_phantomdata_generic_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath{path: syn::Path{ref segments, ..}, ..}) = field.ty {
        if let Some(syn::PathSegment{ref ident, ref arguments}) = segments.last() {
            if ident == "PhantomData" {
                if let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments{args, ..}) = arguments {
                    if let Some(syn::GenericArgument::Type(syn::Type::Path( ref gp))) = args.first() {
                        if let Some(generic_ident) = gp.path.segments.first() {
                            return Ok(Some(generic_ident.ident.to_string()))
                        }
                    }
                }
            }
        }
    }
    return Ok(None)
}
fn get_field_type_name(field: &syn::Field) -> syn::Result<Option<String>> {
    if let syn::Type::Path(syn::TypePath{path: syn::Path{ref segments, ..}, ..}) = field.ty {
        if let Some(syn::PathSegment{ref ident,..}) = segments.last() {
            return Ok(Some(ident.to_string()))
        }
    }
    return Ok(None)
}
type StructFields = syn::punctuated::Punctuated<syn::Field,syn::Token!(,)>;
fn get_fields_from_derive_input(d: &syn::DeriveInput) -> syn::Result<&StructFields> {
    if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = d.data{
        return Ok(named)
    }
    Err(syn::Error::new_spanned(d, "Must define on a Struct, not Enum".to_string()))
}
// 定义一个用于实现`Visit` Trait的结构体，结构体中定义了一些字段，用于存储筛选条件以及筛选结果
struct TypePathVisitor {
    generic_type_names: Vec<String>,  // 这个是筛选条件，里面记录了所有的泛型参数的名字，例如`T`,`U`等
    associated_types: HashMap<String, Vec<syn::TypePath>>,  // 这里记录了所有满足条件的语法树节点
}

impl<'ast> Visit<'ast> for TypePathVisitor {
    // visit_type_path 这个回调函数就是我们所关心的
    fn visit_type_path(&mut self, node: &'ast syn::TypePath) {
        
        if node.path.segments.len() >= 2 {
            let generic_type_name = node.path.segments[0].ident.to_string();
            if self.generic_type_names.contains(&generic_type_name) {
                // 如果满足上面的两个筛选条件，那么就把结果存起来
                self.associated_types.entry(generic_type_name).or_insert(Vec::new()).push(node.clone());
            }
        }
        // Visit 模式要求在当前节点访问完成后，继续调用默认实现的visit方法，从而遍历到所有的
        // 必须调用这个函数，否则遍历到这个节点就不再往更深层走了
        visit::visit_type_path(self, node);
    }
}
fn get_generic_associated_types(st: &syn::DeriveInput) -> HashMap<String, Vec<syn::TypePath>> {
    // 首先构建筛选条件
    let origin_generic_param_names: Vec<String> = st.generics.params.iter().filter_map(|f| {
        if let syn::GenericParam::Type(ty) = f {
            return Some(ty.ident.to_string())
        }
        return None
    }).collect();

    
    let mut visitor = TypePathVisitor {
        generic_type_names: origin_generic_param_names,  // 用筛选条件初始化Visitor
        associated_types: HashMap::new(),
    };

    // 以st语法树节点为起点，开始Visit整个st节点的子节点
    visitor.visit_derive_input(st);
    return visitor.associated_types;
}
fn do_expand(st: &syn::DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let struct_name_ident = &st.ident;
    let fmt_body_stream = generate_debug_trait_core(st)?;

    // 以下代码构建两个列表，一个是`PhantomData`中使用到的泛型参数，另一个是输入结构体中所有字段的类型名称
    let fields = get_fields_from_derive_input(st)?;
    let mut field_type_names = Vec::new();
    let mut phantomdata_type_param_names = Vec::new();
    for field in fields{
        if let Some(s) = get_field_type_name(field)? {
            field_type_names.push(s);
        }
        if let Some(s) = get_phantomdata_generic_type_name(field)? {
            phantomdata_type_param_names.push(s);
        }
    }
    // 下面这一行是第七关新加的，调用函数找到关联类型信息
    let associated_types_map = get_generic_associated_types(st);
    // 从输入的派生宏语法树节点获取被修饰的输入结构体的泛型信息
    let mut generics_param_to_modify = st.generics.clone();
    // 我们需要对每一个泛型参数都添加一个`Debug` Trait限定
    for g in generics_param_to_modify.params.iter_mut() {
        if let syn::GenericParam::Type(t) = g {
            let type_param_name = t.ident.to_string();
            if phantomdata_type_param_names.contains(&type_param_name) && !field_type_names.contains(&type_param_name) {
                continue;
            }

            // 下面这3行是本次新加的，如果是关联类型，就不要对泛型参数`T`本身再添加约束了,除非`T`本身也被直接使用了
            if associated_types_map.contains_key(&type_param_name) && !field_type_names.contains(&type_param_name){
                continue
            }

            t.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }

    generics_param_to_modify.make_where_clause();
    for (_, associated_types) in associated_types_map {
        for associated_type in associated_types {
            generics_param_to_modify.where_clause.as_mut().unwrap().predicates.push(parse_quote!(#associated_type:std::fmt::Debug));
        }
    }

    // 使用工具函数把泛型抽取成3个片段
    let (impl_generics, type_generics, where_clause) = generics_param_to_modify.split_for_impl();
    
    return Ok(quote!(
        impl #impl_generics std::fmt::Debug for #struct_name_ident #type_generics #where_clause {
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                #fmt_body_stream
            }
        }
    ));
}