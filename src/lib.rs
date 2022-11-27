/*
  Renum Proc Macro
    - derives From or TryFrom impls for an enum with integer discriminant values
    -

 */

// #![allow(warnings)]

use proc_macro2::{Ident};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use std::str::FromStr;
use syn::Attribute;
use syn::parse::Parse;
use syn::parse::ParseStream;
use syn::parse_macro_input;
use syn::{Data, DeriveInput};

const ATTR_RENUM: &str = "renum";
const ATTR_REPR: &str = "repr";

const IDENT_ALLOW_PANIC: &str = "allow_panic";
const IDENT_DEFAULT: &str = "default";
const IDENT_ERR: &str = "Err";
const IDENT_ERROR: &str = "Error";
const IDENT_ERROR_FROM: &str = "ErrorFrom";
const IDENT_FROM: &str = "From";
const IDENT_NORMALLY_ERR: &str = "normally_err";
const IDENT_NORMALLY_OK: &str = "normally_ok";
const IDENT_OK: &str = "Ok";
const IDENT_TRY_FROM: &str = "TryFrom";
const IDENT_TYPE_ENUM: &str = "ENUM";
const IDENT_TYPE_REPR: &str = "REPR";

// helper to create idents
macro_rules! ident {
  ($ident:ident) => { Ident::new(stringify!($ident), proc_macro2::Span::call_site()) };
  ($literal:literal) => ( Ident::new($literal, proc_macro2::Span::call_site()) )
}

// integer types used for reprs
trait Repr: std::marker::Copy + std::cmp::Ord + std::fmt::Debug + std::fmt::Display
    + std::str::FromStr + std::ops::Sub<Output = Self> + std::ops::SubAssign<Self> + quote::ToTokens
  where <Self as std::str::FromStr>::Err: std::fmt::Display
{
  const ZERO: Self;
  const ONE: Self;
  const MIN: Self;
  const MAX: Self;
  const BITS: u32;
}

macro_rules! impl_repr {
  {$($type:ty),+} => {
    $(
      impl Repr for $type {
        const ZERO: Self = 0;
        const ONE: Self = 1;
        const MIN: Self = <$type>::MIN;
        const MAX: Self = <$type>::MAX;
        const BITS: u32 = <$type>::BITS;
      }
    )+
  };
}

impl_repr!{ usize, u8, u16, u32, u64, u128, isize, i8, i16, i32, i64, i128 }

#[inline]
fn find_attr_by_path_str<'a>(attrs: &'a Vec::<Attribute>, path_str: &str) -> Option::<&'a Attribute> {

  if let Ok(path) = syn::parse_str::<syn::Path>(path_str) {
    return attrs.iter().find(|attr| attr.path == path )
  }

  None
}

#[derive(Debug)]
struct Sequence<T>(Vec::<T>);

impl<T> IntoIterator for Sequence<T> {
  type Item = T;
  type IntoIter = <Vec<T> as IntoIterator>::IntoIter;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}

impl<T: Parse> Parse for Sequence<T> {
  fn parse(input: ParseStream) -> syn::Result<Self> {

    let mut items = Vec::<T>::new();

    loop {
      if input.is_empty() { break; }

      if input.peek(syn::token::Comma) {
        syn::token::Comma::parse(input)?;
      } else {
        items.push(T::parse(input)?);
      }
    }

    Ok(Sequence(items))
  }
}

impl<T> Sequence<T>
  where T: Parse
{
  fn parse_bracketed(input: ParseStream) -> syn::Result<Self> {
    let inner; syn::bracketed!(inner in input);
    return Self::parse(&inner)
  }
}

#[derive(Debug)]
#[derive(PartialEq)]
enum ErrorType {
  Enum,
  Repr,
  Other(syn::TypePath)
}

#[derive(Debug)]
#[derive(PartialEq)]
enum ErrorFrom {
  Enum,
  Repr
}

impl Parse for ErrorFrom {
  fn parse(input: ParseStream) -> syn::Result<Self> {

    let ident = syn::Ident::parse(input)?;

    match ident.to_string().as_str() {
      IDENT_TYPE_REPR => Ok(ErrorFrom::Repr),
      IDENT_TYPE_ENUM => Ok(ErrorFrom::Enum),
      _ => Err(input.error("invalid ErrorFrom argument"))
    }
  }
}

#[derive(Debug)]
enum ConfigOption {
  From,
  TryFrom,
  AllowPanic,
  NormallyOk,
  NormallyErr,
  ErrorType(ErrorType),
  ErrorFrom(Vec::<ErrorFrom>),
  DefaultVariant(syn::Ident)
}

impl Parse for ConfigOption {
  fn parse(input: ParseStream) -> syn::Result<Self> {

    if input.peek2(syn::token::Eq) {

      let ident = syn::Ident::parse(input)?;
      let _eq = syn::token::Eq::parse(input)?;

      return match ident.to_string().as_str() {
        IDENT_ERROR => {
          let error = syn::TypePath::parse(input)?;

          if let Some(tag) = error.path.segments.first() {
            if tag.ident == IDENT_TYPE_REPR {
              return Ok(ConfigOption::ErrorType(ErrorType::Repr));
            } else if tag.ident == IDENT_TYPE_ENUM {
              return Ok(ConfigOption::ErrorType(ErrorType::Enum));
            }
            // this should likely be an error
          }

          Ok(ConfigOption::ErrorType(ErrorType::Other(error)))
        },
        IDENT_ERROR_FROM => {

          let args = if input.peek(syn::token::Paren) {
            let inner; syn::parenthesized!(inner in input);
            Sequence::<ErrorFrom>::parse(&inner)?.0
          } else {
            vec![ErrorFrom::parse(input)?]
          };

          Ok(ConfigOption::ErrorFrom(args))
        }
        IDENT_DEFAULT => Ok(ConfigOption::DefaultVariant(syn::Ident::parse(input)?)),
        _ => Err(input.error("invalid renum config assignment"))
      }
    } else {
      let ident = syn::Ident::parse(input)?;

      return match ident.to_string().as_str() {
        IDENT_FROM => Ok(ConfigOption::From),
        IDENT_TRY_FROM => Ok(ConfigOption::TryFrom),
        IDENT_ALLOW_PANIC => Ok(ConfigOption::AllowPanic),
        IDENT_NORMALLY_OK => Ok(ConfigOption::NormallyOk),
        IDENT_NORMALLY_ERR => Ok(ConfigOption::NormallyErr),
        _ => Err(input.error(format!("Unexpected Config Option: {}", ident)))
      }
    }

    // Err(input.error(format!("Unexpected Token: {:?}", input)))
  }
}

#[derive(Debug)]
enum VariantOption<R> {
  Default,
  Ok,
  Err,
  Values(Vec::<VariantValue<R>>)
}

impl<R> Parse for VariantOption<R>
  where R: Repr, <R as std::str::FromStr>::Err: std::fmt::Display
{
  fn parse(input: ParseStream) -> syn::Result<Self> {

    if input.peek2(syn::token::Eq) {

      let ident = syn::Ident::parse(input)?;
      let _eq = syn::token::Eq::parse(input)?;

      // let _expr = syn::Expr::parse(input)?;
      if ident == "values" {

        let values = if input.peek(syn::token::Bracket) {
          Sequence::<VariantValue::<R>>::parse_bracketed(input)?.0
        } else {
          vec![VariantValue::<R>::parse(input)?]
        };

        return Ok(VariantOption::Values(values));
      }
    } else {
      let ident = syn::Ident::parse(input)?;

      return match ident.to_string().as_str() {
        IDENT_DEFAULT => Ok(VariantOption::Default),
        IDENT_OK      => Ok(VariantOption::Ok),
        IDENT_ERR     => Ok(VariantOption::Err),
        _         => Err(input.error(format!("Unexpected VariantOption: {}", ident)))
      }
    }

    Err(input.error(format!("Unexpected Token: {}", input)))
  }
}

#[derive(Debug, Clone, PartialEq)]
enum VariantValue<R> {
  Exact(R),
  Range(R, R) // (LO, HI)
}

impl<R> VariantValue<R>
  where R: quote::ToTokens
{
  fn quote(&self) -> TokenStream2 {
    match self {
      VariantValue::Exact(value) => quote!{#value},
      VariantValue::Range(lo, hi) => quote!{#lo..=#hi}
    }
  }
 }

impl<R> Parse for VariantValue<R>
  where R: Repr, <R as std::str::FromStr>::Err: std::fmt::Display
{

  fn parse(input: ParseStream) -> syn::Result<Self> {

    if input.peek(syn::token::Paren) {
      // recurse inside paren
      let inner; syn::parenthesized!(inner in input);
      return Self::parse(&inner);
    } else if /* if the next token is a minus then we have a negative value  */
      input.peek(syn::token::Dot2) // ..X, ..=X
      || input.peek2(syn::token::Dot2) // X..Y, X..=Y
      || (input.peek(syn::token::Sub) && input.peek3(syn::token::Dot2)) // -X..Y, -X..=Y, -X..-Y, -X..=-Y
    {
      // ..HI, ..=HI, LO.., LO..HI, LO..=HI
      let range = syn::ExprRange::parse(input)?;

      let mut lo = R::MIN;
      let mut hi = R::MAX;

      if let Some(expr) = range.from {
        if let syn::Expr::Lit(expr_lit) = *expr {
          if let syn::Lit::Int(lit_int) = expr_lit.lit {
            lo = lit_int.base10_parse::<R>()?;
          }
        }
      }

      if let Some(expr) = range.to {
        if let syn::Expr::Lit(expr_lit) = *expr {
          if let syn::Lit::Int(lit_int) = expr_lit.lit {
            hi = lit_int.base10_parse::<R>()?;
          }
        }
      }

      // correct bounds for range exclusive
      if let syn::RangeLimits::HalfOpen(_) = range.limits {
        if hi == R::MIN {
          panic!("Range ({}..{}) upper bound must be greater than Repr::MIN ({})", lo, hi, R::MIN);
        }
        hi -= R::ONE;
      }

      // maybe handle this?
      if hi > lo {}

      if lo == hi { return Ok(Self::Exact(lo)); }

      return Ok(Self::Range(lo, hi));
    } else if input.peek(syn::LitInt) {
      let value = syn::LitInt::parse(input)?;
      return Ok(Self::Exact(value.base10_parse::<R>()?));
    }

    Err(input.error("Invalid VariantValue"))
  }
}

#[derive(Debug)]
struct RenumVariant<R>
  where R: Repr, <R as std::str::FromStr>::Err: std::fmt::Display
{
  ident: syn::Ident,
  discriminant: Option::<R>,
  values: Option::<Vec::<VariantValue<R>>>,
  is_ok: Option::<bool>,
  is_default: bool
}

impl<R: Repr> RenumVariant<R>
  where <R as FromStr>::Err: std::fmt::Display
{

  // fn overlaps(pair: &[Self]) -> bool {
  //   todo!();
  //   if let [a, b] = pair {}
  //   false
  // }

}

impl<R> TryFrom::<&syn::Variant> for RenumVariant<R>
  where R: Repr, <R as std::str::FromStr>::Err: std::fmt::Display
{

  type Error = syn::Error;

  fn try_from(variant: &syn::Variant) -> Result<RenumVariant<R>, syn::Error> {

    let mut values: Vec::<VariantValue::<R>> = Vec::new();
    let mut discriminant = None;


    if let Some((_, syn::Expr::Lit(expr_lit))) = &variant.discriminant {
      if let syn::Lit::Int(lit_int) = &expr_lit.lit {
        discriminant = Some(lit_int.base10_parse::<R>()?);
      }
    }

    /* parse and map variant options */
    let mut is_default: bool = false;
    let mut is_ok: Option<bool> = None;

    if let Some(attr) = find_attr_by_path_str(&variant.attrs, ATTR_RENUM) {
      for option in attr.parse_args::<Sequence::<VariantOption<R>>>()?.into_iter() {
        match option {
          VariantOption::Default => is_default = true,
          VariantOption::Ok => is_ok = Some(true),
          VariantOption::Err => is_ok = Some(false),
          VariantOption::Values(option_values) => values.extend_from_slice(&option_values),
        }
      }
    }

    values.dedup();

    // todo - range merging - for now i will assume that I will pass optimal ranges

    // if discriminant is in values remove it
    if let Some(discriminant) = discriminant {
      let discriminant = VariantValue::Exact(discriminant);
      values.retain(|value| value != &discriminant);
    }

    let values = if values.is_empty() { None } else { Some(values) };

    Ok(RenumVariant {
      ident: variant.ident.clone(),
      discriminant,
      values,
      is_ok,
      is_default
    })

  }
}

#[derive(Debug)]
struct RenumConfig {
  repr: syn::Ident,
  name: syn::Ident,
  try_from: bool,
  allow_panic: bool,
  error_type: ErrorType,
  error_from: Option::<Vec::<ErrorFrom>>,
  normally_ok: bool,
}

impl Default for RenumConfig {
  fn default() -> Self {
    RenumConfig {
      repr: ident!(C),
      name: ident!(NAME),
      try_from: false,
      allow_panic: false,
      error_type: ErrorType::Enum,
      error_from: None,
      normally_ok: true
    }
  }
}

impl TryFrom::<&syn::DeriveInput> for RenumConfig
{
  type Error = syn::Error;
  fn try_from(input: &syn::DeriveInput) -> Result<Self, syn::Error> {

    let mut config = RenumConfig::default();

    config.repr = parse_repr(&input.attrs);
    config.name = input.ident.clone();

    if let Some(attr) = find_attr_by_path_str(&input.attrs, ATTR_RENUM) {

      for option in attr.parse_args::<Sequence::<ConfigOption>>()? {
        match option {
          ConfigOption::AllowPanic => config.allow_panic = true,
          ConfigOption::From => config.try_from = false,
          ConfigOption::TryFrom => config.try_from = true,
          ConfigOption::ErrorType(error) => config.error_type = error,
          ConfigOption::NormallyOk => config.normally_ok = true,
          ConfigOption::NormallyErr => config.normally_ok = false,
          ConfigOption::DefaultVariant(_) => todo!(),
          ConfigOption::ErrorFrom(args) => config.error_from = Some(args),
        }
      }

    }

    Ok(config)

  }
}

fn parse_repr(attrs: &Vec::<Attribute>) -> Ident {

  let repr_isize = ident!(isize);

  if let Some(attr) = find_attr_by_path_str(attrs, ATTR_REPR) {
    // if a repr is defined we check the ident provided, otherwise we just use isize
    if let Ok(repr) = attr.parse_args::<Ident>() {
      return match repr.to_string().as_str() {
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" |
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" => repr,
        "C" => repr_isize,
        _ => panic!(" Renum valid reprs are `C`, `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`, `u128`, `i128`, `isize`, `usize`"),
      }
    }
  }

  repr_isize
}


fn default_variant<R: Repr>(variants: &Vec::<RenumVariant<R>>) -> Option<&RenumVariant<R>>
  where <R as FromStr>::Err: std::fmt::Display
{

  let explicit = variants.iter()
    .filter(|variant| variant.is_default)
    .collect::<Vec::<_>>();

  if explicit.len() > 1 {
    panic!("multiple explicit #[renum(default)] variants")
  } else if explicit.len() == 1 {
    return explicit.first().copied();
  }

  // 0 explicit targets

  let implicit = variants.iter()
    .filter(|variant| variant.discriminant.is_none() && variant.values.is_none())
    .collect::<Vec::<_>>();

  if implicit.len() > 1 {
    panic!("multiple implicitly default variants identified")
  } else if implicit.len() == 1 {
    return implicit.first().copied();
  }

  // 0 implicit targets (this will mean that we will have to check panic rules later on)
  None
}


fn derive_from<R: Repr>(config: &RenumConfig, variants: &Vec::<RenumVariant::<R>> ) -> TokenStream2
  where <R as FromStr>::Err: std::fmt::Display
{

  let repr = &config.repr;
  let enum_name = &config.name;
  let default = default_variant(variants);

  // anything with a disciminant will be mapped using transmute
  let transmutable = variants.iter()
    .filter_map(|variant| variant.discriminant)
    .collect::<Vec::<R>>();

  // todo? - maybe compress the values into ranges if possible.
  //    the compiler might do this for us but we dont want to rely on that optimization

  let transmute = quote!{  #(#transmutable)|* => unsafe { std::mem::transmute::<#repr, #enum_name>(value) }, };

  // other variants that aren't 1:1

  let other_variants = variants.iter()
    .filter(|variant| variant.values.is_some())
    .map(|variant| {

      let variant_ident = &variant.ident;

      let variant_values = variant.values.as_ref().unwrap()
        .iter().map(VariantValue::quote);

       quote!{ #(#variant_values)|* => #enum_name::#variant_ident }
    })
    .collect::<Vec::<_>>();

  let other_matches = if other_variants.is_empty() {
    quote!{}
  } else {
    quote!{ #(#other_variants),*, }
  };

  let otherwise = match default {
    Some(variant) => {
      let variant_ident = &variant.ident;
      quote!{ _ => #enum_name::#variant_ident }
    },
    None => {
      if !config.allow_panic {
        panic!("no default variants found, either set default or allow_panic")
      }
      quote!{ _ => panic!("enum value cannot be converted") }
    }
  };

  return quote!{
    impl std::convert::From::<#repr> for #enum_name {
      fn from(value: #repr) -> #enum_name {
        match value {
          #transmute
          #other_matches
          #otherwise
        }
      }
    }
  };

}

fn build_error_value(config: &RenumConfig, variant: &TokenStream2) -> TokenStream2 {

   match &config.error_type {
    ErrorType::Repr => quote!{value},
    ErrorType::Enum => quote!{#variant},
    ErrorType::Other(_other) => {
      match &config.error_from {
        None => quote!{#variant.into()},
        Some(mapping) => {
          let values = mapping.iter().map(|item| match item {
            ErrorFrom::Repr => quote!{value},
            ErrorFrom::Enum => quote!{#variant}
          })
          .collect::<Vec::<_>>();

          match values.len() {
            0 => panic!(),
            _ => quote!{ (#(#values),*).into() }
          }

        }
      }

    }
  }

}



fn derive_try_from<R: Repr>(config: &RenumConfig, variants: &Vec::<RenumVariant::<R>> ) -> TokenStream2
  where <R as FromStr>::Err: std::fmt::Display
{

  let repr = &config.repr;
  let enum_name = &config.name;
  let default = default_variant(variants);

  let error_type = match &config.error_type {
    ErrorType::Repr => quote!{#repr},
    ErrorType::Enum => quote!{#enum_name},
    ErrorType::Other(other) => quote!{#other}
  };

  // anything with a disciminant will be mapped using transmute
  let transmute = quote!{ unsafe { std::mem::transmute::<#repr, #enum_name>(value) } };

  let transmutable_ok = variants.iter()
    .filter(|variant| match variant.is_ok {
        Some(is_ok) => is_ok,
        None => config.normally_ok
      })
    .filter_map(|variant| variant.discriminant)
    .collect::<Vec::<R>>();

  // todo? - maybe compress the values into ranges if possible.
  //    the compiler might do this for us but we dont want to rely on that optimization

  let transmute_ok = match transmutable_ok.is_empty() {
    true => None,
    false => Some(quote! { #(#transmutable_ok)|* => Ok(#transmute), })
  };

  let transmutable_err = variants.iter()
    .filter(|variant| match variant.is_ok {
        Some(is_ok) => !is_ok,
        None => !config.normally_ok
      })
    .filter_map(|variant| variant.discriminant)
    .collect::<Vec::<R>>();

  // todo? - maybe compress the values into ranges if possible.
  //    the compiler might do this for us but we dont want to rely on that optimization

  let transmute_err = if transmutable_err.is_empty() {
    quote!{}
  } else {
    let error_value = build_error_value(config, &transmute);
    quote! { #(#transmutable_err)|* => Err(#error_value), }
  };

  // other variants that aren't 1:1

  let other_variants = variants.iter()
    .filter(|variant| variant.values.is_some())
    .map(|variant| {

      let variant_ident = &variant.ident;

      let variant_values = variant.values.as_ref().unwrap()
        .iter().map(VariantValue::quote);

      let error_value = build_error_value(config, &quote!{#enum_name::#variant_ident});

      match (variant.is_ok, config.normally_ok) {
        (None, true)  | (Some(true),  _) => quote!{ #(#variant_values)|* => Ok(#enum_name::#variant_ident) },
        (None, false) | (Some(false), _) => quote! { #(#variant_values)|* => Err(#error_value) }
      }

    })
    .collect::<Vec::<_>>();

  let other_matches = match other_variants.is_empty() {
    true => None,
    false => Some(quote!{ #(#other_variants),*, })
  };

  let otherwise = match default {
    Some(variant) => {
      let variant_ident = &variant.ident;

      let error_value = build_error_value(config, &quote!{#enum_name::#variant_ident});

      match (variant.is_ok, config.normally_ok) {
        (None, false) | (Some(true),  _) => quote!{ _ => Ok(#enum_name::#variant_ident) },
        (None, true)  | (Some(false), _) => quote!{ _ => Err(#error_value) }
      }
    },
    None => {
      if !config.allow_panic {
        panic!("no default variants found, either set default or allow_panic")
      }
      quote!{ _ => panic!("") }
    }
  };

  return quote!{
    impl std::convert::TryFrom::<#repr> for #enum_name {
      type Error = #error_type;
      fn try_from(value: #repr) -> Result<#enum_name, #error_type> {
        match value {
          #transmute_ok
          #transmute_err
          #other_matches
          #otherwise
        }
      }
    }
  };

}

fn renum_typed<R>(config: RenumConfig, data: &syn::DataEnum) -> TokenStream
  where R: Repr, <R as FromStr>::Err: std::fmt::Display
{

  let mut variants: Vec::<RenumVariant::<R>> = Vec::new();

  for variant in &data.variants {
    if let Ok(renum_variant) = RenumVariant::<R>::try_from(variant) {
      variants.push(renum_variant);
    } else {
      panic!("Failed to parse variant: {:?}", variant);
    }
  }

  // todo!
  // if variants.windows(2usize).any(RenumVariant::overlaps) {
  //   panic!("variants overlap")
  // }

  let derived = if config.try_from {
    derive_try_from::<R>(&config, &variants)
  } else {
    derive_from::<R>(&config, &variants)
  };

  return TokenStream::from(quote!{ #derived });
}

#[proc_macro_derive(Renum, attributes(renum))]
pub fn derive_renum(input: TokenStream) -> TokenStream {

  let input = parse_macro_input!(input as DeriveInput);

  let data = match input.data {
    Data::Enum(ref data) => data,
    _ => panic!("Renum can only be used with Enum items.")
  };

  let config = match RenumConfig::try_from(&input) {
    Ok(config) => config,
    Err(error) => panic!("{:?}", error)
  };

  match config.repr.to_string().as_str() {
    "u8"    => renum_typed::<u8>(config, data),
    "u16"   => renum_typed::<u16>(config, data),
    "u32"   => renum_typed::<u32>(config, data),
    "u64"   => renum_typed::<u64>(config, data),
    "u128"  => renum_typed::<u128>(config, data),
    "usize" => renum_typed::<usize>(config, data),
    "i8"    => renum_typed::<i8>(config, data),
    "i16"   => renum_typed::<i16>(config, data),
    "i32"   => renum_typed::<i32>(config, data),
    "i64"   => renum_typed::<i64>(config, data),
    "i128"  => renum_typed::<i128>(config, data),
    "isize" => renum_typed::<isize>(config, data),
    _       => unreachable!("parse_repr would already have hit panic!() before this")
  }
}


#[proc_macro_derive(RenumTryFrom, attributes(renum))]
pub fn derive_renum_try_from(input: TokenStream) -> TokenStream {

  let input = parse_macro_input!(input as DeriveInput);

  let data = match input.data {
    Data::Enum(ref data) => data,
    _ => panic!("Renum can only be used with Enum items.")
  };

  let mut config = match RenumConfig::try_from(&input) {
    Ok(config) => config,
    Err(error) => panic!("{:?}", error)
  };

  config.try_from = true;

  match config.repr.to_string().as_str() {
    "u8"    => renum_typed::<u8>(config, data),
    "u16"   => renum_typed::<u16>(config, data),
    "u32"   => renum_typed::<u32>(config, data),
    "u64"   => renum_typed::<u64>(config, data),
    "u128"  => renum_typed::<u128>(config, data),
    "usize" => renum_typed::<usize>(config, data),
    "i8"    => renum_typed::<i8>(config, data),
    "i16"   => renum_typed::<i16>(config, data),
    "i32"   => renum_typed::<i32>(config, data),
    "i64"   => renum_typed::<i64>(config, data),
    "i128"  => renum_typed::<i128>(config, data),
    "isize" => renum_typed::<isize>(config, data),
    _       => unreachable!("parse_repr would already have hit panic!() before this")
  }
}

#[cfg(test)]
mod tests {

  #[test]
  fn test_renum() {
    let tb = trybuild::TestCases::new();
    tb.pass("tests/from.rs");
    tb.pass("tests/try_from.rs");
    tb.pass("tests/config.rs");
    tb.pass("tests/options.rs");
  }
}
