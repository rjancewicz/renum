

use renum::Renum;


#[derive(Debug)]
#[derive(PartialEq)]
pub struct CustomError;

impl From::<u16> for CustomError {
  fn from(_: u16) -> Self { CustomError }
}

impl From::<ConfigCustomErrorE> for CustomError {
  fn from(_: ConfigCustomErrorE) -> Self { CustomError }
}

impl From::<(u16, ConfigCustomErrorRE)> for CustomError {
  fn from(_: (u16, ConfigCustomErrorRE)) -> Self { CustomError }
}

impl From::<(u16, ConfigCustomErrorRER, u16)> for CustomError {
  fn from(_: (u16, ConfigCustomErrorRER, u16)) -> Self { CustomError }
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, Error = CustomError, ErrorFrom = ENUM)]
pub enum ConfigCustomErrorE {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, Error = CustomError, ErrorFrom = REPR)]
pub enum ConfigCustomErrorR {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, Error = CustomError, ErrorFrom = (REPR, ENUM))]
pub enum ConfigCustomErrorRE {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}


#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, Error = CustomError, ErrorFrom = (REPR, ENUM, REPR))]
pub enum ConfigCustomErrorRER {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}


#[allow(unused_macros)]
macro_rules! construct_tests {

  ($name:ident, $repr:ty) => {
    assert_eq!($name::try_from(0x01 as $repr), Ok($name::A));
    assert_eq!($name::try_from(0x02 as $repr), Ok($name::B));
    assert_eq!($name::try_from(0x03 as $repr), Ok($name::C));
    assert_eq!($name::try_from(0x04 as $repr), Err(CustomError));
  }
}

#[test]
fn custom_error_r() {
  construct_tests!(ConfigCustomErrorR, u16);
}

#[test]
fn custom_error_e() {
  construct_tests!(ConfigCustomErrorE, u16);
}

#[test]
fn custom_error_re() {
  construct_tests!(ConfigCustomErrorRE, u16);
}

#[test]
fn custom_error_rer() {
  construct_tests!(ConfigCustomErrorRER, u16);
}


fn main() {}
