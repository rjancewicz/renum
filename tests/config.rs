use renum::Renum;

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom)]
pub enum ConfigTryFrom {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}

#[derive(Debug)]
#[derive(PartialEq)]
pub struct CustomError;

impl From::<u16> for CustomError {
  fn from(_: u16) -> Self { CustomError }
}

impl From::<ConfigCustomError> for CustomError {
  fn from(_: ConfigCustomError) -> Self { CustomError }
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, Error = CustomError)]
pub enum ConfigCustomError {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}


#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, normally_err)]
pub enum ConfigNormallyErr {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, normally_ok)]
pub enum ConfigNormallyOk {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(allow_panic)]
pub enum ConfigPanic {
  A = 0x01,
  B = 0x02,
  C = 0x03
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, Error = ENUM)]
pub enum ConfigEnumError {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, Error = REPR)]
pub enum ConfigReprError {
  A = 0x01,
  B = 0x02,
  C = 0x03,
  Undefined
}

#[test]
fn config_try_from() {
    assert_eq!(ConfigTryFrom::try_from(0x01 as u16), Ok(ConfigTryFrom::A));
    assert_eq!(ConfigTryFrom::try_from(0x02 as u16), Ok(ConfigTryFrom::B));
    assert_eq!(ConfigTryFrom::try_from(0x03 as u16), Ok(ConfigTryFrom::C));
    assert_eq!(ConfigTryFrom::try_from(0x04 as u16), Err(ConfigTryFrom::Undefined));
}

#[test]
fn config_normally_ok() {
    assert_eq!(ConfigNormallyOk::try_from(0x01 as u16), Ok(ConfigNormallyOk::A));
    assert_eq!(ConfigNormallyOk::try_from(0x02 as u16), Ok(ConfigNormallyOk::B));
    assert_eq!(ConfigNormallyOk::try_from(0x03 as u16), Ok(ConfigNormallyOk::C));
    assert_eq!(ConfigNormallyOk::try_from(0x04 as u16), Err(ConfigNormallyOk::Undefined));
}

#[test]
fn config_normally_err() {
    assert_eq!(ConfigNormallyErr::try_from(0x01 as u16), Err(ConfigNormallyErr::A));
    assert_eq!(ConfigNormallyErr::try_from(0x02 as u16), Err(ConfigNormallyErr::B));
    assert_eq!(ConfigNormallyErr::try_from(0x03 as u16), Err(ConfigNormallyErr::C));
    assert_eq!(ConfigNormallyErr::try_from(0x04 as u16), Ok(ConfigNormallyErr::Undefined));
}

#[test]
#[should_panic]
fn config_allow_panic() {
    assert_eq!(ConfigPanic::try_from(0x01 as u16), Ok(ConfigPanic::A));
    assert_eq!(ConfigPanic::try_from(0x02 as u16), Ok(ConfigPanic::B));
    assert_eq!(ConfigPanic::try_from(0x03 as u16), Ok(ConfigPanic::C));
    let _panic = ConfigPanic::try_from(0x04 as u16); // panic
}

#[test]
fn config_enum_error() {
    assert_eq!(ConfigEnumError::try_from(0x01 as u16), Ok(ConfigEnumError::A));
    assert_eq!(ConfigEnumError::try_from(0x02 as u16), Ok(ConfigEnumError::B));
    assert_eq!(ConfigEnumError::try_from(0x03 as u16), Ok(ConfigEnumError::C));
    assert_eq!(ConfigEnumError::try_from(0x04 as u16), Err(ConfigEnumError::Undefined));
}

#[test]
fn config_repr_error() {
    assert_eq!(ConfigReprError::try_from(0x01 as u16), Ok(ConfigReprError::A));
    assert_eq!(ConfigReprError::try_from(0x02 as u16), Ok(ConfigReprError::B));
    assert_eq!(ConfigReprError::try_from(0x03 as u16), Ok(ConfigReprError::C));
    assert_eq!(ConfigReprError::try_from(0x04 as u16), Err(0x04));
}

#[test]
fn config_custom_error() {
    assert_eq!(ConfigCustomError::try_from(0x01 as u16), Ok(ConfigCustomError::A));
    assert_eq!(ConfigCustomError::try_from(0x02 as u16), Ok(ConfigCustomError::B));
    assert_eq!(ConfigCustomError::try_from(0x03 as u16), Ok(ConfigCustomError::C));
    assert_eq!(ConfigCustomError::try_from(0x04 as u16), Err(CustomError));
}




fn main() {}
