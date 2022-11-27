

use renum::Renum;

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
pub enum OptionsDefault {
  A = 0x01,
  B = 0x02,
  #[renum(default)]
  C = 0x03
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom)]
pub enum OptionsResults {
  #[renum(Err)]
  A = 0x01,
  #[renum(Ok)]
  B = 0x02,
  #[renum(Err)]
  C = 0x03,
  #[renum(Ok)]
  Undefined
}

#[repr(u16)]
#[derive(Debug, Clone, Copy, PartialEq, Renum)]
#[renum(TryFrom, normally_err)]
pub enum OptionsResults2 {
  #[renum(Ok)]
  A = 0x01,
  #[renum(Ok)]
  B = 0x02,
  C = 0x03,
  Undefined
}

#[test]
fn options_default() {
    assert_eq!(OptionsDefault::from(0x01 as u16), OptionsDefault::A);
    assert_eq!(OptionsDefault::from(0x02 as u16), OptionsDefault::B);
    assert_eq!(OptionsDefault::from(0x03 as u16), OptionsDefault::C);
    assert_eq!(OptionsDefault::from(0x04 as u16), OptionsDefault::C);
}

#[test]
fn options_results() {
    assert_eq!(OptionsResults::try_from(0x01 as u16), Err(OptionsResults::A));
    assert_eq!(OptionsResults::try_from(0x02 as u16), Ok(OptionsResults::B));
    assert_eq!(OptionsResults::try_from(0x03 as u16), Err(OptionsResults::C));
    assert_eq!(OptionsResults::try_from(0x04 as u16), Ok(OptionsResults::Undefined));
}

#[test]
fn options_results_2() {
    assert_eq!(OptionsResults2::try_from(0x01 as u16), Ok(OptionsResults2::A));
    assert_eq!(OptionsResults2::try_from(0x02 as u16), Ok(OptionsResults2::B));
    assert_eq!(OptionsResults2::try_from(0x03 as u16), Err(OptionsResults2::C));
    assert_eq!(OptionsResults2::try_from(0x04 as u16), Ok(OptionsResults2::Undefined));
}


fn main() {}
