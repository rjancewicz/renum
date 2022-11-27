
// #![feature(repr128)]

use renum::Renum;

macro_rules! construct_reprs {
  [$(($name:ident, $repr:ty)),*] => {
    $(
      #[repr($repr)]
      #[derive(Debug, Clone, Copy, PartialEq, Renum)]
      pub enum $name {
        A = 0x01,
        B = 0x02,
        C = 0x03,
        Undefined
      }
    )*
  }
}

construct_reprs![
  (TestUsize, usize),
  (TestU8, u8),
  (TestU16, u16),
  (TestU32, u32),
  (TestU64, u64),
  // (TestU128, u128),
  (TestIsize, isize),
  (TestI8, i8),
  (TestI16, i16),
  (TestI32, i32),
  (TestI64, i64)
  // (TestI128, i128)
  ];

#[allow(unused_macros)]
macro_rules! construct_tests {

  ($name:ident, $repr:ty) => {
    assert_eq!($name::from(0x01 as $repr), $name::A);
    assert_eq!($name::from(0x02 as $repr), $name::B);
    assert_eq!($name::from(0x03 as $repr), $name::C);
    assert_eq!($name::from(0x04 as $repr), $name::Undefined);
  }
}


#[test]
fn from_usize() { construct_tests!(TestUsize, usize); }
#[test]
fn from_u8() { construct_tests!(TestU8, u8); }
#[test]
fn from_u16() { construct_tests!(TestU16, u16); }
#[test]
fn from_u32() { construct_tests!(TestU32, u32); }
#[test]
fn from_u64() { construct_tests!(TestU64, u64); }

#[test]
fn from_isize() { construct_tests!(TestIsize, isize); }
#[test]
fn from_i8() { construct_tests!(TestI8, i8); }
#[test]
fn from_i16() { construct_tests!(TestI16, i16); }
#[test]
fn from_i32() { construct_tests!(TestI32, i32); }
#[test]
fn from_i64() { construct_tests!(TestI64, i64); }


fn main() {}
