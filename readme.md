
# Renum

Derive the From or TryFrom traits for enums with integer values. 

Designed to provide a consistant mechanism to work with C-style Enums in Rust such as when implementing a parser.

# Usage

```toml
[dependencies]
renum = "0.1.0"
```

```rust
use renum::Renum;

#[derive(Debug, Clone, Copy, Renum)]
#[renum(TryFrom)]
pub enum MyEnum {
  VariantA = 0x01,
  VariantB = 0x02,
  VariantC = 0x03,
  Undefined
}

let x = MyEnum::try_from(0x04);

assert_eq!(x, Err(MyEnum::Undefined))

```

# Enum Options

## normally_ok 
**default behavior**

```rust
#[renum(normally_ok)]
```

The `Result` returned by the derived `try_from` will be `Ok(Enum::Variant)` unless the variant is  explicitly tagged `#[renum(Err)]`

The default case will return `Err(Enum::Default)` unless explicitly tagged `#[renum(Ok)]`

## normally_err
```rust
#[renum(normally_err)]
```

Inverts the `normally_ok` behavior

The `Result` returned by the derived `try_from` will be `Err(Enum::Variant)` unless explicitly tagged `#[renum(Ok)]`
  
The default case will return `Ok(Enum::Default)` unless explicitly tagged `#[renum(Err)]`

example usage: status code enums

```rust
#[derive(Renum)]
#[renum(normally_err)]
pub enum StatusCodes {
  Success,
  FailureA = 1,
  FailureB = 2,
  FailureC = 3
}

// ...

let status_code = StatusCodes::try_from(2)?;

```

## allow_panic
```rust
#[renum(allow_panic)]
```

If an enum does not have a default variant `allow_panic` will allow the derived method to panic on undefined values.

Example:


```rust
#[repr(u8)]
#[derive(Renum)
#[renum(allow_panic)]
pub enum PanicEnum {
	A = 0,
	B = 1,
}

#[renum(should_panic)]
fn test() {
	let _ = PanicEnum::from(0xff);
}

```

## Error
**Only Valid when deriving TryFrom**

```rust
#[renum(Error = REPR)]
#[renum(Error = ENUM)]
#[renum(Error = Other)] // custom
```

The error type can be controlled with the `Error` option; `REPR`, and `ENUM` are builtin values which return the value passed in or enum variant.

If a custom error is used it must implement `From::<#ErrorFrom>` (discussed below), by default this will be the repr type.

## ErrorFrom
**Only Valid when deriving TryFrom**

```rust
#[renum(ErrorFrom = REPR)]
#[renum(ErrorFrom = ENUM)]
#[renum(ErrorFrom = (ENUM, REPR))]
```

The value used to initialize custom errors can be set to `REPR`, `ENUM`, or any sequence of `([REPR|ENUM],*)`

Example:

```rust

struct CustomError;

impl From<(MyEnum, u16)> for CustomError {
	fn from(from: (MyEnum, u16)) -> Self {
		CustomError;
	}
}

#[repr(u16)]
#[derive(Renum)]
#[renum(TryFrom, ErrorType = CustomError, ErrorFrom = (ENUM, REPR))]
pub enum MyEnum {
	A = 1,
	B = 2,
	C = 3,
	#[renum(Err, values = 4..=34 )]
	ReservedSome,
	#[renum(Err)]
	ReservedRest
}

if let Err(error) = MyEnum::try_from(64) {
	// do something with custom error
}

	
```


# Variant Options 

## Ok
**Only Valid when deriving TryFrom**

```rust
#[renum(Ok)]
Variant = 0
```

The variant will always return `Ok(Enum::Variant)` when matched even if it would otherwise be an error condition.

## Err
**Only Valid when deriving TryFrom**

```rust
#[renum(Err)]
Variant = 0
```

The variant will always return `Err(Enum::Variant)` when matched even if it would otherwise be an error condition.

## Default
```rust
#[renum(default)]
Variant
```

If no values match this variant will be returned, if no variants are explicitly annotated with the `default` option, default is automatically selected to be the variant without a desciminant or values attribute. 

## Values
```rust
#[renum(values = 10)]     // this shouldn't be needed, use a discriminant
#[renum(values = ..10)]   // REPR::MIN to 9 
#[renum(values = ..=10)]  // REPR::MIN to 10
#[renum(values = 5..10)]  // Range 5 to 9
#[renum(values = 5..=10)] // RangeInclusive 5 to 10
#[renum(values = [0, 15..20, 25..30, 45])] // any combination of values for non-consecutive ranges
```

Specify values for a variant where it may take on multiple values.
