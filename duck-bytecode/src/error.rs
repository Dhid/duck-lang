use displaydoc::Display;
use thiserror::Error;

#[derive(Error, Debug, Display)]
pub enum DecoderError {
    /// >>> Unexpected end of file
    UnexpectedEndOfData,

    /// >>> Cannot decode UTF8 bytes
    CannotDecodeUTF8,

    /// >>> Invalid Constant code
    InvalidConstantCode,

    /// >>> Invalid Boolean encoded data
    InvalidBooleanData,

    /// >>> Unexpected magic number {num:?}
    UnexpectedMagicNumner { num: usize },

    /// >>> OpCode {code:?} not supported
    OpCodeNotSupported { code: u8 },

    /// >>> MagicCode {code:?} not supported
    MagicCodeNotSupported { code: u8 },

    /// >>> Length out of range for unsigned 32 bits
    LengthOutOfU32,
}
