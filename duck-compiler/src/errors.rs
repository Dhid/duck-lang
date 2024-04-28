use displaydoc::Display;
use thiserror::Error;

#[derive(Error, Debug, Display)]
pub enum CompilerError {
    /// >>> Cannot find Data struct: {name:?}
    DataStructNotFound { name: String },

    /// >>> Missing non-nullable properties for Data struct {data:?}
    MissingNonNullableProperty { data: String },
}
