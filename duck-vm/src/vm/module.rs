use std::collections::BTreeMap;

use super::{errors::VMError, memory::Value};
use std::path::Path;

type Identifier = String;

/// Struct that manages the resolution for globals defined in the file identified by the identifier
/// string
pub struct Module {
    /// id of the module, e.g. file path
    pub identifier: String,
    /// sub modules building the dependecy chain
    submodules: BTreeMap<Identifier, Module>,

    /// global functions defined in the module
    // functions: BTreeMap<String, DuckFn>,

    /// global values defined in the module
    globals: BTreeMap<Identifier, Value>,
}

impl Module {
    pub fn new(identifier: Identifier) -> Self {
        Module {
            identifier,
            submodules: BTreeMap::new(),
            globals: BTreeMap::new(),
        }
    }

    pub fn add_submodule(&mut self, module: Module) {
        let identifier = module.identifier.clone();
        self.submodules.insert(identifier, module);
    }

    pub fn add_global(&mut self, identifier: Identifier, global: Value) {
        self.globals.insert(identifier, global);
    }

    pub fn resolve_global(&self, identifier: &Identifier) -> Option<Value> {
        let current_module_global = self.globals.get(identifier);
        match current_module_global {
            Some(value) => Some(*value),
            None => {
                // search in submodules
                for (_, submodule) in &self.submodules {
                    let global_submodule = submodule.resolve_global(identifier);
                    match global_submodule {
                        Some(value) => return Some(value),
                        None => {}
                    }
                }
                None
            }
        }
    }
}

pub fn import_module(_filename: &impl AsRef<Path>) -> Result<Module, VMError> {
    todo!()
}
