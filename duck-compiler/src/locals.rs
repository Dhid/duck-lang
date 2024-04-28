use duck_syntax::ast::{Identifier, Mutability};

type UpvalueIndex = usize;
pub struct Local {
    pub name: Identifier,
    pub depth: usize,
    pub init: bool,
    pub slot: usize,
    pub captured: bool,
    pub mutability: Mutability,
}

impl Local {
    pub fn new(name: Identifier, depth: usize, slot: usize, mutability: Mutability) -> Self {
        Local {
            name,
            depth,
            init: false,
            slot,
            captured: false,
            mutability,
        }
    }

    pub fn captured(&self) -> bool {
        self.captured
    }

    pub fn mark_initialized(&mut self) {
        self.init = true;
    }

    pub fn is_initialized(&self) -> bool {
        self.init
    }
}

pub struct Locals {
    pub stack: Vec<Local>,
    pub actual_depth: usize,
}

pub struct Upvalues {
    pub stack: Vec<UpvalueIndex>,
    pub actual_depth: usize,
}

impl Locals {
    pub fn new() -> Self {
        Locals {
            stack: Vec::new(),
            actual_depth: 0,
        }
    }

    pub fn end_scope(&mut self) -> Vec<Local> {
        self.actual_depth -= 1;
        let index = self
            .stack
            .iter()
            .enumerate()
            .find_map(|(i, l)| {
                if l.depth > self.actual_depth {
                    Some(i)
                } else {
                    None
                }
            })
            .unwrap_or(self.stack.len());
        self.stack.split_off(index)
    }

    pub fn mark_captured(&mut self, slot: usize) {
        let local = self.stack.iter_mut().find(|l| l.slot == slot);
        if let Some(local) = local {
            local.captured = true;
        }
    }

    /// Adds a local, and throws an error if a local with the same name and scope is present
    pub fn add(&mut self, local: Local) -> Result<(), ()> {
        let already_present = self
            .stack
            .iter()
            .filter(|l| l.depth == local.depth && l.name == local.name);
        if already_present.count() > 0 {
            return Err(());
        }
        self.stack.push(local);

        Ok(())
    }
}
