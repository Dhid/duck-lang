use std::{
    cell::Cell,
    fmt,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use super::memory::{Closure, ObjInstance};

/// Trait used for mutate the internal state of a GC's struct
pub trait Trace {
    fn trace(&self);
}

impl fmt::Debug for dyn Trace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<Trace>")
    }
}

// Use interior mutability for the GC
// so a simple reference of the VM can be used
#[derive(Debug)]
struct Header {
    marked: Cell<bool>,
    root: Cell<bool>,
}

impl Header {
    fn root() -> Self {
        Header {
            marked: Cell::new(false),
            root: Cell::new(true),
        }
    }
}
// Each data T must have static lifetype
// the value is dropped during the mark and sweep operation
// the values to removed will be retained from the Vac of the Heap
// so Rust will run the decostructor over them beside the static lifetime
#[derive(Debug)]
pub struct Allocation<T: 'static + Trace + ?Sized> {
    header: Header,
    data: T,
}

impl<T: 'static + Trace + ?Sized> Allocation<T> {
    #[inline]
    fn unmark(&self) {
        self.header.marked.set(false);
    }

    #[inline]
    pub fn unroot(&self) {
        self.header.root.set(false);
    }

    fn is_root(&self) -> bool {
        self.header.root.get()
    }
    fn is_marked(&self) -> bool {
        self.header.marked.get()
    }
}

impl Default for Header {
    fn default() -> Self {
        Header {
            marked: Cell::new(false),
            root: Cell::new(false),
        }
    }
}

impl<T: 'static + Trace + ?Sized> Trace for Allocation<T> {
    #[inline]
    fn trace(&self) {
        if !self.header.marked.replace(true) {
            self.data.trace();
        }
    }
}

pub struct Heap {
    // store Vec of elements on Rust's heap
    objects: Vec<Box<Allocation<dyn Trace>>>,
    bytes_allocated: usize,
    threshold: usize,
}

impl fmt::Debug for Heap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Bytes allocated: {} \nTotal objects: {}",
            self.bytes_allocated,
            self.objects.len(),
            // self.objects
        )
    }
}

impl Heap {
    const THRESHOLD_ADJ: f32 = 1.4;

    pub fn new() -> Self {
        Heap {
            objects: Vec::with_capacity(8192),
            bytes_allocated: 0,
            threshold: 100,
        }
    }

    fn allocate<T: 'static + Trace>(&mut self, data: T) -> NonNull<Allocation<T>> {
        let mut alloc = Box::new(Allocation {
            header: Header::default(),
            data,
        });
        let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
        // so a new NonNull can interop as a Box, nice to have
        self.objects.push(alloc);
        self.bytes_allocated += std::mem::size_of::<T>();
        ptr
    }

    fn allocate_root<T: 'static + Trace>(&mut self, data: T) -> NonNull<Allocation<T>> {
        let mut alloc = Box::new(Allocation {
            header: Header::root(),
            data,
        });
        let ptr = unsafe { NonNull::new_unchecked(&mut *alloc) };
        self.objects.push(alloc);
        self.bytes_allocated += std::mem::size_of::<T>();
        ptr
    }
}

#[derive(Debug)]
pub struct Gc<T: 'static + Trace + ?Sized> {
    pub ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Trace + ?Sized> Gc<T> {
    pub fn allocation(&self) -> &Allocation<T> {
        unsafe { &self.ptr.as_ref() }
    }
    fn allocation_mut(&mut self) -> &mut Allocation<T> {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T: 'static + Trace + ?Sized> Trace for Gc<T> {
    #[inline]
    fn trace(&self) {
        self.allocation().trace()
    }
}

impl<T: 'static + Trace + ?Sized> Copy for Gc<T> {}
impl<T: 'static + Trace + ?Sized> Clone for Gc<T> {
    #[inline]
    fn clone(&self) -> Gc<T> {
        *self
    }
}

impl Trace for Closure {
    #[inline]
    fn trace(&self) {}
}

impl Trace for ObjInstance {
    #[inline]
    fn trace(&self) {
        for (_, val) in self.properties.iter() {
            val.trace();
        }
    }
}

impl<T: Trace + Copy> Trace for Cell<T> {
    fn trace(&self) {
        self.get().trace();
    }
}
impl<T: Trace> Trace for Vec<T> {
    #[inline]
    fn trace(&self) {
        for el in self {
            el.trace();
        }
    }
}
impl<T: Trace> Trace for &Vec<T> {
    #[inline]
    fn trace(&self) {
        for el in *self {
            el.trace();
        }
    }
}
impl Trace for String {
    #[inline]
    fn trace(&self) {}
}
// get data inside the Gc
impl<T: 'static + Trace + ?Sized> Deref for Gc<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.allocation().data
    }
}

impl<T: 'static + Trace + ?Sized> DerefMut for Gc<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut T {
        &mut self.allocation_mut().data
    }
}

pub struct Root<T: 'static + Trace + ?Sized> {
    ptr: NonNull<Allocation<T>>,
}

impl<T: 'static + Trace + ?Sized> Root<T> {
    fn allocation(&self) -> &Allocation<T> {
        unsafe { &self.ptr.as_ref() }
    }
}

impl Heap {
    fn mark_roots(&self) {
        self.objects.iter().for_each(|obj| obj.unmark());
        self.objects
            .iter()
            .filter(|o| o.is_root())
            .for_each(|o| o.trace());
    }
    fn sweep(&mut self) {
        self.objects.retain(|obj| obj.is_marked());
    }

    pub fn collect(&mut self) {
        self.mark_roots();
        self.sweep();
    }

    pub fn manage<T: 'static + Trace>(&mut self, data: T) -> Gc<T> {
        Gc {
            ptr: self.allocate(data),
        }
    }

    pub fn root<T: 'static + Trace>(&mut self, data: T) -> Gc<T> {
        Gc {
            ptr: self.allocate_root(data),
        }
    }
}
