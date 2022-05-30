use std::alloc::Layout;
use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;

pub struct Stack<T, const SIZE: usize> {
    ptr: NonNull<T>,
    max_offset: usize,
    top_offset: usize,
    _marker: PhantomData<T>,
}

impl<T, const SIZE: usize> Stack<T, SIZE> {
    pub fn new() -> Self {
        Self {
            ptr: Self::allocate(),
            max_offset: 0,
            top_offset: 0,
            _marker: PhantomData,
        }
    }

    pub fn len(&self) -> usize {
        self.top_offset
    }

    pub fn push(&mut self, value: T) {
        unsafe {
            ptr::write(self.ptr.as_ptr().add(self.top_offset), value);
        }

        self.top_offset += 1;
        self.max_offset = std::cmp::max(self.max_offset, self.top_offset);
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.top_offset == 0 {
            None
        } else {
            self.top_offset -= 1;

            unsafe { Some(ptr::read(self.ptr.as_ptr().add(self.top_offset))) }
        }
    }

    pub fn peek(&self, distance: usize) -> Option<&T> {
        if self.top_offset <= 1 {
            None
        } else {
            unsafe {
                self.ptr
                    .as_ptr()
                    .add(self.top_offset - 1 - distance)
                    .as_ref()
            }
        }
    }

    pub fn replace_top(&mut self, value: T) {
        unsafe {
            let ptr = self.ptr.as_ptr().add(self.top_offset);

            let old_value = ptr::read(ptr);
            if std::mem::needs_drop::<T>() {
                drop(old_value);
            }

            ptr::write(ptr, value);
        }
    }

    pub fn reset(&mut self) {
        self.top_offset = 0;
    }

    fn allocate() -> NonNull<T> {
        let layout = Layout::array::<T>(SIZE).unwrap();
        let memory = unsafe { std::alloc::alloc(layout) };

        match NonNull::new(memory as *mut T) {
            Some(p) => p,
            None => std::alloc::handle_alloc_error(layout),
        }
    }
}

impl<T, const SIZE: usize> Drop for Stack<T, SIZE> {
    fn drop(&mut self) {
        let layout = Layout::array::<T>(SIZE).unwrap();
        unsafe {
            std::alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout);
        }
    }
}

impl<T, const SIZE: usize> Debug for Stack<T, SIZE> {
    fn fmt(&self, _f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO
        Ok(())
    }
}
