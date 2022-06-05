use core::fmt::{Debug, Display, Formatter};
use std::alloc::{alloc_zeroed, dealloc, handle_alloc_error, realloc, Layout};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;

const MAX_TABLE_LOAD: f64 = 0.75;

fn grow_capacity(capacity: usize) -> usize {
    if capacity < 8 {
        8
    } else {
        capacity * 2
    }
}

enum TableEntry<K: Hash + PartialEq, V> {
    Default,
    Initialized(K, V),
}

pub struct Table<K: Hash + PartialEq, V> {
    count: usize,
    capacity: usize,
    entries: NonNull<TableEntry<K, V>>,
    _marker: PhantomData<TableEntry<K, V>>,
}

impl<K: Hash + PartialEq + Debug, V: Debug> Debug for Table<K, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut repr = String::new();
        repr.push_str("Table {\n");

        for n in 0..self.capacity {
            unsafe {
                let entry = Table::<K, V>::entry_ptr_to_ref(self.entries.as_ptr().add(n));
                match entry {
                    TableEntry::Initialized(k, v) => {
                        let entry_repr = format!("{:?}@{} => {:?},\n", k, n, v);
                        repr.push_str(&entry_repr);
                    }
                    TableEntry::Default => (),
                }
            }
        }
        repr.push('}');

        f.write_str(&repr)
    }
}

impl<K: Hash + PartialEq + Display, V: Display> Display for Table<K, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut repr = String::new();
        repr.push('{');

        for n in 0..self.capacity {
            unsafe {
                let entry = Table::<K, V>::entry_ptr_to_ref(self.entries.as_ptr().add(n));
                match entry {
                    TableEntry::Initialized(k, v) => {
                        let entry_repr = format!("{} => {},\n", k, v);
                        repr.push_str(&entry_repr);
                    }
                    TableEntry::Default => (),
                }
            }
        }
        repr.push('}');

        f.write_str(&repr)
    }
}

impl<K: Hash + PartialEq, V> Table<K, V> {
    fn entry_ptr_to_ref<'a>(entry: *mut TableEntry<K, V>) -> &'a TableEntry<K, V> {
        unsafe { entry.as_ref().unwrap_unchecked() }
    }
}

impl<K: Hash + PartialEq + Clone, V: Clone> Table<K, V> {
    pub fn add_all(src: &Table<K, V>, dst: &mut Table<K, V>) {
        for n in 0..src.capacity {
            unsafe {
                let elem = Table::<K, V>::entry_ptr_to_ref(src.entries.as_ptr().add(n));
                if let TableEntry::Initialized(k, v) = elem {
                    dst.insert(k.clone(), v.clone());
                }
            }
        }
    }
}

impl<K: Hash + PartialEq, V> Drop for Table<K, V> {
    fn drop(&mut self) {
        if self.capacity != 0 {
            for n in 0..self.capacity {
                unsafe {
                    ptr::read(self.entries.as_ptr().add(n));
                }
            }

            let layout = Layout::array::<TableEntry<K, V>>(self.capacity).unwrap();
            unsafe { dealloc(self.entries.as_ptr() as *mut u8, layout) }
        }
    }
}

impl<K: Hash + PartialEq, V> Table<K, V> {
    pub fn new() -> Self {
        Self {
            count: 0,
            capacity: 0,
            entries: NonNull::dangling(),
            _marker: PhantomData,
        }
    }

    pub fn size(&self) -> usize {
        self.count
    }

    pub fn insert(&mut self, mut key: K, value: V) -> bool {
        if self.is_at_capacity() {
            let new_capacity = grow_capacity(self.capacity);
            unsafe { self.grow(new_capacity) };
        }

        let (_ignored, entry) = self.find_entry(&mut key, self.capacity);
        let is_new = unsafe {
            match entry.as_ref().unwrap() {
                TableEntry::Initialized(_ignored_k, _ignored_v) => false,
                TableEntry::Default => true,
            }
        };

        unsafe {
            if is_new {
                ptr::write(entry, TableEntry::Initialized(key, value));
            } else {
                let _ = ptr::replace(entry, TableEntry::Initialized(key, value));
            }
        }

        if is_new {
            self.count += 1;
        }

        is_new
    }

    pub fn get(&mut self, key: &mut K) -> Option<&V> {
        if self.count == 0 {
            None
        } else {
            let (_, entry_ptr) = self.find_entry(key, self.capacity);
            let entry_ref = Self::entry_ptr_to_ref(entry_ptr);
            match entry_ref {
                TableEntry::Initialized(_ignored, value) => Some(value),
                TableEntry::Default => None,
            }
        }
    }

    pub fn delete(&mut self, key: &mut K) -> bool {
        if self.count == 0 {
            false
        } else {
            let (idx, entry_ptr) = self.find_entry(key, self.capacity);
            unsafe {
                let entry = ptr::read(entry_ptr);
                match entry {
                    TableEntry::Initialized(_, _) => {
                        ptr::write(self.entries.as_ptr().add(idx), TableEntry::Default);
                        self.count -= 1;
                        true
                    }
                    TableEntry::Default => false,
                }
            }
        }
    }

    fn find_entry(&mut self, key: &mut K, capacity: usize) -> (usize, *mut TableEntry<K, V>) {
        let mut hasher = DefaultHasher::new();
        key.hash(&mut hasher);
        let mut index = hasher.finish() % capacity as u64;

        loop {
            let entry_ptr = unsafe { self.entries.as_ptr().add(index as usize) };
            let entry = unsafe { entry_ptr.as_ref().unwrap() };
            match entry {
                TableEntry::Initialized(entry_key, _ignored) => {
                    if entry_key == key {
                        return (index as usize, entry_ptr);
                    }
                }
                TableEntry::Default => return (index as usize, entry_ptr),
            }

            index = (index + 1) % capacity as u64;
        }
    }

    unsafe fn grow(&mut self, new_capacity: usize) {
        let new_layout = Layout::array::<TableEntry<K, V>>(new_capacity).unwrap();

        assert!(
            new_layout.size() <= isize::MAX as usize,
            "Cannot grow past {} elements",
            isize::MAX
        );

        let new_ptr = if self.capacity == 0 {
            alloc_zeroed(new_layout)
        } else {
            let old_layout = Layout::array::<TableEntry<K, V>>(self.capacity).unwrap();
            let old_ptr = self.entries.as_ptr() as *mut u8;
            realloc(old_ptr, old_layout, new_layout.size())
        };

        // Possibly updates pointer locations
        self.entries = match NonNull::new(new_ptr as *mut TableEntry<K, V>) {
            Some(ptr) => ptr,
            None => handle_alloc_error(new_layout),
        };

        // Initialize new memory with zeroes so that empty entries will return valid pointers.
        ptr::write_bytes(
            self.entries.as_ptr().add(self.capacity /* old capacity */),
            0,
            new_capacity - self.capacity,
        );

        self.capacity = new_capacity;

        // Relocate entries due to new table size
        for old_index in 0..self.capacity {
            let entry = unsafe {
                self.entries
                    .as_ptr()
                    .add(old_index)
                    .as_mut()
                    .unwrap_unchecked()
            };
            match entry {
                TableEntry::Default => continue,
                TableEntry::Initialized(k, _ignored_v) => {
                    let (new_index, _ignored) = self.find_entry(k, new_capacity);
                    if old_index != new_index {
                        ptr::copy::<TableEntry<K, V>>(
                            entry,
                            self.entries.as_ptr().add(new_index),
                            1,
                        );

                        ptr::write_bytes::<TableEntry<K, V>>(
                            self.entries.as_ptr().add(old_index),
                            0,
                            1,
                        );
                    }
                }
            }
        }
    }

    fn is_at_capacity(&self) -> bool {
        (self.count + 1) as f64 > self.capacity as f64 * MAX_TABLE_LOAD
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_alloc_in_empty_table() {
        let table = Table::<String, f64>::new();
        assert_eq!(table.size(), 0);
    }

    #[test]
    fn table_insert_primitive_value() {
        let mut table = Table::<u64, f64>::new();
        assert!(table.insert(42, 1.337));
        assert_eq!(table.size(), 1);
    }

    #[test]
    fn table_insert_non_primitive_value() {
        struct Element {
            value: String,
        }
        let mut table = Table::<u64, Element>::new();
        assert!(table.insert(
            0,
            Element {
                value: String::from("foobar")
            }
        ));
        assert_eq!(table.size(), 1);

        match table.get(&mut 0) {
            Some(elem) => {
                assert_eq!(elem.value, String::from("foobar"))
            }
            None => panic!("Expect element to be found"),
        }

        assert!(table.delete(&mut 0));
        assert_eq!(table.size(), 0);
    }

    #[test]
    fn table_delete() {
        let mut table = Table::<String, f64>::new();
        assert!(table.insert(String::from("foobar"), 1.337));
        assert_eq!(table.size(), 1);

        assert!(table.delete(&mut String::from("foobar")));
        assert_eq!(table.size(), 0);
    }

    #[test]
    fn enforce_table_grow() {
        let mut table = Table::<String, f64>::new();

        // Insert items to one above the growth threshold to force a Table::grow()
        let required_capacity = grow_capacity(0);
        for n in 0..required_capacity {
            let s = n.to_string();
            table.insert(s, 1.337);
        }

        assert_eq!(table.size(), required_capacity);
        assert_eq!(table.capacity, grow_capacity(required_capacity));
    }

    #[test]
    fn table_add_all() {
        let mut table = Table::<String, f64>::new();

        // Insert items to one above the growth threshold to force a Table::grow()
        let required_capacity = grow_capacity(0);
        for n in 0..required_capacity {
            let s = n.to_string();
            table.insert(s, 1.337);
        }

        let mut other_table = Table::<String, f64>::new();
        Table::add_all(&table, &mut other_table);

        assert_eq!(other_table.size(), table.size());
        for n in 0..required_capacity {
            let mut key = n.to_string();
            let lhs = table.get(&mut key);
            let rhs = other_table.get(&mut key);

            assert_eq!(lhs, rhs);
        }
    }
}
