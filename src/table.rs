use crate::object::Hashed;
use core::fmt::{Debug, Display, Formatter};
use std::alloc::{alloc_zeroed, dealloc, handle_alloc_error, realloc, Layout};
use std::marker::PhantomData;
use std::ptr;
use std::ptr::NonNull;

const MAX_TABLE_LOAD: f64 = 0.75;

fn next_capacity(capacity: usize) -> usize {
    if capacity < 8 {
        8
    } else {
        capacity * 2
    }
}

enum TableEntry<K: Hashed + PartialEq, V> {
    Uninitialized,
    Initialized(K, V),
}

pub struct Table<K: Hashed + PartialEq, V> {
    count: usize,
    capacity: usize,
    entries: NonNull<TableEntry<K, V>>,
    _marker: PhantomData<TableEntry<K, V>>,
}

impl<K: Hashed + PartialEq + Debug, V: Debug> Debug for Table<K, V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        let mut repr = String::from("Table {\n");

        for n in 0..self.capacity {
            unsafe {
                let entry = Table::<K, V>::entry_ptr_to_ref(self.entries.as_ptr().add(n));
                match entry {
                    TableEntry::Initialized(k, v) => {
                        let entry_repr = format!("{:?}@{} => {:?},\n", k, n, v);
                        repr.push_str(&entry_repr);
                    }
                    TableEntry::Uninitialized => (),
                }
            }
        }
        repr.push('}');

        f.write_str(&repr)
    }
}

impl<K: Hashed + PartialEq + Display, V: Display> Display for Table<K, V> {
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
                    TableEntry::Uninitialized => (),
                }
            }
        }
        repr.push('}');

        f.write_str(&repr)
    }
}

impl<K: Hashed + PartialEq, V> Table<K, V> {
    fn entry_ptr_to_ref<'a>(entry: *mut TableEntry<K, V>) -> &'a TableEntry<K, V> {
        unsafe { entry.as_ref().unwrap_unchecked() }
    }
}

impl<K: Hashed + PartialEq + Clone, V: Clone> Table<K, V> {
    pub fn add_all(src: &Table<K, V>, dst: &mut Table<K, V>) {
        for n in 0..src.capacity {
            unsafe {
                let elem = &*src.entries.as_ptr().add(n);
                match elem {
                    TableEntry::Initialized(k, v) => {
                        dst.insert(k.clone(), v.clone());
                    }
                    TableEntry::Uninitialized => (),
                }
            }
        }
    }

    pub fn from(src: &Table<K, V>) -> Table<K, V> {
        let mut table = Table::new();
        Self::add_all(src, &mut table);

        table
    }
}

impl<K: Hashed + PartialEq, V> Drop for Table<K, V> {
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

impl<K: Hashed + PartialEq, V> Table<K, V> {
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

    pub fn insert(&mut self, key: K, value: V) -> bool {
        let (is_new, _ignored) = unsafe { self.insert_internal(key, value) };
        is_new
    }

    pub fn get_or_insert(&mut self, key: K, value: V) -> &K {
        let (_ignored, entry) = unsafe { self.insert_internal(key, value) };
        match entry {
            TableEntry::Initialized(k, _v) => k,

            // This should have been prevented by a panic during memory allocation in
            // Table::grow(). But since you never know, panic here anyway.
            TableEntry::Uninitialized => panic!("Could not get or insert into Table."),
        }
    }

    pub fn get(&mut self, key: &K) -> Option<&V> {
        if self.count == 0 {
            None
        } else {
            let (_, entry_ptr) = self.find_entry(key, self.capacity);
            let entry_ref = Self::entry_ptr_to_ref(entry_ptr);
            match entry_ref {
                TableEntry::Initialized(_ignored, value) => Some(value),
                TableEntry::Uninitialized => None,
            }
        }
    }

    pub fn delete(&mut self, key: &K) -> bool {
        if self.count == 0 {
            false
        } else {
            let (idx, entry_ptr) = self.find_entry(key, self.capacity);
            unsafe {
                let entry = ptr::read(entry_ptr);
                match entry {
                    TableEntry::Initialized(_, _) => {
                        ptr::write(self.entries.as_ptr().add(idx), TableEntry::Uninitialized);
                        self.count -= 1;
                        true
                    }
                    TableEntry::Uninitialized => false,
                }
            }
        }
    }

    unsafe fn insert_internal(&mut self, key: K, value: V) -> (bool, &TableEntry<K, V>) {
        if self.is_at_capacity() {
            let new_capacity = next_capacity(self.capacity);
            self.grow(new_capacity);
        }

        let (_idx, entry) = self.find_entry(&key, self.capacity);
        let is_new = match entry.as_ref().unwrap() {
            TableEntry::Initialized(_ignored_k, _ignored_v) => false,
            TableEntry::Uninitialized => true,
        };

        if is_new {
            self.count += 1;
        } else {
            ptr::drop_in_place(entry);
        }

        ptr::write(entry, TableEntry::Initialized(key, value));

        (is_new, &*entry)
    }

    fn find_entry(&mut self, key: &K, capacity: usize) -> (usize, *mut TableEntry<K, V>) {
        let mut index = key.calculated_hash() % capacity as u64;

        loop {
            let entry_ptr = unsafe { self.entries.as_ptr().add(index as usize) };
            let entry = unsafe { entry_ptr.as_ref().unwrap() };
            match entry {
                TableEntry::<K, V>::Uninitialized => return (index as usize, entry_ptr),
                TableEntry::<K, V>::Initialized(entry_key, _ignored) => {
                    if entry_key == key {
                        return (index as usize, entry_ptr);
                    }
                }
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
        for n in self.capacity..new_capacity {
            ptr::write(self.entries.as_ptr().add(n), TableEntry::Uninitialized);
        }

        self.capacity = new_capacity;

        // Relocate entries due to new table size
        for old_index in 0..self.capacity {
            let entry = self
                .entries
                .as_ptr()
                .add(old_index)
                .as_mut()
                .unwrap_unchecked();
            match entry {
                TableEntry::Uninitialized => continue,
                TableEntry::Initialized(k, _ignored_v) => {
                    let (new_index, _ignored) = self.find_entry(k, new_capacity);
                    if old_index != new_index {
                        ptr::copy::<TableEntry<K, V>>(
                            entry,
                            self.entries.as_ptr().add(new_index),
                            1,
                        );

                        ptr::write(
                            self.entries.as_ptr().add(old_index),
                            TableEntry::Uninitialized,
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
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    impl Hashed for String {
        fn calculated_hash(&self) -> u64 {
            let mut hasher = DefaultHasher::new();
            self.hash(&mut hasher);

            hasher.finish()
        }
    }

    impl Hashed for u64 {
        fn calculated_hash(&self) -> u64 {
            *self
        }
    }

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

        match table.get(&0) {
            Some(elem) => {
                assert_eq!(elem.value, String::from("foobar"))
            }
            None => panic!("Expect element to be found"),
        }

        assert!(table.delete(&0));
        assert_eq!(table.size(), 0);
    }

    #[test]
    fn table_delete() {
        let mut table = Table::<String, f64>::new();
        assert!(table.insert(String::from("foobar"), 1.337));
        assert_eq!(table.size(), 1);

        assert!(table.delete(&String::from("foobar")));
        assert_eq!(table.size(), 0);
    }

    #[test]
    fn enforce_table_grow() {
        let mut table = Table::<String, f64>::new();

        // Insert items to one above the growth threshold to force a Table::grow()
        let required_capacity = next_capacity(0);
        for n in 0..required_capacity {
            let s = n.to_string();
            table.insert(s, 1.337);
        }

        assert_eq!(table.size(), required_capacity);
        assert_eq!(table.capacity, next_capacity(required_capacity));
    }

    #[test]
    fn table_add_all() {
        let mut table = Table::<String, f64>::new();

        // Insert items to one above the growth threshold to force a Table::grow()
        let required_capacity = next_capacity(0);
        for n in 0..required_capacity {
            let s = n.to_string();
            table.insert(s, 1.337);
        }

        let mut other_table = Table::<String, f64>::new();
        Table::add_all(&table, &mut other_table);

        assert_eq!(other_table.size(), table.size());
        for n in 0..required_capacity {
            let key = n.to_string();
            let lhs = table.get(&key);
            let rhs = other_table.get(&key);

            assert_eq!(lhs, rhs);
        }
    }
}
