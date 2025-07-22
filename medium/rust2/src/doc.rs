use std::collections::HashMap;
use std::fs::File;

struct Doc {
    // Stores the lookup table copied from the end of the file.
    lookup_table: HashMap<u32, usize>,
    // Stores objects recently loaded from the file.
    object_cache: HashMap<u32, Object>,
    // Pointer to the open file.
    f: File,
}

impl Doc {
    // Gets the object from the cache or reads it from the file.
    pub fn lookup(&self, object_number: u32) -> Object;
    // Overwrites the value of an object in the document.
    pub fn set(&mut self, object_number: u32, obj: Object);
}

enum Object {
    String(String),
    Array(Vec<Object>),
    Link(u32),
    // ...
}

impl Object {
    pub fn write_to_file(&mut self, &mut File);
}
