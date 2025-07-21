class Doc {
    // Stores the lookup table copied from the end of the file.
    hash_map<unsigned, size_t> lookup_table;
    // Stores objects recently loaded from the file.
    hash_map<unsigned, Object*> object_cache;
    // Pointer to the open file.
    FILE* f;
    // Counts incoming pointers.  This object gets deleted when ref_count
    // drops to zero.
    int ref_count;
  public:
    // Gets the object from the cache or reads it from the file.
    Object* lookup(unsigned object_number);
};

class Object {
    // Pointer to the document that owns this object.
    Doc* doc; 
    // Counts incoming pointers.  This object gets deleted when ref_count
    // drops to zero.
    int ref_count; 
    // Writes the object to a PDF file.   
    virtual void write_to_file(FILE* f) = 0;
};

class String : Object {
    string s;
    void write_to_file(FILE* f);
};

class Array : Object {
    vector<Object*> array;
    void write_to_file(FILE* f);
};

class Link : Object {
    unsigned object_number;
    void write_to_file(FILE* f);
};

// ...