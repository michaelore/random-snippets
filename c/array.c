// Some macros that do stuff with arrays.

// Executes a piece of code once for each element of an array.
// 'it' is a pointer to the element, and 'i' is the current index.
// SIZE the number of elements, not the size in bytes.
#define doarray(TYPE, ARRAY, SIZE, CODE) \
    { \
	long i; \
	TYPE *it; \
	for(i = 0, it = ARRAY; i < SIZE; i++, it++) { \
	    CODE \
	} \
    }

// Instance of doarray for static arrays.
#define dostatarray(TYPE, ARRAY, CODE) doarray(TYPE, ARRAY, sizeof(ARRAY) / sizeof(TYPE), CODE)

// Instance of doarray for dynamice arrays.
#define dodynarray(TYPE, ARRAY, CODE) doarray(TYPE, ARRAY, ARRAY.elems, CODE)

// Defines dynamic arrays.
typedef struct { \
    void *array; \
    long elems; \
    long alloc; \
} dynamic_array;

// Expands a dynamic array.
#define dynamic_expand(TYPE, NAME, AMOUNT) { \
	void *new_array = realloc(NAME.array, NAME.alloc * sizeof(TYPE)); \
	if (!new_array) { \
	    fprintf(stderr, "ERROR: Could not expand array.\n"); \
	} else { \
	NAME.alloc += AMOUNT; \
	NAME.array = (TYPE*)new_array; \
        } \
    }

// Adds an element to a dynamic array, doubling its size with dynamic_expand if necessary.
#define dynamic_add(TYPE, NAME, ELEM) { \
	if (NAME.elems == NAME.alloc) { \
	    dynamic_expand(TYPE, NAME, NAME.alloc); \
	} \
	((TYPE *)NAME.array)[NAME.elems] = ELEM; \
	NAME.elems++; \
    }

//Initializes a dynamic array.
#define dynamic_init(TYPE, NAME, SIZE) \
    dynamic_array NAME; \
    NAME.elems = 0; \
    NAME.alloc = SIZE; \
    NAME.array = calloc(SIZE, sizeof(TYPE));
