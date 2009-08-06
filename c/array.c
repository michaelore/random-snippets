// Some macros that do stuff with arrays.

// Executes a piece of code once for each element of an array.
// 'it' is a pointer to the element, and 'i' is the current index.
// SIZE the number of elements, not the size in bytes.
#define doarray(TYPE, ARRAY, SIZE, CODE) \
    { \
	int i; \
	TYPE *it; \
	for(i = 0, it = ARRAY; i < SIZE; i++, it++) { \
	    CODE \
	} \
    }

// Instance of doarray for static arrays.
#define dostatarray(TYPE, ARRAY, CODE) doarray(TYPE, ARRAY, sizeof(ARRAY) / sizeof(TYPE), CODE)

// Instance of doarray for dynamice arrays.
#define dodynarray(TYPE, ARRAY, CODE) doarray(TYPE, ARRAY, ARRAY##_alloc, CODE)

// Defines a dynamic array and a function that adds an element to it.
// If there is not enough room to add an element, the array doubles in length.
// The name of the function is (name_of_array)_add.
// SIZE is the number of initial elements, not the number of bytes.
#define defdynamicarray(TYPE, ARRAY, SIZE) \
    int ARRAY##_elems = 0; \
    int ARRAY##_alloc = SIZE; \
    TYPE *ARRAY = calloc(SIZE, sizeof(TYPE)); \
    int ARRAY##_expand(int amount) { \
	ARRAY##_alloc += amount; \
	void *new_array = realloc(ARRAY, ARRAY##_alloc * sizeof(TYPE)); \
	if (!new_array) { \
	    fprintf(stderr, "ERROR: Could not expand array.\n"); \
	    return -1; \
	} \
	ARRAY = (TYPE*)new_array; \
	return 0; \
    } \
    int ARRAY##_add(TYPE elem) { \
	if (ARRAY##_elems == ARRAY##_alloc) { \
	    if (ARRAY##_expand(ARRAY##_alloc)) { \
		return -1; \
	    } \
	} \
	ARRAY[ARRAY##_elems] = elem; \
	ARRAY##_elems++; \
	return 0; \
    }
