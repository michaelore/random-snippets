// Some macros that do stuff with arrays.

// Executes a piece of code once for each element of an array.
// The element is bound to 'it' and the index is bound to 'i'.
// Does not work with dynamic arrays at the moment.
#define doarray(TYPE, ARRAY, CODE) \
{ \
    int i; \
    TYPE it; \
    for(i = 0; i < sizeof(ARRAY) / sizeof(TYPE); i++) { \
	it = ARRAY[i]; \
	CODE \
    } \
}

// Defines an array and a function that adds an element to it, adding more space to it if necessary.
// The name of the function is (name_of_array)_add.
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
