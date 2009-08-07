#include "array.c"
#include <stdio.h>

int main() {
    int statarray[6];
    defdynamicarray(int, dynarray, 6);
    dostatarray(int, statarray, {*it = i; dynarray_add(5 - i);});
    int i;
    for(i = 0; i <= 5; i++) {
	printf("%d\n", statarray[i] + dynarray[i]);
    }
}
