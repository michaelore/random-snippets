#include "array.c"
#include <stdio.h>

int main() {
    int statarray[6];
    dynamic_init(int, dyn, 2);
    dostatarray(int, statarray, {*it = i; dynamic_add(int, dyn, 5 - i);});
    int i;
    for(i = 0; i <= 5; i++) {
	printf("%d\n", statarray[i] + ((int *)dyn.array)[i]);
    }
}
