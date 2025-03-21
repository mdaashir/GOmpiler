#include <stdio.h>

// This is a single-line comment

/*
   This is a
   multi-line comment
*/

int main() {
    char c = 'A';      // Character literal
    char str[] = "Hello, World!"; // String literal
    int x = 10;
    float y = 3.14;

    if (x > 5 && y < 4.0) {
        printf("%s\n", str);
    }

    return 0;
}
