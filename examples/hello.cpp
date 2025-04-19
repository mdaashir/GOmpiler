#include <iostream>

/**
 * A simple Hello World program to demonstrate the compiler
 */
int main() {
    std::cout << "Hello, World!" << std::endl;
    
    // Variable declaration and initialization
    int a = 10;
    int b = 20;
    int result = a + b;
    
    // Control structures
    if (result > 25) {
        std::cout << "Result is greater than 25: " << result << std::endl;
    } else {
        std::cout << "Result is not greater than 25: " << result << std::endl;
    }
    
    // Loop
    std::cout << "Counting down: ";
    for (int i = 5; i > 0; i--) {
        std::cout << i << " ";
    }
    std::cout << std::endl;
    
    return 0;
}
