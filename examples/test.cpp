// Simple test program for the compiler
#include <iostream>

// Function declaration
int add(int a, int b);

// Global variable declaration
const int MAX_VALUE = 100;

/**
 * Main function that demonstrates various language features
 */
int main() {
    // Variable declarations with initialization
    int x = 10;
    int y = 20;
    
    // Function call
    int sum = add(x, y);
    
    // Output with stream operators
    std::cout << "The sum of " << x << " and " << y << " is: " << sum << std::endl;
    
    // If statement
    if (sum > MAX_VALUE) {
        std::cout << "Sum exceeds maximum value" << std::endl;
    } else {
        std::cout << "Sum is within acceptable range" << std::endl;
    }
    
    // For loop
    std::cout << "Counting: ";
    for (int i = 0; i < 5; i++) {
        std::cout << i << " ";
    }
    std::cout << std::endl;
    
    return 0;
}

// Function definition
int add(int a, int b) {
    return a + b;
}
