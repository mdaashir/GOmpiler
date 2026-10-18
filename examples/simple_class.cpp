#include <iostream>
#include <string>

/**
 * A simple class example to demonstrate class support
 */
class Person {
private:
    std::string name;
    int age;

public:
    // Constructor
    Person(const std::string& name, int age) : name(name), age(age) {}
    
    // Member functions
    void setName(const std::string& newName) {
        name = newName;
    }
    
    void setAge(int newAge) {
        if (newAge >= 0) {
            age = newAge;
        }
    }
    
    std::string getName() const {
        return name;
    }
    
    int getAge() const {
        return age;
    }
    
    void printInfo() const {
        std::cout << "Name: " << name << ", Age: " << age << std::endl;
    }
};

int main() {
    // Create a Person object
    Person person1("Alice", 30);
    person1.printInfo();
    
    // Modify the object
    person1.setName("Alice Smith");
    person1.setAge(31);
    person1.printInfo();
    
    // Create another Person object
    Person person2("Bob", 25);
    person2.printInfo();
    
    return 0;
}
