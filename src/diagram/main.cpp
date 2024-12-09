
#include "SystemParser.h"
#include <iostream>
#include <string>
#include "System.h"
#include "Outcome.h"
#include <unistd.h>
#include <filesystem>
namespace fs = std::filesystem;
int main() {
    try {
        // Specify the path to the test JSON file
        fs::path filePath = fs::current_path() / "src/diagram/system.json";

        std::string pathString = filePath.string();
        // Parse the system from the JSON file
        System system = parseSystemJson(pathString);


        std::cout << "System successfully parsed and initialized!" << std::endl;
        std::string name = "O1"; 
        std::cout << system.getOutcome(name)->getName() << std::endl;
        
        // Additional test cases
        // 1. Verify that the first component was set correctly.
        // 2. Print the components and connections for manual verification.
        // (This assumes System has a method to print its structure.)
        // e.g., system.printStructure();

    } catch (const std::exception &e) {
        // Catch and display any exceptions
        std::cerr << "Error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

