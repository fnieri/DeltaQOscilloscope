
#include "Outcome.h"
#include "System.h"
#include "SystemParser.h"
#include <filesystem>
#include <iostream>
#include <string>
#include <unistd.h>
namespace fs = std::filesystem;
int main()
{
    try {
        // Specify the path to the test JSON file
        fs::path filePath = fs::current_path() / "src/diagram/parsed_system.json";

        std::string pathString = filePath.string();
        // Parse the system from the JSON file
        System system = parseSystemJson(pathString);

        std::cout << "System successfully parsed and initialized!" << std::endl;
        std::string name = "O1";

    } catch (const std::exception &e) {
        // Catch and display any exceptions
        std::cerr << "Error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
