
#include "../parser/ParserWrapper.h" // Include the parser
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
        fs::path filePath = fs::current_path() / "src/diagram/data.json";

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
/*
int main()
{
    std::string input = "probe1 = o1 -> o2 -> s:o3; system = s:probe1";
    std::string output_file = "parsed_system.json";

    std::string result = parseAndSaveJson(input, output_file);
    std::cout << "Parser output saved to: " << result << std::endl;

    return 0;
} */
