
#include "SystemParserInterface.h"

int main()
{
    std::string path = "src/parser/example.dq";
    auto result = SystemParserInterface::parseFile(path);

    if (!result) {
        std::cerr << "Failed to parse system.\n";
        return 1;
    }

    System system = *result;
    std::cout << "System parsed successfully.\n";

    return 0;
}
