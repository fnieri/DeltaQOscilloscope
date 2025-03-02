
#ifndef PARSER_WRAPPER_H
#define PARSER_WRAPPER_H

#include <string>

// Declare function to call the Python parser
std::string parseAndSaveJson(const std::string &input_string, const std::string &filename = "parsed_system.json");

#endif // PARSER_WRAPPER_H
