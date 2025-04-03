
#ifndef PARSER_WRAPPER_H
#define PARSER_WRAPPER_H

#include <string>

// Declare function to call the Python parser
std::string parseAndSaveJson(const std::string &inputString, const std::string &filename);
std::string parseJson(const std::string &inputString);
#endif // PARSER_WRAPPER_H
