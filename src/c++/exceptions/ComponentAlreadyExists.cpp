/**
 * @author Francesco Nieri
 * @date 26/10/2024
 * Exception thrown when adding a component to a system which already has a component with the same name
 */

#include <iostream>


class ComponentAlreadyExists : public std::exception {
    private:
    std::string message;

    public:
    explicit ComponentAlreadyExists(const std::string& msg) : message(msg) {}

    const char* what() const noexcept override {
        return message.c_str();  // Return C-string for compatibility with std::exception
    }
};