
#include "ParserWrapper.h"
#include <Python.h>
#include <iostream>
#include <string>
// Function to initialize Python, call the function, and handle errors
std::string parseAndSaveJson(const std::string &input_string, const std::string &filename)
{
    PyObject *pName, *pModule, *pFunc;
    PyObject *pArgs, *pValue;
    std::string result = "Unknown error";

    // Initialize the Python Interpreter
    Py_Initialize();

    // Add the directory where `parser.py` is located to the Python path
    PyRun_SimpleString("import sys; sys.path.append('src/parser');"); // Adjust this path based on where `parser.py` is located

    const char *scriptPath = "parser"; // Ensure this matches the Python filename (i.e., parser.py)
    pName = PyUnicode_DecodeFSDefault(scriptPath);
    if (pName == nullptr) {
        std::cerr << "Failed to convert script name to Python string!" << std::endl;
        return "Error: Invalid script name!";
    }

    // Import the Python module
    pModule = PyImport_Import(pName);
    Py_DECREF(pName);

    if (pModule != nullptr) {
        // Get the function from the module
        pFunc = PyObject_GetAttrString(pModule, "parse_and_save_json");

        // Ensure it's a callable function
        if (pFunc && PyCallable_Check(pFunc)) {
            // Create a tuple of arguments to pass to the Python function
            pArgs = PyTuple_New(2); // 2 arguments: input_string and filename
            pValue = PyUnicode_FromString(input_string.c_str());
            PyTuple_SetItem(pArgs, 0, pValue); // First argument: input_string
            pValue = PyUnicode_FromString(filename.c_str());
            PyTuple_SetItem(pArgs, 1, pValue); // Second argument: filename

            // Call the Python function
            pValue = PyObject_CallObject(pFunc, pArgs);
            Py_DECREF(pArgs);

            // Handle the result
            if (pValue != nullptr) {
                result = PyUnicode_AsUTF8(pValue);
                Py_DECREF(pValue);
            } else {
                PyErr_Print();
                result = "Error: Python function call failed!";
            }
        } else {
            if (PyErr_Occurred()) {
                PyErr_Print();
            }
            result = "Error: Function not found or not callable!";
        }

        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
    } else {
        PyErr_Print();
        result = "Error: Failed to load Python module!";
    }

    // Finalize the Python Interpreter
    Py_FinalizeEx();

    return result;
}
