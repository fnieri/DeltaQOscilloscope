# Compiler and flags
CXX = g++
CXXFLAGS = -g -std=c++17 -Wall -Wextra -O2

# Target executable
TARGET = main

# Directories
SRC_DIR = c++
DIAGRAM_DIR = $(SRC_DIR)/diagram
EXCEPTIONS_DIR = $(SRC_DIR)/exceptions
MATHS_DIR = $(SRC_DIR)/maths
TESTS_DIR = $(SRC_DIR)/tests
BUILD_DIR = build

# Source files
SRCS = $(SRC_DIR)/main.cpp \
       $(DIAGRAM_DIR)/AllToFinish.cpp \
       $(DIAGRAM_DIR)/DiagramComponent.cpp \
       $(DIAGRAM_DIR)/Event.cpp \
       $(DIAGRAM_DIR)/EventSample.cpp \
       $(DIAGRAM_DIR)/FirstToFinish.cpp \
       $(DIAGRAM_DIR)/Outcome.cpp \
       $(DIAGRAM_DIR)/ProbabilisticOperator.cpp \
       $(DIAGRAM_DIR)/Probe.cpp \
       $(EXCEPTIONS_DIR)/ComponentAlreadyExists.cpp \
       $(MATHS_DIR)/DeltaQ.cpp \
       $(MATHS_DIR)/DeltaQOperations.cpp \
       $(SRC_DIR)/System.cpp

# Object files (generated in the build directory)
OBJS = $(patsubst $(SRC_DIR)/%.cpp, $(BUILD_DIR)/%.o, $(SRCS))

# Include directories
INCLUDE_DIRS = -I$(SRC_DIR) -I$(DIAGRAM_DIR) -I$(EXCEPTIONS_DIR) -I$(MATHS_DIR)

# Default target
all: $(TARGET)

# Link the target executable
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $@ $^

# Compile object files into the build directory
$(BUILD_DIR)/%.o: $(SRC_DIR)/%.cpp
	@mkdir -p $(dir $@)
	$(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) -c $< -o $@

# Run tests (optional)
test: $(TARGET)
	$(CXX) $(CXXFLAGS) $(INCLUDE_DIRS) $(TESTS_DIR)/DeltaQTests.cpp -o tests.out
	./tests.out

# Clean up build artifacts
clean:
	rm -rf $(BUILD_DIR) $(TARGET) tests.out

# Rebuild everything
rebuild: clean all
