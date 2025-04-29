MAKEFLAGS += --silent

all: build

.PHONY: setup

#-Wno-dev allows us to suppress Qt version warnings for non qt directories

setup:
	cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER:FILEPATH=/usr/bin/gcc -DCMAKE_CXX_COMPILER:STRING=/usr/bin/g++ -Wno-dev

.PHONY: build
build:
	cmake --build build 
	echo -e "\e[32m== SUCCESSFUL BUILDING! ==\e[0m\n"


.PHONY: test
test:
	ctest --test-dir build --output-on-failure
	echo -e "\e[32m== SUCCESSFUL TESTING! ==\e[0m\n"
