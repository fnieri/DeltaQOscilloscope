MAKEFLAGS += --silent

all: build

.PHONY: setup

#-Wno-dev allows us to suppress Qt version warnings for non qt directories

setup:
	cmake -B build -DCMAKE_BUILD_TYPE=Debug -Wno-dev

.PHONY: build
build:
	cmake --build build
	echo -e "\e[32m== SUCCESSFUL BUILDING! ==\e[0m\n"