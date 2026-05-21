MAKEFLAGS += --silent

all: build

# Override QT_PREFIX if Qt6 is not on the system path, e.g.:
#   macOS (Homebrew):  make setup QT_PREFIX=$(brew --prefix qt6)
#   Windows (Qt installer): make setup QT_PREFIX=C:/Qt/6.x.x/msvc2022_64
QT_PREFIX ?=

.PHONY: setup

#-Wno-dev allows us to suppress Qt version warnings for non qt directories

setup:
	cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_C_COMPILER:FILEPATH=/usr/bin/gcc -DCMAKE_CXX_COMPILER:STRING=/usr/bin/g++ $(if $(QT_PREFIX),-DCMAKE_PREFIX_PATH=$(QT_PREFIX),) -DCMAKE_POLICY_VERSION_MINIMUM=3.5 -Wno-dev

.PHONY: build
build:
	cmake --build build 
	echo -e "\e[32m== SUCCESSFUL BUILDING! ==\e[0m\n"


.PHONY: test
test:
	ctest --test-dir build --output-on-failure
	echo -e "\e[32m== SUCCESSFUL TESTING! ==\e[0m\n"
