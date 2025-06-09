## How to build

The DeltaQ oscilloscope is a C++ graphical interface to observe running Erlang programs, giving real time insights about the instrumented Erlang system under test. The oscilloscope needs to be paired with the dqsd_otel Erlang wrapper, which can be found [here](https://github.com/fnieri/dqsd_otel).

The wrapper sends data about the execution of the Erlang system and the oscilloscope will perform real time statistical computations to give detailed insights about the running problem.

This project is part of a master thesis.

### Dependencies

To build this project you need a few dependencies

 - ANTLR4
 - GTest
 - Qt6

They need to be installed before compiling the project.
The rest of the dependencies will be added by [CPM](https://github.com/cpm-cmake/CPM.cmake) when building
### Build 

To build you need to run the following commands from source
```bash
    make setup
    make build
```
After building, the oscilloscope will be put inside build/bin/ and can be run by
```bash
    ./DQOscilloscope
    ./build/bin/DQOscilloscope # if at root 
```

#### Tests

After building, you can run tests with
```bash
    make test
```
