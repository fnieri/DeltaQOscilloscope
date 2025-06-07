#!/usr/bin/bash

$(doxygen Doxyfile &> doxygen.log)
$(firefox docs/html/index.html)
