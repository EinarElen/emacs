#!/bin/bash
cd sources/Bear/ && mkdir build && cd build && cmake .. && make -j4 && sudo make install
cd -
cd sources/rtags/ && mkdir build && cd build && cmake .. && make -j4 && sudo make install
