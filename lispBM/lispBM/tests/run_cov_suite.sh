#!/bin/bash

./run_tests.sh

./run_tests64.sh

./run_c_unit.sh

./run_image_tests.sh

./run_repl_tests.sh

./run_sdl_tests.sh

./collect_coverage.sh
