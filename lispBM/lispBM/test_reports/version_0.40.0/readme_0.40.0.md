# LispBM Release 0.40.0 Test logs

## Build Machine Information

- **Date**: 2026-09-06 17:31:19 CEST
- **Hostname**: joels-ThinkStation-P340
- **OS**: Linux 6.8.0-138-generic
- **Architecture**: x86_64
- **CPU**: Intel(R) Core(TM) i7-10700 CPU @ 2.90GHz
- **CPU Cores**: 16
- **Memory**: 62Gi
- **GCC Version**: gcc (Ubuntu 11.4.0-1ubuntu1~22.04.3) 11.4.0



## Tools versions
gcovr 8.4

Copyright (c) 2013-2025 the gcovr authors
Copyright (c) 2013 Sandia Corporation.
Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
the U.S. Government retains certain rights in this software.

Cppcheck 2.16.0

Infer version v1.2.0
Copyright 2009 - present Facebook. All Rights Reserved.

## CPPCHECK

See the cppcheck directory for results.
elapsed: 00 hours 04 min 26 sec

## 32BIT UNIT TESTS RESULTS
(OK - expected to fail) test_lisp_code_cps -t 50 -h 8192 tests/test_is_64bit.lisp
(OK - expected to fail) test_lisp_code_cps -t 50 -i -h 8192 tests/test_is_64bit.lisp
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_32.json
elapsed: 00 hours 06 min 57 sec

## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 07 min 43 sec

## 64BIT UNIT TESTS RESULTS
(OK - expected to fail) test_lisp_code_cps_64 -t 50 -h 8192 tests/test_is_32bit.lisp
(OK - expected to fail) test_lisp_code_cps_64 -t 50 -i -h 8192 tests/test_is_32bit.lisp
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_64.json
elapsed: 00 hours 10 min 03 sec

## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 10 min 47 sec

## ALWAYS GC UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 20 min 59 sec

## REPL TESTS
Tests passed: 181
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 21 min 09 sec

## IMAGE TESTS
Tests passed: 74
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 21 min 14 sec

## PERSIST TESTS
Tests passed: 181
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 21 min 29 sec

## SDL TESTS
Test OK: sdl_tests/test_ttf_text_vertical.lisp
Tests passed: 89
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!

Comparing rendered images against gold references:
DIFFERS FROM GOLD: test_img_circle_sector_angles.png (differing pixels: 1)
DIFFERS FROM GOLD: test_img_circle_sectors.png (differing pixels: 6)
DIFFERS FROM GOLD: test_img_circle_segment_outline.png (differing pixels: 46)
DIFFERS FROM GOLD: test_img_circle_segments.png (differing pixels: 98)

Gold comparison: 68 match, 4 differ, 0 without a gold reference
Differing images (gold: sdl_tests/gold/<name>, new: sdl_tests/png_out/<name>, highlighted diff: sdl_tests/png_diff/<name>):
  test_img_circle_sector_angles.png
  test_img_circle_sectors.png
  test_img_circle_segment_outline.png
  test_img_circle_segments.png
elapsed: 00 hours 22 min 22 sec

## C UNIT TESTS
Tests failed: 1
Expected fails: 1
Actual fails: 0
All C unit tests passed!
elapsed: 00 hours 23 min 25 sec

## Coverage collection
elapsed: 00 hours 23 min 34 sec

## scan-build version 14
scan-build: Run 'scan-view /home/joels/Current/lispbm/test_reports/version_0.40.0/scan-build/2026-09-06-175452-2751221-1' to examine bug reports.
elapsed: 00 hours 23 min 48 sec

## INFER ISSUES
elapsed: 00 hours 24 min 07 sec
