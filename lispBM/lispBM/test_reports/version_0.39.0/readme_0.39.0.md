# LispBM Release 0.39.0 Test logs

## Build Machine Information

- **Date**: 2026-09-05 07:58:58 CEST
- **Hostname**: joels-ThinkPad-T480s
- **OS**: Linux 7.0.0-28-generic
- **Architecture**: x86_64
- **CPU**: Intel(R) Core(TM) i5-8350U CPU @ 1.70GHz
- **CPU Cores**: 8
- **Memory**: 15Gi
- **GCC Version**: gcc (Ubuntu 13.3.0-6ubuntu2~24.04.1) 13.3.0



## Tools versions
gcovr 7.0

Copyright (c) 2013-2024 the gcovr authors
Copyright (c) 2013 Sandia Corporation.
Under the terms of Contract DE-AC04-94AL85000 with Sandia Corporation,
the U.S. Government retains certain rights in this software.

Cppcheck 2.13.0

Infer version v1.2.0
Copyright 2009 - present Facebook. All Rights Reserved.

## CPPCHECK

See the cppcheck directory for results.
elapsed: 00 hours 00 min 45 sec

## 32BIT UNIT TESTS RESULTS
(OK - expected to fail) test_lisp_code_cps -t 50 -h 8192 tests/test_is_64bit.lisp
(OK - expected to fail) test_lisp_code_cps -t 50 -i -h 8192 tests/test_is_64bit.lisp
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_32.json
elapsed: 00 hours 05 min 12 sec

## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 06 min 36 sec

## 64BIT UNIT TESTS RESULTS
(OK - expected to fail) test_lisp_code_cps_64 -t 50 -h 8192 tests/test_is_32bit.lisp
(OK - expected to fail) test_lisp_code_cps_64 -t 50 -i -h 8192 tests/test_is_32bit.lisp
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_64.json
elapsed: 00 hours 10 min 20 sec

## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 11 min 41 sec

## ALWAYS GC UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 36 min 01 sec

## REPL TESTS
Tests passed: 182
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 36 min 18 sec

## IMAGE TESTS
Tests passed: 74
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 36 min 26 sec

## PERSIST TESTS
Tests passed: 182
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 36 min 53 sec

## SDL TESTS
Tests passed: 89
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!

Comparing rendered images against gold references:

Gold comparison: 72 match, 0 differ, 0 without a gold reference
elapsed: 00 hours 37 min 45 sec

## C UNIT TESTS
Tests failed: 1
Expected fails: 1
Actual fails: 0
All C unit tests passed!
elapsed: 00 hours 39 min 25 sec

## Coverage collection
elapsed: 00 hours 39 min 29 sec

## scan-build version 18
scan-build: No bugs found.
elapsed: 00 hours 40 min 08 sec

## INFER ISSUES
elapsed: 00 hours 40 min 46 sec
