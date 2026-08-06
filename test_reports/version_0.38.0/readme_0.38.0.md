# LispBM Release 0.38.0 Test logs

## Build Machine Information

- **Date**: 2026-07-11 22:30:17 CEST
- **Hostname**: joels-ThinkPad-T480s
- **OS**: Linux 6.17.0-35-generic
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
elapsed: 00 hours 01 min 53 sec

## 32BIT UNIT TESTS RESULTS
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_32.json
elapsed: 00 hours 06 min 09 sec

## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 07 min 31 sec

## 64BIT UNIT TESTS RESULTS
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_64.json
elapsed: 00 hours 11 min 15 sec

## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 12 min 34 sec

## ALWAYS GC UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 36 min 55 sec

## POINTER REVERSAL GC UNIT TESTS RESULTS
Tests passed: 22024
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 40 min 32 sec

## REPL TESTS
Tests passed: 178
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 40 min 51 sec

## IMAGE TESTS
Tests passed: 74
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 41 min 01 sec

## PERSIST TESTS
Tests passed: 178
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 41 min 33 sec

## SDL TESTS
Tests passed: 65
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 42 min 08 sec

## C UNIT TESTS
Tests failed: 1
Expected fails: 1
Actual fails: 0
All C unit tests passed!
elapsed: 00 hours 43 min 41 sec

## Coverage collection
elapsed: 00 hours 43 min 47 sec

## scan-build version 18
scan-build: No bugs found.
elapsed: 00 hours 44 min 28 sec

## INFER ISSUES
elapsed: 00 hours 45 min 08 sec
