# LispBM Release 0.37.0 Test logs

## Build Machine Information

- **Date**: 2026-07-02 09:40:23 CEST
- **Hostname**: joels-ThinkStation-P340
- **OS**: Linux 6.8.0-124-generic
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
elapsed: 00 hours 09 min 45 sec

## 32BIT UNIT TESTS RESULTS
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_32.json
elapsed: 00 hours 12 min 15 sec

## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 13 min 00 sec

## 64BIT UNIT TESTS RESULTS
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_64.json
elapsed: 00 hours 15 min 19 sec

## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 16 min 03 sec

## ALWAYS GC UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 26 min 19 sec

## POINTER REVERSAL GC UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 28 min 41 sec

## REPL TESTS
Tests passed: 169
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 28 min 50 sec

## IMAGE TESTS
Tests passed: 71
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 28 min 54 sec

## SDL TESTS
Tests passed: 67
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 29 min 35 sec

## C UNIT TESTS
Tests failed: 1
Expected fails: 1
Actual fails: 0
All C unit tests passed!
elapsed: 00 hours 30 min 26 sec

## Coverage collection
elapsed: 00 hours 30 min 34 sec

## scan-build version 14
scan-build: No bugs found.
elapsed: 00 hours 30 min 48 sec

## INFER ISSUES
elapsed: 00 hours 31 min 06 sec
