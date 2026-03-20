# LispBM Release 0.35.0 Test logs

## Build Machine Information

- **Date**: 2025-12-14 14:51:35 CET
- **Hostname**: joels-ThinkPad-T480s
- **OS**: Linux 6.14.0-33-generic
- **Architecture**: x86_64
- **CPU**: Intel(R) Core(TM) i5-8350U CPU @ 1.70GHz
- **CPU Cores**: 8
- **Memory**: 7.6Gi
- **GCC Version**: gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0

## CPPCHECK

See the cppcheck directory for results.
elapsed: 00 hours 01 min 04 sec

## 32BIT UNIT TESTS RESULTS
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_32.json
elapsed: 00 hours 04 min 58 sec

## 32BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 06 min 15 sec

## 64BIT UNIT TESTS RESULTS
Expected fails: 40
Actual fails: 0
Generating coverage report...
Coverage report generated: tests_cov_64.json
elapsed: 00 hours 09 min 38 sec

## 64BIT TIME BASED SCHEDULER UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 10 min 52 sec

## ALWAYS GC UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 37 min 48 sec

## POINTER REVERSAL GC UNIT TESTS RESULTS
Tests passed: 21996
Tests failed: 40
Expected fails: 40
Actual fails: 0
elapsed: 00 hours 41 min 12 sec

## REPL TESTS
Tests passed: 147
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 41 min 24 sec

## IMAGE TESTS
Tests passed: 71
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 41 min 30 sec

## SDL TESTS
Tests passed: 64
Expected failures: 1
Unexpected failures: 0
All tests completed as expected!
elapsed: 00 hours 41 min 57 sec

## C UNIT TESTS
Tests failed: 1
Expected fails: 1
Actual fails: 0
All C unit tests passed!
elapsed: 00 hours 43 min 04 sec

## Coverage collection
elapsed: 00 hours 43 min 07 sec

## scan-build version 18
scan-build: No bugs found.
elapsed: 00 hours 43 min 30 sec

## INFER ISSUES
elapsed: 00 hours 44 min 05 sec
