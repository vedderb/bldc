# LispBM Test reports

This directory stores LBM test reports. Under
the `test_reports` directory, you find directories named `version_X.Y.Z`.
The test reports in directory `version_X.Y.Z` corresponds to the git commit
tagged (git tag) with tag X.Y.Z.

## Testing approach

We utilize the following methods of testing:
1. Unit tests
2. Static analysis using [CLANG scan-build](https://clang-analyzer.llvm.org/scan-build.html)
3. Static analysis using [Infer](https://github.com/facebook/infer)
4. Work in progress - Property based testing using [Haskell QuickCheck](https://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
