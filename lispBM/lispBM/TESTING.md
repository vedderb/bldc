# Testing of LispBM

Since version 0.23.0 each X.Y.0 version of LispBM has been sent
through a full run of the testing-suite. Each X.Y.0 version is tagged
on Github for easy access.

Testing is no quarantee of correctness! It is just evidence that very
specific things work as they should within a certain context of
operation. It is no quarantee of freedom from errors. Please report
any errors via the issues tracker on
[github](https://github.com/svenssonjoel/lispBM/issues).

We take testing of LispBM seriously and improve upon our methodologies
incrementally over time.

LispBM is an "embeddable" scripting language, meaning that it is meant
to be made a part of a larger application. The goal of LispBM is to
provide a suitably sandboxed (with tentacles into the application
provided by the application developer) scripting language. The
testlogs directory provides logs of test results for given versions of
LispBM and can be used for traceability. Note that when it comes to
safety, reliability and security, the full application (which contains
LispBM) needs to be thuroughly analysed as a whole!

