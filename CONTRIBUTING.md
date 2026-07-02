
# Contributing

  Contributions to LispBM are very welcome but as the project is
  growing larger we would like to ask contributors to follow the
  following guideline.

## Slow and methodical

   The overarching design principle of the core of LispBM can be
   summarized as "slow and methodical". The main goals of the runtime
   system core is to be reliable, sandboxed, safe and secure. At the
   same time the runtime system is doing a lot of very complicated
   operations such as process management and scheduling, garbage
   collection etc. These are things that should not be rushed. Changes
   should be motivated and planned, followed by rigorous testing.

   The core of LispBM is the source code in the /src and /include
   directories (excluding the extensions subdirectory).

   Outside of the core a faster and more easy-going approach is fine.

## Getting in touch

   If you have a good idea for a larger or more intrusive contribution
   please get in touch so that we can coordinate our efforts and
   align.  Use issues, or discussion functions in github for reaching
   out.

## How to submit

   Submit your contribution as a pull request on github.

## Bug reports

   Please file bug reports as an issue on github. If the bug has
   security implications please follow the procedure from SECURITY.md.

## Copyright and ownership

   1. RIGHT TO SUBMIT: The contributor must personally have the right to
      contribute the code.
   2. LICENSE COMPATIBILITY: The code submitted must be under a license
      compatible with GPL v3.
   3. Points 1 and 2 also apply to contributions including third party code
      as well as AI generated code. See sections **Third-party policy**
      and **AI policy** 
   
## Style

   The following are guidelines for styling of C code.

   Indentation: 2 spaces

   Naming:

     1. Functions, variables and types: snake_case.
     2. Macros and constants: UPPER_CASE.
     3. Type names are suffixed with `_t`.
     4. Public API is prefixed with `lbm_`
     5. Static internal names need no prefix.

## Comments

   Do not comment obvious things like `a = 5; // assign 5 to a`
   Comments should only be there for things that are genuinely tricky
   to grasp from just looking at the code. There are many examples of
   this in eval_cps.c.

   Bulk comments can also be added before a particularly tricky
   algorithm documenting the thinking leading up to its implementation
   and design.

## Testing

   We encourage contributors to include additional tests targeting
   the contributed code.

   At a minimum, the existing test-suites should be run on the code
   before issuing a pull request.

## Static analysis

   It is recommended that the static analysis tools (cppcheck, infer
   and scanbuild) are run on any contributions before issuing a pull
   request.

## Third-party policy

   Using third-party code is a great way to add value with little
   effort and in the Open Source world it is something we should
   endorse!

   1. Code in /src and /include directories (excluding the extensions
      subdir) should not contain any third party code! It must be
      entirely original and project natively developed code.

   2. In extensions and any code outside of the /src and /include
      third-party code is allowed and encouraged IF:
      
      - The license of the third-party software allows its use.
      - The THIRD-PARTY-NOTICES.md file is updated to reflect the inclusion
        of the third-party software.

## AI policy

   AI is a very helpful tool and it should be used to automate and
   speed up repetitive tasks. With AI we can move faster and get more
   functionality faster. Using AI assistance is allowed in the LispBM
   repository under the following restrictions.

   1. Code in /src and /include directories (excluding the extensions
      subdir) should not be AI generated. These directories represent
      the core functionality of the runtime system and should be
      worked on in an intentionally slow and methodical way. You can
      ask AI questions about what the code does, and to analyze your
      solution to a problem in the code, but not to bulk insert
      generated code directly into the source.

      Note that all changes in these directories should be tested with
      the test-suites, checked with static analysis (cppcheck, infer
      and scanbuild).  Ideally new tests should be added to target the
      added functionality.

   2. The extensions subdirectory in /src and /include is sometimes
      used for code that bridges between third party code and project
      native code. This code can sometimes be repetitive and boring.

      Recommended approach is to write the code in the extensions
      directory by hand. A hybrid approach where you establish a
      pattern by implementing a few of the "extension functions"
      rigorously and then ask AI to extrapolate the rest.
      
   3. The tests directories contain tests of the runtime system and
      extensions.  Tests of core runtime system functionality should
      be written with the same intentionally slow and methodical way
      as the core code itself.

      Tests of extensions should be written with methodology similar
      to implementation of the extension itself.

   4. Examples and code outside of the core /src /include directories
      are developed in a less principled way. Methods used are less
      important than the benefit added.