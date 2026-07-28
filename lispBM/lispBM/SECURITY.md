
# Security Policy

LispBM releases are given git tags in the a.b.c version format. Only
the commit with the numbered tag has been subjected to the full
test-suite and considered a stable point. Between releases many git
commits can occur where the version number stored in lbm_version.h
does not change, these are to be considered experimental!

## The purpose of LispBM and how it relates to security

The purpose of LispBM is to provide a scripting layer for embedded
systems. It can be used by an embedded system designer as a tool to
speed up development, or it can be used to empower the end user of a
product to modify and extend the behavior of a system they own.

When the LispBM scripting layer is exposed to the end user, there is
an inherent tension with security regulations and practices that aim
to restrict what code can run on a system. LispBM resolves this
tension at the integration boundary rather than within the core
library.

The core of LispBM operates only within memory areas explicitly
assigned to it (a sandbox). The integrator controls what extensions
are registered, and those extensions define what LispBM programs can
reach outside of that sandbox, including hardware peripherals,
communication interfaces, safety-critical firmware functions, and so
on. The security question for an integrator is therefore not "who is
allowed to upload code?", but "what should user code be able to do?"
The registered extensions define the security boundary.

## Supported Versions

Only the latest version of lispbm is to be considered
supported. Security patches will be worked into the main branch of the
LispBM repository and will then make it into the next numbered release
of LispBM.

## Reporting Vulnerabilities

Please report security vulnerabilities to bo.joel.svensson@gmail.com
rather than opening a public issue.

Include a description of the vulnerability and the steps needed to
reproduce it. We are very grateful for your contributions.

## Conclusion

LispBM is free and open source and we do our best to create a very
well tested, stable and secure base system. Contributions are very
welcome. If there is a desire to use LispBM in a way that requires
additional safety features, we are open to helping out with such work
for compensation.