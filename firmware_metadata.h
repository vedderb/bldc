#ifndef FIRMWARE_METADATA_H
#define FIRMWARE_METADATA_H

//#define STRING2(x) #x
//#define STRING(x) STRING2(x)
//#pragma message "ARM_GCC_VERSION = " STRING(ARM_GCC_VERSION)

// Track the git branch name
#ifndef GIT_BRANCH_NAME
#define GIT_BRANCH_NAME "undefined"
#endif

// Track the git hash version
#ifndef GIT_COMMIT_HASH
#define GIT_COMMIT_HASH "undefined"
#endif

// Track the cross-compiler version
#ifndef ARM_GCC_VERSION
#define ARM_GCC_VERSION "undefined"
#endif


#endif  // FIRMWARE_METADATA_H
