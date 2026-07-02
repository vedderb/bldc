
# Qt5 platform files

The purpose of the Qt5 platform support files is to make it easier
for people to integrate LispBM scripting into Qt5 applications.

The Qt5 platform support provides a QLispBM class which is meant to be
used to create just a single Object. This is, I understand, called a
singleton in the Object Oriented tongue.

The code is written using lowerCamelCase style which is the norm in Qt
applications. 

## API

**Create** the lispbm evaluator object:
```
QLispBM *lbm = new QLispBM(this);
```

**Initialize** the lbm object with default configuration:
```
lbm->init(nullptr);
```

The default configuration is:
```
QLispBMConfig myConfig;
myConfig.heapCells = 16384;
myConfig.memoryBlocks = 160;
myConfig.gcStackSize = 256;
myConfig.printStackSize = 256;
myConfig.maxExtensions = 256;
myConfig.imageWords = 32768;
myConfig.maxEvents = 20;
myConfig.extensions = QLispBMConfig::ExtAll;
```

**Initialize** the lbm object with a custom configuration:
```
QLispBMConfig myConfig;
myConfig.heapCells= 32768;
myConfig.memoryBlocks = 200;
myConfig.gcStackSize = 256;
myConfig.printStackSize = 256;
myConfig.maxExtensions = 256;
myConfig.imageWords = 60000;
myConfig.maxEvents = 100;
myConfig.extensions = QLispBMConfig::ExtAll;
lbm->init(&myConfig);
```

Possible extensions to load are:
- ExtArray
- ExtString
- ExtMath
- ExtRuntime
- ExtRandom
- ExtSet
- ExtMutex
- ExtCrypto
- ExtEcc
- ExtDisplay
- ExtDsp
- ExtTtf
- ExtAll

**Start** the lbm evaluator thread.
```
lbm->start();
```

**Evaluate** expressions in the lbm runtime.
```
 lbm->eval("(+ 1 2)");
```
Note that `eval` takes a QString in appropriate Qt fashion.

**addExtension** is used to add more user defined extensions.
```
lbm->addExtension("lisp-name", ext_my_extension_fptr);
```

`addExtension` can be called at any time when the runtime system is
initialized but it is most efficient to add them before running
start. Either way, each call to addExtension is a one time cost.

## QLbmValue

QLbmValue is a wrapper and an interface (protocol) for how to transfer
values to and from LispBM from the Qt application. A QLbmValue can
represent the same kind of values that LispBM's lbm_val can, but their
in memory representation is entirely Qt managed!

The idea is that when moving a value from LispBM to Qt, one translates
the lbm_value to a QLbmValue at this point the QLbmValue is allocated
on the Qt side and the contents of the lbm_value is copied over to the
QLbmValue.

Moving QLbmValues to LispBM should be done either via the event
system, where an intermediate flat value is created and the lbm_value
is then created in the evaluation thread. Or an extension can turn a
QLbmValue into a LispBM lbm_value.

If conversion between lbm_value <-> QLbmValue is taking place in a
thread different than the evaluation thread, the runtime system must
be paused.

## Slots

The following slots are available on the QLispBM object:
- eval : accepts a QString expression and passes it to the LispBM reader for read and eval.
- evalProgram : accepts a QString program and passes it to the LispBM reader for read and eval. 
- sendEvent : accepts a QLbmValue that is sent to the event handler (if registered).

## Signals

The following signals are emitted from the QLispBM object:
- output : emits when there is output from the LispBM runtime.
- evalFinished : emits when a context finishes successfully.
- evalFailed : emits when a context finishes with an error.
- initializedChanged : emits when initialized.
- runningChanged : emits when running state changes

## Usage

Add the following to your Qt `.pro` file:

```  
include(Path/to/lispbm/platform/qt5/lispbm.pri)
```

If you happen to be on a 32bit machine and using a 32bit Qt, then you should do:

```
DEFINES += LBM32
include(Path/to/lispbm/platform/qt5/lispbm.pri)
```

That is, 64BIT is default on Qt while for LispBM in general 32BIT is always the
default! 