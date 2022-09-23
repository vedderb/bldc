ROOT_SRC_DIR =     ../../

SOURCES += \
    $${ROOT_SRC_DIR}/*.c \
    $${ROOT_SRC_DIR}/appconf/*.c \
    $${ROOT_SRC_DIR}/applications/*.c \
    $${ROOT_SRC_DIR}/blackmagic/*.c \
    $${ROOT_SRC_DIR}/compression/*.c \
    $${ROOT_SRC_DIR}/hwconf/*.c \
    $${ROOT_SRC_DIR}/hwconf/luna/*.c \
    $${ROOT_SRC_DIR}/imu/*.c \
    $${ROOT_SRC_DIR}/imu/Fusion/*.c \
    $${ROOT_SRC_DIR}/imu/BMI160_driver/*.c \
    $${ROOT_SRC_DIR}/mcconf/*.c \
    $${ROOT_SRC_DIR}/nrf/*.c \
    $${ROOT_SRC_DIR}/tests/packet_recovery/*.c \


HEADERS += \
    $${ROOT_SRC_DIR}/*.h \
    $${ROOT_SRC_DIR}/math/*.h \
    $${ROOT_SRC_DIR}/appconf/*.h \
    $${ROOT_SRC_DIR}/applications/*.h \
    $${ROOT_SRC_DIR}/blackmagic/*.h \
    $${ROOT_SRC_DIR}/compression/*.h \
    $${ROOT_SRC_DIR}/hwconf/*.h \
    $${ROOT_SRC_DIR}/hwconf/luna/*.h \
    $${ROOT_SRC_DIR}/imu/*.h \
    $${ROOT_SRC_DIR}/imu/Fusion/*.h \
    $${ROOT_SRC_DIR}/imu/BMI160_driver/*.h \
    $${ROOT_SRC_DIR}/mcconf/*.h \
    $${ROOT_SRC_DIR}/nrf/*.h \
    $${ROOT_SRC_DIR}/tests/packet_recovery/*.h \

OTHER_FILES += \
    $${ROOT_SRC_DIR}/.gitlab-ci.yml \
    $${ROOT_SRC_DIR}/README.md \
    $${ROOT_SRC_DIR}/Makefile \
    $${ROOT_SRC_DIR}/make/* \
    $${ROOT_SRC_DIR}/applications/*.mk \
    $${ROOT_SRC_DIR}/applications/*.mk \
    $${ROOT_SRC_DIR}/Project/scripts/* \
