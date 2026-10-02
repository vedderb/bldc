# libcanard needs a 32-bit host. Build and run the test from the repository root with
#
#   docker build -f tests/uavcan_vesc_frames/Dockerfile .
#
# or, to avoid sending the whole repository (build/, tools/) as build context:
#
#   git ls-files datatypes.h conf_general.h timeout.h terminal.h flash_helper.h applications/app.h \
#       comm motor/mc_interface.h motor/mcpwm_foc.h hwconf/hw.h util driver/nrf imu libcanard \
#       tests/uavcan_vesc_frames | tar cT - | docker build -f tests/uavcan_vesc_frames/Dockerfile -

FROM --platform=linux/386 i386/debian:bookworm-slim

RUN apt-get update && \
	apt-get install -y --no-install-recommends gcc make libc6-dev && \
	rm -rf /var/lib/apt/lists/*

WORKDIR /bldc

# Only the headers canard_driver.c includes are needed, see main.c
COPY datatypes.h conf_general.h timeout.h terminal.h flash_helper.h ./
COPY applications/app.h applications/
COPY comm/comm_can.h comm/commands.h comm/
COPY motor/mc_interface.h motor/mcpwm_foc.h motor/
COPY hwconf/hw.h hwconf/
COPY util/mempools.h util/crc.h util/buffer.h util/utils.h util/
COPY driver/nrf/nrf_driver.h driver/nrf/
COPY imu/imu.h imu/
COPY libcanard libcanard/
COPY tests/uavcan_vesc_frames tests/uavcan_vesc_frames/

RUN make -B -C tests/uavcan_vesc_frames run
