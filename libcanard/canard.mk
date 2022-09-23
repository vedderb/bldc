CANARDSRC =	libcanard/canard.c \
			libcanard/canard_driver.c \
			libcanard/dsdl/uavcan/equipment/esc/esc_Status.c \
			libcanard/dsdl/uavcan/equipment/esc/esc_RawCommand.c \
			libcanard/dsdl/uavcan/equipment/esc/esc_RPMCommand.c \
			libcanard/dsdl/uavcan/protocol/protocol_GetNodeInfo.c \
			libcanard/dsdl/uavcan/protocol/protocol_HardwareVersion.c \
			libcanard/dsdl/uavcan/protocol/protocol_NodeStatus.c \
			libcanard/dsdl/uavcan/protocol/protocol_SoftwareVersion.c \
			libcanard/dsdl/uavcan/protocol/param/param_Empty.c \
			libcanard/dsdl/uavcan/protocol/param/param_ExecuteOpcode.c \
			libcanard/dsdl/uavcan/protocol/param/param_GetSet.c \
			libcanard/dsdl/uavcan/protocol/param/param_NumericValue.c \
			libcanard/dsdl/uavcan/protocol/param/param_Value.c \
			libcanard/dsdl/uavcan/protocol/file/file_BeginFirmwareUpdate.c \
			libcanard/dsdl/uavcan/protocol/file/file_Read.c \
			libcanard/dsdl/vesc/vesc_RTData.c 

CANARDINC = libcanard \
			libcanard/dsdl
