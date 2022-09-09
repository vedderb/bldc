

/**
 * Author Wojciech Domski <Wojciech.Domski@gmail.com>
 * www: www.Domski.pl
 *
 * work based on DORJI.COM sample code and
 * https://github.com/realspinner/SX1278_LoRa
 *
 * Modified for vesc firmware by schardt@team-ctech.de

  This code uses InvertIQ function to create a simple Gateway/Node logic.
  VESC = Gateway
  Gateway - Sends messages with enableInvertIQ()
          - Receives messages with disableInvertIQ()

  Node    - Sends messages with disableInvertIQ()
          - Receives messages with enableInvertIQ()

  With this arrangement a Gateway never receive messages from another Gateway
  and a Node never receive message from another Node.
  Only Gateway to Node and vice versa.
 *
 */

#include "conf_general.h"
#ifdef HW_HAS_LORA

#include "SX1278.h"
#include <string.h>

uint8_t SX1278_SPIRead(uint8_t addr) {
	uint8_t tmp;
	SX1278_hw_SPICommand(addr);
	tmp = SX1278_hw_SPIReadByte();
	SX1278_hw_SetNSS(1);
	return tmp;
}

void SX1278_SPIWrite(uint8_t addr, uint8_t cmd) {
	SX1278_hw_SetNSS(0);
	SX1278_hw_SPICommand(addr | 0x80);
	SX1278_hw_SPICommand(cmd);
	SX1278_hw_SetNSS(1);
}

void SX1278_SPIBurstRead(uint8_t addr, uint8_t *rxBuf,
	uint8_t length) {
	uint8_t i;
	if (length <= 1) {
		return;
	} else {
		SX1278_hw_SetNSS(0);
		SX1278_hw_SPICommand(addr);
		for (i = 0; i < length; i++) {
			*(rxBuf + i) = SX1278_hw_SPIReadByte();
		}
		SX1278_hw_SetNSS(1);
	}
}

void SX1278_SPIBurstWrite(uint8_t addr, uint8_t *txBuf,
		uint8_t length) {
	unsigned char i;
	if (length <= 1) {
		return;
	} else {
		SX1278_hw_SetNSS(0);
		SX1278_hw_SPICommand(addr | 0x80);
		for (i = 0; i < length; i++) {
			SX1278_hw_SPICommand(*(txBuf + i));
		}
		SX1278_hw_SetNSS(1);
	}
}

void SX1278_config(SX1278_t *module) {
	SX1278_sleep(module); //Change modem mode Must in Sleep mode
	SX1278_hw_DelayMs(15);

	SX1278_entryLoRa();
	//SX1278_SPIWrite(module, 0x5904); //?? Change digital regulator form 1.6V to 1.47V: see errata note

	uint64_t freq = ((uint64_t) module->frequency << 19) / 32000000;
	uint8_t freq_reg[3];
	freq_reg[0] = (uint8_t) (freq >> 16);
	freq_reg[1] = (uint8_t) (freq >> 8);
	freq_reg[2] = (uint8_t) (freq >> 0);
	SX1278_SPIBurstWrite(LR_RegFrMsb, (uint8_t*) freq_reg, 3); //setting  frequency parameter

	//setting base parameter
	SX1278_SPIWrite(LR_RegPaConfig, SX1278_Power[module->power]); //Setting output power parameter

	SX1278_SPIWrite(LR_RegOcp, 0x2B); //war 0x0B			//RegOcp,Close Ocp
	SX1278_SPIWrite(LR_RegLna, 0x23); //OK		//RegLNA,High & LNA Enable
	if (SX1278_SpreadFactor[module->LoRa_SF] == 6) {	//SFactor=6
		uint8_t tmp;
		SX1278_SPIWrite(LR_RegModemConfig1,
				((SX1278_LoRaBandwidth[module->LoRa_BW] << 4)
						+ (SX1278_CodingRate[module->LoRa_CR] << 1) + 0x01)); //Implicit Enable CRC Enable(0x02) & Error Coding rate 4/5(0x01), 4/6(0x02), 4/7(0x03), 4/8(0x04)

		SX1278_SPIWrite(LR_RegModemConfig2,
				((SX1278_SpreadFactor[module->LoRa_SF] << 4)
						+ (SX1278_CRC_Sum[module->LoRa_CRC_sum] << 2) + 0x03));

		tmp = SX1278_SPIRead(0x31);
		tmp &= 0xF8;
		tmp |= 0x05;
		SX1278_SPIWrite(0x31, tmp);
		SX1278_SPIWrite(0x37, 0x0C);
	} else { //???
		SX1278_SPIWrite(LR_RegModemConfig1,
				((SX1278_LoRaBandwidth[module->LoRa_BW] << 4)
						+ (SX1278_CodingRate[module->LoRa_CR] << 1) + 0x00)); //Explicit Enable CRC Enable(0x02) & Error Coding rate 4/5(0x01), 4/6(0x02), 4/7(0x03), 4/8(0x04)

		SX1278_SPIWrite(LR_RegModemConfig2,
				((SX1278_SpreadFactor[module->LoRa_SF] << 4)
						+ (SX1278_CRC_Sum[module->LoRa_CRC_sum] << 2) + 0x00)); //SFactor &  LNA gain set by the internal AGC loop
	}

	SX1278_SPIWrite(LR_RegModemConfig3, 0x04);  //OK
	SX1278_SPIWrite(LR_RegSymbTimeoutLsb, 0x08); //RegSymbTimeoutLsb Timeout = 0x3FF(Max)
	SX1278_SPIWrite(LR_RegPreambleMsb, 0x00); //RegPreambleMsb
	SX1278_SPIWrite(LR_RegPreambleLsb, 8); //RegPreambleLsb 8+4=12byte Preamble
	SX1278_SPIWrite(REG_LR_DIOMAPPING2, 0x01); //RegDioMapping2 DIO5=00, DIO4=01
	module->readBytes = 0;
	SX1278_standby(module); //Entry standby mode
}

void SX1278_standby(SX1278_t *module) {
	SX1278_SPIWrite(LR_RegOpMode, 0x09);
	module->status = STANDBY;
}

void SX1278_sleep(SX1278_t *module) {
	SX1278_SPIWrite(LR_RegOpMode, 0x08);
	module->status = SLEEP;
}

void SX1278_entryLoRa() {
	SX1278_SPIWrite(LR_RegOpMode, 0x88);
}

void SX1278_clearLoRaIrq() {
	SX1278_SPIWrite(LR_RegIrqFlags, 0xFF);
}

void SX1278_enableInvertIQ()
{
	SX1278_SPIWrite(LR_InvertIQ,  0x66);
	SX1278_SPIWrite(LR_InvertIQ2, 0x19);
}

void SX1278_disableInvertIQ()
{
	SX1278_SPIWrite(LR_InvertIQ,  0x27);
	SX1278_SPIWrite(LR_InvertIQ2, 0x1d);
}


int SX1278_LoRaEntryRx(SX1278_t *module, uint8_t length, uint32_t timeout) {
	uint8_t addr;

	module->packetLength = length;

	SX1278_config(module);		//Setting base parameter
	SX1278_disableInvertIQ();
	SX1278_SPIWrite(REG_LR_PADAC, 0x84);	//Normal and RX
	SX1278_SPIWrite(LR_RegHopPeriod, 0xFF);	//No FHSS
	SX1278_SPIWrite(REG_LR_DIOMAPPING1, 0x01);//DIO=00,DIO1=00,DIO2=00, DIO3=01
	SX1278_SPIWrite(LR_RegIrqFlagsMask, 0x3F);//Open RxDone interrupt & Timeout
	SX1278_clearLoRaIrq();
	SX1278_SPIWrite(LR_RegPayloadLength, length);//Payload Length 21byte(this register must difine when the data long of one byte in SF is 6)
	addr = SX1278_SPIRead(LR_RegFifoRxBaseAddr); //Read RxBaseAddr
	SX1278_SPIWrite(LR_RegFifoAddrPtr, addr); //RxBaseAddr->FiFoAddrPtr
	SX1278_SPIWrite(LR_RegOpMode, 0x8d);	//Mode//Low Frequency Mode
	module->readBytes = 0;


	while (1) {
		if ((SX1278_SPIRead(LR_RegModemStat) & 0x04) == 0x04) {	//Rx-on going RegModemStat
			module->status = RX;
			return 1;
		}
		if (--timeout == 0) {
			SX1278_hw_Reset();
			SX1278_config(module);
			return 0;
		}
		SX1278_hw_DelayMs(1);
	}
}

uint8_t SX1278_LoRaRxPacket(SX1278_t *module) {
	unsigned char addr;
	unsigned char packet_size;

	if (SX1278_hw_GetDIO0()) {
		memset(module->rxBuffer, 0x00, SX1278_MAX_PACKET);

		addr = SX1278_SPIRead(LR_RegFifoRxCurrentaddr); //last packet addr
		SX1278_SPIWrite(LR_RegFifoAddrPtr, addr); //RxBaseAddr -> FiFoAddrPtr

		if (module->LoRa_SF == SX1278_LORA_SF_6) { //When SpreadFactor is six,will used Implicit Header mode(Excluding internal packet length)
			packet_size = module->packetLength;
		} else {
			packet_size = SX1278_SPIRead(LR_RegRxNbBytes); //Number for received bytes
		}

		SX1278_SPIBurstRead(0x00, module->rxBuffer, packet_size);
		module->readBytes = packet_size;
		SX1278_clearLoRaIrq();
	}
	return module->readBytes;
}

int SX1278_LoRaEntryTx(SX1278_t *module, uint8_t length, uint32_t timeout) {
	uint8_t addr;
	uint8_t temp;

	module->packetLength = length;

	SX1278_config(module); //setting base parameter
	SX1278_SPIWrite(REG_LR_PADAC, 0x87);	//Tx for 20dBm
	SX1278_SPIWrite(LR_RegHopPeriod, 0x00); //RegHopPeriod NO FHSS
	SX1278_SPIWrite(REG_LR_DIOMAPPING1, 0x41); //DIO0=01, DIO1=00,DIO2=00, DIO3=01
	SX1278_clearLoRaIrq();
	SX1278_SPIWrite(LR_RegIrqFlagsMask, 0xF7); //Open TxDone interrupt
	SX1278_SPIWrite(LR_RegPayloadLength, length); //RegPayloadLength 21byte
	addr = SX1278_SPIRead(LR_RegFifoTxBaseAddr); //RegFiFoTxBaseAddr
	SX1278_SPIWrite(LR_RegFifoAddrPtr, addr); //RegFifoAddrPtr

	SX1278_enableInvertIQ();

	while (1) {
		temp = SX1278_SPIRead(LR_RegPayloadLength);
		if (temp == length) {
			module->status = TX;
			return 1;
		}

		if (--timeout == 0) {
			SX1278_hw_Reset();
			SX1278_config(module);
			return 0;
		}
	}
}

int SX1278_LoRaTxPacket(SX1278_t *module, uint8_t *txBuffer, uint8_t length,
		uint32_t timeout) {
	SX1278_SPIBurstWrite(0x00, txBuffer, length);
	SX1278_SPIWrite(LR_RegOpMode, 0x8b);	//Tx Mode
	while (1) {
		if (SX1278_hw_GetDIO0()) { //if(Get_NIRQ()) //Packet send over
			SX1278_SPIRead(LR_RegIrqFlags);
			SX1278_clearLoRaIrq(); //Clear irq
			SX1278_standby(module); //Entry Standby mode
			return 1;
		}

		if (--timeout == 0) {
			SX1278_hw_Reset();
			SX1278_config(module);
			return 0;
		}
		SX1278_hw_DelayMs(1);
	}
}

void SX1278_init(SX1278_t *module, uint64_t frequency, uint8_t power,
		uint8_t LoRa_SF, uint8_t LoRa_BW, uint8_t LoRa_CR,
		uint8_t LoRa_CRC_sum, uint8_t packetLength) {
	SX1278_hw_init();
	module->frequency = frequency;
	module->power = power;
	module->LoRa_SF = LoRa_SF;
	module->LoRa_BW = LoRa_BW;
	module->LoRa_CR = LoRa_CR;
	module->LoRa_CRC_sum = LoRa_CRC_sum;
	module->packetLength = packetLength;
	SX1278_config(module);
}

int SX1278_transmit(SX1278_t *module, uint8_t *txBuf, uint8_t length,
		uint32_t timeout) {
	if (SX1278_LoRaEntryTx(module, length, timeout)) {
		return SX1278_LoRaTxPacket(module, txBuf, length, timeout);
	}
	return 0;
}

int SX1278_receive(SX1278_t *module, uint8_t length, uint32_t timeout) {
	return SX1278_LoRaEntryRx(module, length, timeout);
}

uint8_t SX1278_available(SX1278_t *module) {
	return SX1278_LoRaRxPacket(module);
}

uint8_t SX1278_read(SX1278_t *module, uint8_t *rxBuf, uint8_t length) {
	if (length != module->readBytes)
		length = module->readBytes;
	memcpy(rxBuf, module->rxBuffer, length);
	rxBuf[length] = '\0';
	module->readBytes = 0;
	return length;
}

uint8_t SX1278_RSSI_LoRa() {
	uint32_t temp = 10;
	temp = SX1278_SPIRead(LR_RegRssiValue); //Read RegRssiValue, Rssi value
	temp = temp + 127 - 137; //127:Max RSSI, 137:RSSI offset
	return (uint8_t) temp;
}

uint8_t SX1278_RSSI() {
	uint8_t temp = 0xff;
	temp = SX1278_SPIRead(RegRssiValue);
	temp = 127 - (temp >> 1);	//127:Max RSSI
	return temp;
}
#endif
