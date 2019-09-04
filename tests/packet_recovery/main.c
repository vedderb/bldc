#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "packet.h"

const unsigned int rand_prepend = 50;
static uint8_t buffer[250000];
static unsigned int write = 0;

void send_packet(unsigned char *data, unsigned int len) {
	memcpy(buffer + write, data, len);
	write += len;
}

void process_packet(unsigned char *data, unsigned int len) {
	printf("Packet rx (%03d bytes): %s\r\n", len, (char*)data + rand_prepend);
}

void process_packet_perf(unsigned char *data, unsigned int len) {
	(void)data;
	(void)len;
}

int main(void) {
	packet_init(send_packet, process_packet, 0);
	
	srand(104);
	
	for (int i = 0;i < 20;i++) {
		char asd[rand_prepend + 100];
		
		for (unsigned int j = 0;j < rand_prepend;j++) {
			asd[j] = rand();
		}
		
		sprintf(asd + rand_prepend, "Offset: %d Test %d", write, i);
		packet_send_packet((unsigned char*)asd, strlen(asd + rand_prepend) + rand_prepend + 1, 0);
	}
	
	// Ability to recover
	unsigned int offsets[] = {121, 1250, 1121, 1122, 1187, 1188, 1189, 1036, 1112, 1264};
	for (unsigned int ofs = 0;ofs < sizeof(offsets) / sizeof(int);ofs++) {
		printf("Decode from offset %d\r\n", offsets[ofs]);
		
		//packet_reset(0);
		for(unsigned int i = offsets[ofs];i < write;i++) {
			packet_process_byte(buffer[i], 0);
		}
		
		printf("\r\n");
	}
	
	// Corruption
	printf("Corruption Test\r\n");
	buffer[12] = 91;
	buffer[13] = 0xFF;
	buffer[14] = 0;
	buffer[15] = 221;
	buffer[800] = 0;
	buffer[345] = 0xFF;
	buffer[1200]++;
	buffer[1342]++;
	for(unsigned int i = 0;i < write;i++) {
		packet_process_byte(buffer[i], 0);
	}
	
	// Performance
	printf("\r\nPerformance Test\r\n");
	packet_init(send_packet, process_packet_perf, 0);
	
	srand(104);
	write = 0;
	unsigned char asd[500];
	for (unsigned int i = 0;i < sizeof(asd);i++) {
		asd[i] = rand();
	}
	
	clock_t start, end;
	double cpu_time_used;
	
	start = clock();
	for (int i = 0;i < 1e6;i++) {
		packet_send_packet(asd, sizeof(asd), 0);
		for (unsigned int j = 0;j < write;j++) {
			packet_process_byte(buffer[j], 0);
		}
		write = 0;
	}
	end = clock();
	cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
	
	printf("Time: %.3f s\r\n", cpu_time_used);
	
	return 0;
}
