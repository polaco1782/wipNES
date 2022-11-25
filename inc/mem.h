#include <stdint.h>

#ifndef __MEM_H__
#define __MEM_H__

#define RAM_ADDR 0
#define ROM_ADDR 0x8000
#define RAM_SIZE 0x800
#define RAM_MIRROR1 0x800
#define RAM_MIRROR2 0x1000
#define RAM_MIRROR3 0x1800

#define PPU_CTRL_REG1 0x2000
#define PPU_CTRL_REG2 0x2001
#define PPU_STATUS 0x2002
#define PPU_SPR_ADDR 0x2003
#define PPU_SPR_DATA 0x2004
#define PPU_SCROLL_REG 0x2005
#define PPU_ADDRESS 0x2006
#define PPU_DATA 0x2007

#define SND_REGISTER 0x4000
#define SND_SQUARE1_REG 0x4000
#define SND_SQUARE2_REG 0x4004
#define SND_TRIANGLE_REG 0x4008
#define SND_NOISE_REG 0x400c
#define SND_DELTA_REG 0x4010
#define SND_MASTERCTRL_REG 0x4015

#define SPR_DMA 0x4014
#define JOYPAD_PORT 0x4016
#define JOYPAD_PORT1 0x4016
#define JOYPAD_PORT2 0x4017

#define STACK_ADDR 0x100

typedef struct {
    uint8_t ram[RAM_SIZE];
    uint8_t *lookup[0xffff];
} Mem;

extern Mem mem;

// macros to read and write memory
// 8 bit aligned reads/writes
#define mem_write8(addr, data) *mem.lookup[addr] = data;
#define mem_read8(addr) (*mem.lookup[addr])

// 16 bit aligned reads/writes
#define mem_write16(addr, data) *((uint16_t *)mem.lookup[addr]) = data
#define mem_read16(addr) (*((uint16_t *)(mem.lookup[addr])))

void mem_init(void);

#endif