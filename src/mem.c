#include "inc/mem.h"
#include "inc/apu.h"
#include "inc/cart.h"
#include "inc/ppu.h"
#include <memory.h>
#include <stdio.h>

Mem mem;

uint8_t IO[0xa];
uint16_t openbus = 0xff;

void mem_init(void) {
    int i;

    // flush RAM contents
    memset(mem.ram, 0, RAM_SIZE);

    // initialize lookup table
    for (i = 0; i < 0xffff; i++)
        mem.lookup[i] = &openbus;
    // mem.lookup[i] = NULL;

    // map ram
    for (i = 0; i < RAM_SIZE; i++) {
        mem.lookup[RAM_ADDR + i] = &mem.ram[i];
        mem.lookup[RAM_MIRROR1 + i] = &mem.ram[i];
        mem.lookup[RAM_MIRROR2 + i] = &mem.ram[i];
        mem.lookup[RAM_MIRROR3 + i] = &mem.ram[i];
    }

    // map PRG banks
    for (i = 0; i < cart.prgbanks * 16384; i++)
        mem.lookup[ROM_ADDR + i] = &cart.prg[i];

    // map PPU registers
    mem.lookup[PPU_CTRL_REG1] = &ppu.PPU_ctrl.R;
    mem.lookup[PPU_CTRL_REG2] = &ppu.PPU_mask.R;
    mem.lookup[PPU_STATUS] = &ppu.PPU_stats.R;
    mem.lookup[PPU_SPR_ADDR] = &ppu.OAM_addr;
    mem.lookup[PPU_SPR_DATA] = &ppu.OAM_data;
    mem.lookup[PPU_SCROLL_REG] = &ppu.scroll;
    mem.lookup[PPU_ADDRESS] = &ppu.address;
    mem.lookup[PPU_DATA] = &ppu.data;

    // TODO: map APU registers (not implemented yet)
    for (i = 0; i < 0x18; i++)
        mem.lookup[SND_REGISTER + i] = &apu.apubus[i];

    // map joypad registers
    mem.lookup[JOYPAD_PORT] = &IO[0];
    mem.lookup[JOYPAD_PORT1] = &IO[1];
    mem.lookup[JOYPAD_PORT2] = &IO[2];
}
