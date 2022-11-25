#include "inc/ppu.h"
#include <memory.h>

PPU_t ppu;

void ppu_init(void) { memset(&ppu, 0, sizeof(PPU_t)); }

void ppu_exec(void) {
    ppu.cycle++;

    // a complete PPU scanline is 341 cycles long
    if (ppu.cycle >= 341) {
        ppu.cycle = 0;
        ppu.scanline++;

        // update vblank register, generate NMI
        if (ppu.scanline == 241) {
            ppu.PPU_stats.r7 = 1;
        }

        // a complete PPU frame is 262 scanlines long
        if (ppu.scanline >= 262) {
            ppu.scanline = 0;
            ppu.even_frame = !ppu.even_frame;
            ppu.PPU_stats.r7 = 0;
        }
    }
}