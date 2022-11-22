#include <stdint.h>
#include <stdbool.h>

#ifndef __PPU_H__
#define __PPU_H__

typedef struct
{
    uint8_t oamdata[265];
    uint8_t palette[32];
    uint8_t nametabledata[2048];

    uint8_t nametablebyte;
    uint8_t attrbyte;
    uint8_t tile_lo;
    uint8_t tile_hi;
    uint64_t tiledata;

    bool write;
    bool odd_frame;

    struct
    {
        union
        {
            struct
            {
                uint8_t r0:2;   // nametable address;
                uint8_t r2:1;   // vram address increment
                uint8_t r3:1;   // sprite table address
                uint8_t r4:1;   // background table address
                uint8_t r5:1;   // sprite size
                uint8_t r6:1;   // ppu master slave select
                uint8_t r7:1;   // nmi on blanking interval
            };
            uint8_t R;
        };

    } PPU_ctrl; // 0x2000

    struct
    {
        union
        {
            struct
            {
                uint8_t r0:1;   // Emphasize blue
                uint8_t r1:1;   // Emphasize green
                uint8_t r2:1;   // Emphasize red
                uint8_t r3:1;   // Show sprites
                uint8_t r4:1;   // Show background
                uint8_t r5:1;   // Show sprites in leftmost 8 pixels
                uint8_t r6:1;   // Show background in leftmost 8 pixels
                uint8_t r7:1;   // grayscale
            };
            uint8_t R;
        };

    } PPU_mask; // 0x2001

    struct
    {
        union
        {
            struct
            {
                uint8_t r0:5;   // bits previously written
                uint8_t r5:1;   // sprite overflow
                uint8_t r6:1;   // sprite 0 hit
                uint8_t r7:1;   // vblank started
            };
            uint8_t R;
        };

    } PPU_stats; // 0x2002

    uint8_t OAM_addr; // 0x2003
    uint8_t OAM_data; // 0x2004
    uint8_t scroll; // 0x2005
    uint8_t address; // 0x2006
    uint8_t data; // 0x2007

    uint32_t cycle;     // current PPU cycle
    uint32_t scanline;  // current PPU scanline
    uint64_t frames;    // current PPU frame

    bool even_frame;

} PPU_t;

extern PPU_t ppu;

void ppu_init(void);
void ppu_exec(void);

#endif