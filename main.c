#include "mem.h"
#include "cart.h"
#include "cpu.h"
#include "ppu.h"

main()
{
    cart_load();
    mem_init();
    cpu_init();
    ppu_init();

    // PRG ROM dump
    // for(int i=0; i<cart.prgbanks*16384; i++)
    // {
    //     if(i==0)
    //         printf("0x%04X: ", ROM_ADDR + i);

    //     printf("%02x ", mem_read(ROM_ADDR + i));
    //     if((i+1)%32==0 && i>0)
    //         printf("\n0x%04X: ", ROM_ADDR + i);
    // }

    while(1)
    {
        cpu_execop();

        // each cpu cycle is 3 ppu ticks
        ppu_exec();
        ppu_exec();
        ppu_exec();
    }
}
