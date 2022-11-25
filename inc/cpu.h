#include <stdbool.h>
#include <stdint.h>

#ifndef __CPU_H__
#define __CPU_H__

#define SET_NZ(x)                                                              \
    cpu.Z = ((x) == 0);                                                        \
    cpu.N = ((x) >> 7) & 1;

typedef void (*optable_t)(void);

typedef struct {
    // general purpose registers
    uint8_t A;
    uint8_t X;
    uint8_t Y;

    union {
        // bit access to CPU state register
        struct {
            uint8_t C : 1; // carry
            uint8_t Z : 1; // zero
            uint8_t I : 1; // interrupt disable
            uint8_t D : 1; // decimal mode
            uint8_t B : 1; // break
            uint8_t R : 1; // reserved
            uint8_t V : 1; // overflow
            uint8_t N : 1; // negative
        };
        uint8_t P;
    };

    bool NMI;
    bool IRQ;

    uint8_t OP;     // opcode
    uint8_t SP;     // stack pointer
    uint16_t PC;    // program counter
    uint16_t oldPC; // previous program counter

    uint64_t cycles;
    uint32_t stall;

    uint32_t addr_mode;
    uint16_t memaddr;
} Cpu;

extern Cpu cpu;

void cpu_init(void);
int cpu_execop();

void bug(void);
void nop(void);
void brk(void);
void php(void);
void clc(void);
void jsr(void);
void bpl(void);
void clv(void);
void sec(void);
void rti(void);
void pla(void);
void rts(void);
void sei(void);
void clc(void);
void cli(void);
void ora(void);
void and (void);
void eor(void);
void adc(void);
void sta(void);
void lda(void);
void cmp(void);
void sbc(void);
void asl(void);
void rol(void);
void lsr(void);
void ror(void);
void stx(void);
void ldx(void);
void cpx(void);
void dec(void);
void dex(void);
void inx(void);
void inc(void);
void bne(void);
void bcc(void);
void bcs(void);
void beq(void);
void bvc(void);
void bvs(void);
void bit(void);
void sty(void);
void ldy(void);
void cpy(void);
void jmp(void);
void pha(void);
void pla(void);
void dey(void);
void tay(void);
void iny(void);
void tya(void);
void txs(void);
void txa(void);
void tax(void);
void tsx(void);
void sed(void);
void cld(void);
void plp(void);
void bmi(void);

#endif