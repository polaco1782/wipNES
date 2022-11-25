#include "inc/cpu.h"
#include "inc/mem.h"
#include "inc/disasm.h"
#include <assert.h>
#include <stdio.h>

Cpu cpu;

enum {
    Absolute = 1,
    AbsoluteX = 2,
    AbsoluteY = 3,
    Accumulator = 4,
    Immediate = 5,
    Implied = 6,
    IndexedIndirect = 7,
    Indirect = 8,
    IndirectIndexed = 9,
    Relative = 10,
    ZeroPage = 11,
    ZeroPageX = 12,
    ZeroPageY = 13
};

// op addressing modes
const uint8_t addr_mode[256] = {
    6,  7,  6,  7,  11, 11, 11, 11, 6,  5,  4,  5,  1,  1,  1,  1,  10, 9,  6,
    9,  12, 12, 12, 12, 6,  3,  6,  3,  2,  2,  2,  2,  1,  7,  6,  7,  11, 11,
    11, 11, 6,  5,  4,  5,  1,  1,  1,  1,  10, 9,  6,  9,  12, 12, 12, 12, 6,
    3,  6,  3,  2,  2,  2,  2,  6,  7,  6,  7,  11, 11, 11, 11, 6,  5,  4,  5,
    1,  1,  1,  1,  10, 9,  6,  9,  12, 12, 12, 12, 6,  3,  6,  3,  2,  2,  2,
    2,  6,  7,  6,  7,  11, 11, 11, 11, 6,  5,  4,  5,  8,  1,  1,  1,  10, 9,
    6,  9,  12, 12, 12, 12, 6,  3,  6,  3,  2,  2,  2,  2,  5,  7,  5,  7,  11,
    11, 11, 11, 6,  5,  6,  5,  1,  1,  1,  1,  10, 9,  6,  9,  12, 12, 13, 13,
    6,  3,  6,  3,  2,  2,  3,  3,  5,  7,  5,  7,  11, 11, 11, 11, 6,  5,  6,
    5,  1,  1,  1,  1,  10, 9,  6,  9,  12, 12, 13, 13, 6,  3,  6,  3,  2,  2,
    3,  3,  5,  7,  5,  7,  11, 11, 11, 11, 6,  5,  6,  5,  1,  1,  1,  1,  10,
    9,  6,  9,  12, 12, 12, 12, 6,  3,  6,  3,  2,  2,  2,  2,  5,  7,  5,  7,
    11, 11, 11, 11, 6,  5,  6,  5,  1,  1,  1,  1,  10, 9,  6,  9,  12, 12, 12,
    12, 6,  3,  6,  3,  2,  2,  2,  2};

// op size used to increment PC register
const uint8_t optable_size[256] = {
    1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0, 2, 2, 0, 0, 2, 2, 2, 0,
    1, 3, 1, 0, 3, 3, 3, 0, 3, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0, 1, 2, 0, 0, 2, 2, 2, 0,
    1, 2, 1, 0, 3, 3, 3, 0, 2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0, 2, 2, 0, 0, 2, 2, 2, 0,
    1, 3, 1, 0, 3, 3, 3, 0, 2, 2, 0, 0, 2, 2, 2, 0, 1, 0, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 0, 3, 0, 0, 2, 2, 2, 0, 2, 2, 2, 0,
    1, 2, 1, 0, 3, 3, 3, 0, 2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0, 2, 2, 0, 0, 2, 2, 2, 0,
    1, 3, 1, 0, 3, 3, 3, 0, 2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0};

// jump table for opcodes
optable_t optable[] = {
    brk, ora, bug, bug, nop, ora, asl, bug, php, ora, asl, bug, nop, ora, asl,
    bug, bpl, ora, bug, bug, nop, ora, asl, bug, clc, ora, nop, bug, nop, ora,
    asl, bug, jsr, and, bug, bug, bit, and, rol, bug, plp, and, rol, bug, bit,
    and, rol, bug, bmi, and, bug, bug, nop, and, rol, bug, sec, and, nop, bug,
    nop, and, rol, bug, rti, eor, bug, bug, nop, eor, lsr, bug, pha, eor, lsr,
    bug, jmp, eor, lsr, bug, bvc, eor, bug, bug, nop, eor, lsr, bug, cli, eor,
    nop, bug, nop, eor, lsr, bug, rts, adc, bug, bug, nop, adc, ror, bug, pla,
    adc, ror, bug, jmp, adc, ror, bug, bvs, adc, bug, bug, nop, adc, ror, bug,
    sei, adc, nop, bug, nop, adc, ror, bug, nop, sta, nop, bug, sty, sta, stx,
    bug, dey, nop, txa, bug, sty, sta, stx, bug, bcc, sta, bug, bug, sty, sta,
    stx, bug, tya, sta, txs, bug, bug, sta, bug, bug, ldy, lda, ldx, bug, ldy,
    lda, ldx, bug, tay, lda, tax, bug, ldy, lda, ldx, bug, bcs, lda, bug, bug,
    ldy, lda, ldx, bug, clv, lda, tsx, bug, ldy, lda, ldx, bug, cpy, cmp, nop,
    bug, cpy, cmp, dec, bug, iny, cmp, dex, bug, cpy, cmp, dec, bug, bne, cmp,
    bug, bug, nop, cmp, dec, bug, cld, cmp, nop, bug, nop, cmp, dec, bug, cpx,
    sbc, nop, bug, cpx, sbc, inc, bug, inx, sbc, nop, sbc, cpx, sbc, inc, bug,
    beq, sbc, bug, bug, nop, sbc, inc, bug, sed, sbc, nop, bug, nop, sbc, inc,
    bug};

void cpu_init() {
    cpu.A = 0;
    cpu.X = 0;
    cpu.Y = 0;
    cpu.SP = 0xfd;
    cpu.PC = mem_read16(0xfffc);
    cpu.P = 0x24;
}

inline void bug(void) {
    printf("bug\n");
    assert(0);
};
inline void nop(void) {}
inline void brk(void) {
    printf("brk\n");
    assert(0);
};
inline void nmi() {
    mem_write16(STACK_ADDR + cpu.SP, cpu.PC);
    mem_write8(STACK_ADDR + cpu.SP, cpu.P);
    cpu.SP -= 2;
    cpu.PC = mem_read16(0xfffa);
    cpu.I = 1;
};
inline void jmp(void) { cpu.PC = cpu.memaddr; };
inline void jsr(void) {
    cpu.SP -= 2;
    mem_write16(STACK_ADDR + cpu.SP, cpu.PC - 1);
    cpu.PC = cpu.memaddr;
};
inline void rts(void) {
    cpu.PC = mem_read16(STACK_ADDR + cpu.SP) + 1;
    cpu.SP += 2;
};
inline void rti(void) {
    cpu.P = mem_read8(STACK_ADDR + cpu.SP);
    cpu.SP++;
    cpu.PC = mem_read16(STACK_ADDR + cpu.SP);
    cpu.SP += 2;
};
inline void sei(void) { cpu.I = 1; };
inline void sec(void) { cpu.C = 1; };
inline void sed(void) { cpu.D = 1; };
inline void cli(void) { cpu.I = 0; };
inline void clc(void) { cpu.C = 0; };
inline void cld(void) { cpu.D = 0; };
inline void clv(void) { cpu.V = 0; };
inline void sta(void) { mem_write8(cpu.memaddr, cpu.A); };
inline void sty(void) { mem_write8(cpu.memaddr, cpu.Y); };
inline void stx(void) { mem_write8(cpu.memaddr, cpu.X); };
inline void lda(void) {
    cpu.A = mem_read8(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void ldx(void) {
    cpu.X = mem_read8(cpu.memaddr);
    SET_NZ(cpu.X);
};
inline void ldy(void) {
    cpu.Y = mem_read8(cpu.memaddr);
    SET_NZ(cpu.Y);
};
inline void cmp(void) {
    uint8_t val = mem_read8(cpu.memaddr);
    cpu.C = (cpu.A >= val);
    SET_NZ(cpu.A - val);
};
inline void cpy(void) {
    uint8_t val = mem_read8(cpu.memaddr);
    cpu.C = (cpu.Y >= val);
    SET_NZ(cpu.Y - val);
};
inline void cpx(void) {
    uint8_t val = mem_read8(cpu.memaddr);
    cpu.C = (cpu.X >= val);
    SET_NZ(cpu.X - val);
};
inline void ora(void) {
    cpu.A |= mem_read8(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void and (void) {
    cpu.A &= mem_read8(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void eor(void) {
    cpu.A ^= mem_read8(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void sbc(void) {
    uint8_t val = mem_read8(cpu.memaddr);
    uint16_t result = cpu.A - val - (1 - cpu.C);
    cpu.C = (result < 0x100);
    cpu.V = ((cpu.A ^ val) & 0x80) && ((cpu.A ^ result) & 0x80);
    cpu.A = result;
    SET_NZ(cpu.A);
};
inline void adc(void) {
    uint8_t a = cpu.A;
    uint8_t b = mem_read8(cpu.memaddr);
    uint8_t c = cpu.C;

    cpu.A = a + b + c;
    cpu.C = (cpu.A < a) || (cpu.A < b);
    cpu.V = ((a ^ cpu.A) & (b ^ cpu.A) & 0x80) != 0;
    SET_NZ(cpu.A);
};
inline void asl(void) {
    if (cpu.addr_mode == Accumulator) {
        cpu.C = (cpu.A >> 7) & 1;
        cpu.A <<= 1;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read8(cpu.memaddr);
        cpu.C = (val >> 7) & 1;
        val <<= 1;
        SET_NZ(val);
        mem_write8(cpu.memaddr, val);
    }
};
inline void rol(void) {
    if (cpu.addr_mode == Accumulator) {
        uint8_t oldC = cpu.C;
        cpu.C = (cpu.A >> 7) & 1;
        cpu.A <<= 1;
        cpu.A |= oldC;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read8(cpu.memaddr);
        uint8_t oldC = cpu.C;
        cpu.C = (val >> 7) & 1;
        val <<= 1;
        val |= oldC;
        SET_NZ(val);
        mem_write8(cpu.memaddr, val);
    }
};
inline void lsr(void) {
    if (cpu.addr_mode == Accumulator) {
        cpu.C = cpu.A & 1;
        cpu.A >>= 1;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read8(cpu.memaddr);
        cpu.C = val & 1;
        val >>= 1;
        SET_NZ(val);
        mem_write8(cpu.memaddr, val);
    }
};
inline void ror(void) {
    if (cpu.addr_mode == Accumulator) {
        uint8_t oldC = cpu.C;
        cpu.C = cpu.A & 1;
        cpu.A >>= 1;
        cpu.A |= oldC << 7;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read8(cpu.memaddr);
        uint8_t oldC = cpu.C;
        cpu.C = val & 1;
        val >>= 1;
        val |= oldC << 7;
        SET_NZ(val);
        mem_write8(cpu.memaddr, val);
    }
};
inline void dec(void) {
    uint8_t val = mem_read8(cpu.memaddr);
    mem_write8(cpu.memaddr, --val);
    SET_NZ(val);
};
inline void dex(void) {
    cpu.X--;
    SET_NZ(cpu.X);
};
inline void inx(void) {
    cpu.X++;
    SET_NZ(cpu.X);
};
inline void inc(void) {
    uint8_t val = mem_read8(cpu.memaddr);
    mem_write8(cpu.memaddr, ++val);
    SET_NZ(val);
};
inline void bne(void) {
    if (!cpu.Z)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void beq(void) {
    if (cpu.Z)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void bcc(void) {
    if (!cpu.C)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void bcs(void) {
    if (cpu.C)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void bvc(void) {
    if (!cpu.V)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void bvs(void) {
    if (cpu.V)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void bpl(void) {
    if (!cpu.N)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void bmi(void) {
    if (cpu.N)
        cpu.PC += (int8_t)mem_read8(cpu.memaddr);
};
inline void bit(void) {
    uint8_t val = mem_read8(cpu.memaddr);
    cpu.Z = !(cpu.A & val);
    cpu.N = (val >> 7) & 1;
    cpu.V = (val >> 6) & 1;
};
inline void pha(void) {
    cpu.SP--;
    mem_write8(STACK_ADDR + cpu.SP, cpu.A);
};
inline void php(void) {
    cpu.SP--;
    mem_write8(STACK_ADDR + cpu.SP, cpu.P);
};
inline void pla(void) {
    cpu.A = mem_read8(STACK_ADDR + cpu.SP);
    cpu.SP++;
    SET_NZ(cpu.A);
};
inline void plp(void) {
    cpu.P = mem_read8(STACK_ADDR + cpu.SP);
    cpu.SP++;
};
inline void dey(void) {
    cpu.Y--;
    SET_NZ(cpu.Y);
};
inline void tay(void) {
    cpu.Y = cpu.A;
    SET_NZ(cpu.Y);
};
inline void iny(void) {
    cpu.Y++;
    SET_NZ(cpu.Y);
};
inline void tya(void) {
    cpu.A = cpu.Y;
    SET_NZ(cpu.A);
};
inline void txs(void) { cpu.SP = cpu.X; };
inline void txa(void) {
    cpu.A = cpu.X;
    SET_NZ(cpu.A);
};
inline void tax(void) { cpu.X = cpu.A; };
inline void tsx(void) {
    cpu.X = cpu.SP;
    SET_NZ(cpu.X);
};

int cpu_execop() {
    cpu.OP = mem_read8(cpu.PC);
    cpu.addr_mode = addr_mode[cpu.OP];

    switch (cpu.addr_mode) {
    case Absolute:
        cpu.memaddr = mem_read16(cpu.PC + 1);
        break;
    case AbsoluteX:
        // addr = mem_read16(cpu.PC+1) + cpu.X;
        // if(pagecrossed(addr-cpu.X, addr))
        //     cpu.cycles += pagecrosscyles[cpu.OP];
        // cpu.memaddr = addr;
        cpu.memaddr = mem_read16(cpu.PC + 1) + cpu.X;
        break;
    case AbsoluteY:
        // addr = mem_read16(cpu.PC+1) + cpu.Y;
        // if(pagecrossed(addr-cpu.Y, addr))
        //     cpu.cycles += pagecrosscyles[cpu.OP];
        // cpu.memaddr = addr;
        cpu.memaddr = mem_read16(cpu.PC + 1) + cpu.Y;
        break;
    case Accumulator:
        cpu.memaddr = cpu.A;
        break;
    case Immediate:
        cpu.memaddr = cpu.PC + 1;
        break;
    case Implied:
        cpu.memaddr = 0;
        break;
    case IndexedIndirect:
        // cpu.memaddr = read16wrap((uint16_t)mem_read8(cpu.PC+1)+cpu.X);
        cpu.memaddr = mem_read16((uint16_t)mem_read8(cpu.PC + 1) + cpu.X);
        break;
    case Indirect:
        // cpu.memaddr = read16wrap(mem_read16(cpu.PC+1));
        cpu.memaddr = mem_read16(mem_read16(cpu.PC + 1));
        break;
    case IndirectIndexed:
        // addr = read16wrap((uint16_t)mem_read8(cpu.PC+1))+cpu.Y;
        // if(pagecrossed(addr-cpu.Y, addr))
        //     cpu.cycles += pagecrosscyles[cpu.OP];
        // cpu.memaddr = addr;
        cpu.memaddr = mem_read16((uint16_t)mem_read8(cpu.PC + 1)) + cpu.Y;
        break;
    case Relative:
        cpu.memaddr = cpu.PC + 1;
        break;
    case ZeroPage:
        cpu.memaddr = mem_read8(cpu.PC + 1);
        break;
    case ZeroPageX:
        cpu.memaddr = mem_read8(cpu.PC + 1) + cpu.X;
        break;
    case ZeroPageY:
        cpu.memaddr = mem_read8(cpu.PC + 1) + cpu.Y;
        break;
    }

    if (cpu.PC == 0x8060)
        printf("here");

    cpu.oldPC = cpu.PC;
    cpu.PC += optable_size[cpu.OP];

    optable[cpu.OP]();

    Disassemble6502Op();
}
