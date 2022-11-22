#include <stdio.h>
#include <assert.h>
#include "cpu.h"
#include "mem.h"

Cpu cpu;

enum
{
    Absolute         = 1,
    AbsoluteX        = 2,
    AbsoluteY        = 3,
    Accumulator      = 4,
    Immediate        = 5,
    Implied          = 6,
    IndexedIndirect  = 7,
    Indirect         = 8,
    IndirectIndexed  = 9,
    Relative         = 10,
    ZeroPage         = 11,
    ZeroPageX        = 12,
    ZeroPageY        = 13
};

// op addressing modes
const uint8_t addr_mode[256] =
{
    6, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
    1, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
    6, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
    6, 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 8, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
    5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 13, 13, 6, 3, 6, 3, 2, 2, 3, 3,
    5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 13, 13, 6, 3, 6, 3, 2, 2, 3, 3,
    5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,
    5, 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,
    10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2
};

// op size used to increment PC register
const uint8_t optable_size[256] = 
{
    1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    3, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 0, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 0, 3, 0, 0,
    2, 2, 2, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,
    2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0
};

// jump table for opcodes
optable_t optable[] =
{//  0    1    2    3    4    5    6    7
    brk, ora, bug, bug, nop, ora, asl, bug, 
    php, ora, asl, bug, nop, ora, asl, bug, 
    bpl, ora, bug, bug, nop, ora, asl, bug, 
    clc, ora, nop, bug, nop, ora, asl, bug, 
    jsr, and, bug, bug, bit, and, rol, bug, 
    plp, and, rol, bug, bit, and, rol, bug, 
    bmi, and, bug, bug, nop, and, rol, bug, 
    sec, and, nop, bug, nop, and, rol, bug, 
    rti, eor, bug, bug, nop, eor, lsr, bug, 
    pha, eor, lsr, bug, jmp, eor, lsr, bug, 
    bvc, eor, bug, bug, nop, eor, lsr, bug, 
    cli, eor, nop, bug, nop, eor, lsr, bug, 
    rts, adc, bug, bug, nop, adc, ror, bug, 
    pla, adc, ror, bug, jmp, adc, ror, bug, 
    bvs, adc, bug, bug, nop, adc, ror, bug, 
    sei, adc, nop, bug, nop, adc, ror, bug, 
    nop, sta, nop, bug, sty, sta, stx, bug, 
    dey, nop, txa, bug, sty, sta, stx, bug, 
    bcc, sta, bug, bug, sty, sta, stx, bug, 
    tya, sta, txs, bug, bug, sta, bug, bug,
    ldy, lda, ldx, bug, ldy, lda, ldx, bug, 
    tay, lda, tax, bug, ldy, lda, ldx, bug, 
    bcs, lda, bug, bug, ldy, lda, ldx, bug, 
    clv, lda, tsx, bug, ldy, lda, ldx, bug, 
    cpy, cmp, nop, bug, cpy, cmp, dec, bug,
    iny, cmp, dex, bug, cpy, cmp, dec, bug,
    bne, cmp, bug, bug, nop, cmp, dec, bug,
    cld, cmp, nop, bug, nop, cmp, dec, bug,
    cpx, sbc, nop, bug, cpx, sbc, inc, bug,
    inx, sbc, nop, sbc, cpx, sbc, inc, bug,
    beq, sbc, bug, bug, nop, sbc, inc, bug,
    sed, sbc, nop, bug, nop, sbc, inc, bug
};

void Disassemble6502Op()
{
	char opstr[256];
	uint8_t *opcode = &mem_read(cpu.oldPC);

	printf ("$%04X  %02X ", cpu.oldPC, opcode[0]);

	switch (opcode[0])
	{
        case 0x00: sprintf(opstr, "BRK"); break;
        case 0x01: sprintf(opstr, "ORA ($%02x,X)", opcode[1]); break;
        case 0x05: sprintf(opstr, "ORA $%02x", opcode[1]); break;
        case 0x06: sprintf(opstr, "ASL $%02x", opcode[1]); break;
        case 0x08: sprintf(opstr, "PHP"); break;
        case 0x09: sprintf(opstr, "ORA #$%02x", opcode[1]); break;
        case 0x0a: sprintf(opstr, "ASL A"); break;
        case 0x0d: sprintf(opstr, "ORA $%02x%02x", opcode[2], opcode[1]); break;
        case 0x0e: sprintf(opstr, "ASL $%02x%02x", opcode[2], opcode[1]); break;
        case 0x10: sprintf(opstr, "BPL $%02x", opcode[1]); break;
        case 0x11: sprintf(opstr, "ORA ($%02x),Y", opcode[1]); break;
        case 0x15: sprintf(opstr, "ORA $%02x,X", opcode[1]); break;
        case 0x16: sprintf(opstr, "ASL $%02x,X", opcode[1]); break;
        case 0x18: sprintf(opstr, "CLC"); break;
        case 0x19: sprintf(opstr, "ORA $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0x1d: sprintf(opstr, "ORA $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x1e: sprintf(opstr, "ASL $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x20: sprintf(opstr, "JSR $%02x%02x", opcode[2], opcode[1]); break;
        case 0x21: sprintf(opstr, "AND ($%02x,X)", opcode[1]); break;
        case 0x24: sprintf(opstr, "BIT $%02x", opcode[1]); break;
        case 0x25: sprintf(opstr, "AND $%02x", opcode[1]); break;
        case 0x26: sprintf(opstr, "ROL $%02x", opcode[1]); break;
        case 0x28: sprintf(opstr, "PLP"); break;
        case 0x29: sprintf(opstr, "AND #$%02x", opcode[1]); break;
        case 0x2a: sprintf(opstr, "ROL A"); break;
        case 0x2c: sprintf(opstr, "BIT $%02x%02x", opcode[2], opcode[1]); break;
        case 0x2d: sprintf(opstr, "AND $%02x%02x", opcode[2], opcode[1]); break;
        case 0x2e: sprintf(opstr, "ROL $%02x%02x", opcode[2], opcode[1]); break;
        case 0x30: sprintf(opstr, "BMI $%02x", opcode[1]); break;
        case 0x31: sprintf(opstr, "AND ($%02x),Y", opcode[1]); break;
        case 0x35: sprintf(opstr, "AND $%02x,X", opcode[1]); break;
        case 0x36: sprintf(opstr, "ROL $%02x,X", opcode[1]); break;
        case 0x38: sprintf(opstr, "SEC"); break;
        case 0x39: sprintf(opstr, "AND $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0x3d: sprintf(opstr, "AND $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x3e: sprintf(opstr, "ROL $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x40: sprintf(opstr, "RTI"); break;
        case 0x41: sprintf(opstr, "EOR ($%02x,X)", opcode[1]); break;
        case 0x45: sprintf(opstr, "EOR $%02x", opcode[1]); break;
        case 0x46: sprintf(opstr, "LSR $%02x", opcode[1]); break;
        case 0x48: sprintf(opstr, "PHA"); break;
        case 0x49: sprintf(opstr, "EOR #$%02x", opcode[1]); break;
        case 0x4a: sprintf(opstr, "LSR A"); break;
        case 0x4c: sprintf(opstr, "JMP $%02x%02x", opcode[2], opcode[1]); break;
        case 0x4d: sprintf(opstr, "EOR $%02x%02x", opcode[2], opcode[1]); break;
        case 0x4e: sprintf(opstr, "LSR $%02x%02x", opcode[2], opcode[1]); break;
        case 0x50: sprintf(opstr, "BVC $%02x", opcode[1]); break;
        case 0x51: sprintf(opstr, "EOR ($%02x),Y", opcode[1]); break;
        case 0x55: sprintf(opstr, "EOR $%02x,X", opcode[1]); break;
        case 0x56: sprintf(opstr, "LSR $%02x,X", opcode[1]); break;
        case 0x58: sprintf(opstr, "CLI"); break;
        case 0x59: sprintf(opstr, "EOR $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0x5d: sprintf(opstr, "EOR $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x5e: sprintf(opstr, "LSR $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x60: sprintf(opstr, "RTS"); break;
        case 0x61: sprintf(opstr, "ADC ($%02x,X)", opcode[1]); break;
        case 0x65: sprintf(opstr, "ADC $%02x", opcode[1]); break;
        case 0x66: sprintf(opstr, "ROR $%02x", opcode[1]); break;
        case 0x68: sprintf(opstr, "PLA"); break;
        case 0x69: sprintf(opstr, "ADC #$%02x", opcode[1]); break;
        case 0x6a: sprintf(opstr, "ROR A"); break;
        case 0x6c: sprintf(opstr, "JMP ($%02x%02x)", opcode[2], opcode[1]); break;
        case 0x6d: sprintf(opstr, "ADC $%02x%02x", opcode[2], opcode[1]); break;
        case 0x6e: sprintf(opstr, "ROR $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x70: sprintf(opstr, "BVS $%02x", opcode[1]); break;
        case 0x71: sprintf(opstr, "ADC ($%02x),Y", opcode[1]); break;
        case 0x75: sprintf(opstr, "ADC $%02x,X", opcode[1]); break;
        case 0x76: sprintf(opstr, "ROR $%02x,X", opcode[1]); break;
        case 0x78: sprintf(opstr, "SEI"); break;
        case 0x79: sprintf(opstr, "ADC $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0x7d: sprintf(opstr, "ADC $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0x7e: sprintf(opstr, "ROR $%02x%02x", opcode[2], opcode[1]); break;
        case 0x81: sprintf(opstr, "STA ($%02x,X)", opcode[1]); break;
        case 0x84: sprintf(opstr, "STY $%02x", opcode[1]); break;
        case 0x85: sprintf(opstr, "STA $%02x", opcode[1]); break;
        case 0x86: sprintf(opstr, "STX $%02x", opcode[1]); break;
        case 0x88: sprintf(opstr, "DEY"); break;
        case 0x8a: sprintf(opstr, "TXA"); break;
        case 0x8c: sprintf(opstr, "STY $%02x%02x", opcode[2], opcode[1]); break;
        case 0x8d: sprintf(opstr, "STA $%02x%02x", opcode[2], opcode[1]); break;
        case 0x8e: sprintf(opstr, "STX $%02x%02x", opcode[2], opcode[1]); break;
        case 0x90: sprintf(opstr, "BCC $%02x", opcode[1]); break;
        case 0x91: sprintf(opstr, "STA ($%02x),Y", opcode[1]); break;
        case 0x94: sprintf(opstr, "STY $%02x,X", opcode[1]); break;
        case 0x95: sprintf(opstr, "STA $%02x,X", opcode[1]); break;
        case 0x96: sprintf(opstr, "STX $%02x,Y", opcode[1]); break;
        case 0x98: sprintf(opstr, "TYA"); break;
        case 0x99: sprintf(opstr, "STA $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0x9a: sprintf(opstr, "TXS"); break;
        case 0x9d: sprintf(opstr, "STA $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0xa0: sprintf(opstr, "LDY #$%02x", opcode[1]); break;
        case 0xa1: sprintf(opstr, "LDA ($%02x,X)", opcode[1]); break;
        case 0xa2: sprintf(opstr, "LDX #$%02x", opcode[1]); break;
        case 0xa4: sprintf(opstr, "LDY $%02x", opcode[1]); break;
        case 0xa5: sprintf(opstr, "LDA $%02x", opcode[1]); break;
        case 0xa6: sprintf(opstr, "LDX $%02x", opcode[1]); break;
        case 0xa8: sprintf(opstr, "TAY"); break;
        case 0xa9: sprintf(opstr, "LDA #$%02x", opcode[1]); break;
        case 0xaa: sprintf(opstr, "TAX"); break;
        case 0xac: sprintf(opstr, "LDY $%02x%02x", opcode[2], opcode[1]); break;
        case 0xad: sprintf(opstr, "LDA $%02x%02x", opcode[2], opcode[1]); break;
        case 0xae: sprintf(opstr, "LDX $%02x%02x", opcode[2], opcode[1]); break;
        case 0xb0: sprintf(opstr, "BCS $%02x", opcode[1]); break;
        case 0xb1: sprintf(opstr, "LDA ($%02x),Y", opcode[1]); break;
        case 0xb4: sprintf(opstr, "LDY $%02x,X", opcode[1]); break;
        case 0xb5: sprintf(opstr, "LDA $%02x,X", opcode[1]); break;
        case 0xb6: sprintf(opstr, "LDX $%02x,Y", opcode[1]); break;
        case 0xb8: sprintf(opstr, "CLV"); break;
        case 0xb9: sprintf(opstr, "LDA $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0xba: sprintf(opstr, "TSX"); break;
        case 0xbc: sprintf(opstr, "LDY $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0xbd: sprintf(opstr, "LDA $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0xbe: sprintf(opstr, "LDX $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0xc0: sprintf(opstr, "CPY #$%02x", opcode[1]); break;
        case 0xc1: sprintf(opstr, "CMP ($%02x,X)", opcode[1]); break;
        case 0xc4: sprintf(opstr, "CPY $%02x", opcode[1]); break;
        case 0xc5: sprintf(opstr, "CMP $%02x", opcode[1]); break;
        case 0xc6: sprintf(opstr, "DEC $%02x", opcode[1]); break;
        case 0xc8: sprintf(opstr, "INY"); break;
        case 0xc9: sprintf(opstr, "CMP #$%02x", opcode[1]); break;
        case 0xca: sprintf(opstr, "DEX"); break;
        case 0xcc: sprintf(opstr, "CPY $%02x%02x", opcode[2], opcode[1]); break;
        case 0xcd: sprintf(opstr, "CMP $%02x%02x", opcode[2], opcode[1]); break;
        case 0xce: sprintf(opstr, "DEC $%02x%02x", opcode[2], opcode[1]); break;
        case 0xd0: sprintf(opstr, "BNE $%02x", opcode[1]); break;
        case 0xd1: sprintf(opstr, "CMP ($%02x),Y", opcode[1]); break;
        case 0xd5: sprintf(opstr, "CMP $%02x,X", opcode[1]); break;
        case 0xd6: sprintf(opstr, "DEC $%02x,X", opcode[1]); break;
        case 0xd8: sprintf(opstr, "CLD"); break;
        case 0xd9: sprintf(opstr, "CMP $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0xdd: sprintf(opstr, "CMP $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0xde: sprintf(opstr, "DEC $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0xe0: sprintf(opstr, "CPX #$%02x", opcode[1]); break;
        case 0xe1: sprintf(opstr, "SBC ($%02x,X)", opcode[1]); break;
        case 0xe4: sprintf(opstr, "CPX $%02x", opcode[1]); break;
        case 0xe5: sprintf(opstr, "SBC $%02x", opcode[1]); break;
        case 0xe6: sprintf(opstr, "INC $%02x", opcode[1]); break;
        case 0xe8: sprintf(opstr, "INX"); break;
        case 0xe9: sprintf(opstr, "SBC #$%02x", opcode[1]); break;
        case 0xea: sprintf(opstr, "NOP"); break;
        case 0xec: sprintf(opstr, "CPX $%02x%02x", opcode[2], opcode[1]); break;
        case 0xed: sprintf(opstr, "SBC $%02x%02x", opcode[2], opcode[1]); break;
        case 0xee: sprintf(opstr, "INC $%02x%02x", opcode[2], opcode[1]); break;
        case 0xf0: sprintf(opstr, "BEQ $%02x", opcode[1]); break;
        case 0xf1: sprintf(opstr, "SBC ($%02x),Y", opcode[1]); break;
        case 0xf5: sprintf(opstr, "SBC $%02x,X", opcode[1]); break;
        case 0xf6: sprintf(opstr, "INC $%02x,X", opcode[1]); break;
        case 0xf8: sprintf(opstr, "SED"); break;
        case 0xf9: sprintf(opstr, "SBC $%02x%02x,Y", opcode[2], opcode[1]); break;
        case 0xfd: sprintf(opstr, "SBC $%02x%02x,X", opcode[2], opcode[1]); break;
        case 0xfe: sprintf(opstr, "INC $%02x%02x,X", opcode[2], opcode[1]); break;
		default:
			sprintf(opstr, ".db $%02x", opcode[0]); break;
	}

	if (optable_size[opcode[0]] > 1)
		printf("%02X ", opcode[1]);
	else
		printf("   ");
	if (optable_size[opcode[0]] > 2)
		printf("%02X ", opcode[2]);
	else
		printf("   ");

    switch (opcode[0])
	{
		case 0x90:
		case 0xB0:
		case 0xF0:
		case 0x30:
		case 0xD0:
		case 0x10:
		case 0x50:
		case 0x70:
			// print branch target
			printf(" | %-8s ;$%04x |", opstr, cpu.oldPC+2+(int8_t)opcode[1]);
            break;
        default:
        	printf(" | %-15s |", opstr);
            break;
	}

    printf("    A: 0x%02x X: 0x%02x Y: 0x%02x P: 0x%02x SP: 0x%02x    ", cpu.A, cpu.X, cpu.Y, cpu.P, cpu.SP);
    printf("ST: %s%s%s%s%s%s%s%s", (cpu.C) ? "C" : "-",
                                    (cpu.Z) ? "Z" : "-",
                                    (cpu.I) ? "I" : "-",
                                    (cpu.D) ? "D" : "-",
                                    (cpu.B) ? "B" : "-",
                                    (cpu.R) ? "R" : "-",
                                    (cpu.V) ? "V" : "-",
                                    (cpu.N) ? "N" : "-");
	printf("\n");
}

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
inline void nop(void){
}
inline void brk(void){
    printf("brk\n");
    assert(0);
};
inline void nmi() {
    mem_write16(STACK_ADDR+cpu.SP, cpu.PC);
    mem_write(STACK_ADDR+cpu.SP, cpu.P);
    cpu.SP -= 2;
    cpu.PC = mem_read16(0xfffa);
    cpu.I = 1;
};
inline void jmp(void){
    cpu.PC = cpu.memaddr;
};
inline void jsr(void){
    cpu.SP -= 2;
    mem_write16(STACK_ADDR+cpu.SP, cpu.PC-1);
    cpu.PC = cpu.memaddr;
};
inline void rts(void){
    cpu.PC = mem_read16(STACK_ADDR+cpu.SP)+1;
    cpu.SP += 2;
};
inline void rti(void){
    cpu.P = mem_read(STACK_ADDR+cpu.SP);
    cpu.SP++;
    cpu.PC = mem_read16(STACK_ADDR+cpu.SP);
    cpu.SP += 2;
};
inline void sei(void) { 
    cpu.I = 1;
};
inline void sec(void) { 
    cpu.C = 1;
};
inline void sed(void) { 
    cpu.D = 1;
};
inline void cli(void) {
    cpu.I = 0;
};
inline void clc(void) {
    cpu.C = 0;
};
inline void cld(void) {
    cpu.D = 0;
};
inline void clv(void) {
    cpu.V = 0;
};
inline void sta(void){
    mem_write(cpu.memaddr, cpu.A);
};
inline void sty(void){
    mem_write(cpu.memaddr, cpu.Y);
};
inline void stx(void){
    mem_write(cpu.memaddr, cpu.X);
};
inline void lda(void){
    cpu.A = mem_read(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void ldx(void){ 
    cpu.X = mem_read(cpu.memaddr);
    SET_NZ(cpu.X);
};
inline void ldy(void){
    cpu.Y = mem_read(cpu.memaddr);
    SET_NZ(cpu.Y);
};
inline void cmp(void){
    uint8_t val = mem_read(cpu.memaddr);
    cpu.C = (cpu.A >= val);
    SET_NZ(cpu.A - val);
};
inline void cpy(void){
    uint8_t val = mem_read(cpu.memaddr);
    cpu.C = (cpu.Y >= val);
    SET_NZ(cpu.Y - val);
};
inline void cpx(void){ 
    uint8_t val = mem_read(cpu.memaddr);
    cpu.C = (cpu.X >= val);
    SET_NZ(cpu.X - val);
};
inline void ora(void){
    cpu.A |= mem_read(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void and(void){
    cpu.A &= mem_read(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void eor(void){
    cpu.A ^= mem_read(cpu.memaddr);
    SET_NZ(cpu.A);
};
inline void sbc(void){ 
    uint8_t val = mem_read(cpu.memaddr);
    uint16_t result = cpu.A - val - (1 - cpu.C);
    cpu.C = (result < 0x100);
    cpu.V = ((cpu.A ^ val) & 0x80) && ((cpu.A ^ result) & 0x80);
    cpu.A = result;
    SET_NZ(cpu.A);
};
inline void adc(void){
    uint8_t a = cpu.A;
    uint8_t b = mem_read(cpu.memaddr);
    uint8_t c = cpu.C;

    cpu.A = a + b + c;
    cpu.C = (cpu.A < a) || (cpu.A < b);
    cpu.V = ((a ^ cpu.A) & (b ^ cpu.A) & 0x80) != 0;
    SET_NZ(cpu.A);
};
inline void asl(void){
    if(cpu.addr_mode == Accumulator) {
        cpu.C = (cpu.A >> 7) & 1;
        cpu.A <<= 1;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read(cpu.memaddr);
        cpu.C = (val >> 7) & 1;
        val <<= 1;
        SET_NZ(val);
        mem_write(cpu.memaddr, val);
    }
};
inline void rol(void){
    if(cpu.addr_mode == Accumulator) {
        uint8_t oldC = cpu.C;
        cpu.C = (cpu.A >> 7) & 1;
        cpu.A <<= 1;
        cpu.A |= oldC;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read(cpu.memaddr);
        uint8_t oldC = cpu.C;
        cpu.C = (val >> 7) & 1;
        val <<= 1;
        val |= oldC;
        SET_NZ(val);
        mem_write(cpu.memaddr, val);
    }
};
inline void lsr(void){
    if(cpu.addr_mode == Accumulator) {
        cpu.C = cpu.A & 1;
        cpu.A >>= 1;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read(cpu.memaddr);
        cpu.C = val & 1;
        val >>= 1;
        SET_NZ(val);
        mem_write(cpu.memaddr, val);
    }
};
inline void ror(void){
    if(cpu.addr_mode == Accumulator) {
        uint8_t oldC = cpu.C;
        cpu.C = cpu.A & 1;
        cpu.A >>= 1;
        cpu.A |= oldC << 7;
        SET_NZ(cpu.A);
    } else {
        uint8_t val = mem_read(cpu.memaddr);
        uint8_t oldC = cpu.C;
        cpu.C = val & 1;
        val >>= 1;
        val |= oldC << 7;
        SET_NZ(val);
        mem_write(cpu.memaddr, val);
    }
};
inline void dec(void){
    uint8_t val = mem_read(cpu.memaddr);
    mem_write(cpu.memaddr, --val);
    SET_NZ(val);
};
inline void dex(void){
    cpu.X--;
    SET_NZ(cpu.X);
};
inline void inx(void){
    cpu.X++;
    SET_NZ(cpu.X);
};
inline void inc(void){
    uint8_t val = mem_read(cpu.memaddr);
    mem_write(cpu.memaddr, ++val);
    SET_NZ(val);
};
inline void bne(void){
    if(!cpu.Z)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void beq(void){
    if(cpu.Z)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void bcc(void){
    if(!cpu.C)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void bcs(void){
    if(cpu.C)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void bvc(void){
    if(!cpu.V)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void bvs(void){
    if(cpu.V)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void bpl(void){
    if(!cpu.N)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void bmi(void){
    if(cpu.N)
        cpu.PC += (int8_t)mem_read(cpu.memaddr);
};
inline void bit(void){
    uint8_t val = mem_read(cpu.memaddr);
    cpu.Z = !(cpu.A & val);
    cpu.N = (val >> 7) & 1;
    cpu.V = (val >> 6) & 1;
};
inline void pha(void){
    cpu.SP--;
    mem_write(STACK_ADDR+cpu.SP, cpu.A);
};
inline void php(void){
    cpu.SP--;
    mem_write(STACK_ADDR+cpu.SP, cpu.P);
};
inline void pla(void){
    cpu.A = mem_read(STACK_ADDR+cpu.SP);
    cpu.SP++;
    SET_NZ(cpu.A);
};
inline void plp(void){
    cpu.P = mem_read(STACK_ADDR+cpu.SP);
    cpu.SP++;
};
inline void dey(void){
    cpu.Y--;
    SET_NZ(cpu.Y);
};
inline void tay(void){
    cpu.Y = cpu.A;
    SET_NZ(cpu.Y);
};
inline void iny(void){
    cpu.Y++;
    SET_NZ(cpu.Y);
};
inline void tya(void){
    cpu.A = cpu.Y;
    SET_NZ(cpu.A);
};
inline void txs(void){
    cpu.SP = cpu.X;
};
inline void txa(void){
    cpu.A = cpu.X;
    SET_NZ(cpu.A);
};
inline void tax(void){
    cpu.X = cpu.A;
};
inline void tsx(void){
    cpu.X = cpu.SP;
    SET_NZ(cpu.X);
};

int cpu_execop() {
    cpu.OP = mem_read(cpu.PC);
    cpu.addr_mode = addr_mode[cpu.OP];

    switch(cpu.addr_mode)
    {
        case Absolute:
            cpu.memaddr = mem_read16(cpu.PC+1);
            break;
        case AbsoluteX:
            // addr = mem_read16(cpu.PC+1) + cpu.X;
            // if(pagecrossed(addr-cpu.X, addr))
            //     cpu.cycles += pagecrosscyles[cpu.OP];
            //cpu.memaddr = addr;
            cpu.memaddr = mem_read16(cpu.PC+1) + cpu.X;
            break;
        case AbsoluteY:
            // addr = mem_read16(cpu.PC+1) + cpu.Y;
            // if(pagecrossed(addr-cpu.Y, addr))
            //     cpu.cycles += pagecrosscyles[cpu.OP];
            // cpu.memaddr = addr;
            cpu.memaddr = mem_read16(cpu.PC+1) + cpu.Y;
            break;
        case Accumulator:
            cpu.memaddr = cpu.A;
            break;
        case Immediate:
            cpu.memaddr = cpu.PC+1;
            break;
        case Implied:
            cpu.memaddr = 0;
            break;
        case IndexedIndirect:
            //cpu.memaddr = read16wrap((uint16_t)mem_read(cpu.PC+1)+cpu.X);
            cpu.memaddr = mem_read16((uint16_t)mem_read(cpu.PC+1)+cpu.X);
            break;
        case Indirect:
            //cpu.memaddr = read16wrap(mem_read16(cpu.PC+1));
            cpu.memaddr = mem_read16(mem_read16(cpu.PC+1));
            break;
        case IndirectIndexed:
            // addr = read16wrap((uint16_t)mem_read(cpu.PC+1))+cpu.Y;
            // if(pagecrossed(addr-cpu.Y, addr))
            //     cpu.cycles += pagecrosscyles[cpu.OP];
            // cpu.memaddr = addr;
            cpu.memaddr = mem_read16((uint16_t)mem_read(cpu.PC+1))+cpu.Y;
            break;
        case Relative:
            cpu.memaddr = cpu.PC+1;
            break;
        case ZeroPage:
            cpu.memaddr = mem_read(cpu.PC+1);
            break;
        case ZeroPageX:
            cpu.memaddr = mem_read(cpu.PC+1)+cpu.X;
            break;
        case ZeroPageY:
            cpu.memaddr = mem_read(cpu.PC+1)+cpu.Y;
            break;
    }

    // if(cpu.PC == 0x8230 || cpu.PC == 0x8046)
    //     printf("here");

    if(cpu.PC == 0x8060)
        printf("here");

    cpu.oldPC = cpu.PC;
    cpu.PC += optable_size[cpu.OP];

    optable[cpu.OP]();

    Disassemble6502Op();

    //usleep(50000);
}
