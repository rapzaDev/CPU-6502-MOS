#include "./olc6502.h"

olc6502::olc6502() {

    using a = olc6502;
    
    // Populated sisxteen by sisxteen matrix:
    // { "Mnemonic", <Opcode>, <Addressing Mode>, <Clock Cycles> },
    lookup = 
    {
        { "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
        { "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
        { "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
        { "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
        { "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
        { "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
        { "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
    };
    
}

olc6502::~olc6502() {
    
}

uint8_t olc6502::read(uint16_t a) {
    return bus->read(a, false);
}

void olc6502::write(uint16_t a, uint8_t d) {
    bus->write(a,d);
}

// Only will execute when the internal cycles variable is equal to zero.
void olc6502::clock() {
    if (cycles == 0) {
        opcode = read(pc);
        pc++;

        // Get Starting number of cycles
        cycles = lookup[opcode].cycles;
        
        uint8_t additional_cycle1 = (this->*lookup[opcode].addrmode)();
        
        uint8_t additional_cycle2 = (this->*lookup[opcode].operate)();

        cycles += (additional_cycle1 & additional_cycle2);
    }

    // Everytime the clock function is called the number of cycles is decremented
    cycles--;

}

/**Set the value of SR */
void olc6502::setFlag(FLAGS6502 f, bool v) {
    if (v) {
        status |= f;
    } else {
        status &= ~f;
    }
}

// ---------------------------------------- Addressing Modes ----------------------------------------

// <IMPLIED MODE>
uint8_t olc6502::IMP() {
    fetched = a;
    return 0;
}

// <IMMEDIATE MODE> 
//  THE DATA IS IN THE NEXT BYTE
uint8_t olc6502::IMM() {
    addr_abs = pc++;
    return 0;
}

// <ZERO PAGE MODE>
// THE DATA IS LOCATED SOMEWHERE IN PAGE 0.
// The 6502 tend to have their working memory located around page zero.
// Because this is a way of directly acessing those bytes with
// instructions that require fewer bytes. Instructions consist of multiple bytes and each byte 
// takes time to read, so in order to optimize the speed of the programm, it just can read in 
// the low byte of the 0th Page.
uint8_t olc6502::ZP0() {
    addr_abs = read(pc);
    pc++;
    addr_abs &= 0X00FF; // Mask for page zero and the offset.
    return 0;
}

// <ZERO PAGE WITH X REGISTER OFFSET>
// This is useful for iterating through regions of memory.
uint8_t olc6502::ZPX() {
    addr_abs = (read(pc) + x);
    pc++;
    addr_abs &= 0X00FF; // Mask for page zero and the offset.
    return 0;
}

// <ZERO PAGE WITH Y REGISTER OFFSET>
// This is useful for iterating through regions of memory.
uint8_t olc6502::ZPY() {
    addr_abs = (read(pc) + y);
    pc++;
    addr_abs &= 0X00FF; // Mask for page zero and the offset.
    return 0;
}

// <ABSOLUTE ADDRESS>
// Is the absolute address suplied whit the instructions will have 3 byte
// consisted in the low byte and a high byte fo the address.
uint8_t olc6502::ABS() {
    uint16_t lo = read(pc);
    pc++;
    uint16_t hi = read(pc);
    pc++;

    addr_abs = (hi << 8) | lo;

    return 0;
}

// <ABSOLUTE ADDRESS WITH X REGISTER OFFSET>
uint8_t olc6502::ABX() {
    uint16_t lo = read(pc);
    pc++;
    uint16_t hi = read(pc);
    pc++;

    addr_abs = (hi << 8) | lo;
    addr_abs += x;

    // if the page has changed to a different page, 
    // the functon need to indicate to the system that it may need
    // an additional clock cycle. To do that is only verify if the 
    // high byte is changed after added the X valeu to it, because if it has
    // changed it's changed due to overflow, the carry bit from the lower byte
    // has been carried to the high byte therefore it changed page.
    if ( (addr_abs & 0XFF00) != (hi << 8) )
        return 1;
    else 
        return 0;

    return 0;
}

// <ABSOLUTE ADDRESS WITH Y REGISTER OFFSET>
uint8_t olc6502::ABY() {
    uint16_t lo = read(pc);
    pc++;
    uint16_t hi = read(pc);
    pc++;

    addr_abs = (hi << 8) | lo;
    addr_abs += y;

    if ( (addr_abs & 0XFF00) != (hi << 8) )
        return 1;
    else 
        return 0;

    return 0;
}

// Indirect Addressing of the zero page 
uint8_t olc6502::IND() {
    uint16_t ptr_lo = read(pc);
    pc++;
    uint16_t ptr_hi = read(pc);
    pc++;

    uint16_t ptr = (ptr_hi << 8) | (ptr_lo);

    // ---------- BUG AVOID CONTROL ---------------
        // Simulate page boundary hardware bug
        if (ptr_lo == 0x00FF) {

            // the low byte is the original address and the high byte
            // is the new address.
            addr_abs = ( read(ptr & 0xFF00) << 8 ) | read(ptr + 0);
        } 
        else { // Behave normally 
            
            // the low byte is the original address and the high byte
            // is the new address.
            addr_abs = ( read(ptr + 1) << 8 ) | read(ptr + 0);
        }

    return 0;
}

// Indirect Addressing of the zero page with X Offset
uint8_t olc6502::IZX() {
    uint16_t t = read(pc);
    pc++;

    uint16_t lo = read( (uint16_t)(t + (uint16_t)x) & 0X00FF );
    uint16_t hi = read( (uint16_t)(t + (uint16_t)x + 1) & 0X00FF );

    addr_abs = (hi << 8) | lo;

    return 0;
}

// Indirect Addressing of the zero page with Y Offset
uint8_t olc6502::IZY() {

    uint16_t t = read(pc);
    pc++;

    uint16_t lo = read(t & 0X00FF) ;
    uint16_t hi = read((t +  1) & 0X00FF);

    addr_abs = (hi << 8) | lo;
    addr_abs += y;

    // if the page has changed to a different page, 
    // the functon need to indicate to the system that it may need
    // an additional clock cycle. To do that is only verify if the 
    // high byte is changed after added the X valeu to it, because if it has
    // changed it's changed due to overflow, the carry bit from the lower byte
    // has been carried to the high byte therefore it changed page.
    if ((addr_abs & 0XFF00) != (hi << 8)) 
        return 1;
    else
        return 0;

    return 0;
}

// Relative Addressing Mode
/*This function only applies to branching instructions.
 Branching Instructions can't jump to anywhere in the address range,
 they can only jump to a location. It can't jump anywhere further away
 than 127 memory locations.
 */
uint8_t olc6502::REL() {
    addr_rel = read(pc);
    pc++;

    // if the higher bit is 1, I will set thye high byte 
    // of the relative address to all ones.
    if (addr_rel & 0X80) addr_rel |= 0XFF00;

    return 0; 
}

// ---------------------------------------- Instructions ----------------------------------------

uint8_t olc6502::fetch() {
    // If is equal to Implied Address Mode, don't do nothing, because
    // in nothing to fetch.
    if ( !(lookup[opcode].addrmode == &olc6502::IMP) ) 
        fetched = read(addr_abs);

    return fetched;
}

/*
    The AND operation performs a logic bitwise AND between the accumulator
    and the data that's been fetched.
*/
uint8_t olc6502::AND() {
    fetch();
    a = a & fetched;

    /*  If the result of the logic AND resulted in all the bits being zero,
        I set the Zero flag. */
    setFlag(Z, a == 0X00);

    /* The Negative flag will be setted if the bit seven is equal to one. */
    setFlag(N, a & 0X80);
    
    // this instruction require an additional clocl cycle
    return 1;
} 

// BRANCHING INSTRUCTION
/*  Is the branch if the carry bit of the SR is Set Instruction.
    So, is checked if the carry bit is equal to 1.
*/
uint8_t olc6502::BCS() {
    if ( getFlag(C) == 1 ) {

        /*  Branch instructions are unique*. These will directly modify the cycles variable.
            When a branch is taken, that automatically adds 1 to the required number of cycles
            for that instruction.
        */
        cycles++;
        addr_abs = pc + addr_rel;

        /*  If the branch needs to cross a page boundary, it incurs a second cycle of clock penalty.*/
        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// BRANCH CARRY CLEAR
uint8_t olc6502::BCC() {
    if ( getFlag(C) == 0 ) {

        cycles++;
        addr_abs = pc + addr_rel;

        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// BRANCH IF EQUAL
uint8_t olc6502::BEQ() {
    if ( getFlag(Z) == 1 ) {

        cycles++;
        addr_abs = pc + addr_rel;

        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// BRANCH IF NEGATIVE
uint8_t olc6502::BMI() {
    if ( getFlag(N) == 1 ) {

        cycles++;
        addr_abs = pc + addr_rel;

        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// BRANCH IF NOT EQUAL
uint8_t olc6502::BNE() {
    if ( getFlag(Z) == 0 ) {

        cycles++;
        addr_abs = pc + addr_rel;

        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// BRANCH IF POSITIVE
uint8_t olc6502::BPL() {
    if ( getFlag(N) == 0 ) {

        cycles++;
        addr_abs = pc + addr_rel;

        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// BRANCH IF OVERFLOWED
uint8_t olc6502::BVC() {
    if ( getFlag(V) == 0 ) {

        cycles++;
        addr_abs = pc + addr_rel;

        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// BRANCH IF NOT  OVERFLOWED
uint8_t olc6502::BVS() {
    if ( getFlag(V) == 1 ) {

        cycles++;
        addr_abs = pc + addr_rel;

        if ( (addr_abs & 0XFF00) != (pc & 0XFF00) ) cycles++;

        pc = addr_abs;
    }

    return 0;
} 

// CLEAR THE CARRY BIT ( SETS A BIT IN THE STATUS REGISTER)
uint8_t olc6502::CLC() {
    setFlag(C, false);
    return 0;
} 

uint8_t olc6502::CLD() {
    setFlag(D, false);
    return 0;
} 

// ADDITION INSTRUCTION
/*
    Overflow logic: V = ( A ^ R )  &  ~( A ^ M )  ;
    V: Overflow
    A: Accumulator
    R: Result
    M: Data
*/
uint8_t olc6502::ADC() {
    fetch();

    /*  Working in a 16bit domain, allows to easily check if I need to have a carry
        bit out, because the high byte of the 16bits will have a bit set in it.*/
    uint16_t temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)getFlag(C);

    /*  Set my carry out flag*/
    setFlag(C, temp > 255);

    /*  Zero flag*/
    setFlag(Z, (temp & 0X00FF) == 0);

    /*  Negative flag*/
    setFlag(N, temp & 0X80);

    /*  Overflow flag. Since I want to look to the most significant bit of the low Byte, I apllied 
        the 0X0080 mask.
    */
    setFlag(V, ( ~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0X0080);

    /*  Store the result back to the accumulator*/
    a = temp & 0X00FF;

    /*  It can will require an additional clock cycle*/
    return 1;

}

// SUBTRACTION INSTRUCTION
uint8_t olc6502::SBC() {
    
}


