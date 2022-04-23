#pragma once

#include "../BUS/Bus.h"

class Bus;

class olc6502 {

public:
    olc6502();
    ~olc6502();

    void ConnectBus(Bus *n) {
        bus = n;
    }

private:
    Bus *bus = nullptr;
    uint8_t read(uint16_t a);
    void write(uint16_t a, uint8_t d);

public:
    // SR FLAGS
    enum FLAGS6502 {
        C = (1 << 0),   // Carry Bit
        Z = (1 << 1),   // Zero
        I = (1 << 2),   // Disable Interrupts
        D = (1 << 3),   // Decimal Mode
        B = (1 << 4),   // Break
        U = (1 << 5),   // Unused
        V = (1 << 6),   // Overflow
        N = (1 << 7),   // Negative
    };
};