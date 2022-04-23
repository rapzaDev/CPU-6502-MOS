#pragma once

#include <cstdint>
#include <array>

#include "../CPU/olc6502.h"

using namespace std;

class Bus {

public:
    Bus();
    ~Bus();

public: // Devices on Bus
    olc6502 cpu;

    //Fake RAM memory
    array<uint8_t, 64 * 1024> ram;


public: // Bus Read & Write
    void write(uint16_t addr, uint8_t data);
    uint8_t read(uint16_t addr, bool bReadOnly = false);
    
};

