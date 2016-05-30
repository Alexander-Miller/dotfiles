#ifndef MEMBLOCK_H
#define MEMBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class MemBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    MemBlock();
};

#endif /* MEMBLOCK_H */
