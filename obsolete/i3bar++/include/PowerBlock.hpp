#ifndef POWERBLOCK_H
#define POWERBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class PowerBlock : public Block {
    ParseResult parse() const override;
public:
    void update() override;
    PowerBlock();
};

#endif /* POWERBLOCK_H */
