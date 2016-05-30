#ifndef CHARGEBLOCK_H
#define CHARGEBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class ChargeBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    ChargeBlock();
};

#endif /* CHARGEBLOCK_H */
