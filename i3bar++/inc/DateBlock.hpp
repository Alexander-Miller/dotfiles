#ifndef DATEBLOCK_H
#define DATEBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class DateBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    DateBlock();
};

#endif /* DATEBLOCK_H */
