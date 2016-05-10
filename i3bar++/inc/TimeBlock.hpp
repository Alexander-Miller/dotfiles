#ifndef TIMEBLOCK_H
#define TIMEBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class TimeBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    TimeBlock();
};

#endif /* TIMEBLOCK_H */
