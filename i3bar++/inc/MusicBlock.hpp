#ifndef MUSICBLOCK_H
#define MUSICBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class MusicBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    MusicBlock();
};

#endif /* MUSICBLOCK_H */
