#ifndef VOLUMEBLOCK_H
#define VOLUMEBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class VolumeBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    VolumeBlock();
};

#endif /* VOLUMEBLOCK_H */
