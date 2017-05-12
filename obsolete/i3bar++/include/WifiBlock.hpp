#ifndef WIFIBLOCK_H
#define WIFIBLOCK_H

#include "Block.hpp"
#include "ParseResult.hpp"

class WifiBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    WifiBlock();
};

#endif /* WIFIBLOCK_H */
