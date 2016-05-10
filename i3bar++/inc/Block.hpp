#ifndef BLOCK_H
#define BLOCK_H

#include <string>
#include "ParseResult.hpp"
#include "Globals.hpp"

class Block {
public:
    Block(const int);
    void update_maybe();
    const std::string& get_cache() const;
    const bool should_print() const;
protected:
    std::string cache;
    const int freq;
    bool is_cache_valid = true;
    int freq_count = 0;
    virtual void update() = 0;
    virtual ParseResult parse() = 0;
};

#endif /* BLOCK_H */
