#include <string>
#include "Block.hpp"

Block::Block(const int f)
    : cache("")
    , freq(f)
{ ; }

void Block::update_maybe() {
    if (this->freq == -1) {
        this->update();
    } else if (this->freq_count == this->freq) {
        this->freq_count = 0;
        this->update();
    } else {
        this->freq_count++;
    }
}

bool Block::should_print() const {
    return this->is_cache_valid;
}

const std::string& Block::get_cache() const {
    return this->cache;
}
