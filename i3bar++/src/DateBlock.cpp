#include <chrono>
#include <sstream>
#include <memory>
#include <iomanip>
#include "DateBlock.hpp"
#include "ParseResult.hpp"
#include "Format.hpp"
#include "Globals.hpp"

DateBlock::DateBlock() : Block(20) {
    this->update();
}

void DateBlock::update() {
    const ParseResult res = this->parse();
    switch(res.state) {
        case ParseResult::State::Single: {
            Format::cell_content c = {COLOR_TXT, res.content_single()};
            this->cache = Format::format_block_i3(SYM_DATE, {c});
            break;
        }
        default: {
            Format::cell_content c = {COLOR_ERR, "INVALID PARSE STATE"};
            this->cache = Format::format_block_i3(SYM_DATE, {c});
        }
    }
}

ParseResult DateBlock::parse() {
    const std::time_t now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
    std::stringstream ss;
    ss << std::put_time(std::localtime(&now), "%a %e %b");
    return ParseResult::single(std::make_unique<std::string>(ss.str()));
}
