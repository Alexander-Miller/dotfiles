#include <string>
#include <sstream>
#include <memory>
#include <chrono>
#include <iomanip>
#include "TimeBlock.hpp"
#include "ParseResult.hpp"
#include "Format.hpp"
#include "Globals.hpp"

#define TIME_FREQ 5

TimeBlock::TimeBlock() : Block(TIME_FREQ) {
    this->update();
}

void TimeBlock::update() {
    const ParseResult res = this->parse();
    switch(res.state) {
        case ParseResult::State::Single: {
            Format::cell_content c = {COLOR_TXT, res.content_single()};
            this->cache = Format::format_block_i3(SYM_TIME, {c});
            break;
        }
        default: {
            Format::cell_content c = {COLOR_ERR, "INVALID PARSE STATE"};
            this->cache = Format::format_block_i3(SYM_TIME, {c});
        }
    }
}

ParseResult TimeBlock::parse() {
    const std::time_t now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now() - std::chrono::hours(24));
    std::stringstream ss;
    ss << std::put_time(std::localtime(&now), "%T");
    return ParseResult::single(std::make_unique<std::string>(ss.str()));
}
