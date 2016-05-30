#include <memory>
#include <string>
#include "ParseResult.hpp"
#include "MusicBlock.hpp"
#include "Utils.hpp"
#include "Format.hpp"
#include "Globals.hpp"

#define MUSIC_CMD "mpc current"
#define MUSIC_FREQ -1

MusicBlock::MusicBlock() : Block(MUSIC_FREQ) {
    this->update();
}

ParseResult MusicBlock::parse() {
    std::string mpc_output = Util::read_stdout(MUSIC_CMD, 128);
    if (mpc_output.empty()) {
        return ParseResult::none();
    } else {
        return ParseResult::single(std::make_unique<std::string>(Util::rtrim(mpc_output)));
    }
}

void MusicBlock::update() {
    auto res = this->parse();
    switch (res.state) {
        case ParseResult::State::Single: {
            this->is_cache_valid = true;
            Format::cell_content c {COLOR_TXT, res.content_single()};
            this->cache = Format::format_block_i3(SYM_MUSIC, {c});
            break;
        }
        case ParseResult::State::None: {
            this->is_cache_valid = false;
            break;
        }
        default: {
            this->is_cache_valid = true;
            Format::cell_content c {SYM_MUSIC, "INVALID PARSE STATE"};
            this->cache = Format::format_block_i3(SYM_MUSIC, {c});
        }
    }
}
