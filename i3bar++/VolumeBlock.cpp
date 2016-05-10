#include <sstream>
#include <string>
#include <memory>
#include <iostream>
#include <regex>
#include "VolumeBlock.hpp"
#include "ParseResult.hpp"
#include "Block.hpp"
#include "Utils.hpp"
#include "Format.hpp"
#include "Globals.hpp"

#define VOL_CMD "amixer get Master"
#define VOL_FREQ 2

VolumeBlock::VolumeBlock() : Block(VOL_FREQ) {
    this->update();
}

ParseResult VolumeBlock::parse() {
    // Simple mixer control 'Master',0
    //   Capabilities: pvolume pswitch pswitch-joined
    //   Playback channels: Front Left - Front Right
    //   Limits: Playback 0 - 65536
    //   Mono:
    //   Front Left: Playback 0 [0%] [on]
    //   Front Right: Playback 0 [0%] [on]
    const std::string amixer_output = Util::read_stdout(VOL_CMD, 256);
    const std::regex vol_regex{"[[:alnum:]]+%"};
    std::smatch match;

    if (std::regex_search(amixer_output.begin(), amixer_output.end(), match, vol_regex)) {
        return ParseResult::single(std::make_unique<std::string>(match[0]));
    } else {
        return ParseResult::error(std::make_unique<std::string>("Volume Parse Error"));
    }
}

void VolumeBlock::update() {
    const ParseResult res = this->parse();
    switch (res.state) {
        case ParseResult::State::Single: {
            Format::cell_content c {COLOR_TXT, res.content_single()};
            this->cache = Format::format_block_i3(SYM_VOL, {c});
            break;
        }
        case ParseResult::State::Error: {
            Format::cell_content c {COLOR_ERR, res.error_msg()};
            this->cache = Format::format_block_i3(SYM_VOL, {c});
            break;
        }
        default: {
            Format::cell_content c {COLOR_ERR, "INVALID PARSE STATE"};
            this->cache = Format::format_block_i3(SYM_VOL, {c});
            break;
        }
    }
}
