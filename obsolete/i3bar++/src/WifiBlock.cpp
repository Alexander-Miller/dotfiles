#include <algorithm>
#include <iostream>
#include <fstream>
#include <memory>
#include <string>
#include <sstream>
#include <regex>
#include "WifiBlock.hpp"
#include "ParseResult.hpp"
#include "Format.hpp"
#include "Globals.hpp"
#include "Utils.hpp"

#define WIFI_FREQ -1
#define WIFI_CMD "iwgetid --raw"

WifiBlock::WifiBlock() : Block(WIFI_FREQ) {
    this->update();
}

void WifiBlock::update() {
    const auto res = this->parse();
    switch (res.state) {
        case ParseResult::State::Single: {
            this->is_cache_valid = true;
            Format::cell_content c {COLOR_TXT, res.content_single()};
            this->cache = Format::format_block_i3(SYM_WIFI, {c});
            break;
        }
        case ParseResult::State::None: {
            this->is_cache_valid = false;
            break;
        }
        default: {
            Format::cell_content c {COLOR_ERR, "INVALID PARSE STATE"};
            this->cache =  Format::format_block_i3(SYM_RAM, {c});
        }
    }
}

ParseResult WifiBlock::parse() {
    std::string wifi_cmd_stdout = Util::read_stdout(WIFI_CMD, 64);
    if (wifi_cmd_stdout.empty()) {
        return ParseResult::none();
    } else {
        return ParseResult::single(std::make_unique<std::string>(Util::rtrim(wifi_cmd_stdout)));
    }
}
