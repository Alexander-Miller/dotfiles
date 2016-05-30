#include <fstream>
#include <sstream>
#include "Format.hpp"
#include "Globals.hpp"
#include <MemBlock.hpp>
#include <ParseResult.hpp>

#define MEM_LOCATION "/proc/meminfo"
#define MEM_FREQ 6

MemBlock::MemBlock() : Block(MEM_FREQ) {
    this->update();
}

void MemBlock::update() {
    ParseResult res = this->parse();
    switch (res.state) {
        case ParseResult::State::Multi: {
            int mem_total = std::stoi(res.content_multi()[0]) / 1024;
            int mem_avail = std::stoi(res.content_multi()[1]) / 1024;

            std::stringstream text;
            text << mem_total - mem_avail << "MB/" << mem_total << "MB";
            Format::cell_content c {COLOR_TXT, text.str()};
            this->cache = Format::format_block_i3(SYM_RAM, {c});
            break;
        }
        default: {
            Format::cell_content c {COLOR_ERR, "INVALID PARSE STATE"};
            this->cache =  Format::format_block_i3(SYM_RAM, {c});
        }
    }
}

ParseResult MemBlock::parse() {
    // MemTotal:        3931644 kB
    // MemFree:          136404 kB
    // MemAvailable:    1618504 kB

    std::string mem_total, mem_avail, waste_buffer;
    std::ifstream mem_file;
    mem_file.open(MEM_LOCATION);

    mem_file >> waste_buffer
             >> mem_total
             >> waste_buffer
             >> waste_buffer
             >> waste_buffer
             >> waste_buffer
             >> waste_buffer
             >> mem_avail;

    mem_file.close();

    std::vector<std::string> result {mem_total, mem_avail};
    return ParseResult::multi(std::make_unique<std::vector<std::string>>(result));
}
