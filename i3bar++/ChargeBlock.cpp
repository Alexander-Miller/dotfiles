#include <string>
#include <vector>
#include <fstream>
#include <sstream>
#include "ChargeBlock.hpp"
#include "Block.hpp"
#include "ParseResult.hpp"
#include "Utils.hpp"
#include "Globals.hpp"
#include "Format.hpp"

#define CHARGE_FREQ 5

ChargeBlock::ChargeBlock() : Block(CHARGE_FREQ) {
    this->update();
}

void ChargeBlock::update() {
    const auto res = this->parse();
    switch (res.state) {
        // {Status, Percentage, Time Left}
        case ParseResult::State::Multi: {
            const std::string charging = "Charging";
            const std::string symbol
                = (Util::is_prefix(res.content_multi()[0], charging))
                ? SYM_PLUG
                : SYM_CHARGE;
            const std::string color
                = std::stoi(res.content_multi()[1].substr(0, res.content_multi().size()-1)) > 30
                ? COLOR_TXT
                : COLOR_ERR;
            const Format::cell_content c {color, res.content_multi()[2]};
            this->cache = Format::format_block_i3(symbol, {c});
            break;
        }
        default: {
            Format::cell_content c = {COLOR_ERR, "INVALID PARSE STATE"};
            this->cache = Format::format_block_i3(SYM_DATE, {c});
        }
    }
}

ParseResult ChargeBlock::parse() {
    const std::string acpi_stdout = Util::read_stdout("acpi", 64);
    std::string waste, status, time, percent;
    std::stringstream tokenizer {acpi_stdout};
    tokenizer >> waste >> waste >> status >> percent >> time;
    std::vector<std::string> ret {status, percent, time};
    return ParseResult::multi(std::make_unique<std::vector<std::string>>(ret));
}
