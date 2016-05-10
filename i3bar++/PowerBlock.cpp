#include <string>
#include "PowerBlock.hpp"
#include "aliases.hpp"


// #define DATE_FORMAT "date \"+%a %e %b\""
// #define DATE_FREQ   15
#define POWER_FREQ   5
#define CMD_CAPACITY "cat /sys/class/power_supply/BAT1/capacity"
#define CMD_STATUS   "cat /sys/class/power_supply/BAT1/status"

std::string get_stdout(const std::string& cmd, const int buffer_size) {
    std::shared_ptr<FILE> pipe(popen(cmd.c_str(), "r"), pclose);
    char buffer[128];
    std::string result = "";
    result.reserve(buffer_size);
    while (!feof(pipe.get())) {
        if (fgets(buffer, 128, pipe.get()) != NULL)
            result += buffer;
    }
    return result;
}

PowerBlock::PowerBlock() : Block("", POWER_FREQ) { ; }

// void DateBlock::update() {
//     ParseResult res = this->parse();
//     switch(res.state) {
//     default:
//         this->cache = res.content_single();
//     }
// }

// ParseResult PowerBlock::parse() const {
//     vecp_t res = std::make_unique<std::vector<std::string>> (get_stdout(CMD_CAPACITY, 128), get_stdout(CMD_STATUS, 64));
//     return ParseResult::multi(ref);
// }
