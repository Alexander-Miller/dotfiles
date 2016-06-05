#include <iostream>
#include <iomanip>
#include <chrono>
#include <thread>
#include <vector>
#include <memory>
#include <string>
#include <algorithm>
#include <functional>
#include <fstream>
#include <sstream>
#include <cmath>
#include "CpuBlock.hpp"
#include "ParseResult.hpp"
#include "Format.hpp"
#include "Globals.hpp"
#include "Utils.hpp"

#define CPU_FREQ 2
#define PROC_STAT "/proc/stat"

CpuBlock::CpuBlock()
    : Block(CPU_FREQ)
{
    auto new_measure = this->fetch_loads();
    this->previous_measure.reserve(new_measure.size());
    for (auto m : new_measure) this->previous_measure.push_back(m);
    std::this_thread::sleep_for(std::chrono::milliseconds(100));
    this->update();
}

void CpuBlock::update() {
    const auto res = this->parse();
    switch (res.state) {
        case ParseResult::State::Multi: {
            Format::cell_content sep {COLOR_TXT, " | "};
            std::vector<Format::cell_content> cells;
            cells.push_back({res.content_multi()[0], res.content_multi()[1]});
            unsigned int i = 1;
            while (i < res.content_multi().size()-1) {
                cells.push_back(sep);
                cells.push_back({res.content_multi()[++i], res.content_multi()[++i]});
            }
            this->cache = Format::format_block_i3(SYM_CPU, cells);
            break;
        }
        default: {
            Format::cell_content c {COLOR_ERR, "INVALID PARSE STATE"};
            this->cache = Format::format_block_i3(SYM_CPU, {c});
        }
    }
}

ParseResult CpuBlock::parse() {
    auto new_measure = this->fetch_loads();
    std::vector<std::string> ret;
    ret.reserve(new_measure.size());

    for (unsigned int i = 0; i < new_measure.size(); i++) {
        double total_new  = new_measure[i].total;
        double total_prev = this->previous_measure[i].total;
        double idle_new   = new_measure[i].idle;
        double idle_prev  = this->previous_measure[i].idle;
        double load_f     = ((total_new - total_prev)-(idle_new - idle_prev)) / (total_new - total_prev) * 100.0;
        if (std::isnan(load_f)) load_f = 100.0;
        std::stringstream loadstream;
        loadstream << std::fixed
                   << std::setprecision(0)
                   << std::setw(2)
                   << std::setfill('0')
                   << load_f
                   << "%";
        std::string color = load_f > 75.0 ? COLOR_ERR : COLOR_TXT;
        ret.push_back(color);
        ret.push_back(loadstream.str());
    }

    this->previous_measure.clear();
    for (auto m : new_measure) this->previous_measure.push_back(m);

    return ParseResult::multi(std::make_unique<std::vector<std::string>>(ret));
}

const std::vector<cpu_parse_info> CpuBlock::fetch_loads() const {
    const std::string cpu = "cpu";
    std::ifstream proc_stat;
    std::string line_buffer;
    std::string waste;
    std::vector<cpu_parse_info> ret;
    ret.reserve(this->previous_measure.size());

    proc_stat.open(PROC_STAT);

    std::getline(proc_stat, waste);
    std::getline(proc_stat, line_buffer);

    for (;Util::is_prefix(line_buffer, cpu); std::getline(proc_stat, line_buffer)) {
        std::stringstream tokenizer {line_buffer};
        std::string user,nice,system,idle,iowait,irq,softirq,steal,guest,guest_nice;
        tokenizer >> waste >> user >> nice >> system >> idle >> iowait >> irq >> softirq >> steal >> guest >> guest_nice;

        double all_idle = std::stod(idle) + std::stod(iowait);
        double all_non_idle
            = std::stod(user)
            + std::stod(nice)
            + std::stod(system)
            + std::stod(irq)
            + std::stod(softirq)
            + std::stod(steal);
        double total = all_non_idle + all_idle;
        ret.push_back({total, all_idle});
    }
    proc_stat.close();

    return ret;
}
