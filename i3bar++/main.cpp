#include <iostream>
#include <iomanip>
#include "ParseResult.hpp"
#include "Block.hpp"
#include "VolumeBlock.hpp"
#include "TimeBlock.hpp"
#include "DateBlock.hpp"
#include "ChargeBlock.hpp"
#include "MemBlock.hpp"
#include "MusicBlock.hpp"
#include "CpuBlock.hpp"
#include "Utils.hpp"

enum class Signal {
    Volume,
    Music,
    Sleep,
    NoMatch,
};

Signal match_signal(const std::string& str) {
    const std::string prefix_volume = "Event 'change' on sink #0";
    const std::string prefix_music  = "player";
    const std::string prefix_update = "update";

    if (Util::is_prefix(str, prefix_volume)) {
        return Signal::Volume;
    } else if (Util::is_prefix(str, prefix_music)) {
        return Signal::Music;
    } else if (Util::is_prefix(str, prefix_update)) {
        return Signal::Sleep;
    } else {
        return Signal::NoMatch;
    }
}

void print_all(std::vector<Block*>& all_blocks) {
    unsigned int i = 0;
    while (!all_blocks[i]->should_print()) i++;
    std::cout << "[" << all_blocks[i]->get_cache();
    while (++i < all_blocks.size()) {
        if (all_blocks[i]->should_print()) std::cout << "," << all_blocks[i]->get_cache();
    }
    std::cout << "],";
    std::cout.flush();
}

FILE* open_stream() {
    const char* worker_cmd = "echo \"pactl subscribe &; mpc idleloop player &; while true; echo \"update\"; sleep 1; end\" | fish";
    FILE* stream = popen(worker_cmd, "r");
    if (stream) {
        return stream;
    } else {
        exit(1);
    }
}

int main(int argc, char *argv[]) {
    std::cout.sync_with_stdio(false);

    VolumeBlock volume;
    TimeBlock time;
    DateBlock date;
    ChargeBlock charge;
    MemBlock memory;
    CpuBlock cpu;
    MusicBlock music;

    std::vector<Block*> all_blocks {&music, &cpu, &memory, &charge, &date, &time, &volume};
    std::vector<Block*> no_event_blocks {&cpu, &memory, &charge, &date, &time};
    // std::vector<Block*> all_blocks {&cpu, &volume};
    // std::vector<Block*> no_event_blocks {&cpu};

    std::cout << "{\"click_events\": true, \"version\": 1}[[],";

    print_all(all_blocks);

    FILE* stream = open_stream();
    while (true) {
        char buffer[128];
        fgets(buffer, 128, stream);
        Signal s = match_signal(buffer);
        switch (s) {
            case Signal::Volume:
                volume.update_maybe();
                break;
            case Signal::Music:
                music.update_maybe();
                break;
            case Signal::Sleep:
                for (auto block : no_event_blocks) block->update_maybe();
                break;
            case Signal::NoMatch: { ; }
        }
        if (s != Signal::NoMatch) {
            print_all(all_blocks);
        }
    }
    return 0;
}
