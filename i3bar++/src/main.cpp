#include <iostream>
#include "ParseResult.hpp"
#include "Block.hpp"
#include "VolumeBlock.hpp"
#include "TimeBlock.hpp"
#include "DateBlock.hpp"
#include "ChargeBlock.hpp"
#include "MemBlock.hpp"
#include "MusicBlock.hpp"
#include "CpuBlock.hpp"
#include "WifiBlock.hpp"
#include "Utils.hpp"

enum class Signal {
    Volume,
    Music,
    Sleep,
    Wifi,
    NoMatch,
};

Signal match_signal(const std::string& str) {
    const std::string ident_vol    = "Event 'change' on sink #0";
    const std::string ident_music  = "player";
    const std::string ident_update = "update";
    const std::string ident_wifi   = "New Access Point/Cell";

    if (Util::is_prefix(str, ident_vol)) {
        return Signal::Volume;
    } else if (Util::is_prefix(str, ident_music)) {
        return Signal::Music;
    } else if (Util::is_prefix(str, ident_update)) {
        return Signal::Sleep;
    } else if (str.find(ident_wifi)) {
        return Signal::Wifi;
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

int main() {
    std::cout.sync_with_stdio(false);

    VolumeBlock volume;
    TimeBlock   time;
    DateBlock   date;
    ChargeBlock charge;
    WifiBlock   wifi;
    MemBlock    memory;
    CpuBlock    cpu;
    MusicBlock  music;

    std::vector<Block*> all_blocks {&music, &cpu, &memory, &charge, &wifi, &date, &time, &volume};
    std::vector<Block*> no_event_blocks {&cpu, &memory, &charge, &date, &time};

    std::cout << "{\"click_events\": true, \"version\": 1}[[],";

    print_all(all_blocks);

    std::string stdin_buffer;

    while (true) {
        std::getline(std::cin, stdin_buffer);
        Signal signal = match_signal(stdin_buffer);
        switch (signal) {
            case Signal::Volume:
                volume.update_maybe();
                break;
            case Signal::Music:
                music.update_maybe();
                break;
            case Signal::Wifi:
                wifi.update_maybe();
                break;
            case Signal::Sleep:
                for (auto block : no_event_blocks) block->update_maybe();
                break;
            case Signal::NoMatch: {
                break;
            }
        }
        if (signal != Signal::NoMatch) {
            print_all(all_blocks);
        }
    }
    return 0;
}
