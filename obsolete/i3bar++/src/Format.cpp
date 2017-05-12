#include <iostream>
#include <string>
#include <sstream>
#include "Format.hpp"
#include "Globals.hpp"

void format_cell_i3(const std::string& txt, const std::string& color, std::stringstream& stream) {
    stream << "{\"full_text\":\"" << txt << "\","
           << "\"color\": \"" << color << "\","
           << "\"separator\":\"false\",\"separator_block_width\":0}";
}

std::string Format::format_block_i3(const std::string& symbol, const std::vector<cell_content>& content) {
    std::stringstream ret_stream;
    format_cell_i3(SYM_SEP, COLOR_SEP, ret_stream);
    ret_stream << ",";
    format_cell_i3(symbol, COLOR_SYM, ret_stream);
    ret_stream << ",";
    int s = content.size() - 1;
    for (int i = 0; i < s; i++) {
        auto color = content[i].color;
        auto text  = content[i].text;
        format_cell_i3(text, color, ret_stream);
        ret_stream << ",";
    }
    format_cell_i3(content[s].text, content[s].color, ret_stream);
    return ret_stream.str();
}
