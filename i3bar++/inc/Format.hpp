#ifndef FORMATTER_H
#define FORMATTER_H

#include <string>
#include <vector>

namespace Format {

    struct cell_content {
        const std::string& color;
        const std::string& text;
    };

    std::string format_block_i3(const std::string&, const std::vector<cell_content>&);
}

#endif /* FORMATTER_H */
