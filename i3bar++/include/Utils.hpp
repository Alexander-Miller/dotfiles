#ifndef UTILS_H
#define UTILS_H

#include <string>

namespace Util {
    const std::string read_stdout(const std::string&, const int);
    const std::string& rtrim(std::string& s);
    bool is_prefix(const std::string& str, const std::string& prefix);
}

#endif /* UTILS_H */
