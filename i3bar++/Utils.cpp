#include <string>
#include <memory>
#include <functional>
#include <algorithm>
#include "Utils.hpp"

const std::string Util::read_stdout(const std::string& cmd, const int result_buffer_size) {
    std::shared_ptr<FILE> pipe(popen(cmd.c_str(), "r"), pclose);
    char buffer[128];
    std::string result;
    result.reserve(result_buffer_size);
    while(!feof(pipe.get())) {
        if (fgets(buffer, 128, pipe.get()) != NULL) {
            result.append(buffer);
        }
    }
    return result;
}

const std::string& Util::rtrim(std::string &s) {
        s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
        return s;
}

bool Util::is_prefix(const std::string &str, const std::string& prefix) {
    return (!str.compare(0, prefix.size(), prefix));
}
