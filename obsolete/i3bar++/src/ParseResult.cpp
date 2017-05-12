#include <memory>
#include <string>
#include <iostream>
#include <vector>
#include "ParseResult.hpp"
#include "Globals.hpp"

ParseResult::ParseResult(const State s, std::unique_ptr<std::string> strp)
    : state(s)
    , content_s(std::move(strp))
{ ; }

ParseResult::ParseResult(const State s, std::unique_ptr<std::vector<std::string>> vecp)
    : state(s)
    , content_m(std::move(vecp))
{ ; }

ParseResult::ParseResult(const State s)
    : state(s)
{ ; }

ParseResult::~ParseResult() {
    using strp_t = std::unique_ptr<std::string>;
    using vecp_t = std::unique_ptr<std::vector<std::string>>;
    switch (this->state) {
    case Single:
        case Error:
            this->content_s.~strp_t();
            break;
        case Multi:
            this->content_m.~vecp_t();
            break;
        case None: { ; }
    }
}

const std::string& ParseResult::content_single() const {
    return *this->content_s;
}

const std::string& ParseResult::error_msg() const {
    return *this->content_s;
}

const std::vector<std::string>& ParseResult::content_multi() const {
    return *this->content_m;
}

ParseResult ParseResult::single(std::unique_ptr<std::string> strp) {
    return ParseResult { Single, std::move(strp) };
}

ParseResult ParseResult::multi(std::unique_ptr<std::vector<std::string>> vecp) {
    return ParseResult { Multi, std::move(vecp) };
}

ParseResult ParseResult::error(std::unique_ptr<std::string> strp) {
    return ParseResult { Error, std::move(strp) };
}

ParseResult ParseResult::none() {
    return ParseResult { None };
}
