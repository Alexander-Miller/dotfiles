#ifndef PARSERESULT_H
#define PARSERESULT_H

#include <memory>
#include <string>
#include <vector>

class ParseResult {
public:
    ParseResult(ParseResult&&);
    ~ParseResult();

    const enum State { Single, Multi, None, Error } state;

    const std::string& content_single() const;
    const std::string& error_msg() const;
    const std::vector<std::string>& content_multi() const;

    static ParseResult single(std::unique_ptr<std::string>);
    static ParseResult multi(std::unique_ptr<std::vector<std::string>>);
    static ParseResult error(std::unique_ptr<std::string>);
    static ParseResult none();
private:
    union {
        std::unique_ptr<std::string> content_s;
        std::unique_ptr<std::vector<std::string>> content_m;
    };

    ParseResult(State, std::unique_ptr<std::string>);
    ParseResult(State, std::unique_ptr<std::vector<std::string>>);
    ParseResult(State);
};

#endif /* PARSERESULT_H */
