#ifndef CPUBLOCK_H
#define CPUBLOCK_H

#include <vector>
#include "Block.hpp"
#include "ParseResult.hpp"

struct cpu_parse_info {
    const double total;
    const double idle;
};

class CpuBlock : public Block {
    ParseResult parse() override;
public:
    void update() override;
    CpuBlock();
private:
    std::vector<cpu_parse_info> previous_measure;
    const std::vector<cpu_parse_info> fetch_loads() const;
};


#endif /* CPUBLOCK_H */
