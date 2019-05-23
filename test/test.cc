#include <iostream>
#include "../include/node.h"

using namespace dart::literals;

int main() {
  auto wrap = daggr::noop.then([] (auto pkt) {
    return dart::packet::make_object("wrapped", std::move(pkt));
  });
  auto print = wrap.then([] (auto pkt) {
    std::cout << pkt << std::endl;
    return pkt;
  });
  auto combined = wrap.join(print).then([] (auto comb) {
    auto printed = comb.back();
    auto wrapped = comb.front();
    return dart::packet::make_object("wrapped", std::move(wrapped), "printed", std::move(printed));
  });
  std::cout << combined("hello"_dart) << std::endl;
}
