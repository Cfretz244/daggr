#ifndef DAGGR_NODE_HELPER_H
#define DAGGR_NODE_HELPER_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Local Includes -----*/

#include "../node.h"

/*----- Type Declarations -----*/

namespace daggr::node::detail {

  // Helper meta-functions to write our applicability in terms of our children.
  template <class Node, class Input>
  struct child_is_applicable : Node::template is_applicable<Input> {
    using canonical_type = std::bool_constant<Node::template is_applicable<Input>::value>;
  };
  template <class Node, class Input>
  constexpr auto child_is_applicable_v = child_is_applicable<Node, Input>::value;

  // Helper meta-functions to write our results in terms of our children.
  template <class Node, class Input>
  struct child_apply_result : Node::template apply_result<Input> {};
  template <class Node, class Input>
  using child_apply_result_t = typename child_apply_result<Node, Input>::type;

  // Global empty lambda to use for defaulted parameters.
  constexpr auto noop_v = [] {};
  using noop_t = decltype(noop_v) const&;

  template <class MaybeNode>
  decltype(auto) normalize(MaybeNode&& mnode) {
    if constexpr (daggr::is_node_v<std::decay_t<MaybeNode>>) {
      return std::forward<MaybeNode>(mnode);
    } else {
      return comp {std::forward<MaybeNode>(mnode)};
    }
  }

  template <class MaybeNode>
  using normalize_t = std::decay_t<decltype(normalize(std::declval<MaybeNode>()))>;

}

#endif
