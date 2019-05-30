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

  template <class Node, class Input>
  struct child_has_result :
    std::conjunction<
      child_is_applicable<Node, Input>,
      std::negation<
        std::is_same<
          child_apply_result_t<Node, Input>,
          meta::none
        >
      >
    >
  {};
  template <class Node, class Input>
  static constexpr auto child_has_result_v = child_has_result<Node, Input>::value;

  struct noop {
    void operator ()() const noexcept {}
    template <class Arg>
    Arg&& operator ()(Arg&& argument) const noexcept {
      return std::forward<Arg>(argument);
    }
  };
  struct indirect {
    template <class Func, class Succ>
    static constexpr auto nothrow_indirect_v = noexcept(std::declval<Succ>()(std::declval<Func>()()));

    template <class Func, class Succ>
    decltype(auto) operator ()(Func&& func, Succ&& succ) const noexcept(nothrow_indirect_v<Func, Succ>) {
      return std::invoke(std::forward<Succ>(succ), std::invoke(std::forward<Func>(func)));
    }
  };

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
