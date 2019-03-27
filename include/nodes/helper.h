#ifndef DAGGR_NODE_HELPER_H
#define DAGGR_NODE_HELPER_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Local Includes -----*/

#include "../meta/function_traits.h"
#include "../meta/lifecycle_traits.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  // Helper meta-functions to write our applicability in terms of our children.
  template <class Node, class Input>
  struct child_is_applicable : Node::template is_applicable<Input> {
    using canonical_type = std::bool_constant<Node::template is_applicable<Input>::value>;
  };
  template <class Node, class Input>
  constexpr auto child_is_applicable_v = child_is_applicable<Node, Input>::value;

  template <class Node, class Input>
  struct child_apply_result : Node::template apply_result<Input> {};
  template <class Node, class Input>
  using child_apply_result_t = typename child_apply_result<Node, Input>::type;

}

#endif
