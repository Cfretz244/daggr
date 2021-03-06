#ifndef DAGGR_NODES_H
#define DAGGR_NODES_H

/*----- System Includes -----*/

#include <chrono>
#include <optional>
#include <functional>
#include <type_traits>

/*----- Type Declarations -----*/

namespace daggr {

  namespace node {

    template <class>
    class comp;
    template <class, class>
    class seq;
    template <class...>
    class all;
    template <class>
    class win;

    using clock = std::chrono::steady_clock;

  }

  template <class T>
  struct is_node : std::false_type {};
  template <class T>
  struct is_node<node::comp<T>> : std::true_type {};
  template <class P, class C>
  struct is_node<node::seq<P, C>> : std::true_type {};
  template <class... Ts>
  struct is_node<node::all<Ts...>> : std::true_type {};
  template <class T>
  constexpr auto is_node_v = is_node<T>::value;

}

/*----- Local Includes -----*/

#include "meta/lifecycle_traits.h"
#include "meta/function_traits.h"
#include "node/helper.h"
#include "node/comp.h"
#include "node/seq.h"
#include "node/all.h"
#include "node/win.h"

/*----- Globals -----*/

namespace daggr {

  // Used to bootstrap the chaining API.
  inline daggr::node::comp<daggr::meta::none> const noop;

}

#endif
