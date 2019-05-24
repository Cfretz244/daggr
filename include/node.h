#ifndef DAGGR_NODES_H
#define DAGGR_NODES_H

/*----- System Includes -----*/

#include <dart.h>
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
    class erased;

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
  template <>
  struct is_node<node::erased> : std::true_type {};
  template <class T>
  constexpr auto is_node_v = is_node<T>::value;

  template <class T>
  struct is_packet : std::false_type {};
  template <>
  struct is_packet<dart::packet> : std::true_type {};
  template <class T>
  constexpr auto is_packet_v = is_packet<T>::value;

}

/*----- Local Includes -----*/

#include "meta/lifecycle_traits.h"
#include "meta/function_traits.h"
#include "node/helper.h"
#include "node/erased.h"
#include "node/comp.h"
#include "node/seq.h"
#include "node/all.h"

/*----- Globals -----*/

namespace daggr {

  namespace detail {
    struct noop_comp {
      dart::packet operator ()(dart::packet const& pkt) {
        return pkt;
      }
    };
  }

  // Used to bootstrap the chaining API.
  inline daggr::node::comp<detail::noop_comp> const noop;

}

#endif
