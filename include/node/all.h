#ifndef DAGGR_ALL_H
#define DAGGR_ALL_H

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  namespace detail {
    template <class Arg, class... Args>
    void expand_tuple_impl(dart::packet& pkt, Arg&& arg, Args&&... the_args) {
      pkt.push_back(std::forward<Arg>(arg));
      if constexpr (sizeof...(Args)) {
        expand_tuple_impl(pkt, std::forward<Args>(the_args)...);
      }
    }
    template <class Tup, size_t... idxs>
    dart::packet expand_tuple(Tup&& tuple, std::index_sequence<idxs...>) {
      auto arr = dart::packet::make_array();
      expand_tuple_impl(arr, std::get<idxs>(std::forward<Tup>(tuple))...);
      return arr;
    }
  }

  template <class... Nodes>
  class all {

    template <class>
    using dart_t = dart::packet;

    public:

      /*----- Lifecycle Functions -----*/

      template <class Storage = std::tuple<Nodes...>, class =
        std::enable_if_t<
          std::is_default_constructible_v<Storage>
        >
      >
      all() : nodes(std::make_shared<Storage>()) {}
      template <class... Ns, class =
        std::enable_if_t<
          sizeof...(Ns) == sizeof...(Nodes)
          &&
          !std::is_same_v<
            std::decay_t<meta::first_type_t<Ns...>>,
            all
          >
        >
      >
      all(Ns&&... nodes) :
        nodes(std::make_shared<std::tuple<Nodes...>>(detail::normalize(std::forward<Ns>(nodes))...))
      {}

      all(all const& other) : nodes(std::make_shared<std::tuple<Nodes...>>(*other.nodes)) {}
      all(all&&) = default;
      ~all() = default;

      /*----- Operators -----*/

      all& operator =(all const& other) {
        if (this == &other) return *this;
        auto tmp {other};
        *this = std::move(tmp);
        return *this;
      }
      all& operator =(all&&) = default;

      template <class Packet = dart::packet, class =
        std::enable_if_t<
          is_packet_v<Packet>
        >
      >
      dart::packet operator ()(Packet&& pkt = std::decay_t<Packet>::make_null()) {
        dart::packet opt;
        sched::eager sched;
        execute(sched, std::forward<Packet>(pkt), [&] (auto res) { opt = std::move(res); });
        return opt;
      }

      /*----- Public API -----*/

      template <class Scheduler, class Packet = dart::packet, class Then = detail::noop_t>
      void execute(Scheduler& sched,
          Packet&& pkt = std::decay_t<Packet>::make_null(), Then&& next = detail::noop_v) {
        // Get a tuple to hold the calculated results.
        auto state = std::make_shared<execution_storage>(std::forward<Packet>(pkt));

        // Statically iterate over each contained node.
        meta::for_each_t<Nodes...>([&] (auto idx, auto) {
          // Create an intermediate computation to run this node, and schedule it.
          auto copy = nodes;
          auto intermediate = [nodes = std::move(copy), &sched, state, next] {
            auto& curr = std::get<decltype(idx) {}>(*nodes);
            curr.execute(sched, state->in, [state = std::move(state), next] (auto&& out) mutable {
              // Write the value in for this node of the computation.
              state->store.set(decltype(idx) {}, std::move(out));

              // Call into our continuation if we were the final node of the all.
              if (state->remaining.fetch_sub(1) == 1) {
                std::invoke(next, std::move(state->store));
              }
            });
          };

          // If we're working with the final node, yield the current thread and execute
          // the node directly, otherwise pass it to our scheduler.
          if constexpr (idx != sizeof...(Nodes) - 1) sched(std::move(intermediate));
          else intermediate();
        });
      }

      template <class Then>
      auto then(Then&& next) const& {
        return node::seq {*this, std::forward<Then>(next)};
      }

      template <class Then>
      auto then(Then&& next) && {
        return node::seq {std::move(*this), std::forward<Then>(next)};
      }

      template <class... Ns>
      auto join(Ns&&... nodes) const& {
        return node::all {*this, std::forward<Ns>(nodes)...};
      }

      template <class... Ns>
      auto join(Ns&&... nodes) && {
        return node::all {std::move(*this), std::forward<Ns>(nodes)...};
      }

    private:

      /*----- Private Types -----*/

      struct execution_storage {
        template <class P>
        explicit execution_storage(P&& in) :
          in(std::forward<P>(in)),
          remaining(sizeof...(Nodes))
        {
          store.resize(sizeof...(Nodes));
        }
        dart::packet const in;
        dart::packet::array store;
        std::atomic<int64_t> remaining;
      };

      /*----- Private Members -----*/

      std::shared_ptr<std::tuple<Nodes...>> nodes;

  };

  template <class... Ts>
  all(Ts...) -> all<detail::normalize_t<Ts>...>;

}

#endif
