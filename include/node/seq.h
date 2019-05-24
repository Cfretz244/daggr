#ifndef DAGGR_SEQUENCE_H
#define DAGGR_SEQUENCE_H

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  template <class Producer, class Consumer>
  class seq {

    template <class P, class C>
    static constexpr auto nothrow_partially_constructible_v =
        meta::is_nothrow_forward_constructible_v<P> && std::is_nothrow_default_constructible_v<C>;

    public:

      /*----- Lifecycle Functions -----*/

      template <class P = Producer, class C = Consumer, class =
        std::enable_if_t<
          meta::are_default_constructible_v<P, C>
        >
      >
      seq() : state(std::make_shared<storage>()) {}
      template <class P, class =
        std::enable_if_t<
          std::is_same_v<
            detail::normalize_t<P>,
            Producer
          >
          &&
          std::is_default_constructible_v<Consumer>
        >
      >
      explicit seq(P&& prod) :
        state(std::make_shared<storage>(detail::normalize(std::forward<P>(prod))))
      {}
      template <class P, class C, class =
        std::enable_if_t<
          std::is_same_v<
            detail::normalize_t<P>,
            Producer
          >
          &&
          std::is_same_v<
            detail::normalize_t<C>,
            Consumer
          >
        >
      >
      explicit seq(P&& prod, C&& cons) :
        state(
          std::make_shared<storage>(
            detail::normalize(std::forward<P>(prod)),
            detail::normalize(std::forward<C>(cons))
          )
        )
      {}

      seq(seq const& other) : state(std::make_shared<storage>(*other.state)) {}
      seq(seq&&) = default;
      ~seq() = default;

      /*----- Operators -----*/

      seq& operator =(seq const& other) {
        if (this == &other) return *this;
        auto tmp {other};
        *this = std::move(tmp);
        return *this;
      }
      seq& operator =(seq&&) = default;

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
        auto copy = state;
        state->prod.execute(sched, std::forward<Packet>(pkt),
            [&sched, state = std::move(copy), next = std::forward<Then>(next)] (auto tmp) mutable {
          state->cons.execute(sched, std::move(tmp), std::move(next));
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

      template <class... Nodes>
      auto join(Nodes&&... nodes) const& {
        return node::all {*this, std::forward<Nodes>(nodes)...};
      }

      template <class... Nodes>
      auto join(Nodes&&... nodes) && {
        return node::all {std::move(*this), std::forward<Nodes>(nodes)...};
      }

    private:

      /*----- Private Types -----*/

      // FIXME: Make alignment smarter than this.
      struct storage {
        storage() = default;
        template <class P, class =
          std::enable_if_t<
            !std::is_same_v<
              std::decay_t<P>,
              storage
            >
          >
        >
        storage(P&& prod) :
          prod(std::forward<P>(prod))
        {}
        template <class P, class C>
        storage(P&& prod, C&& cons) :
          prod(std::forward<P>(prod)),
          cons(std::forward<C>(cons))
        {}
        storage(storage const&) = default;
        storage(storage&&) = default;
        ~storage() = default;

        Producer prod;
        Consumer cons;
      };

      /*----- Private Members -----*/

      std::shared_ptr<storage> state;

  };

  template <class P, class C>
  seq(P, C) -> seq<detail::normalize_t<P>, detail::normalize_t<C>>;

  template <class P, class C>
  using sequence = seq<P, C>;

}

#endif
