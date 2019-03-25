#ifndef DAGGER_SEQUENCE_H
#define DAGGER_SEQUENCE_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Local Includes -----*/

#include "node.h"
#include "../meta/lifecycle_traits.h"

/*----- Type Declarations -----*/

namespace dagger {

  template <class F>
  class node;

  template <class Producer, class Consumer>
  class sequence : meta::copy_guard<Producer, Consumer>, meta::move_guard<Producer, Consumer> {

    template <class P, class C>
    static constexpr auto nothrow_partially_constructible_v =
        meta::is_nothrow_forward_constructible_v<P> && std::is_nothrow_default_constructible_v<C>;

    public:

      /*----- Public Types -----*/

      using producer_type = Producer;
      using consumer_type = Consumer;

      /*----- Lifecycle Functions -----*/

      template <class P = Producer, class C = Consumer, class =
        std::enable_if_t<
          meta::are_default_constructible_v<P, C>
        >
      >
      sequence() noexcept(meta::are_nothrow_default_constructible_v<P, C>) {}
      template <class P, class =
        std::enable_if_t<
          std::is_same_v<
            std::decay_t<P>,
            Producer
          >
        >
      >
      explicit sequence(P&& prod) noexcept(nothrow_partially_constructible_v<P, Consumer>) :
        prod(std::forward<P>(prod))
      {}
      template <class P, class C, class =
        std::enable_if_t<
          std::is_same_v<
            std::decay_t<P>,
            Producer
          >
          &&
          std::is_same_v<
            std::decay_t<C>,
            Consumer
          >
        >
      >
      explicit sequence(P&& prod, C&& cons) noexcept(meta::are_nothrow_forward_constructible_v<P, C>) :
        prod(std::forward<P>(prod)),
        cons(std::forward<C>(cons))
      {}

      sequence(sequence const&) = default;
      sequence(sequence&&) = default;
      ~sequence() = default;

      /*----- Operators -----*/

      sequence& operator =(sequence const&) = default;
      sequence& operator =(sequence&&) = default;

      /*----- Public API -----*/

      template <class Scheduler, class Input, class Then, class Cleanup>
      void execute(Scheduler& sched, Input&& in, Then&& next, Cleanup&& clean) {

      }

      template <class Then>
      sequence<sequence, node<Then>> then(Then&& next) const&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return sequence {*this, node {std::forward<Then>(next)}};
      }

      template <class Then>
      sequence<sequence, node<Then>> then(Then&& next) &&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return sequence {std::move(*this), node {std::forward<Then>(next)}};
      }

    private:

      /*----- Private Members -----*/

      producer_type prod;
      consumer_type cons;

  };

  template <class P, class C>
  sequence(P, C) -> sequence<P, C>;

}

#endif
