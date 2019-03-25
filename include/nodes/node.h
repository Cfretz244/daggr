#ifndef DAGGER_NODE_H
#define DAGGER_NODE_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Local Includes -----*/

#include "sequence.h"
#include "../meta/lifecycle_traits.h"

/*----- Type Declarations -----*/

namespace dagger {

  template <class Producer, class Consumer>
  class sequence;

  template <class F>
  class node : meta::copy_guard<F>, meta::move_guard<F> {

    public:

      /*----- Public Types -----*/

      using functor_type = F;

      /*----- Lifecycle Functions -----*/

      template <class U = F, class =
        std::enable_if_t<
          std::is_default_constructible_v<U>
        >
      >
      node() noexcept(std::is_nothrow_default_constructible_v<U>) {}
      template <class U, class =
        std::enable_if_t<
          std::is_same_v<
            std::decay_t<U>,
            F
          >
        >
      >
      explicit node(U&& func) noexcept(meta::is_nothrow_forward_constructible_v<U>) : func(func) {}
      node(node const&) = default;
      node(node&&) = default;
      ~node() = default;

      /*----- Operators -----*/

      node& operator =(node const&) = default;
      node& operator =(node&&) = default;

      /*----- Public API -----*/

      template <class Scheduler, class Input, class Then, class Cleanup>
      void execute(Scheduler&, Input&& in, Then&& next, Cleanup&& clean) {
        std::forward<Then>(next)(func(std::forward<Input>(in)));
      }

      template <class Then>
      sequence<node, node<Then>> then(Then&& next) const&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return sequence {*this, node {std::forward<Then>(next)}};
      }

      template <class Then>
      sequence<node, node<Then>> then(Then&& next) &&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return sequence {std::move(*this), node {std::forward<Then>(next)}};
      }

    private:

      /*----- Private Members -----*/

      functor_type func;

  };

}

#endif
