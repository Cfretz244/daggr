#ifndef DAGGR_SEQUENCE_H
#define DAGGR_SEQUENCE_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Local Includes -----*/

#include "comp.h"
#include "../meta/function_traits.h"
#include "../meta/lifecycle_traits.h"

/*----- Type Declarations -----*/

namespace daggr {

  template <class F>
  class comp;

  template <class Producer, class Consumer>
  class seq {

    template <class P, class C>
    static constexpr auto nothrow_partially_constructible_v =
        meta::is_nothrow_forward_constructible_v<P> && std::is_nothrow_default_constructible_v<C>;

    // Helper meta-functions to write our invocability in terms of our children.
    template <class F, class Input>
    struct is_invocable_impl : F::template is_invocable<Input> {
      using canonical_type = std::bool_constant<F::template is_invocable<Input>::value>;
    };
    template <class F, class Input>
    struct invoke_result_impl : F::template invoke_result<Input> {};

    // Helper meta-functions to write our applicability in terms of our children.
    template <class F, class Input>
    struct is_applicable_impl : F::template is_applicable<Input> {
      using canonical_type = std::bool_constant<F::template is_applicable<Input>::value>;
    };
    template <class F, class Input>
    struct apply_result_impl : F::template apply_result<Input> {};

    public:

      /*----- Public Types -----*/

      using producer_type = Producer;
      using consumer_type = Consumer;

      template <class Input>
      struct is_invocable :
        meta::sequence_is_walkable<
          is_invocable_impl,
          invoke_result_impl,
          Input,
          Producer,
          Consumer
        >
      {};
      template <class Input>
      static constexpr auto is_invocable_v = is_invocable<Input>::value;

      template <class Input>
      struct invoke_result {
        using type = typename is_invocable<Input>::result_type;
      };
      template <class Input>
      using invoke_result_t = typename invoke_result<Input>::type;

      template <class Input>
      struct is_applicable :
        meta::sequence_is_walkable<
          is_applicable_impl,
          apply_result_impl,
          Input,
          Producer,
          Consumer
        >
      {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;

      template <class Input>
      struct apply_result {
        using type = typename is_applicable<Input>::result_type;
      };
      template <class Input>
      using apply_result_t = typename apply_result<Input>::type;

      /*----- Lifecycle Functions -----*/

      template <class P = Producer, class C = Consumer, class =
        std::enable_if_t<
          meta::are_default_constructible_v<P, C>
        >
      >
      seq() noexcept(meta::are_nothrow_default_constructible_v<P, C>) {}
      template <class P, class =
        std::enable_if_t<
          std::is_same_v<
            std::decay_t<P>,
            Producer
          >
        >
      >
      explicit seq(P&& prod) noexcept(nothrow_partially_constructible_v<P, Consumer>) :
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
      explicit seq(P&& prod, C&& cons) noexcept(meta::are_nothrow_forward_constructible_v<P, C>) :
        prod(std::forward<P>(prod)),
        cons(std::forward<C>(cons))
      {}

      seq(seq const&) = default;
      seq(seq&&) = default;
      ~seq() = default;

      /*----- Operators -----*/

      seq& operator =(seq const&) = default;
      seq& operator =(seq&&) = default;

      /*----- Public API -----*/

      template <class Scheduler, class Input, class Then, class Terminate>
      void execute(Scheduler& sched, Input&& in, Then&& next, Terminate&& term) {
        prod.execute(sched, std::forward<Input>(in),
            [this, &sched, next = std::forward<Then>(next), term] (auto&& tmp) {
          cons.execute(sched,
              std::forward<decltype(tmp)>(tmp), std::move(next), std::move(term));
        }, std::forward<Terminate>(term));
      }

      template <class Then>
      seq<seq, comp<Then>> then(Then&& next) const&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return seq<seq, comp<Then>> {*this, comp {std::forward<Then>(next)}};
      }

      template <class Then>
      seq<seq, comp<Then>> then(Then&& next) &&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return seq<seq, comp<Then>> {std::move(*this), comp {std::forward<Then>(next)}};
      }

      static size_t async_count() noexcept {
        return producer_type::async_count() + consumer_type::async_count();
      }

    private:

      /*----- Private Members -----*/

      producer_type prod;
      consumer_type cons;

  };

  template <class P, class C>
  seq(P, C) -> seq<P, C>;

  template <class P, class C>
  using sequence = seq<P, C>;

}

#endif
