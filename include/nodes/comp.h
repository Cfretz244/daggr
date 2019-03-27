#ifndef DAGGR_COMPUTATION_H
#define DAGGR_COMPUTATION_H

/*----- System Includes -----*/

#include <functional>
#include <type_traits>

/*----- Local Includes -----*/

#include "seq.h"
#include "../meta/function_traits.h"
#include "../meta/lifecycle_traits.h"

/*----- Type Declarations -----*/

namespace daggr {

  template <class Producer, class Consumer>
  class seq;

  template <class Func>
  class comp {

    public:

      /*----- Public Types -----*/

      template <class Input>
      struct is_applicable : meta::is_applicable<Func, Input> {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;
      template <class Input>
      struct apply_result : meta::apply_result<Func, Input> {};
      template <class Input>
      using apply_result_t = typename apply_result<Input>::type;

      /*----- Lifecycle Functions -----*/

      template <class U = Func, class =
        std::enable_if_t<
          std::is_default_constructible_v<U>
        >
      >
      comp() noexcept(std::is_nothrow_default_constructible_v<U>) {}
      template <class U, class =
        std::enable_if_t<
          std::is_same_v<
            std::decay_t<U>,
            Func
          >
        >
      >
      explicit comp(U&& func) noexcept(meta::is_nothrow_forward_constructible_v<U>) : func(func) {}
      comp(comp const&) = default;
      comp(comp&&) = default;
      ~comp() = default;

      /*----- Operators -----*/

      comp& operator =(comp const&) = default;
      comp& operator =(comp&&) = default;

      /*----- Public API -----*/

      template <class Scheduler, class Input, class Then, class Terminate, class =
        std::enable_if_t<
          is_applicable_v<Input>
        >
      >
      void execute(Scheduler&, Input&& in, Then&& next, Terminate&&) {
        meta::apply(std::forward<Then>(next), meta::apply(func, std::forward<Input>(in)));
      }

      template <class Then>
      seq<comp, comp<Then>> then(Then&& next) const&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return seq {*this, comp<Then> {std::forward<Then>(next)}};
      }

      template <class Then>
      seq<comp, comp<Then>> then(Then&& next) &&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return seq {std::move(*this), comp<Then> {std::forward<Then>(next)}};
      }

      static size_t async_count() noexcept {
        return 0;
      }

    private:

      /*----- Private Members -----*/

      Func func;

  };

  template <>
  class comp<meta::none> {

    public:

      /*----- Public Types -----*/

      template <class Input>
      struct is_applicable : std::is_same<std::decay_t<Input>, meta::none> {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;

      template <class>
      struct apply_result {
        using type = meta::none;
      };
      template <class T>
      using apply_result_t = typename apply_result<T>::type;

      /*----- Lifecycle Functions -----*/

      comp() = default;
      explicit comp(meta::none) noexcept {}
      comp(comp const&) = default;
      comp(comp&&) = default;
      ~comp() = default;

      /*----- Operators -----*/

      comp& operator =(comp const&) = default;
      comp& operator =(comp&&) = default;

      /*----- Public API -----*/

      template <class Scheduler, class Then, class Terminate>
      void execute(Scheduler&, meta::none, Then&& next, Terminate&&) {
        std::invoke(std::forward<Then>(next), meta::none_v);
      }

      template <class Then>
      seq<comp, comp<Then>> then(Then&& next)
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return seq {*this, comp<Then> {std::forward<Then>(next)}};
      }

      static size_t async_count() noexcept {
        return 0;
      }

  };

  template <class F>
  comp(F) -> comp<F>;

  template <class F>
  using computation = comp<F>;

}

#endif
