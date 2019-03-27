#ifndef DAGGR_SEQUENCE_H
#define DAGGR_SEQUENCE_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  template <class F>
  class comp;

  template <class Producer, class Consumer>
  class seq {

    template <class P, class C>
    static constexpr auto nothrow_partially_constructible_v =
        meta::is_nothrow_forward_constructible_v<P> && std::is_nothrow_default_constructible_v<C>;

    public:

      /*----- Public Types -----*/

      template <class Input>
      struct is_applicable :
        meta::sequence_is_walkable<
          detail::child_is_applicable,
          detail::child_apply_result,
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

      template <class Input>
      struct has_result :
        std::conjunction<
          is_applicable<Input>,
          std::negation<
            std::is_same<apply_result_t<Input>, meta::none>
          >
        >
      {};
      template <class Input>
      static constexpr auto has_result_v = has_result<Input>::value;

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
            std::decay_t<P>,
            Producer
          >
        >
      >
      explicit seq(P&& prod) noexcept(nothrow_partially_constructible_v<P, Consumer>) :
        state(std::make_shared<storage>(std::forward<P>(prod)))
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
        state(std::make_shared<storage>(std::forward<P>(prod), std::forward<C>(cons)))
      {}

      seq(seq const&) = default;
      seq(seq&&) = default;
      ~seq() = default;

      /*----- Operators -----*/

      seq& operator =(seq const&) = default;
      seq& operator =(seq&&) = default;

      template <class Arg,
        std::enable_if_t<
          has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      auto operator ()(Arg&& arg) {
        sched::eager sched;
        std::optional<apply_result_t<Arg>> opt;
        execute(sched, std::forward<Arg>(arg), [&] (auto&& res) {
          opt.emplace(std::forward<decltype(res)>(res));
        });
        return *std::move(opt);
      }

      template <class Arg,
        std::enable_if_t<
          !has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      void operator ()(Arg&& arg) {
        sched::eager sched;
        execute(sched, std::forward<Arg>(arg));
      }

      template <class Arg = meta::none,
        std::enable_if_t<
          has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      auto operator ()() {
        sched::eager sched;
        std::optional<apply_result_t<Arg>> opt;
        execute(sched, meta::none_v, [&] (auto&& res) {
          opt.emplace(std::forward<decltype(res)>(res));
        });
        return *std::move(opt);
      }

      template <class Arg = meta::none,
        std::enable_if_t<
          !has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      void operator ()() {
        sched::eager sched;
        execute(sched, meta::none_v);
      }

      /*----- Public API -----*/

      template <class Scheduler, class Input,
               class Then = detail::noop_t, class Terminate = detail::noop_t, class =
        std::enable_if_t<
          is_applicable_v<Input>
        >
      >
      void execute(Scheduler& sched, Input&& in,
          Then&& next = detail::noop_v, Terminate&& term = detail::noop_v) {
        auto copy = state;
        state->prod.execute(sched, std::forward<Input>(in),
            [&sched, state = std::move(copy), next = std::forward<Then>(next), term] (auto&& tmp) {
          state->cons.execute(sched,
              std::forward<decltype(tmp)>(tmp), std::move(next), std::move(term));
        }, std::forward<Terminate>(term));
      }

      template <class Then>
      seq<seq, comp<Then>> then(Then&& next) const& {
        return seq<seq, comp<Then>> {*this, comp {std::forward<Then>(next)}};
      }

      template <class Then>
      seq<seq, comp<Then>> then(Then&& next) &&
        noexcept(meta::is_nothrow_forward_constructible_v<Then>)
      {
        return seq<seq, comp<Then>> {std::move(*this), comp {std::forward<Then>(next)}};
      }

      static size_t async_count() noexcept {
        return Producer::async_count() + Consumer::async_count();
      }

    private:

      /*----- Private Types -----*/

      // Make alignment smarter than this.
      struct storage {
        storage() = default;
        template <class P>
        storage(P&& prod) :
          prod(std::forward<P>(prod))
        {}
        template <class P, class C>
        storage(P&& prod, C&& cons) :
          prod(std::forward<P>(prod)),
          cons(std::forward<C>(cons))
        {}
        storage(storage const&) = delete;
        storage(storage&&) = delete;

        alignas(64) Producer prod;
        alignas(64) Consumer cons;
      };

      /*----- Private Members -----*/

      std::shared_ptr<storage> state;

  };

  template <class P, class C>
  seq(P, C) -> seq<P, C>;

  template <class P, class C>
  using sequence = seq<P, C>;

}

#endif
