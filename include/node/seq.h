#ifndef DAGGR_SEQUENCE_H
#define DAGGR_SEQUENCE_H

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

      template <class Arg,
        std::enable_if_t<
          has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      auto operator ()(Arg&& arg, clock::time_point ts = clock::now()) {
        sched::eager sched;
        std::optional<apply_result_t<Arg>> opt;
        execute(sched, std::forward<Arg>(arg), [&] (auto&& res) {
          opt.emplace(std::forward<decltype(res)>(res));
        }, ts);
        return *std::move(opt);
      }

      template <class Arg,
        std::enable_if_t<
          !has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      void operator ()(Arg&& arg, clock::time_point ts = clock::now()) {
        sched::eager sched;
        execute(sched, std::forward<Arg>(arg), detail::noop_v, ts);
      }

      template <class Arg = meta::none,
        std::enable_if_t<
          has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      auto operator ()(clock::time_point ts = clock::now()) {
        sched::eager sched;
        std::optional<apply_result_t<Arg>> opt;
        execute(sched, meta::none_v, [&] (auto&& res) {
          opt.emplace(std::forward<decltype(res)>(res));
        }, ts);
        return *std::move(opt);
      }

      template <class Arg = meta::none,
        std::enable_if_t<
          !has_result_v<Arg>
          &&
          is_applicable_v<Arg>
        >* = nullptr
      >
      void operator ()(clock::time_point ts = clock::now()) {
        sched::eager sched;
        execute(sched, meta::none_v, detail::noop_v, ts);
      }

      /*----- Public API -----*/

      template <class Scheduler, class Input, class Then = detail::noop_t, class =
        std::enable_if_t<
          is_applicable_v<Input>
        >
      >
      void execute(Scheduler& sched, Input&& in, Then&& next = detail::noop_v, clock::time_point ts = clock::now()) {
        auto copy = state;
        state->prod.execute(sched, std::forward<Input>(in),
            [&sched, state = std::move(copy), next = std::forward<Then>(next), ts] (auto&& tmp) mutable {
          state->cons.execute(sched,
              std::forward<decltype(tmp)>(tmp), std::move(next), ts);
        }, ts);
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

      auto window(clock::duration window_size) const& {
        return node::win {window_size, *this};
      }

      auto window(clock::duration window_size) && {
        return node::win {window_size, std::move(*this)};
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
