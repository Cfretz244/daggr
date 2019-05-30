#ifndef DAGGR_ERR_H
#define DAGGR_ERR_H

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  namespace detail {
    template <class Comp, class Catch, class Input>
    struct err_is_symmetric :
      std::is_same<
        child_apply_result_t<Comp, Input>,
        child_apply_result_t<Catch, Input>
      >
    {};
    template <class Comp, class Catch, class Input>
    static constexpr auto err_is_symmetric_v = err_is_symmetric<Comp, Catch, Input>::value;

    template <class Comp, class Catch, class Input>
    constexpr auto err_combined_return() {
      if constexpr (err_is_symmetric_v<Comp, Catch, Input>) {
        return meta::identity<child_apply_result_t<Comp, Input>> {};
      } else if constexpr (!child_has_result_v<Comp, Input>) {
        return meta::identity<std::optional<child_apply_result_t<Catch, Input>>> {};
      } else if constexpr (!child_has_result_v<Catch, Input>) {
        return meta::identity<std::optional<child_apply_result_t<Comp, Input>>> {};
      } else {
        using comp_type = child_apply_result_t<Comp, Input>;
        using catch_type = child_apply_result_t<Catch, Input>;
        return meta::identity<std::variant<comp_type, catch_type>> {};
      }
    }

    template <class Ex, class... Exs, class Try, class Catch>
    void try_stack(Try&& test, Catch&& handler) {
      try {
        if constexpr (sizeof...(Exs)) {
          try_stack<Exs...>(std::forward<Try>(test), std::forward<Catch>(handler));
        } else {
          std::forward<Try>(test)();
        }
      } catch (Ex) {
        std::forward<Catch>(handler)();
      }
    }
  }

  template <class Comp, class Catch, class... Exs>
  class err {

    public:

      /*----- Public Types -----*/

      template <class Input>
      struct is_applicable :
        std::conjunction<
          detail::child_is_applicable<Comp, Input>,
          detail::child_is_applicable<Catch, Input>
        >
      {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;

      template <class Input>
      struct apply_result {
        using type = typename decltype(detail::err_combined_return<Comp, Catch, Input>())::type;
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

      template <class Co = Comp, class Ca = Catch, class =
        std::enable_if_t<
          meta::are_default_constructible_v<Co, Ca>
        >
      >
      err() : impl(std::make_shared<storage>()) {}
      template <class Co, class =
        std::enable_if_t<
          std::is_same_v<
            detail::normalize_t<Co>,
            Comp
          >
          &&
          std::is_default_constructible_v<Catch>
        >
      >
      explicit err(Co&& comp) :
        impl(std::make_shared<Comp>(detail::normalize(std::forward<Co>(comp))))
      {}
      template <class Co, class Ca, class =
        std::enable_if_t<
          std::is_same_v<
            detail::normalize_t<Co>,
            Comp
          >
          &&
          std::is_same_v<
            detail::normalize_t<Ca>,
            Catch
          >
        >
      >
      err(Co&& comp, Ca&& handler) :
        impl(
          std::make_shared<storage>(
            detail::normalize(std::forward<Co>(comp)),
            detail::normalize(std::forward<Ca>(handler))
          )
        )
      {}

      err(err const& other) : impl(std::make_shared<storage>(*other.impl)) {}
      err(err&&) = default;
      ~err() = default;

      /*----- Operators -----*/

      err& operator =(err const& other) {
        if (this == &other) return *this;
        auto tmp {other};
        *this = std::move(tmp);
        return *this;
      }
      err& operator =(err&&) = default;

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
        execute(sched, std::forward<Arg>(arg), detail::noop {});
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
        execute(sched, meta::none_v, detail::noop {});
      }

      template <class Scheduler, class Input,
               class Then = detail::noop, class Handler = detail::indirect, class =
        std::enable_if_t<
          is_applicable_v<Input>
        >
      >
      void execute(Scheduler& sched, Input&& in,
          Then&& next = Then {}, Handler&& handler = Handler {}) {
        using result_type = apply_result_t<Input>;

        // Allocate some execution state to keep track of whether an exception
        // has already been thrown.
        auto state = std::make_shared<err_state<std::decay_t<Input>>>(std::forward<Input>(in));

        auto adjust = [next = std::forward<Then>(next)] (result_type val = result_type {}) mutable {
          std::move(next)(std::move(val));
        };

        // Setup a handler for the requested exception types.
        auto copy = impl;
        impl->computation.execute(sched, state->in, adjust,
            [&sched, adjust, state, impl = std::move(copy),
                handler = std::forward<Handler>(handler)] (auto&& cb, auto&& succ) mutable {
          detail::try_stack<Exs...>(
            [&] {
              std::forward<decltype(succ)>(succ)(std::forward<decltype(cb)>(cb)());
            },
            [&] {
              if (!state->tripped.test_and_set()) {
                impl->handler.execute(sched, state->in, std::move(adjust), std::move(handler));
              }
            }
          );
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

      template <class... Errs, class Then>
      auto error(Then&& next) const& {
        return make_err<Errs...>(*this, std::forward<Then>(next));
      }

      template <class... Errs, class Then>
      auto error(Then&& next) && {
        return make_err<Errs...>(std::move(*this), std::forward<Then>(next));
      }

    private:

      /*----- Private Types -----*/

      template <class Input>
      struct err_state {
        template <class In>
        explicit err_state(In&& in) : in(std::forward<In>(in)) {
          tripped.clear();
        }
        err_state(err_state const&) = delete;
        err_state(err_state&&) = delete;
        ~err_state() = default;

        Input const in;
        std::atomic_flag tripped;
      };

      // FIXME: Make alignment smarter than this.
      struct storage {
        storage() = default;
        template <class Co, class =
          std::enable_if_t<
            !std::is_same_v<
              std::decay_t<Co>,
              storage
            >
          >
        >
        storage(Co&& comp) :
          computation(std::forward<Co>(comp))
        {}
        template <class Co, class Ca>
        storage(Co&& comp, Ca&& hand) :
          computation(std::forward<Co>(comp)),
          handler(std::forward<Ca>(hand))
        {}
        storage(storage const&) = default;
        storage(storage&&) = default;
        ~storage() = default;

        Comp computation;
        Catch handler;
      };

      /*----- Private Members -----*/

      std::shared_ptr<storage> impl;

  };

  template <class... Exs, class Comp, class Catch>
  auto make_err(Comp&& comp, Catch&& handler) {
    using err_type = err<detail::normalize_t<Comp>, detail::normalize_t<Catch>, Exs...>;
    return err_type {std::forward<Comp>(comp), std::forward<Catch>(handler)};
  }

}

#endif
