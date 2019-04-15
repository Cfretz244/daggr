#ifndef DAGGR_ALL_H
#define DAGGR_ALL_H

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  namespace detail {
    template <class Input>
    struct bound_child_is_applicable {
      template <class Node>
      struct perform : child_is_applicable<Node, Input> {};
    };

    template <class Input>
    struct bound_child_apply_result {
      template <class Node>
      struct perform : child_apply_result<Node, Input> {};
    };

    template <class Input, class... Nodes>
    struct intermediate_storage :
      meta::transform_type_sequence<
        bound_child_apply_result<Input>::template perform,
        Nodes...
      >
    {};
    template <class Input, class... Nodes>
    using intermediate_storage_t = typename intermediate_storage<Input, Nodes...>::type;
  }

  template <class... Nodes>
  class all {

    public:

      /*----- Public Types -----*/

      template <class Input>
      struct is_applicable :
        std::conjunction<
          meta::is_forward_constructible<Input>,
          meta::all_of<
            detail::bound_child_is_applicable<std::decay_t<Input> const&>::template perform,
            Nodes...
          >
        >
      {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;

      template <class Input>
      struct apply_result {
        using type = meta::filter_tuple_none_t<
          detail::intermediate_storage_t<std::decay_t<Input> const&, Nodes...>
        >;
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
        // Get a tuple to hold the calculated results.
        auto state = std::make_shared<execution_storage<Input>>(std::forward<Input>(in));

        // Statically iterate over each contained node.
        meta::for_each_t<Nodes...>([&] (auto idx, auto) {
          // Create an intermediate computation to run this node, and schedule it.
          auto copy = nodes;
          auto intermediate = [nodes = std::move(copy), &sched, state, next, ts] {
            auto& curr = std::get<decltype(idx) {}>(*nodes);
            curr.execute(sched, state->in, [state = std::move(state), next] (auto&& out) mutable {
              // Write the value in for this node of the computation.
              std::get<decltype(idx) {}>(state->store) = std::forward<decltype(out)>(out);

              // Call into our continuation if we were the final node of the all.
              if (state->remaining.fetch_sub(1) == 1) {
                meta::apply(next, std::move(state->store));
              }
            }, ts);
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

      auto window(clock::duration window_size) const& {
        return node::win {window_size, *this};
      }

      auto window(clock::duration window_size) && {
        return node::win {window_size, std::move(*this)};
      }

    private:

      /*----- Private Types -----*/

      template <class Input>
      struct execution_storage {
        template <class In>
        execution_storage(In&& in) :
          in(std::forward<In>(in)),
          remaining(sizeof...(Nodes))
        {}
        std::decay_t<Input> const in;
        std::atomic<int64_t> remaining;
        detail::intermediate_storage_t<Input, Nodes...> store;
      };

      /*----- Private Members -----*/

      std::shared_ptr<std::tuple<Nodes...>> nodes;

  };

  template <class... Ts>
  all(Ts...) -> all<detail::normalize_t<Ts>...>;

}

#endif
