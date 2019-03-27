#ifndef DAGGR_ALL_H
#define DAGGR_ALL_H

/*----- System Includes -----*/

#include <type_traits>

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
          std::is_copy_constructible<std::decay_t<Input>>,
          meta::all_of<
            detail::bound_child_is_applicable<Input>::template perform,
            Nodes...
          >
        >
      {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;

      template <class Input>
      struct apply_result {
        using type = meta::filter_tuple_none_t<
          detail::intermediate_storage_t<Input, Nodes...>
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
      all() noexcept(std::is_nothrow_default_constructible_v<Storage>) :
        remaining(sizeof...(Nodes))
      {}
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
        nodes(std::forward<Ns>(nodes)...),
        remaining(sizeof...(Nodes))
      {}

      all(all const&) = default;
      all(all&&) = default;
      ~all() = default;

      /*----- Operators -----*/

      all& operator =(all const&) = default;
      all& operator =(all&&) = default;

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
        // Get a tuple to hold the calculated results.
        auto state = std::make_shared<detail::intermediate_storage_t<Input, Nodes...>>();
        
        // Statically iterate over each contained node.
        meta::for_each_t<Nodes...>([&] (auto idx, auto) {
          // Grab the current node.
          auto& curr = std::get<decltype(idx) {}>(nodes);

          // Create an intermediate computation to run this node, and schedule it.
          auto intermediate = [this, &sched, state, in, &curr, next, term] {
            curr.execute(sched, in, [this, state = std::move(state), next] (auto&& out) {
              // Write the value in for this node of the computation.
              std::get<decltype(idx) {}>(*state) = std::forward<decltype(out)>(out);

              // Call into our continuation if we were the final node of the all.
              if (remaining.fetch_sub(1) == 1) meta::apply(next, std::move(*state));
            }, term);
          };

          // If we're working with the final node, yield the current thread and execute
          // the node directly, otherwise pass it to our scheduler.
          if constexpr (idx != sizeof...(Nodes) - 1) sched(std::move(intermediate));
          else intermediate();
        });
      }

    private:

      std::tuple<Nodes...> nodes;
      std::atomic<int64_t> remaining;

  };

  template <class... Ts>
  all(Ts...) -> all<Ts...>;

}

#endif
