#ifndef DAGGR_ALL_H
#define DAGGR_ALL_H

/*----- System Includes -----*/

#include <type_traits>

/*----- Local Includes -----*/

#include "seq.h"
#include "../meta/function_traits.h"
#include "../meta/lifecycle_traits.h"

/*----- Type Declarations -----*/

namespace daggr {

  namespace node::detail {
    template <class Input>
    struct bound_child_is_applicable {
      template <class Node>
      struct perform : node::child_is_applicable<Node, Input> {};
    };

    template <class Input>
    struct bound_child_apply_result {
      template <class Node>
      struct perform : node::child_apply_result<Node, Input> {};
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
            node::detail::bound_child_is_applicable<Input>::template perform,
            Nodes...
          >
        >
      {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;

      template <class Input>
      struct apply_result {
        using type = meta::filter_tuple_none_t<
          node::detail::intermediate_storage_t<Input, Nodes...>
        >;
      };
      template <class Input>
      using apply_result_t = typename apply_result<Input>::type;

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

      /*----- Public API -----*/

      template <class Scheduler, class Input, class Then, class Terminate, class =
        std::enable_if_t<
          is_applicable_v<Input>
        >
      >
      void execute(Scheduler& sched, Input&& in, Then&& next, Terminate&& term) {
        // Get a tuple to hold the calculated results.
        auto state = std::make_shared<node::detail::intermediate_storage_t<Input, Nodes...>>();
        
        // Statically iterate over each contained node.
        meta::for_each_t<Nodes...>([&] (auto idx, auto type) {
          // Grab the current node.
          auto& curr = std::get<decltype(idx) {}>(nodes);

          // Create an intermediate computation to run this node, and schedule it.
          auto intermediate = [this, &sched, state, in, &curr, next, term] {
            curr.execute(sched, in, [this, state = std::move(state), next] (auto&& out) {
              // Write the value in for this node of the computation.
              std::get<decltype(idx) {}>(*state) = std::forward<decltype(out)>(out);

              // Call into our continuation if we were the final node of the all.
              if (remaining.fetch_sub(1) == 1) next(std::move(*state));
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
