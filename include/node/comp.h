#ifndef DAGGR_COMPUTATION_H
#define DAGGR_COMPUTATION_H

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  template <class Producer, class Consumer>
  class seq;

  // FIXME: Make alignment smarter than this.
  template <class Func>
  class alignas(64) comp {

    static_assert(!is_node_v<std::decay_t<Func>>);

    public:

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

      dart::packet operator ()(dart::packet const& pkt) {
        dart::packet opt;
        sched::eager sched;
        execute(sched, pkt, [&] (auto res) { opt = std::move(res); });
        return opt;
      }

      /*----- Public API -----*/

      template <class Scheduler, class Then = detail::noop_t>
      void execute(Scheduler&, dart::packet const& pkt, Then&& next = detail::noop_v) {
        std::invoke(std::forward<Then>(next), std::invoke(func, pkt));
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

    private:

      /*----- Private Members -----*/

      Func func;

  };

  template <class F>
  comp(F) -> comp<F>;

  template <class F>
  using computation = comp<F>;

}

#endif
