#ifndef DAGGR_WINDOW_H
#define DAGGR_WINDOW_H

/*----- System Includes -----*/

#include <set>
#include <mutex>
#include <typeindex>
#include <unordered_map>

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  template <class Graph>
  class win {

    template <class Input>
    struct intermediate_apply_result {
      using type = typename Graph::template apply_result<Input>::type;
    };
    template <class Input>
    using intermediate_apply_result_t = typename intermediate_apply_result<Input>::type;

    public:

      /*----- Public Types -----*/

      template <class Input>
      struct window_entry {
        template <class... Args>
        explicit window_entry(clock::time_point ts, Args&&... the_args) :
          ts(ts),
          data(std::forward<Args>(the_args)...)
        {}
        window_entry(window_entry const&) = default;
        window_entry(window_entry&&) = default;
        ~window_entry() = default;

        bool operator <(window_entry const& other) const noexcept {
          return ts < other.ts;
        }

        clock::time_point ts;
        intermediate_apply_result_t<Input> data;
      };

      template <class Input>
      struct is_applicable : Graph::template is_applicable<Input> {};
      template <class Input>
      static constexpr auto is_applicable_v = is_applicable<Input>::value;

      template <class Input>
      struct apply_result {
        using type = std::set<window_entry<Input>>;
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

      template <class G = Graph, class =
        std::enable_if_t<
          std::is_default_constructible_v<G>
        >
      >
      explicit win(clock::duration window_size) : window_size(window_size) {}
      template <class G, class =
        std::enable_if_t<
          std::is_same_v<
            detail::normalize_t<G>,
            Graph
          >
        >
      >
      win(clock::duration window_size, G&& graph) :
        window_size(window_size),
        graph(std::forward<G>(graph))
      {}
      win(win const& other) {
        // XXX: If the graph is executing, this won't go well.
        [[maybe_unused]] std::lock_guard guard(other.lock);
        graph = other.graph;
        state = other.state;
      }
      win(win&& other) noexcept {
        // XXX: If the graph is executing, this won't go well.
        [[maybe_unused]] std::lock_guard guard(other.lock);
        graph = std::move(other.graph);
        state = std::move(other.state);
      }
      ~win() = default;

      /*----- Operators -----*/

      win& operator =(win const& other) {
        if (this == &other) return *this;
        auto tmp {other};
        *this = std::move(tmp);
        return *this;
      }
      win& operator =(win&& other) noexcept {
        if (this == &other) return *this;
        this->~win();
        new(this) win(std::move(other));
        return *this;
      }

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
        // Locate/generate the state for this graph specialization.
        auto state = locate_state<std::decay_t<Input>>(typeid(in));

        // Defer to our graph and inject ourselves as a continuation.
        auto winlen = window_size;
        graph.execute(sched, std::forward<Input>(in),
            [state = std::move(state), next = std::forward<Then>(next), ts, winlen] (auto&& out) mutable {
          // FIXME: Slow af, will require more consideration.
          // Insert the value from this invocation into the window and run any necessary expirations.
          auto window = [&] {
            auto now = clock::now();
            [[maybe_unused]] std::lock_guard guard(state->lock);

            // Insert
            auto& window = state->window;
            window.emplace(ts, std::forward<decltype(out)>(out));

            // Expire.
            auto it = window.begin();
            while (it->ts - now > winlen && it != window.end()) it = window.erase(it);

            // Return.
            return window;
          }();

          // Continue.
          meta::apply(next, std::move(window));
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
      struct window_state {
        std::mutex lock;
        apply_result_t<Input> window;
      };

      /*----- Private Helpers -----*/

      template <class Input>
      std::shared_ptr<window_state<Input>> locate_state(std::type_index idx) {
        // Grab our state, construct it if necessary
        [[maybe_unused]] std::lock_guard guard(lock);
        auto& ptr = state[idx];
        if (!ptr) {
          auto del = [] (void* ptr) { delete reinterpret_cast<window_state<Input>*>(ptr); };
          ptr = std::shared_ptr<void>(new window_state<Input>, +del);
        }

        // Alias our void pointer so we don't have to keep putting casts all over the place.
        auto* raw = reinterpret_cast<window_state<Input>*>(ptr.get());
        return std::shared_ptr<window_state<Input>>(ptr, raw);
      }

      /*----- Private Members -----*/

      Graph graph;

      std::mutex mutable lock;
      clock::duration window_size;
      std::unordered_map<std::type_index, std::shared_ptr<void>> state;

  };

  template <class Graph>
  win(clock::duration, Graph) -> win<detail::normalize_t<Graph>>;

}

#endif
