#ifndef DAGGR_ERASED_H
#define DAGGR_ERASED_H

/*----- Local Includes -----*/

#include "../node.h"
#include "../sched/eager.h"

/*----- Type Declarations -----*/

namespace daggr::node {

  class erased {

    class eraser;

    template <class E, class S, class P, class N>
    using execute_t = decltype(std::declval<E>().execute(std::declval<S>(), std::declval<P>(), std::declval<N>()));

    public:

      /*----- Lifecycle Functions -----*/

      template <class Node, class =
        std::enable_if_t<
          is_node_v<std::decay_t<Node>>
        >
      >
      erased(Node&& node) :
        impl(std::make_unique<erased_node<std::decay_t<Node>>>(std::forward<Node>(node)))
      {}
      erased(erased const& other) : impl(other.impl->clone()) {}
      erased(erased&&) = default;
      ~erased() = default;

      /*----- Operators -----*/

      template <class Node, class =
        std::enable_if_t<
          is_node_v<std::decay_t<Node>>
          &&
          !std::is_same_v<
            std::decay_t<Node>,
            erased
          >
        >
      >
      erased& operator =(Node&& node) {
        impl = std::make_unique<erased_node<std::decay_t<Node>>>(std::forward<Node>(node));
        return *this;
      };

      erased& operator =(erased const& other) {
        if (this == &other) return *this;
        auto tmp {other};
        *this = std::move(tmp);
        return *this;
      }

      erased& operator =(erased&&) = default;

      template <class Packet = dart::packet, class =
        std::enable_if_t<
          is_packet_v<Packet>
        >
      >
      dart::packet operator ()(Packet&& pkt = std::decay_t<Packet>::make_null()) {
        dart::packet opt;
        sched::eager sched;
        execute(sched, std::forward<Packet>(pkt), [&] (auto res) { opt = std::move(res); });
        return opt;
      }

      /*----- Public API -----*/

      template <class Scheduler, class Packet = dart::packet, class Then = detail::noop_t, class =
        std::enable_if_t<
          meta::is_detected_v<execute_t, eraser&, Scheduler, Packet, Then>
        >
      >
      void execute(Scheduler& sched,
          Packet&& pkt = std::decay_t<Packet>::make_null(), Then&& next = detail::noop_v) {
        impl->execute(sched, std::forward<Packet>(pkt), std::forward<Then>(next));
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

    private:

      /*----- Private Types -----*/

      // Yikes.
      struct eraser {
        virtual ~eraser() = default;
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)> const& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)> const& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)> const& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)> const& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>&& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>&& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>&& next) = 0;
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>&& next) = 0;
        virtual std::unique_ptr<eraser> clone() const = 0;
      };

      template <class Node>
      struct erased_node : eraser {

        /*----- Lifecycle Functions -----*/

        template <class N>
        erased_node(N&& n) : impl(std::forward<N>(n)) {}
        erased_node(erased_node const&) = delete;
        erased_node(erased_node&&) = delete;
        virtual ~erased_node() = default;

        /*----- API -----*/

        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>& next) override {
          impl.execute(sched, pkt, next);
        }
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>& next) override {
          impl.execute(sched, std::move(pkt), next);
        }
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>& next) override {
          impl.execute(sched, pkt, next);
        }
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>& next) override {
          impl.execute(sched, std::move(pkt), next);
        }
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)> const& next) override {
          impl.execute(sched, pkt, next);
        }
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)> const& next) override {
          impl.execute(sched, std::move(pkt), next);
        }
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)> const& next) override {
          impl.execute(sched, pkt, next);
        }
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)> const& next) override {
          impl.execute(sched, std::move(pkt), next);
        }
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>&& next) override {
          impl.execute(sched, pkt, std::move(next));
        }
        virtual void execute(std::function<void (std::function<void ()>)>& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>&& next) override {
          impl.execute(sched, std::move(pkt), std::move(next));
        }
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet const& pkt, std::function<void (dart::packet const&)>&& next) override {
          impl.execute(sched, pkt, std::move(next));
        }
        virtual void execute(std::function<void (std::function<void ()>)> const& sched,
            dart::packet&& pkt, std::function<void (dart::packet const&)>&& next) override {
          impl.execute(sched, std::move(pkt), std::move(next));
        }

        virtual std::unique_ptr<eraser> clone() const override {
          return std::make_unique<erased_node>(impl);
        }

        /*----- Members -----*/

        Node impl;

      };

      /*----- Private Members -----*/

      std::unique_ptr<eraser> impl;

  };

}

#endif
