#ifndef DAGGR_LAZY_SCHEDULER_H
#define DAGGR_LAZY_SCHEDULER_H

/*----- System Includes -----*/

#include <thread>

/*----- Type Declarations -----*/

namespace daggr::sched {

  struct lazy {
    template <class Graph>
    void operator ()(Graph&& g) {
      std::thread {std::forward<Graph>(g)}.detach();
    }
  };

}

#endif
