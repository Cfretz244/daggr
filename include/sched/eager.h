#ifndef DAGGR_EAGER_SCHEDULER_H
#define DAGGR_EAGER_SCHEDULER_H

/*----- System Includes -----*/

#include <utility>

/*----- Type Declarations -----*/

namespace daggr::sched {

  struct eager {
    template <class Graph>
    void operator ()(Graph&& g) {
      std::forward<Graph>(g)();
    }
  };

}

#endif
