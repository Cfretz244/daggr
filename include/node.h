#ifndef DAGGR_NODES_H
#define DAGGR_NODES_H

/*----- Local Includes -----*/

#include "node/helper.h"
#include "node/comp.h"
#include "node/seq.h"
#include "node/all.h"

/*----- Type Declarations -----*/

namespace daggr {

  // Used to bootstrap the chaining API.
  inline daggr::node::comp<daggr::meta::none> noop;

}

#endif
