ast_as_tree <- function(expr) {
  visit_node(expr, nid = Counter$new())
}

visit_node <- function(expr, parent = NULL, nid) {
  nid$inc()
  root <- if(is.call(expr)) {
    visit_call(expr, parent, nid)
  } else {
    visit_other(expr, parent, nid)
  }
}

visit_other <- function(expr, parent, nid) {
  mk_node(deparse(expr), nid)
}

mk_node <- function(name, nid) {
  DNode$new(
    nid = nid$n,
    label = name
  )
}

visit_call <- function(expr, parent, nid) {
  elements <- as.list(expr)
  op <- deparse(elements[[1]])
  
  if(!prune(op)) {
    parent <- mk_node(op, nid)
  }
  
  for(child in elements[-1]) {
    child_node <- visit_node(child, parent, nid)
    if (!is.null(child_node)) {
      parent$add_edge(child_node)
    }
  }
  if(!prune(op)) parent else NULL
}

prune <- function(op) { op == '(' }

Counter <- R6::R6Class(
  'Counter',
  public = list(
    n = 0,
    inc = function() {
      self$n <- self$n + 1
    }
  )
)

