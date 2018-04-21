#' @include ast.R
NULL

#' Graphs from R Expressions
#'
#' Creates a plottable (DiagrammeR) expression graph from an unevaluated R expression.
#' By default, merges the innermost symbols (input variables) with the same label into
#' common nodes, similarly to the kind of expression graph one would see in a neural
#' network.
#'
#' @param expr an R expression. Typically a composite function (e.g. `f(g(x), h(x))`).
#' @param merge if set to `TRUE` (the default), merges input variables with the same name
#'   into the same graph node.
#' @param label_formatter a function which takes a character vector and returns a character
#'   vector.
#' @return a DiagrammeR graph corresponding to the supplied expression.
#'
#' @export
#' @examples
#' g <- expression_graph(f_1(f_2(f_3(x_1, x_2), x_3), f_1(f_4(f_5(x_1), f_6(x_2), x_4))))
#' g %>% DiagrammeR::render_graph()
expression_graph <- function(expr, merge = TRUE, label_formatter = default_label_formatter) {
  root <- ast_as_tree(rlang::enexpr(expr))
  root <- if(merge) merge_leaves_by_label(root) else root
  DiagrammeR::create_graph(
    nodes_df = nodes(root) %>%
      mutate(
        label = label_formatter(label),
        fillcolor = ifelse(is_leaf, 'lightgreen', 'lightblue')
      ),
    edges_df = edges(root)
  ) %>%
    add_global_graph_attrs('rankdir', 'LR', 'graph') %>%
    add_global_graph_attrs('layout', 'dot', 'graph') %>%
    add_global_graph_attrs('style', 'filled', 'node')
}

default_label_formatter <- function(labels) {
  gsub('\\_(.)', '@_{\\1}', labels)
}

nodes <- function(root) {
  dfs(root, NodesVisitor$new())$result()
}

edges <- function(root) {
  dfs(root, EdgesVisitor$new())$result()
}

merge_leaves_by_label <- function(root) {
  leaves <- Filter(
    dfs(root, NodesVisitor$new())$nodes,
    f = function(x) x$is_leaf()
  )
  seen = list()
  for(leaf in leaves) {
    if(leaf$label %in% names(seen)) {
      seen[[leaf$label]]$merge(leaf)
    } else {
      seen[[leaf$label]] <- leaf
    }
  }
  root
}

dfs <- function(root, visitor) {
  .dfs(root, list2env(list(seen = c())), visitor)
}

.dfs = function(root, state, visitor) {
  if (root$nid %in% state$seen){
    return(NULL)
  }
  state$seen <- unique(c(state$seen, root$nid))
  visitor$visit(root)
  for (neighbor in root$edges) {
    .dfs(neighbor, state, visitor)
  }
  visitor
}

FunVisitor <- R6::R6Class(
  "FuncVisitor",
  public = list(
    fun = NULL,
    initialize = function(fun) {
      self$fun = fun
    },
    visit = function(node) {
      self$fun(node)
    },
    results = function() { NULL }
  )
)

EdgesVisitor <- R6::R6Class(
  "EdgesVisitor",
  public = list(
    edges_df = NULL,
    visit = function(node) {
      node_edges <- node$edges_df()
      if (is.null(self$edges_df)) {
        self$edges_df = node_edges
      } else {
        self$edges_df <- self$edges_df %>% bind_rows(node_edges)
      }
    },
    result = function() {
      self$edges_df
    }
  )
)

NodesVisitor <- R6::R6Class(
  "NodesVisitor",
  public = list(
    nodes = list(),
    visit = function(node) {
      self$nodes <- c(node, self$nodes)
    },
    result = function() {
      lapply(
        self$nodes,
        function(x) {
          list(id = x$nid, type = NA, label = x$label, is_leaf = x$is_leaf())
        }
      ) %>% bind_rows
    }
  )
)

DNode <- R6::R6Class(
  'DNode',
  public = list(
    nid = 0,
    label = NULL,
    edges = list(),
    reverse = list(),

    initialize = function(nid, label) {
      self$nid <- as.integer(nid)
      self$label <- label
    },

    add_edge = function(destination) {
      self$edges <- c(self$edges, destination)
      destination$reverse <- c(destination$reverse, self)
    },

    merge = function(other) {
      self$edges <- c(self$edges, other$edges)
      self$reverse <- c(self$reverse, other$reverse)
      # Update parent pointers.
      for(parent in other$reverse) {
        if(parent$nid == self$nid) {
          next()
        }
        old_links <- sapply(parent$edges, function(node) node$nid == other$nid)
        if(length(old_links) != 0) {
          parent$edges[old_links] <- list(self)
        }
      }
      # Update our pointers.
      self$edges <- Filter(self$edges, f = function(node) node$nid != other$nid)
    },

    edges_df = function() {
      direct_ids <- sapply(
        self$edges,
        function(x) x$nid
      )
      tibble(
        from = rep(self$nid, length(self$edges)),
        to = direct_ids
      )
    },

    is_leaf = function() {
      length(self$edges) == 0
    }
  )
)
