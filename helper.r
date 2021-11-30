

###########helper function
#
# HELPERS  ----------
# ///////////////////

#
# LIBRARIES  ==========
# /////////////////////////////
library(scales)


#
# CONFIG  ==========
# /////////////////////////////


#
# FUNCTIONS  ==========
# /////////////////////////////

config_plotly_modebar = function(p)
{
  return(plotly::config(p, "displaylogo" = FALSE, "displayModeBar" = TRUE))
}


if_end <- function(node, true, false) {
  ifelse(node %in% c("ARTIFICIAL_START","ARTIFICIAL_END"), true, false)
}
if_start <- function(node, true, false) {
  ifelse(node %in% c("ARTIFICIAL_START"), true, false)
}


case_id_ <- function(eventlog) sym(case_id(eventlog))
activity_id_ <- function(eventlog) sym(activity_id(eventlog))
activity_instance_id_ <- function(eventlog) sym(activity_instance_id(eventlog))
resource_id_ <- function(eventlog) sym(resource_id(eventlog))
timestamp_ <- function(eventlog) sym(timestamp(eventlog))
lifecycle_id_ <- function(eventlog) sym(lifecycle_id(eventlog))


custom_process_map <- function(eventlog,
                               type = frequency("absolute"),
                               sec = NULL,
                               type_nodes = type,
                               type_edges = type,
                               sec_nodes = sec,
                               sec_edges = sec,
                               rankdir = "LR",
                               render = T,
                               fixed_edge_width = F,
                               layout = layout_pm(),
                               fixed_node_pos = NULL,
                               edge_cutoff=0,
                               ...) {
  
  from <- NULL
  to <- NULL
  label_numeric <- NULL
  node <- NULL
  min_order <- NULL
  ACTIVITY_CLASSIFIER_ <- NULL
  ACTIVITY_INSTANCE_CLASSIFIER_ <- NULL
  CASE_CLASSIFIER_ <- NULL
  TIMESTAMP_CLASSIFIER_ <- NULL
  start_time <- NULL
  end_time <- NULL
  node_id <- NULL
  n.x <- NULL
  n.y <- NULL
  from_id <- NULL
  tooltip <- NULL
  label <- NULL
  next_act <- NULL
  to_id <- NULL
  duration <- NULL
  value <- NULL
  color_level <- NULL
  sec_label <- NULL
  node_id.y <- NULL
  node_id.x <- NULL
  weight <- NULL
  constraint <- NULL
  
  if(!is.null(fixed_node_pos)) {
    warning("Argument fixed_node_pos deprecated, use layout argument instead.")
    layout <- layout_pm(fixed_positions = fixed_node_pos)
  }
  
  if (any(is.na(eventlog %>% pull(!!timestamp_(eventlog))))) {
    warning("Some of the timestamps in the supplied event log are missing (NA values). This may result in a invalid process map!")
    
  }
  
  
  #base_precedence <- create_base_precedence(eventlog, type_nodes, type_edges)
  
  eventlog <- ungroup_eventlog(eventlog)
  
  eventlog %>%
    as.data.frame() %>%
    droplevels %>%
    select(ACTIVITY_CLASSIFIER_ = !!activity_id_(eventlog),
           ACTIVITY_INSTANCE_CLASSIFIER_ = !!activity_instance_id_(eventlog),
           CASE_CLASSIFIER_ = !!case_id_(eventlog),
           TIMESTAMP_CLASSIFIER_ = !!timestamp_(eventlog),
           .order,
           everything()) -> prepared_log
  
  perspective_nodes <- attr(type_nodes, "perspective")
  perspective_edges <- attr(type_edges, "perspective")
  
  #create base_log: list of case > activity > instance - start + end + min + order (+ custom attributes)
  
  group_log <- function(log) {
    group_by(log, ACTIVITY_CLASSIFIER_, ACTIVITY_INSTANCE_CLASSIFIER_, CASE_CLASSIFIER_)
  }
  
  if(perspective_nodes == "custom" && perspective_edges == "custom") {
    attributeNode <- sym(attr(type_nodes, "attribute"))
    attributeEdge <- sym(attr(type_edges, "attribute"))
    prepared_log %>%
      group_log() %>%
      summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
                end_time = max(TIMESTAMP_CLASSIFIER_),
                min_order = min(.order),
                !!attributeNode := first(!!attributeNode),
                !!attributeEdge := first(!!attributeEdge)) -> base_log
  } else if(perspective_nodes == "custom") {
    attribute <- sym(attr(type_nodes, "attribute"))
    prepared_log %>%
      group_log() %>%
      summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
                end_time = max(TIMESTAMP_CLASSIFIER_),
                min_order = min(.order),
                !!attribute := first(!!attribute)) -> base_log
  } else if (perspective_edges == "custom") {
    attribute <- sym(attr(type_edges, "attribute"))
    prepared_log %>%
      group_log() %>%
      summarize(start_time = min(TIMESTAMP_CLASSIFIER_),
                end_time = max(TIMESTAMP_CLASSIFIER_),
                min_order = min(.order),
                !!attribute := first(!!attribute)) -> base_log
  } else {
    # speed up standard aggregation by using data.table fast grouping (GForce)
    data.table::setDT(prepared_log)
    prepared_log[, list(start_time = min(TIMESTAMP_CLASSIFIER_),
                        end_time = max(TIMESTAMP_CLASSIFIER_),
                        min_order = min(.order)),
                 by = c("ACTIVITY_CLASSIFIER_", "ACTIVITY_INSTANCE_CLASSIFIER_", "CASE_CLASSIFIER_")] %>%
      as.data.frame() -> base_log
  }
  
  #create end points for graph
  
  base_log %>%
    group_by(CASE_CLASSIFIER_) %>%
    arrange(start_time, min_order) -> points_temp
  
  points_temp %>%
    slice(1) %>%
    mutate(ACTIVITY_CLASSIFIER_ = "ARTIFICIAL_START",
           end_time = start_time,
           min_order = -Inf) -> end_points_start
  points_temp %>%
    slice(n()) %>%
    mutate(ACTIVITY_CLASSIFIER_ = "ARTIFICIAL_END",
           start_time = end_time,
           min_order = Inf) -> end_points_end
  
  #add endpoints to base log
  
  suppressWarnings(
    bind_rows(end_points_start, end_points_end, base_log) %>%
      ungroup() -> base_log
  )
  
  #create base nodes list
  
  base_log %>%
    count(ACTIVITY_CLASSIFIER_) %>%
    mutate(node_id = 1:n()) -> base_nodes
  data.table::setDT(base_nodes, key = c("ACTIVITY_CLASSIFIER_"))
  
  #create base precedence list
  
  data.table::setDT(base_log, key = c("start_time", "min_order"))
  base_log[, ACTIVITY_CLASSIFIER_ := ordered(ACTIVITY_CLASSIFIER_,
                                             levels = c("ARTIFICIAL_START", as.character(sort(activity_labels(eventlog))), "ARTIFICIAL_END"))
           ][, `:=`(next_act = data.table::shift(ACTIVITY_CLASSIFIER_, 1, type = "lead"),
                    next_start_time = data.table::shift(start_time, 1, type = "lead"),
                    next_end_time = data.table::shift(end_time, 1, type = "lead")),
             by = CASE_CLASSIFIER_] %>%
    merge(base_nodes, by.x = c("ACTIVITY_CLASSIFIER_"), by.y = c("ACTIVITY_CLASSIFIER_"), all = TRUE) %>%
    merge(base_nodes, by.x = c("next_act"), by.y = c("ACTIVITY_CLASSIFIER_"), all = TRUE) %>%
    as.data.frame() %>%
    select(everything(),
           -n.x, -n.y,
           from_id = node_id.x,
           to_id = node_id.y) -> base_precedence
  
  extra_data <- list()
  extra_data$n_cases <- n_cases(eventlog)
  extra_data$n_activity_instances <- n_activity_instances(eventlog)
  
  # primary info
  nodes <- attr(type_nodes, "create_nodes")(base_precedence, type_nodes, extra_data)
  edges <- attr(type_edges, "create_edges")(base_precedence, type_edges, extra_data)
  
  if(edge_cutoff>0)
  {
    ncases <- n_cases(eventlog)
    threshold_value <-ncases * (edge_cutoff/100)
    
    tobe_filtered_edges <- edges %>% filter(.data$n<=threshold_value) %>%  mutate(penwidth=0.1) %>%  mutate(label="")
    filtered_edges <- edges %>% filter(.data$n>threshold_value)
    edges <- rbind(filtered_edges,tobe_filtered_edges)
    
    nodes_tobe_filtered <- union(unique(tobe_filtered_edges$ACTIVITY_CLASSIFIER_),unique(tobe_filtered_edges$next_act))
    nodes_tobe_retained <- union(unique(filtered_edges$ACTIVITY_CLASSIFIER_),unique(filtered_edges$next_act))
    nodes_tobe_removed <- nodes_tobe_filtered[nodes_tobe_filtered %in% nodes_tobe_retained == FALSE]
    nodes_tobe_removed <- nodes_tobe_removed[!nodes_tobe_removed %in% c("ARTIFICIAL_START","ARTIFICIAL_END")]
    
    nodes <- nodes %>% mutate(color_level = ifelse(.data$ACTIVITY_CLASSIFIER_ %in% nodes_tobe_removed,0.1,.data$color_level)) %>%
      mutate(label = ifelse(.data$ACTIVITY_CLASSIFIER_ %in% nodes_tobe_removed,n,.data$label)) %>%
      mutate(shape = ifelse(.data$ACTIVITY_CLASSIFIER_ %in% nodes_tobe_removed,"circle",.data$shape))
    
  }
  
  
  # secondary info
  if(!is.null(sec_nodes)) {
    nodes_secondary <- attr(sec_nodes, "create_nodes")(base_precedence, sec_nodes, extra_data) %>%
      select(ACTIVITY_CLASSIFIER_, from_id, label) %>%
      rename(sec_label = label)
    
    
    nodes %>%
      full_join(nodes_secondary, by = c("ACTIVITY_CLASSIFIER_", "from_id")) %>%
      mutate(label = if_end(ACTIVITY_CLASSIFIER_,
                            ACTIVITY_CLASSIFIER_,
                            str_replace(paste0(label, "\n","(", map(sec_label, ~str_split(.x, "\n")[[1]][2]), ")"), "\n\\(\\)",""))) -> nodes
  }
  
  if(!is.null(sec_edges)) {
    edges_secondary <- attr(sec_edges, "create_edges")(base_precedence, sec_edges, extra_data) %>%
      select(from_id, to_id, label) %>%
      rename(sec_label = label)
    
    edges %>%
      full_join(edges_secondary, by = c("from_id","to_id")) %>%
      mutate(label = str_replace(paste0(label, "\n (", sec_label, ')'), "\n \\( \\)","")) -> edges
  }
  
  if(fixed_edge_width) {
    edges %>% mutate(penwidth = 1) -> edges
  }
  
  # This is to improve the DOT layout by using the frequency information
  if (layout$edge_weight) {
    edges %>% mutate(weight = as.integer(((n - min(n)) / max(n)) * 100)) -> edges
  } else {
    edges %>% mutate(weight = 1) -> edges
  }
  
  # This is to improve the DOT layout by simply ignoring very infrequent edges in the layout
  if (layout$edge_cutoff > 0) {
    edges %>%
      mutate(constraint = if_else(((n - min(n)) / max(n)) < layout$edge_cutoff, FALSE, TRUE)) %>%
      # at least one output edge per activity should be used in the layout
      group_by(from_id) %>%
      mutate(constraint = n == max(n) | constraint) %>%
      ungroup() %>%
      # same with input edges
      group_by(to_id) %>%
      mutate(constraint = n == max(n) | constraint) %>%
      ungroup() -> edges
  } else {
    edges %>% mutate(constraint = TRUE) -> edges
  }
  
  nodes %>%
    mutate(color_level = rescale(color_level, from = c(0, max(color_level)))) %>%
    mutate(color_level = if_end(ACTIVITY_CLASSIFIER_, Inf, color_level)) -> nodes
  
  
  ## this is not being applied
  # nodes %>% mutate(label = if_end(ACTIVITY_CLASSIFIER_, recode(ACTIVITY_CLASSIFIER_, ARTIFICIAL_START = "Start",ARTIFICIAL_END = "End"),ACTIVITY_CLASSIFIER_)) %>%
  #   mutate(shape = if_end(ACTIVITY_CLASSIFIER_,"circle","rectangle")) %>%
  #   mutate(tooltip  = if_end(ACTIVITY_CLASSIFIER_, recode(ACTIVITY_CLASSIFIER_, ARTIFICIAL_START = "Start",ARTIFICIAL_END = "End"),ACTIVITY_CLASSIFIER_))-> nodes
  # 
  
  custom_create_node_df(n = nodes$from_id,
                        label = nodes$label,
                        shape = nodes$shape,
                        color_level = nodes$color_level,
                        style = "rounded,filled",
                        fontcolor = nodes$fontcolor,
                        color = nodes$color,
                        tooltip = nodes$tooltip,
                        penwidth = 1.5,
                        fixedsize = FALSE,
                        fontname = "Arial",
                        fontsize = 40) -> nodes_df
  
  if (is.data.frame(layout$fixed_positions)) {
    nodes %>%
      left_join(layout$fixed_positions, by = c("ACTIVITY_CLASSIFIER_" = "act")) -> nodes
    nodes_df %>% mutate(x = nodes$x, y = nodes$y) -> nodes_df
  }
  
  min_level <- min(nodes_df$color_level)
  max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])
  
  create_edge_df(from = edges$from_id,
                 to = edges$to_id,
                 label = edges$label,
                 penwidth = edges$penwidth,
                 color = attr(type_edges, "color_edges"),
                 fontname = "Arial",
                 fontsize = 40,
                 weight = edges$weight,
                 constraint = edges$constraint) -> edges_df
  
  create_graph(nodes_df, edges_df) %>%
    add_global_graph_attrs(attr = "rankdir", value = rankdir,attr_type = "graph") %>%
    add_global_graph_attrs(attr = "layout", value = if_else(is.data.frame(layout$fixed_positions), "neato", "dot"), attr_type = "graph") %>%
    colorize_node_attrs(node_attr_from = "color_level",
                        node_attr_to = "fillcolor",
                        palette = attr(type_nodes, "color"),
                        default_color = "white",
                        cut_points = seq(min_level-0.1, max_level+.1, length.out = 9)) -> graph
  
  base_precedence %>%
    rename(case = CASE_CLASSIFIER_,
           aid = ACTIVITY_INSTANCE_CLASSIFIER_,
           act = ACTIVITY_CLASSIFIER_) -> base_precedence
  
  #metadata
  edges %>%
    rename(from = ACTIVITY_CLASSIFIER_,
           to = next_act) %>%
    select(from, to, from_id, to_id, value = label_numeric) -> edges
  nodes %>%
    rename(node = ACTIVITY_CLASSIFIER_) %>%
    select(node, from_id, value) -> nodes
  
  if(render == T) {
    
    # Since DiagrammeR does not support the necessary GraphViz attributes,
    # we use a workaround to add them tot the DOT code. See the issue logged here:
    # https://github.com/rich-iannone/DiagrammeR/issues/360
    
    # hack to add 'weight' attribute to the graph
    graph$edges_df %>%
      mutate(len = weight, decorate = constraint) -> graph$edges_df
    
    graph %>% render_graph() -> graph
    
    graph$x$diagram %>%
      stringr::str_replace_all("len", "weight") %>%
      stringr::str_replace_all("decorate", "constraint") -> graph$x$diagram
    
    attr(graph, "base_precedence") <- base_precedence
    attr(graph, "edges") <- edges
    attr(graph, "nodes") <- nodes
    
    graph %>% return()
  } else  {
    attr(graph, "base_precedence") <- base_precedence
    attr(graph, "edges") <- edges
    attr(graph, "nodes") <- nodes
    graph %>% return()
  }
  
}

custom_create_node_df <- function(n,
                                  type = NULL,
                                  label = NULL,
                                  ...) {
  
  
  if (is.null(type)) {
    type <- rep(as.character(NA), length(n))
  }
  
  if (!is.null(type)) {
    # Expand vectors with single values to fill to
    # the number of nodes
    if (length(type) == 1) {
      type <- rep(type, n)
    }
    
    # Expand vectors with `length` > `1` and
    # `length` < `length(nodes)`
    if (length(type) > 1 &
        length(type) < length(n)) {
      type <-
        c(type, rep(as.character(NA), (length(n) - length(type))))
    }
    
    # Trim vectors with number of values exceeding the
    # number of nodes
    if (length(type) > length(n)) {
      type <- type[1:length(n)]
    }
  }
  
  # Collect extra vectors of data as `extras`
  extras <- list(...)
  
  if (length(extras) > 0) {
    
    for (i in 1:length(extras)) {
      
      # Expand vectors with single values to fill to
      # the number of nodes
      if (length(extras[[i]]) == 1) {
        extras[[i]] <- rep(extras[[i]], length(n))
      }
      
      # Expand vectors with `length` > `1` and
      # `length` < `length(nodes)`
      if (length(extras[[i]]) > 1 &
          length(extras[[i]]) < length(n)) {
        extras[[i]] <-
          c(extras[[i]],
            rep("", (length(n) - length(extras[[i]]))))
      }
      
      # Trim vectors with number of values exceeding
      # the number of nodes
      if (length(extras[[i]]) > length(n)) {
        extras[[i]] <- extras[[i]][1:length(n)]
      }
    }
    
    # Create a data frame from the `extras` list
    extras <-
      as.data.frame(
        extras, stringsAsFactors = FALSE)
  }
  
  # Interpret node label values
  if (is.null(label)) {
    label <- rep(as.character(NA), length(n))
  } else if (inherits(label, "numeric") |
             inherits(label, "character")) {
    label <- as.character(label)
  } else if (inherits(label, "logical") &
             length(label) == 1) {
    if (label == TRUE) {
      label <- as.character(1:length(n))
    } else {
      label <- rep(as.character(NA), length(n))
    }
  }
  
  if (inherits(extras, "data.frame")) {
    nodes_df <-
      dplyr::bind_cols(
        data.frame(
          id = n,
          type = type,
          label = label,
          stringsAsFactors = FALSE),
        extras)
    
  } else {
    nodes_df <-
      data.frame(
        id = n,
        type = type,
        label = label,
        stringsAsFactors = FALSE)
  }
  nodes_df
}

