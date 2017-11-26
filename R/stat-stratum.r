#' Stratum positions
#' 
#' Given a dataset with alluvial structure, \code{stat_stratum} calculates the
#' centroids of the strata at each axis, together with their weights (heights).
#' 

#' @section Aesthetics:
#' \code{stat_stratum} requires one of two sets of aesthetics:
#' \itemize{
#'   \item \code{x}, \code{stratum}, and (optionally) \code{alluvium}
#'   \item any number of \code{axis[0-9]*} (\code{axis1}, \code{axis2}, etc.)
#' }
#' Use \code{x} and \code{stratum} for data in lodes format
#' (\code{alluvium} is ignored)
#' and \code{axis[0-9]*} for data in alluvia format
#' (see \code{\link{is_alluvial}}).
#' Arguments to parameters inconsistent with the format will be ignored.
#' 
#' Additionally, \code{stat_stratum} accepts the following optional aesthetics:
#' \itemize{
#'   \item \code{weight}
#'   \item \code{label}
#'   \item \code{group}
#' }
#' \code{weight} controls the vertical dimensions of the alluvia
#' and are aggregated across equivalent observations.
#' \code{label} is used to label the strata and must take a unique value across
#' the observations within each stratum.
#' These and any other aesthetics are aggregated as follows:
#' Numeric aesthetics, including \code{weight}, are summed.
#' Character and factor aesthetics, including \code{label},
#' are assigned to strata provided they take unique values across the
#' observations within each stratum (otherwise \code{NA} is assigned).
#' \code{group} is used internally; arguments are ignored.
#' 

#' @import ggplot2
#' @seealso \code{\link[ggplot2]{layer}} for additional arguments,
#'   \code{\link{geom_stratum}} for the corresponding geom, and
#'   \code{\link{stat_alluvium}} and \code{\link{geom_alluvium}} for
#'   alluvial flows.
#' @inheritParams ggplot2::layer
#' @template common-params
#' @param geom The geometric object to use display the data;
#'    override the default.
#' @param decreasing Logical; whether to arrange the strata at each axis
#'   in the order of the variable values (\code{NA}, the default),
#'   in ascending order of total weight (largest on top, \code{FALSE}), or
#'   in descending order of total weight (largest on bottom, \code{TRUE}).
#' @param reverse Logical; if \code{decreasing} is \code{NA},
#'   whether to arrange the strata at each axis
#'   in the reverse order of the variable values,
#'   so that they match the order of the values in the legend.
#'   Ignored if \code{decreasing} is not \code{NA}.
#'   Defaults to \code{TRUE}.
#' @param label.strata Logical; whether to assign the values of the axis 
#'   variables to the strata. Defaults to FALSE, and requires that no
#'   \code{label} aesthetic is assigned.
#' @example inst/examples/ex-stat-stratum.r
#' @export
stat_stratum <- function(mapping = NULL,
                         data = NULL,
                         geom = "stratum",
                         position = "identity",
                         decreasing = NA,
                         reverse = TRUE,
                         label.strata = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         na.rm = FALSE,
                         ...) {
  layer(
    stat = StatStratum,
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      decreasing = decreasing,
      reverse = reverse,
      label.strata = label.strata,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggalluvial-ggproto
#' @usage NULL
#' @export
StatStratum <- ggproto(
  "StatStratum", Stat,
  
  required_aes = c("x"),
  
  setup_data = function(data, params) {
    
    # if 'alluvium' not provided, assign each row its own, grouped by 'x'
    if (is.null(data$alluvium) & !is.null(data$x)) {
      data$alluvium <- NA
      for (xx in unique(data$x)) {
        ww <- which(data$x == xx)
        data$alluvium[ww] <- 1:length(ww)
      }
    }
    
    # assign uniform weight if not provided
    if (is.null(data$weight)) {
      data$weight <- rep(1, nrow(data))
    } else if (any(is.na(data$weight))) {
      stop("Data contains NA weights.")
    }
    
    type <- get_alluvial_type(data)
    if (type == "none") {
      stop("Data is not in a recognized alluvial form ",
           "(see `help(is_alluvial)` for details).")
    }
    
    if (params$na.rm) {
      data <- na.omit(object = data)
    } else {
      data <- na_keep(data = data, type = type)
    }
    
    # ensure that data is in lode form
    if (type == "alluvia") {
      axis_ind <- get_axes(names(data))
      data <- to_lodes(data = data, axes = axis_ind)
      # positioning requires numeric 'x'
      data <- dplyr::arrange(data, x, stratum, alluvium)
      data$x <- contiguate(data$x)
    }
    
    # nullify 'group' and 'alluvium' fields (to avoid confusion with geoms)
    data <- transform(data,
                      group = NULL,
                      alluvium = NULL)
    
    data
  },
  
  compute_panel = function(self, data, scales,
                           decreasing = NA, reverse = TRUE,
                           label.strata = FALSE) {
    
    # introduce label (if absent)
    if (label.strata) {
      if (is.null(data$label)) {
        data$label <- data$stratum
      } else {
        warning("Aesthetic 'label' is specified, ",
                "so parameter 'label.strata' will be ignored.")
      }
    }
    
    # remove empty lodes (including labels)
    data <- subset(data, weight > 0)
    
    # aggregate data by 'x' and 'stratum'
    data <- auto_aggregate(data = data, by = c("x", "stratum"))
    
    # sort in preparation for calculating cumulative weights
    data <- if (is.na(decreasing)) {
      arr_fun <- if (reverse) dplyr::desc else identity
      dplyr::arrange(data, PANEL, x, arr_fun(stratum))
    } else {
      arr_fun <- if (decreasing) dplyr::desc else identity
      dplyr::arrange(data, PANEL, x, arr_fun(weight))
    }
    
    # calculate cumulative weights
    data$y <- NA
    for (xx in unique(data$x)) {
      ww <- which(data$x == xx)
      data$y[ww] <- cumsum(data$weight[ww]) - data$weight[ww] / 2
    }
    
    # y bounds
    transform(data,
              ymin = y - weight / 2,
              ymax = y + weight / 2)
  }
)

# summarize (or else return NAs) over numeric, character, and factor fields
auto_aggregate <- function(data, by) {
  agg <- stats::aggregate(x = rep(1, nrow(data)),
                          by = data[, by],
                          FUN = unique)
  agg[[3]] <- NULL
  rem_vars <- setdiff(names(data), by)
  for (var in rem_vars) {
    agg_var <- stats::aggregate(
      x = data[[var]],
      by = data[, by],
      FUN = if (var %in% c("size", "linetype", "fill", "color", "alpha",
                           "PANEL", "group")) {
        only
      } else {
        agg_fn(data[[var]])
      }
    )
    names(agg_var) <- c(by, var)
    agg <- merge(agg, agg_var, by = by, all = TRUE)
  }
  agg
}

# select aggregation function based on variable type
agg_fn <- function(x) {
  if (is.character(x) | is.factor(x)) {
    function(y) as.character(only(y))
  } else if (is.numeric(x)) {
    sum
  } else {
    function(y) NA
  }
}

# single unique value, or else NA
only <- function(x) {
  uniq <- unique(x)
  if (length(uniq) == 1) {
    uniq
  } else {
    NA
  }
}
