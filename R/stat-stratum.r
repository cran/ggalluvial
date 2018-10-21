#' Stratum positions
#'
#' Given a dataset with alluvial structure, `stat_stratum` calculates the
#' centroids of the strata at each axis, together with their weights (heights).
#' @template stat-aesthetics
#'

#' @import ggplot2
#' @family alluvial stat layers
#' @seealso [ggplot2::layer()] for additional arguments and
#'   [geom_stratum()] for the corresponding geom.
#' @inheritParams ggplot2::layer
#' @template layer-params
#' @param geom The geometric object to use display the data;
#'    override the default.
#' @param decreasing Logical; whether to arrange the strata at each axis
#'   in the order of the variable values (`NA`, the default),
#'   in ascending order of total weight (largest on top, `FALSE`), or
#'   in descending order of total weight (largest on bottom, `TRUE`).
#' @param reverse Logical; if `decreasing` is `NA`,
#'   whether to arrange the strata at each axis
#'   in the reverse order of the variable values,
#'   so that they match the order of the values in the legend.
#'   Ignored if `decreasing` is not `NA`.
#'   Defaults to `TRUE`.
#' @param discern Passed to [to_lodes_form()] if `data` is in
#'   alluvia format.
#' @param label.strata Logical; whether to assign the values of the axis
#'   variables to the strata. Defaults to FALSE, and requires that no
#'   `label` aesthetic is assigned.
#' @example inst/examples/ex-stat-stratum.r
#' @export
stat_stratum <- function(mapping = NULL,
                         data = NULL,
                         geom = "stratum",
                         position = "identity",
                         decreasing = NA,
                         reverse = TRUE,
                         discern = FALSE,
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
      discern = discern,
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

    # if `alluvium` not provided, assign each row its own, grouped by `x`
    if (is.null(data$alluvium) & ! is.null(data$x)) {
      data$alluvium <- NA
      for (xx in unique(data$x)) {
        ww <- which(data$x == xx)
        data$alluvium[ww] <- 1:length(ww)
      }
    }

    # assign uniform weight if not provided
    if (is.null(data$y)) {
      if (is.null(data$weight)) {
        data$y <- rep(1, nrow(data))
      } else {
        deprecate_parameter("weight", "y", type = "aesthetic")
        data$y <- data$weight
        data$weight <- NULL
      }
    } else if (any(is.na(data$y))) {
      stop("Data contains missing `y` values.")
    }

    type <- get_alluvial_type(data)
    if (type == "none") {
      stop("Data is not in a recognized alluvial form ",
           "(see `help('alluvial-data')` for details).")
    }

    if (params$na.rm) {
      data <- na.omit(object = data)
    } else {
      data <- na_keep(data = data, type = type)
    }

    # ensure that data is in lode form
    if (type == "alluvia") {
      axis_ind <- get_axes(names(data))
      data <- to_lodes_form(data = data, axes = axis_ind,
                            discern = params$discern)
      # positioning requires numeric `x`
      data <- data[with(data, order(x, stratum, alluvium)), , drop = FALSE]
      data$x <- contiguate(data$x)
    } else {
      if (! is.null(params$discern) && ! (params$discern == FALSE)) {
        warning("Data is already in lodes format, ",
                "so `discern` will be ignored.")
      }
    }

    # nullify `group` and `alluvium` fields (to avoid confusion with geoms)
    data <- transform(data,
                      group = NULL,
                      alluvium = NULL)

    data
  },

  compute_panel = function(self, data, scales,
                           decreasing = NA, reverse = TRUE,
                           discern = FALSE, label.strata = FALSE) {

    # introduce label (if absent)
    if (label.strata) {
      if (is.null(data$label)) {
        data$label <- data$stratum
      } else {
        warning("Aesthetic `label` is specified, ",
                "so parameter `label.strata` will be ignored.")
      }
    }

    # remove empty lodes (including labels)
    data <- subset(data, y != 0)

    # aggregate data by `x` and `stratum`
    data <- auto_aggregate(data = data, by = c("x", "stratum"))

    # sort in preparation for calculating cumulative weights
    data <- if (is.na(decreasing)) {
      arr_fun <- if (reverse) dplyr::desc else identity
      data[with(data, order(PANEL, x, arr_fun(stratum))), , drop = FALSE]
    } else {
      arr_fun <- if (decreasing) dplyr::desc else identity
      data[with(data, order(PANEL, x, arr_fun(y))), , drop = FALSE]
    }

    # calculate cumulative weights
    data$ycum <- NA
    for (xx in unique(data$x)) {
      ww <- which(data$x == xx)
      data$ycum[ww] <- cumsum(data$y[ww]) - data$y[ww] / 2
    }

    # y bounds
    data <- transform(data,
                      ymin = ycum - y / 2,
                      ymax = ycum + y / 2,
                      y = ycum)
    data$ycum <- NULL

    data
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
