#' Check for alluvial structure and convert between alluvial formats
#'
#' Alluvial diagrams consist of multiple horizontally-distributed columns (axes)
#' representing factor variables, vertical divisions (strata) of these axes
#' representing these variables' values; and splines (alluvial flows) connecting
#' vertical subdivisions (lodes) within strata of adjacent axes representing
#' subsets or amounts of observations that take the corresponding values of the
#' corresponding variables. This function checks a data frame for either of two
#' types of alluvial structure:
#' \itemize{
#'   \item One row per \strong{lode}, wherein each row encodes a subset or
#'         amount of observations having a specific profile of axis values, a
#'         \code{key} field encodes the axis, a \code{value} field encodes the
#'         value within each axis, and a \code{id} column identifies multiple
#'         lodes corresponding to the same subset or amount of observations.
#'         \code{is_lodes_form} tests for this structure.
#'   \item One row per \strong{alluvium}, wherein each row encodes a subset or
#'         amount of observations having a specific profile of axis values and a
#'         set \code{axes} of fields encodes its values at each axis variable.
#'         \code{is_alluvia_form} tests for this structure.
#' }
#'

#' \code{to_lodes_form} takes a data frame with several designated variables to
#' be used as axes in an alluvial diagram, and reshapes the data frame so that
#' the axis variable names constitute a new factor variable and their values
#' comprise another. Other variables' values will be repeated, and a
#' row-grouping variable can be introduced. This function invokes
#' \code{\link[tidyr]{gather}}.
#'
#' \code{to_alluvia_form} takes a data frame with axis and axis value variables
#' to be used in an alluvial diagram, and reshape the data frame so that the
#' axes constitute separate variables whose values are given by the value
#' variable. This function invokes \code{\link[tidyr]{spread}}.
#'

#' @name alluvial-data
#' @import tidyselect
#' @family alluvial data manipulation
#' @param data A data frame.
#' @param logical Deprecated. Whether to return a logical value or a character
#'   string indicating the type of alluvial structure ("none", "lodes", or
#'   "alluvia").
#' @param silent Whether to print messages.
#' @param key,value,id In \code{to_lodes_form}, handled as in
#'   \code{\link[tidyr]{gather}} and used to name the new axis (key), stratum
#'   (value), and alluvium (identifying) variables. In \code{to_alluvia_form},
#'   handled as in \code{\link[tidyr]{spread}} and used to identify the fields
#'   of \code{data} to be used as the axis (key), stratum (value), and alluvium
#'   (identifying) variables.
#' @param axes In \code{*_alluvia_form}, handled as in
#'   \code{\link[dplyr]{select}} and used to identify the field(s) of
#'   \code{data} to be used as axes.
#' @param ... Used in \code{is_alluvia_form} and \code{to_lodes_form} as in
#'   \code{\link[dplyr]{select}} to determine axis variables, as an alternative
#'   to \code{axes}. Ignored when \code{axes} is provided.
#' @param weight Optional field of \code{data}, handled using
#'   \code{\link[rlang]{enquo}}, to be used as heights or depths of the alluvia
#'   or lodes.
#' @param diffuse Fields of \code{data}, handled using
#'   \code{\link[tidyselect]{vars_select}}, to merge into the reshapen data by
#'   \code{id}. They must be a subset of the axis variables. Alternatively, a
#'   logical value indicating whether to merge all (\code{TRUE}) or none
#'   (\code{FALSE}) of the axis variables.
#' @param distill A logical value indicating whether to include variables, other
#'   than those passed to \code{key} and \code{value}, that vary within values
#'   of \code{id}. Alternatively, a function (or its name) to be used to distill
#'   each such variable to a single value. In addition to existing functions,
#'   \code{distill} accepts the character values \code{"first"} (used if
#'   \code{distill} is \code{TRUE}), \code{"last"}, and \code{"most"} (which
#'   returns the modal value).
#' @param discern Logical value indicating whether to suffix values of the
#'   variables used as axes that appear at more than one variable in order to
#'   distinguish their factor levels. This forces the levels of the combined
#'   factor variable \code{value} to be in the order of the axes.
#' @example inst/examples/ex-alluvial-data.r

#' @rdname alluvial-data
#' @export
is_lodes_form <- function(data,
                          key, value, id,
                          weight = NULL,
                          logical = TRUE, silent = FALSE) {
  if (!isTRUE(logical)) deprecate_parameter("logical")

  key_var <- vars_pull(names(data), !!rlang::enquo(key))
  value_var <- vars_pull(names(data), !!rlang::enquo(value))
  id_var <- vars_pull(names(data), !!rlang::enquo(id))

  if (any(duplicated(cbind(data[c(key_var, id_var)])))) {
    if (!silent) warning("Duplicated id-axis pairings.")
  }

  n_pairs <-
    dplyr::n_distinct(data[key_var]) * dplyr::n_distinct(data[id_var])
  if (nrow(data) < n_pairs) {
    if (!silent) warning("Missing id-axis pairings.")
  }

  # if `weight` is not `NULL`, use NSE to identify `weight_var`
  if (!is.null(rlang::enexpr(weight))) {
    weight_var <- vars_select(names(data), !!rlang::enquo(weight))
    if (!is.numeric(data[[weight_var]])) {
      if (!silent) message("Lode weights are non-numeric.")
      return(if (logical) FALSE else "none")
    } else if (any(data[[weight_var]] < 0)) {
      if (!silent) message("Some lode weights are negative.")
      #return(if (logical) FALSE else "none")
      return(if (logical) TRUE else "lodes")
    }
  }

  if (logical) TRUE else "lodes"
}

#' @rdname alluvial-data
#' @export
is_alluvia_form <- function(data,
                            ..., axes = NULL,
                            weight = NULL,
                            logical = TRUE, silent = FALSE) {
  if (!isTRUE(logical)) deprecate_parameter("logical")

  if (is.null(rlang::enexpr(weight))) {
    weight_var <- NULL
  } else {
    weight_var <- vars_select(names(data), !!rlang::enquo(weight))
    if (!is.numeric(data[[weight_var]])) {
      if (!silent) message("Alluvium weights are non-numeric.")
      return(if (logical) FALSE else "none")
    } else if (any(data[[weight_var]] < 0)) {
      if (!silent) message("Some alluvium weights are negative.")
      return(if (logical) FALSE else "none")
    }
  }

  if (!is.null(rlang::enexpr(axes))) {
    axes <- data_at_vars(data, axes)
  } else {
    quos <- rlang::quos(...)
    if (rlang::is_empty(quos)) {
      axes <- setdiff(names(data), c(weight_var))
    } else {
      axes <- unname(vars_select(names(data), !!!quos))
    }
  }

  n_alluvia <- nrow(dplyr::distinct(data[axes]))
  n_combns <- do.call(prod, lapply(data[axes], dplyr::n_distinct))
  if (n_alluvia < n_combns) {
    if (!silent) message("Missing alluvia for some stratum combinations.")
  }

  if (logical) TRUE else "alluvia"
}

#' @rdname alluvial-data
#' @export
to_lodes_form <- function(data,
                          ..., axes = NULL,
                          key = "x", value = "stratum", id = "alluvium",
                          diffuse = FALSE, discern = FALSE) {

  key_var <- rlang::quo_name(rlang::enexpr(key))
  value_var <- rlang::quo_name(rlang::enexpr(value))
  id_var <- rlang::quo_name(rlang::enexpr(id))

  if (!is.null(rlang::enexpr(axes))) {
    axes <- data_at_vars(data, axes)
  } else {
    quos <- rlang::quos(...)
    if (rlang::is_empty(quos)) {
      axes <- names(data)
    } else {
      axes <- unname(vars_select(names(data), !!!quos))
    }
  }

  stopifnot(is_alluvia_form(data, axes, silent = TRUE))

  if (!is.data.frame(data)) data <- as.data.frame(data)

  if (is.logical(rlang::enexpr(diffuse))) {
    diffuse <- if (diffuse) axes else NULL
  } else {
    diffuse <- unname(vars_select(names(data), !!rlang::enquo(diffuse)))
    if (!all(diffuse %in% axes)) {
      stop("All `diffuse` variables must be `axes` variables.")
    }
  }

  # combine factor levels
  cat_levels <- unname(unlist(lapply(lapply(data[axes], as.factor), levels)))
  if (any(duplicated(cat_levels)) & is.null(discern)) {
    warning("Some strata appear at multiple axes.")
  }
  if (isTRUE(discern)) {
    data <- discern_data(data, axes)
    # uniquify strata separately from `discern_data` as a validation step
    strata <- make.unique(unname(cat_levels))
  } else {
    strata <- unique(unname(cat_levels))
  }

  # format data in preparation for `gather()`
  data[[id_var]] <- 1:nrow(data)
  if (!is.null(diffuse)) {
    diffuse_data <- data[, c(id_var, diffuse), drop = FALSE]
  }
  for (i in axes) data[[i]] <- as.character(data[[i]])

  # `gather()` by `axes`
  res <- tidyr::gather(data,
                       key = !!key_var, value = !!value_var,
                       axes,
                       factor_key = TRUE)
  res[[value_var]] <- factor(res[[value_var]], levels = strata)
  # recombine with `diffuse_data`
  if (!is.null(diffuse)) {
    res <- merge(diffuse_data, res, by = id_var, all.x = FALSE, all.y = TRUE)
  }

  res
}

#' @rdname alluvial-data
#' @export
to_alluvia_form <- function(data,
                            key, value, id,
                            distill = FALSE) {

  key_var <- vars_pull(names(data), !!rlang::enquo(key))
  value_var <- vars_pull(names(data), !!rlang::enquo(value))
  id_var <- vars_pull(names(data), !!rlang::enquo(id))

  stopifnot(is_lodes_form(data, key_var, value_var, id_var, silent = TRUE))

  # handle any variables that vary within `id`s
  uniq_id <- length(unique(data[[id_var]]))
  uniq_data <- unique(data[setdiff(names(data), c(key_var, value_var))])
  if (! uniq_id == nrow(uniq_data)) {
    distill_vars <- names(which(sapply(
      setdiff(names(uniq_data), id_var),
      function(x) nrow(unique(uniq_data[c(id_var, x)]))
    ) > uniq_id))
    if (is.logical(distill)) {
      if (distill) {
        distill <- most
      } else {
        warning("The following variables vary within `id`s ",
                "and will be dropped: ",
                paste(distill_vars, collapse = ", "))
        distill <- NULL
      }
    } else if (is.character(distill)) {
      distill <- get(distill)
    }
    if (!is.null(distill)) {
      stopifnot(is.function(distill))
      message("Distilled variables: ",
              paste(distill_vars, collapse = ", "))
      distill_data <- stats::aggregate(
        data[distill_vars],
        data[id_var],
        distill
      )
      if (length(distill_vars) == 1) names(distill_data)[-1] <- distill_vars
    }
    data <- data[setdiff(names(data), distill_vars)]
  } else {
    distill <- NULL
  }

  # `spread()` by designated `key` and `value`
  res <- tidyr::spread(data, key = !!key_var, value = !!value_var)
  # recombine with `distill_data`
  if (!is.null(distill)) {
    res <- merge(distill_data, res, by = id_var, all.x = FALSE, all.y = TRUE)
  }

  res
}

# distilling functions
first <- dplyr::first
last <- dplyr::last
most <- function(x) {
  x[which(factor(x) == names(which.max(table(factor(x)))))[1]]
}

# require different character strings to represent strata at different axes
discern_data <- function(data, axes, sep = ".") {
  # strata at each axis in order
  list_levels <- lapply(lapply(data[axes], as.factor), levels)
  # concatenated vector of strata at all axes
  cat_levels <- unlist(list_levels)
  # vector of uniquified strata across all axes
  new_levels <- make.unique(unname(cat_levels))
  # cumulative number of strata before each axis
  i_levels <- cumsum(c(0, sapply(list_levels, length)))
  # characterized, uniquified strata at each axis
  for (i in seq_along(axes)) {
    axis_levels <- as.numeric(as.factor(data[[axes[i]]]))
    level_inds <- (i_levels[i] + 1):i_levels[i + 1]
    data[[axes[i]]] <- new_levels[level_inds][axis_levels]
  }
  data
}

# mimic the behavior of `tbl_at_vars()` in `select_at()`
data_at_vars <- function(data, vars) {
  data_vars <- names(data)
  if (rlang::is_character(vars)) {
    vars
  } else if (rlang::is_integerish(vars)) {
    data_vars[vars]
  } else if (rlang::is_quosures(vars)) {
    out <- dplyr::select_vars(data_vars, !!!vars)
    if (!any(rlang::have_name(vars))) {
      names(out) <- NULL
    }
    out
  } else {
    stop("Either a character or numeric vector ",
         "or a `vars()` object ",
         "is required.")
  }
}
