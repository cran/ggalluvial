# Identify elements in a character vector that fit the pattern of axis aesthetic
# names, and return their indices in the numerical order of the axis numbers
# (with `axis` first, if present). Only non-negative integers are allowed.
get_axes <- function(x) {
  if (anyDuplicated(x)) {
    dupes <- unique(x[duplicated(x)])
    stop("Duplicated variables: ", paste(dupes, collapse = ", "))
  }
  axis_ind <- grep("^axis[0-9]*$", x)
  axis_ind[order(as.numeric(gsub("^axis", "", x[axis_ind])), na.last = FALSE)]
}

get_alluvial_type <- function(data) {
  # ensure that data is alluvial
  if (!is.null(data$x) | !is.null(data$stratum) | !is.null(data$alluvium)) {
    if (is.null(data$x) | is.null(data$stratum) | is.null(data$alluvium)) {
      stop("Parameters `x`, `stratum`, and `alluvium` are required ",
           "for data in lode form.")
    }
    if (is_lodes_form(data,
                      key = "x", value = "stratum", id = "alluvium",
                      weight = "y",
                      silent = TRUE)) return("lodes")
  } else {
    axis_ind <- get_axes(names(data))
    if (is_alluvia_form(data,
                        axes = axis_ind,
                        weight = "y",
                        silent = TRUE)) return("alluvia")
  }
  return("none")
}

# incorporate any missing values into factor levels
na_keep <- function(data, type) {
  if (type == "none") {
    stop("Data is not in a recognized alluvial form ",
         "(see `help('alluvial-data')` for details).")
  } else if (type == "lodes") {
    if (is.factor(data$stratum)) {
      data$stratum <- addNA(data$stratum, ifany = TRUE)
    } else {
      data$stratum[is.na(data$stratum)] <- ""
    }
  } else {
    axis_ind <- get_axes(names(data))
    for (i in axis_ind) {
      if (any(is.na(data[[i]]))) {
        if (is.factor(data[[i]])) {
          data[[i]] <- addNA(data[[i]], ifany = TRUE)
        } else {
          data[[i]][is.na(data[[i]])] <- ""
        }
      }
    }
  }
  data
}

# replace a vector `x` of any type with
# a numeric vector of *contiguous* integers that sort in the same order as `x`
contiguate <- function(x) {
  x <- xtfrm(x)
  match(x, sort(unique(x)))
}

# aggregate weights over otherwise equivalent alluvia (omitting missing values)
aggregate_along <- function(data, key, id, var) {
  # interaction of all variables to aggregate over
  data$agg_vars <- as.numeric(interaction(lapply(
    data[, -match(c(key, id, var), names(data)), drop = FALSE],
    addNA, ifany = FALSE
  ), drop = TRUE))
  # convert to alluvia format
  alluv_data <- alluviate(data, key, "agg_vars", id)
  # sort by everything except `id`
  alluv_data <- alluv_data[do.call(
    order,
    alluv_data[, -match(id, names(alluv_data)), drop = FALSE]
  ), , drop = FALSE]
  # define map from original to aggregated `id`s
  alluv_orig <- alluv_data[[id]]
  alluv_agg <- cumsum(!duplicated(interaction(
    alluv_data[, -match(id, names(alluv_data)), drop = FALSE]
  )))
  # transform `id` in `data` accordingly
  data[[id]] <- alluv_agg[match(data[[id]], alluv_orig)]
  # aggregate `var` by all other variables
  # and ensure that no `key`-`id` pairs are duplicated
  data <- unique(merge(
    stats::aggregate(formula = stats::as.formula(paste(var, "~ .")),
                     data = data[, c(key, id, "agg_vars", var)],
                     FUN = sum),
    data[, -match(var, names(data))],
    all.x = TRUE, all.y = FALSE
  ))
  data$agg_vars <- NULL
  # require that no `key`-`id` pairs are duplicated
  #stopifnot(all(!duplicated(data[, c(key, id)])))
  data
}

# build alluvial dataset for reference during lode-ordering
alluviate <- function(data, key, value, id) {
  to_alluvia_form(
    data[, c(key, value, id)],
    key = key, value = value, id = id
  )
}

# arrange data by aesthetics for consistent (reverse) z-ordering
z_order_aes <- function(data, aesthetics) {

  # `aesthetics` and 'group' are fixed within contiguous alluvial segments
  aes_data <- data[! duplicated(data[, c("alluvium", "group")]),
                   c("alluvium", aesthetics, "group")]
  if (length(aes_data) == 2) return(data)
  aes_data <- aes_data[do.call(order, aes_data[, c(aesthetics, "alluvium")]), ]

  # ensure order of "group" respects aesthetics
  data$group <- match(data$group, unique(aes_data$group))
  data[with(data, order(x, group)), , drop = FALSE]
}
