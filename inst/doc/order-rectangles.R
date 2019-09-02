## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 3, fig.align = "center")
library(ggalluvial)

## ----data----------------------------------------------------------------
# toy data set
set.seed(0)
toy <- data.frame(
  subject = rep(LETTERS[1:5], times = 4),
  collection = rep(1:4, each  = 5),
  category = rep(
    sample(c("X", "Y"), 16, replace = TRUE),
    rep(c(1, 2, 1, 1), times = 4)
  ),
  class = c("one", "one", "one", "two", "two")
)
print(toy)

## ----plot----------------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  geom_alluvium(aes(fill = class)) +
  geom_stratum()

## ----strata--------------------------------------------------------------
# collection point and category variables only
data <- setNames(toy[, 2:3], c("x", "stratum"))
# required fields for stat transformations
data$y <- 1
data$PANEL <- 1
# stratum transformation
StatStratum$compute_panel(data)

## ----strata plot---------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category)) +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----strata reverse------------------------------------------------------
# stratum transformation with strata in original order
StatStratum$compute_panel(data, reverse = FALSE)
ggplot(toy, aes(x = collection, stratum = category)) +
  stat_stratum(reverse = FALSE) +
  stat_stratum(geom = "text", aes(label = category), reverse = FALSE)

## ----strata decreasing---------------------------------------------------
# stratum transformation with strata in original order
StatStratum$compute_panel(data, reverse = FALSE)
ggplot(toy, aes(x = collection, stratum = category)) +
  stat_stratum(decreasing = TRUE) +
  stat_stratum(geom = "text", aes(label = category), decreasing = TRUE)

## ----alluvia-------------------------------------------------------------
# collection point, category, and subject variables
data <- setNames(toy[, 1:3], c("alluvium", "x", "stratum"))
# required fields for stat transformations
data$y <- 1
data$PANEL <- 1
# alluvium transformation
StatAlluvium$compute_panel(data)

## ----alluvia plot--------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class)) +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----flows---------------------------------------------------------------
# flow transformation
StatFlow$compute_panel(data)

## ----flows plot----------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_stratum() +
  stat_flow(aes(fill = class)) +
  stat_stratum(geom = "text", aes(label = category))

## ----lode zigzag---------------------------------------------------------
for (i in 1:4) print(lode_zigzag(4, i))

## ----alluvia plot w/ backfront guidance----------------------------------
for (i in 1:4) print(lode_backfront(4, i))
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class), lode.guidance = "backfront") +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----alluvia plot w/ backward guidance-----------------------------------
for (i in 1:4) print(lode_backward(4, i))
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class), lode.guidance = "backward") +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----alluvia plot w/ aesthetic binding-----------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class), aes.bind = TRUE) +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----flows plot w/ aesthetic binding-------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_flow(aes(fill = class), aes.bind = TRUE) +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----alluvia plot w/ manual lode ordering--------------------------------
lode_ord <- matrix(1:5, nrow = 5, ncol = 4)
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class), lode.ordering = lode_ord) +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----session info--------------------------------------------------------
sessioninfo::session_info()

