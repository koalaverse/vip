## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 3, fig.align = "center")
library(ggalluvial)

## ----data---------------------------------------------------------------------
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

## ----plot---------------------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  geom_alluvium(aes(fill = class)) +
  geom_stratum()

## ----strata-------------------------------------------------------------------
# collection point and category variables only
data <- structure(toy[, 2:3], names = c("x", "stratum"))
# required fields for stat transformations
data$y <- 1
data$PANEL <- 1
# stratum transformation
StatStratum$compute_panel(data)

## ----strata plot--------------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category)) +
  stat_stratum() +
  stat_stratum(geom = "text", aes(label = category))

## ----strata reverse-----------------------------------------------------------
# stratum transformation with strata in original order
StatStratum$compute_panel(data, reverse = FALSE)
ggplot(toy, aes(x = collection, stratum = category)) +
  stat_stratum(reverse = FALSE) +
  stat_stratum(geom = "text", aes(label = category), reverse = FALSE)

## ----strata decreasing--------------------------------------------------------
# stratum transformation with strata in original order
StatStratum$compute_panel(data, reverse = FALSE)
ggplot(toy, aes(x = collection, stratum = category)) +
  stat_stratum(decreasing = TRUE) +
  stat_stratum(geom = "text", aes(label = category), decreasing = TRUE)

## ----alluvia------------------------------------------------------------------
# collection point, category, and subject variables
data <- structure(toy[, 1:3], names = c("alluvium", "x", "stratum"))
# required fields for stat transformations
data$y <- 1
data$PANEL <- 1
# alluvium transformation
StatAlluvium$compute_panel(data)

## ----alluvia plot-------------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class)) +
  stat_stratum(alpha = .25) +
  stat_alluvium(geom = "text", aes(label = subject))

## ----flows--------------------------------------------------------------------
# flow transformation
StatFlow$compute_panel(data)

## ----flows plot---------------------------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_stratum() +
  stat_flow(aes(fill = class)) +
  stat_flow(geom = "text",
            aes(label = subject, hjust = after_stat(flow) == "to"))

## ----lode zigzag--------------------------------------------------------------
for (i in 1:4) print(lode_zigzag(4, i))

## ----alluvia plot w/ backfront guidance---------------------------------------
for (i in 1:4) print(lode_backfront(4, i))
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class), lode.guidance = "backfront") +
  stat_stratum() +
  stat_alluvium(geom = "text", aes(label = subject),
                lode.guidance = "backfront")

## ----alluvia plot w/ backward guidance----------------------------------------
for (i in 1:4) print(lode_backward(4, i))
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class), lode.guidance = "backward") +
  stat_stratum() +
  stat_alluvium(geom = "text", aes(label = subject),
                lode.guidance = "backward")

## ----alluvia plot w/ strong aesthetic binding---------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class, label = subject), aes.bind = "alluvia") +
  stat_stratum() +
  stat_alluvium(geom = "text", aes(fill = class, label = subject),
                aes.bind = "alluvia")

## ----alluvia plot w/ weak aesthetic binding-----------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class, label = subject), aes.bind = "flows") +
  stat_stratum() +
  stat_alluvium(geom = "text", aes(fill = class, label = subject),
                aes.bind = "flows")

## ----flows plots w/ aesthetic binding-----------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_flow(aes(fill = class, label = subject), aes.bind = "flows") +
  stat_stratum() +
  stat_flow(geom = "text",
            aes(fill = class, label = subject,
                hjust = after_stat(flow) == "to"),
            aes.bind = "flows")

## ----alluvia plot w/ manual lode ordering-------------------------------------
lode_ord <- rep(seq(5), times = 4)
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_alluvium(aes(fill = class, order = lode_ord)) +
  stat_stratum() +
  stat_alluvium(geom = "text",
                aes(fill = class, order = lode_ord, label = subject))

## ----flows plot w/ manual lode ordering---------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject)) +
  stat_flow(aes(fill = class, order = lode_ord)) +
  stat_stratum() +
  stat_flow(geom = "text",
            aes(fill = class, order = lode_ord, label = subject,
                hjust = after_stat(flow) == "to"))

## ----bar plot with negative observations--------------------------------------
set.seed(78)
toy$sign <- sample(c(-1, 1), nrow(toy), replace = TRUE)
print(toy)
ggplot(toy, aes(x = collection, y = sign)) +
  geom_bar(aes(fill = class), stat = "identity")

## ----flows plot w/ negative strata--------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject,
                y = sign)) +
  geom_flow(aes(fill = class)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = category))

## ----alluvia plot w/ negative strata------------------------------------------
ggplot(toy, aes(x = collection, stratum = category, alluvium = subject,
                y = sign)) +
  geom_alluvium(aes(fill = class), absolute = FALSE) +
  geom_stratum(absolute = FALSE) +
  geom_text(stat = "alluvium", aes(label = subject), absolute = FALSE)

## ----session info-------------------------------------------------------------
sessioninfo::session_info()

