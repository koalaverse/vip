################################################################################
# Setup
################################################################################

# Load required packages
library(caret)
library(dplyr)
library(ggplot2)
library(NeuralNetTools)
library(nnet)
library(pdp)
library(randomForest)
library(vip)

# Colors
set1 <- RColorBrewer::brewer.pal(9, "Set1")


################################################################################
# Introduction
################################################################################

data(boston, package = "pdp")
set.seed(101)
boston.rf <- randomForest(cmedv ~ ., data = boston, importance = TRUE)

imp <- importance(boston.rf) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  tidyr::gather(Method, Importance, -Variable)

pdf(file = "manuscript-methodology\\boston-rf-vip.pdf", width = 8, height = 5)
ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  facet_wrap(~ Method, scales = "free_x") +
  theme_bw()
dev.off()

p1 <- partial(boston.rf, pred.var = "lstat") %>%
  autoplot() +
  ylim(19.47764, 31.34696) +
  theme_light() +
  geom_hline(yintercept = mean(boston$cmedv), linetype = 2, col = set1[1L],
             alpha = 0.5)
p2 <- partial(boston.rf, pred.var = "rm") %>%
  autoplot() +
  ylim(19.47764, 31.34696) +
  theme_light() +
  geom_hline(yintercept = mean(boston$cmedv), linetype = 2, col = set1[1L],
             alpha = 0.5)
p3 <- partial(boston.rf, pred.var = "zn") %>%
  autoplot() +
  ylim(19.47764, 31.34696) +
  theme_light() +
  geom_hline(yintercept = mean(boston$cmedv), linetype = 2, col = set1[1L],
             alpha = 0.5)
pdf(file = "manuscript-methodology\\boston-rf-pdps.pdf", width = 12, height = 4)
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()


################################################################################
# Friedman 1
################################################################################

# Simulate the data
set.seed(101)  # for reproducibility
trn <- as.data.frame(mlbench::mlbench.friedman1(n = 500, sd = 1))
tibble::glimpse(trn)

# # Setup for k-fold cross-validation
# ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
# set.seed(103)
# trn.nn.tune <- train(
#   x = subset(trn, select = -y),
#   y = trn$y,
#   method = "nnet",
#   linout = TRUE,
#   maxit = 1000,
#   trControl = ctrl,
#   tuneGrid = expand.grid(size = 1:20, decay = c(0, 0.0001, 0.001, 0.01, 0.1))
# )
# plot(trn.nn.tune)
#    size decay     RMSE  Rsquared     RMSESD  RsquaredSD
# 39    8  0.01 1.205598 0.9443347 0.08825044 0.005865337

# Fit a neural network to the Firedman 1 data set
set.seed(103)
trn.nn <- nnet(y ~ ., data = trn, size = 8, linout = TRUE, decay = 0.01,
               maxit = 1000, trace = FALSE)

# Figure ?
pdf(file = "manuscript-methodology\\network.pdf", width = 12, height = 6)
plotnet(trn.nn)
dev.off()

# VIP: partial dependence algorithm
p1 <- vip(trn.nn, use.partial = TRUE, pred.var = paste0("x.", 1:10),
          color = "black", fill = set1[2L]) +
  theme_light() +
  ylab("Importance (partial dependence)")

# VIP: Garson's algorithm
trn.nn.garson <- garson(trn.nn, bar_plot = FALSE) %>%
  tibble::rownames_to_column("Variable") %>%
  select(Variable, Importance = rel_imp)
p2 <- ggplot(trn.nn.garson, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(color = "black", fill = set1[1L]) +
  xlab("") +
  ylab("Importance (Garson's algorithm)") +
  coord_flip() +
  theme_light()

# VIP: Olden's algorithm
trn.nn.olden <- olden(trn.nn, bar_plot = FALSE) %>%
  tibble::rownames_to_column("Variable") %>%
  select(Variable, Importance = importance)
p3 <- ggplot(trn.nn.olden, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(color = "black", fill = set1[1L]) +
  xlab("") +
  ylab("Importance (Olden's algorithm)") +
  coord_flip() +
  theme_light()

# Figure ?
pdf(file = "manuscript-methodology\\network-vip.pdf", width = 12, height = 6)
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()
