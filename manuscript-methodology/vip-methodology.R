################################################################################
# Setup
################################################################################

# Load required packages
library(caret)
library(dplyr)
library(ggplot2)
library(mlbench)
library(NeuralNetTools)
library(nnet)
library(pdp)
library(randomForest)
library(vip)

# Colors
set1 <- RColorBrewer::brewer.pal(9, "Set1")

# Simulate data from the regression problems described in Friedman (1991) and
# Breiman (1996)
set.seed(3101)
trn1 <- as.data.frame(mlbench.friedman1(500))
trn2 <- as.data.frame(mlbench.friedman2(500))
trn3 <- as.data.frame(mlbench.friedman3(500))

# Load the (corrected) Boston housing data
data(boston, package = "pdp")

# Load concrete data
conc <- read.csv("manuscript-methodology//Concrete_Data.csv", header = TRUE)


################################################################################
# Random forest analysis of the Boston housing data
################################################################################

# Fit a random forest to the Boston Housing data (mtry was tuned using cross-
# validation)
set.seed(101)
boston.rf <- randomForest(cmedv ~ ., data = boston, mtry = 6, ntree = 1000,
                          importance = TRUE)

# Figure ?
imp1 <- importance(boston.rf, type = 1) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")
imp2 <- importance(boston.rf, type = 2) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")
p1 <- ggplot(imp1, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_light()
p2 <- ggplot(imp2, aes(x = reorder(Variable, IncNodePurity), y = IncNodePurity)) +
  geom_col() +
  coord_flip() +
  xlab("") +
  theme_light()
pdf(file = "manuscript-methodology\\boston-rf-vip.pdf", width = 8, height = 5)
grid.arrange(p1, p2, ncol = 2)
dev.off()


# Partial dependence plots for lstat, rm, and zn
pd1 <- partial(boston.rf, pred.var = "lstat")
pd2 <- partial(boston.rf, pred.var = "rm")
pd3 <- partial(boston.rf, pred.var = "zn")
pd.range <- range(c(pd1$yhat, pd2$yhat, pd3$yhat))
p1 <- autoplot(pd1) +
  ylim(pd.range[1L], pd.range[2L]) +
  theme_light() +
  geom_hline(yintercept = mean(boston$cmedv), linetype = 2, col = set1[1L],
             alpha = 0.5)
p2 <- autoplot(pd2) +
  ylim(pd.range[1L], pd.range[2L]) +
  theme_light() +
  geom_hline(yintercept = mean(boston$cmedv), linetype = 2, col = set1[1L],
             alpha = 0.5)
p3 <- autoplot(pd3) +
  ylim(pd.range[1L], pd.range[2L]) +
  theme_light() +
  geom_hline(yintercept = mean(boston$cmedv), linetype = 2, col = set1[1L],
             alpha = 0.5)

# Figure ?
pdf(file = "manuscript-methodology\\boston-rf-pdps.pdf", width = 12, height = 4)
grid.arrange(p1, p2, p3, ncol = 3)
dev.off()

# Variable importance scores (partial dependence)
boston.rf.vi <- vi(boston.rf, pred.var = names(subset(boston, select = -cmedv)))
p <- ggplot(boston.rf.vi, aes(x = reorder(Variable, -Importance), y = Importance)) +
  geom_col() +
  xlab("")

# Variable importance plots
p1 <- vip(boston.rf, pred.var = names(subset(boston, select = -cmedv)), FUN = sd)
p2 <- vip(boston.rf, pred.var = names(subset(boston, select = -cmedv)), FUN = mad)

# Figure ?
pdf(file = "manuscript-methodology\\boston-rf-vip-pd.pdf", width = 7, height = 4)
print(p)
dev.off()


################################################################################
#
################################################################################

ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
set.seed(4578)
boston.nn.tune <- train(
  x = subset(boston, select = -cmedv),
  y = boston$cmedv,
  method = "nnet",
  linout = TRUE,
  maxit = 1000,
  trControl = ctrl,
  tuneGrid = expand.grid(size = 1:20, decay = c(0, 0.0001, 0.001, 0.01, 0.1))
)
plot(boston.nn.tune)

set.seed
X <- subset(boston, select = -cmedv)
X$chas <- as.numeric(X$chas)
boston.svm.tune <- train(
  x = X,
  y = boston$cmedv,
  method = "svmRadialCost",
  preProc = c("center", "scale"),
  metric = "Rsquared",
  trControl = ctrl,
  tuneGrid = data.frame("C" = seq(from = 0.25, to = 100, length = 100))
)
plot(boston.svm.tune)

set.seed(301)
fit <- nnet(cmedv ~ ., data = boston, size = 15, decay = 0.1, linout = TRUE,
            maxit = 10000)
vip(fit, pred.var = names(subset(boston, select = -cmedv)), quantiles = TRUE, probs = 10:90/100)
vip(fit, pred.var = names(subset(boston, select = -cmedv)), FUN = var)
vip(fit, pred.var = names(subset(boston, select = -cmedv)), FUN = IQR)

fit <- boston.rf
pred.var <- names(subset(boston, select = -cmedv))
vip(fit, pred.var = pred.var, FUN = function(x) {
  # max(abs(x - mean(boston$cmedv)))
  sqrt(mean((x - mean(boston$cmedv)) ^ 2))
})


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

vip(trn.nn, pred.var = paste0("x.", 1:10), FUN = var)
vip(trn.nn, pred.var = paste0("x.", 1:10), FUN = mad)

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


vint <- function(x) {
  pd <- partial(trn.nn, pred.var = c(x[1L], x[2L]))
  c(sd(tapply(pd$yhat, INDEX = pd[[x[1L]]], FUN = sd)),
    sd(tapply(pd$yhat, INDEX = pd[[x[2L]]], FUN = sd)))
}
combns <- combn(paste0("x.", 1:10), m = 2)
res <- plyr::aaply(combns, .margins = 2, .fun = vint, .progress = "text")
plot(rowMeans(res), type = "h")

d <- data.frame(x = paste0(combns[1L, ], "*", combns[2L, ]), y = rowMeans(res))

pdf(file = "manuscript-methodology\\network-int.pdf", width = 8, height = 4)
ggplot(d, aes(reorder(x, -y), y)) +
  geom_col() +
  xlab("") +
  ylab("Interaction") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1)) +
  theme_light()
dev.off()


################################################################################
#
################################################################################



set.seed(103)
rf3 <- randomForest(y ~ ., data = trn3, ntree = 500, importance = TRUE)
plot(rf3)

set.seed(102)
rf2 <- randomForest(y ~ ., data = trn2, ntree = 500, importance = TRUE)
plot(rf2)


combns <- combn(paste0("x.", 1:4), m = 2)
res <- plyr::aaply(combns, .margins = 2, .progress = "text", .fun = function(x) {
  pd <- partial(rf2, pred.var = c(x[1L], x[2L]))
  c(sd(tapply(pd$yhat, INDEX = pd[[x[1L]]], FUN = sd)),
    sd(tapply(pd$yhat, INDEX = pd[[x[2L]]], FUN = sd)))
})
plot(rowMeans(res), type = "h", ylim = c(0, 110))
text(1:6, rowMeans(res), labels = paste0(combns[1L, ], "*", combns[2L, ]),
     pos = 3)


################################################################################
#
################################################################################

ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
tune.grid <- expand.grid(neighbors = 0:9, committees = 1:20)
set.seed(1001)
boston.cubist <- train(
  x = subset(boston, select = -cmedv),
  y = boston$cmedv,
  method = "cubist",
  metric = "Rsquared",
  trControl = ctrl,
  tuneGrid = tune.grid
)
plot(boston.cubist)

# Variable importance plots
vip(boston.cubist, pred.var = names(subset(boston, select = -cmedv)),
    progress = "text")
plot(varImp(boston.cubist))


################################################################################
# Example: concrete compressive strength
################################################################################

# FIt a cubist model
X <- subset(conc, select = -Concrete.compressive.strength)
y <- conc$Concrete.compressive.strength
set.seed(1141)
conc.cubist.tune <- train(
  x = X,
  y = y,
  method = "cubist",
  metric = "Rsquared",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  tuneGrid = expand.grid(neighbors = 0:9, committees = 1:20)
)
conc.rf.tune <- train(
  x = X,
  y = y,
  method = "rf",
  importance = TRUE,
  metric = "Rsquared",
  trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE),
  tuneGrid = expand.grid(mtry = 1:8)
)
grid.arrange(plot(conc.cubist.tune), plot(conc.rf.tune), ncol = 2)
p1 <- vip(conc.cubist.tune, pred.var = names(X))
p2 <- vip(conc.rf.tune, pred.var = names(X))
grid.arrange(p1, p2, ncol = 2)
grid.arrange(p1, p2, plot(varImp(conc.cubist.tune)), plot(varImp(conc.rf.tune)),
             ncol = 2)
varImpPlot(conc.rf.tune$finalModel)

