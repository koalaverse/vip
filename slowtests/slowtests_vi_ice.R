# Load required packages
library(mlbench)
library(ranger)
library(vip)

# Simulate data from Friedman's paper
set.seed(101)
df1 <- as.data.frame(mlbench.friedman1(n = 500, sd = 1))


# Random forest ----------------------------------------------------------------

# Fit a ranfom forest
set.seed(102)
df1.rfo <- ranger(y ~ ., data = df1, importance = "impurity")

# Variable importance plots
p1 <- vip(df1.rfo)
p2 <- vip(df1.rfo, method = "ice")
p3 <- vip(df1.rfo, method = "pdp")
grid.arrange(p1, p2, p3, ncol = 3)

# Linear model with no interaction terms ---------------------------------------

# Fit a simple additive model
df1.lm <- lm(y ~ ., data = df1)

# Variable importance plots
p1 <- vip(df1.lm)
p2 <- vip(df1.lm, method = "ice")
p3 <- vip(df1.lm, method = "pdp")  # should be the same as ICE-based scores
grid.arrange(p1, p2, p3, ncol = 3)

