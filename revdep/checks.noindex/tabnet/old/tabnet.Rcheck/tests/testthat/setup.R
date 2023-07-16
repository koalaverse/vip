# Run before any test
suppressPackageStartupMessages(library(recipes))
suppressPackageStartupMessages(library(ggplot2))


# ames small data
data("ames", package = "modeldata")
ids <- sample(nrow(ames), 256)
small_ames <- ames[ids,]
x <- ames[ids,-which(names(ames) == "Sale_Price")]
y <- ames[ids,]$Sale_Price

# ames common models
ames_pretrain <- tabnet_pretrain(x, y, epoch = 2, checkpoint_epochs = 1)
ames_pretrain_vsplit <- tabnet_pretrain(x, y, epochs = 3, valid_split=.2,
                                        num_steps = 1, attention_width = 1, num_shared = 1, num_independent = 1)
ames_fit <- tabnet_fit(x, y, epochs = 5 , checkpoint_epochs = 2)
ames_fit_vsplit <- tabnet_fit(x, y, tabnet_model=ames_pretrain_vsplit, epochs = 3,
                              num_steps = 1, attention_width = 1, num_shared = 1, num_independent = 1)

# attrition small data
data("attrition", package = "modeldata")
ids <- sample(nrow(attrition), 256)

# attrition common models
attrix <- attrition[ids,-which(names(attrition) == "Attrition")]
attri_mult_x <- attrix[-which(names(attrix) == "JobSatisfaction")]

attriy <- attrition[ids,]$Attrition

attr_pretrained <- tabnet_pretrain(attrix, attriy, epochs = 12)
attr_pretrained_vsplit <- tabnet_pretrain(attrix, attriy, epochs = 12, valid_split=0.3)
attr_fitted <- tabnet_fit(attrix, attriy, epochs = 12)
attr_fitted_vsplit <- tabnet_fit(attrix, attriy, epochs = 12, valid_split=0.3)


# Run after all tests
withr::defer(teardown_env())
