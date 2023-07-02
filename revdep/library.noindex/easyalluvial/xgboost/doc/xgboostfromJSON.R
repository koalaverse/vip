## -----------------------------------------------------------------------------
require(xgboost)
require(jsonlite)
require(float)
options(digits=22)

## -----------------------------------------------------------------------------
dates <- c(20180130, 20180130, 20180130,
           20180130, 20180130, 20180130,
           20180131, 20180131, 20180131,
           20180131, 20180131, 20180131,
           20180131, 20180131, 20180131,
           20180134, 20180134, 20180134)

labels <- c(1, 1, 1,
            1, 1, 1,
            0, 0, 0,
            0, 0, 0,
            0, 0, 0,
            0, 0, 0)

data <- data.frame(dates = dates, labels=labels)

bst <- xgboost(
  data = as.matrix(data$dates), 
  label = labels,
  nthread = 2,
  nrounds = 1,
  objective = "binary:logistic",
  missing = NA,
  max_depth = 1
)

## -----------------------------------------------------------------------------
bst_json <- xgb.dump(bst, with_stats = FALSE, dump_format='json')
bst_from_json <- fromJSON(bst_json, simplifyDataFrame = FALSE)
node <- bst_from_json[[1]]
cat(bst_json)

## -----------------------------------------------------------------------------
bst_preds_logodds <- predict(bst,as.matrix(data$dates), outputmargin = TRUE)

# calculate the logodds values using the JSON representation
bst_from_json_logodds <- ifelse(data$dates<node$split_condition,
                                node$children[[1]]$leaf,
                                node$children[[2]]$leaf)

bst_preds_logodds
bst_from_json_logodds

# test that values are equal
bst_preds_logodds == bst_from_json_logodds


## -----------------------------------------------------------------------------
round(bst_preds_logodds,2) == round(bst_from_json_logodds,2)

## -----------------------------------------------------------------------------
# now convert the dates to floats first
bst_from_json_logodds <- ifelse(fl(data$dates)<node$split_condition,
                                node$children[[1]]$leaf,
                                node$children[[2]]$leaf)

# test that values are equal
round(bst_preds_logodds,2) == round(bst_from_json_logodds,2)

## -----------------------------------------------------------------------------
fl(20180131)

## -----------------------------------------------------------------------------
# test that values are equal
bst_preds_logodds == bst_from_json_logodds

## -----------------------------------------------------------------------------
# now convert the dates to floats first
bst_from_json_logodds <- ifelse(fl(data$dates)<fl(node$split_condition),
                                as.numeric(fl(node$children[[1]]$leaf)),
                                as.numeric(fl(node$children[[2]]$leaf)))

# test that values are equal
bst_preds_logodds == bst_from_json_logodds

## -----------------------------------------------------------------------------
bst_preds <- predict(bst,as.matrix(data$dates))

# calculate the predictions casting doubles to floats
bst_from_json_preds <- ifelse(fl(data$dates)<fl(node$split_condition),
                              as.numeric(1/(1+exp(-1*fl(node$children[[1]]$leaf)))),
                              as.numeric(1/(1+exp(-1*fl(node$children[[2]]$leaf))))
)

# test that values are equal
bst_preds == bst_from_json_preds

## -----------------------------------------------------------------------------
# calculate the predictions casting doubles to floats
bst_from_json_preds <- ifelse(fl(data$dates)<fl(node$split_condition),
                              as.numeric(fl(1)/(fl(1)+exp(fl(-1)*fl(node$children[[1]]$leaf)))),
                              as.numeric(fl(1)/(fl(1)+exp(fl(-1)*fl(node$children[[2]]$leaf))))
)

# test that values are equal
bst_preds == bst_from_json_preds

