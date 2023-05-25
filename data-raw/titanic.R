library(mice)

# Read in the titanic data set
titanic <- read.csv("https://hbiostat.org/data/repo/titanic3.csv",
                    stringsAsFactors = TRUE)
keep <- c("survived", "pclass", "age", "sex", "sibsp", "parch")
titanic <- titanic[, keep]  # only retain key variables

# Recode response
titanic$survived <- as.factor(ifelse(titanic$survived == 1, "yes", "no"))

# Create a 'mids' (multiply imputed data set) object
set.seed(1125)  # for reproducibility
imp <- mice(titanic, method = "cart", m = 21, minbucket = 5,
            printFlag = FALSE)

titanic_mice <- complete(
  data = imp,      # 'mids' object (multiply imputed data set)
  action = "all",  # return list of all imputed data sets
  include = FALSE  # don't include original data (i.e., data with NAs)
)
length(titanic_mice)  # returns a list of completed data sets


# usethis::use_data(titanic, overwrite = TRUE)
# usethis::use_data(titanic_mice, overwrite = TRUE)
