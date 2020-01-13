# # Grab cited packages from Rmd file
# lines <- readLines("rjournal/greenwell-boehmke.Rmd")
# z <- sapply(lines, FUN = function(x) {
#   stringi::stri_extract(x, pattern = "pkg\\{[:alnum:]*\\}", regex = TRUE)
# })
# z <- unname(sort(unique(z)))
# z <- gsub("^pkg\\{", replacement = "", x = z)
# z <- gsub("\\}$", replacement = "", x = z)

# Remove current bib files, if they exist
files <- c("rjournal/greenwell-boehmke.bib", "rjournal/packages.bib")
for (f in files) {
  if (file.exists(f)) {
    file.remove(f)
  }
}

# List of cited packages to include in the bibliography
pkgs <- c(
  "ALEPlot",
  "AmesHousing",
  "caret",
  "DALEX",
  "doParallel",
  "DT",
  "earth",
  "fastshap",
  "foreach",
  "gbm",
  "ggplot2",
  "glmnet",
  "iml",
  "ingredients",
  "kernlab",
  "microbenchmark",
  "mlr",
  "mmpf",
  "nnet",
  "party",
  "pdp",
  "plyr",
  "R6",
  "randomForestExplainer",
  "ranger",
  "rfVarImpOOB",
  "SuperLearner",
  "tibble",
  "tree.interpreter",
  "varImp",
  "vip",
  "vita",
  "vivo",
  "xgboost"
)

# Make sure the packages listed above are installed and up to date
required_pkgs <- setdiff(pkgs, installed.packages()[, "Package"])
install.packages(required_pkgs)

# Generate bibliography
knitr::write_bib(pkgs, file = "rjournal/greenwell-boehmke.bib", tweak = TRUE,
                 width = NULL, prefix = "R-")

# Create new bib file
file.append("rjournal/greenwell-boehmke.bib", "rjournal/general.bib")
