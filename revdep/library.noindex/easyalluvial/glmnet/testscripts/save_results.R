### Use this file to create a new set of test results
### Run it in the testthat directory, and then move the files across
verbose=TRUE
lines  <- readLines("../testthat.R")
testthat_lib  <- grep("library(testthat)", lines)
test_run  <- grep("test_check", lines)
## Comment out
lines[c(testthat_lib, test_run)]  <- paste0("##", lines[c(testthat_lib, test_run)])
out_dir  <- "new_saved_results"
dir.create(out_dir, showWarnings = FALSE)
expect_equal  <- expect_identical  <- expect_error  <- expect_warning  <- function(...) TRUE
test_files  <- list.files(".", pattern = ".R$", full.names = TRUE)
eval(parse(text = lines))

save_output  <- function(fname) {
    prefix  <- tools::file_path_sans_ext(basename(fname))
    e  <- new.env()
    source(fname, local = e)
    data_file  <- paste0(prefix, ".RDS")
    obj_names  <- names(e$expected)
    names(obj_names)  <- obj_names
    to_save  <- lapply(obj_names, get, pos = e)
    saveRDS(to_save, file.path(out_dir, data_file))
}

for (f in test_files) {
    if(verbose)cat("Running ",f,"\n")
    save_output(f)
}

