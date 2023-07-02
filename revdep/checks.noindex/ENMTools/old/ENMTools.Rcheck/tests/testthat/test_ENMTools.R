library(testthat)
library(ENMTools)

set.seed(2282023)

#data(iberolacerta.clade)
#data(euro.worldclim)

expect_species <- function(species){
  expect_true(inherits(species, c("list", "enmtools.species")))
  expect_equal(names(species), c("range", "presence.points", "background.points",
                                   "models", "species.name"))
  expect_true(inherits(species$range, "SpatRaster"))
  expect_true(inherits(species$presence.points, "SpatVector"))
  expect_true(inherits(species$species.name, "character"))
}


expect_enmtools_model <- function(model){
  expect_true(inherits(model, "enmtools.model"),
              info = "Not an enmtools.model object")

  expect_true(all(names(model) %in% c("species.name", "analysis.df", "test.data", "test.prop", "model",
                                      "training.evaluation", "test.evaluation", "env.training.evaluation",
                                      "env.test.evaluation", "rts.test",  "suitability", "clamping.strength", "call",  "notes", "response.plots",
                                      "formula")), info = "Unexpected items in enmtools.model object!")

  expect_true(inherits(model$species.name, "character"),
              info = "species.name is not a character")

  expect_true(inherits(model$analysis.df, "data.frame"),
              info = "analysis.df is not a data frame")

  expect_true(inherits(model$test.prop, "numeric"),
              info = "test.prop is not numeric")

  expect_true(all(class(model$model) %in% c("MaxEnt", "Domain", "Bioclim",
                                            "randomForest.formula", "randomForest",
                                            "list", "glm", "lm", "gam", "ranger")),
              info = "Class of model is not recognized")

  # Evaluation on training data happens unless it's bypassed (GLM only I think)
  expect_true(inherits(model$training.evaluation, "ModelEvaluation"))
  expect_true(inherits(model$env.training.evaluation, "ModelEvaluation"))

  # Evaluation on test data is for test.prop > 0 only
  if(model$test.prop > 0){
    expect_true(inherits(model$test.evaluation, "ModelEvaluation"),
                info = "Test proportion greater than 0 but test.evaluation is not a ModelEvaluation object")

    expect_true(inherits(model$env.test.evaluation, "ModelEvaluation"),
                info = "Test proportion greater than 0 but env.test.evaluation is not a ModelEvaluation object")

    expect_true(inherits(model$test.data, "SpatVector"),
                info = "Test proportion is greater than 0 but test.data is not a SpatVector")
  } else {
    expect_true(inherits(model$test.evaluation, "logical"))

    expect_true(inherits(model$test.data, "logical"))
  }

  expect_true(inherits(model$suitability, "SpatRaster"))

  expect_true(inherits(model$response.plots, "list"))

  expect_true(all(sapply(model$response.plots, class) %in% c("gg", "ggplot")))
}


monticola <- iberolacerta.clade$species$monticola
martinezricai <- iberolacerta.clade$species$martinezricai
cyreni <- iberolacerta.clade$species$cyreni
horvathi <- iberolacerta.clade$species$horvathi
aurelioi <- iberolacerta.clade$species$aurelioi
aranica <- iberolacerta.clade$species$aranica
bonnali <- iberolacerta.clade$species$bonnali

ib.tree <- iberolacerta.clade$tree

test_that("enmtools.species object work", {
  expect_species(monticola)
  expect_species(martinezricai)
  expect_species(cyreni)
  expect_species(horvathi)
  expect_species(aurelioi)
  expect_species(aranica)
  expect_species(bonnali)
})

#' Make an enmtools.clade object
#'
#'

iberolacerta.clade <- enmtools.clade(species = list(monticola = monticola,
                                                    martinezricai = martinezricai,
                                                    cyreni = cyreni,
                                                    horvathi = horvathi,
                                                    aurelioi = aurelioi,
                                                    aranica = aranica,
                                                    bonnali = bonnali), tree = ib.tree)

check.clade(iberolacerta.clade)

#' Build ENMs using various methods and test outputs
#'
#'


test_that("enmtools.model objects work for core methods", {

  cyreni.dm <- enmtools.dm(cyreni, euro.worldclim, test.prop = 0.2)
  cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0)

  expect_enmtools_model(cyreni.dm)
  expect_enmtools_model(cyreni.bc)


  cyreni.bc <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0.2)
  expect_enmtools_model(cyreni.bc)

  cyreni.glm <- enmtools.glm(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.glm)

  expect_enmtools_model(cyreni.glm)

  p.dm <- plot(cyreni.dm)
  expect_s3_class(p.dm, "ggplot")
  expect_output(print(cyreni.dm, plot = FALSE))

  p.bc <- plot(cyreni.bc)
  expect_s3_class(p.bc, "ggplot")
  expect_output(print(cyreni.bc, plot = FALSE))

  p.glm <- plot(cyreni.glm)
  expect_s3_class(p.glm, "ggplot")
  expect_output(print(cyreni.glm, plot = FALSE))

  # skip_on_ci()
  ## Generally slow tests
  skip_on_cran()
  cyreni.dm.rts1 <- enmtools.dm(cyreni, euro.worldclim, test.prop = 0.2, rts = 2)
  cyreni.bc.rts1 <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0.2, rts = 10)
  cyreni.dm.rts2 <- enmtools.dm(cyreni, euro.worldclim, test.prop = 0, rts = 2)
  cyreni.bc.rts2 <- enmtools.bc(cyreni, euro.worldclim, test.prop = 0, rts = 10)
  cyreni.glm.rts1 <- enmtools.glm(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2,
                                 rts = 10)
  cyreni.glm.rts2 <- enmtools.glm(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0,
                                 rts = 10)
  expect_enmtools_model(cyreni.dm.rts1)
  expect_enmtools_model(cyreni.bc.rts1)
  expect_enmtools_model(cyreni.dm.rts2)
  expect_enmtools_model(cyreni.bc.rts2)
  expect_enmtools_model(cyreni.glm.rts1)
  expect_enmtools_model(cyreni.glm.rts2)
  # cyreni.mx <- enmtools.maxent(cyreni, euro.worldclim, test.prop = 0.2)
  # expect_enmtools_model(cyreni.mx)
})

test_that("rf model objects work", {
  skip_if_not_installed("randomForest")
  expect_warning(cyreni.rf <- enmtools.rf(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2))
  expect_enmtools_model(cyreni.rf)
  p <- plot(cyreni.rf)
  expect_s3_class(p, "ggplot")
  expect_output(print(cyreni.rf, plot = FALSE))

  ## slow
  skip_on_cran()
  suppressWarnings(cyreni.rf.rts1 <- enmtools.rf(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2,
                                               rts.reps = 10))
  suppressWarnings(cyreni.rf.rts2 <- enmtools.rf(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0,
                                               rts.reps = 10))
  expect_enmtools_model(cyreni.rf.rts1)
  expect_enmtools_model(cyreni.rf.rts2)
})


test_that("ranger model objects work", {
  skip_if_not_installed("ranger")
  cyreni.rf.ranger <- enmtools.rf.ranger(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.rf.ranger)
  p <- plot(cyreni.rf.ranger)
  expect_s3_class(p, "ggplot")
  expect_output(print(cyreni.rf.ranger, plot = FALSE))

  ## slow
  skip_on_cran()
  suppressWarnings(cyreni.rf.ranger.rts1 <- enmtools.rf.ranger(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2,
                                               rts.reps = 10))
  suppressWarnings(cyreni.rf.ranger.rts2 <- enmtools.rf.ranger(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0,
                                               rts.reps = 10))
  expect_enmtools_model(cyreni.rf.ranger.rts1)
  expect_enmtools_model(cyreni.rf.ranger.rts2)
})

# test_that("ppm model objects work", {
#   skip_if_not_installed("ppmlasso")
#   cyreni.ppm <- enmtools.ppmlasso(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
#   expect_enmtools_model(cyreni.ppm)
# })

test_that("gam model objects work", {
  skip_if_not_installed("mgcv")
  cyreni.gam <- enmtools.gam(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2)
  expect_enmtools_model(cyreni.gam)
  p <- plot(cyreni.gam)
  expect_s3_class(p, "ggplot")
  expect_output(print(cyreni.gam, plot = FALSE))

  ## slow
  skip_on_cran()
  cyreni.gam.rts1 <- enmtools.gam(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0.2,
                                               rts.reps = 10)
  cyreni.gam.rts2 <- enmtools.gam(cyreni, euro.worldclim, f = pres ~ bio1 + bio9, test.prop = 0,
                                               rts.reps = 10)
  expect_enmtools_model(cyreni.gam.rts1)
  expect_enmtools_model(cyreni.gam.rts2)
})


#' Simple interactive.plot tests
test_that("interactive.plot produces correct object", {
  skip_if_not_installed("leaflet")
  skip_on_ci()
  cyreni.dm <- enmtools.dm(cyreni, euro.worldclim, test.prop = 0.2)
  m_dm <- interactive.plot(cyreni.dm)
  m_dm_cluster <- interactive.plot(cyreni.dm, cluster.points = TRUE)
  expect_s3_class(m_dm, "leaflet")
  expect_s3_class(m_dm_cluster, "leaflet")
  expect_match(sapply(m_dm_cluster$x$calls, function(x) x$method), "addRasterImage", all = FALSE)
  expect_match(sapply(m_dm$x$calls, function(x) x$method), "addRasterImage", all = FALSE)
})

test_that("backwards compatability works", {
  cyreni <- iberolacerta.clade$species$cyreni
  loaded <- load("sysdata.rda")
  on.exit(rm(list = loaded), add = TRUE, after = FALSE)
  expect_warning(cyreni.glm.raster <- enmtools.glm(cyreni,
                                                   euro.worldclim,
                                                   f = pres ~ bio1 + bio9,
                                                   test.prop = 0.2),
                 "env is not the expected SpatRaster class",
                 fixed = TRUE)
  expect_enmtools_model(cyreni.glm.raster)
  suppressWarnings(cyreni.glm.raster2 <- enmtools.glm(iberolacerta.clade$species$cyreni,
                                                   euro.worldclim,
                                                   f = pres ~ bio1 + bio9,
                                                   test.prop = 0.2))
  expect_enmtools_model(cyreni.glm.raster2)
})


#' Geographic space metrics and visualization
#'
#'



#' Env space metrics and visualization
#'
#'



#' Monte Carlo tests, ENMTools-style
#'
#'



#' Ecospat tests
#'
#'






