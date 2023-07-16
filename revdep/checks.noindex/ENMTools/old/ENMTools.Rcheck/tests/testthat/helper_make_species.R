# Comment from Russell: The iberolacerta files don't seem to be in the testdata folder,
# so tabling fixing this for now. It does have ahli and allogus. The testdata folder is
# way to big anyway, we will need to cut it down to the absolute minimum necessary.

# This code is intended to test the making of species objects from raw data.

# iberolacerta <- read.csv(system.file("testdata", "iberolacerta.points.csv", package="ENMTools"))
# ib.tree <- read.tree(system.file("testdata", "iberolacerta.brlens.tree", package="ENMTools"))
# data("euro.worldclim")
#
# expect_equal(class(ib.tree), "phylo")
# expect_equal(length(names(euro.worldclim)), 19)
# expect_true(inherits(euro.worldclim, "SpatRaster"))

#' Make enmtools.species objects
#'
#'

# make.species <- function(points, env, name){
#   this.species <- enmtools.species(presence.points = points,
#                                    species.name = name)
#   this.species <- check.species(this.species)
#   this.species$range <- background.raster.buffer(this.species$presence.points, 50000, mask = env)
#   return(this.species)
# }
# monticola <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta monticola",2:3],
#                           euro.worldclim, "monticola")
#
# martinezricai <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta martinezricai",2:3],
#                               euro.worldclim, "martinezricai")
#
# cyreni <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta cyreni",2:3],
#                        euro.worldclim, "cyreni")
#
# horvathi <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta horvathi",2:3],
#                          euro.worldclim, "horvathi")
#
# aurelioi <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta aurelioi",2:3],
#                          euro.worldclim, "aurelioi")
#
# aranica <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta aranica",2:3],
#                         euro.worldclim, "aranica")
#
# bonnali <- make.species(iberolacerta[iberolacerta$species == "Iberolacerta bonnali",2:3],
#                         euro.worldclim, "bonnali")
