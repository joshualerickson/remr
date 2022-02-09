

test_that("Checking Point and Raster CRS", {

  pts = matrix(c(170800,172000, 5410500, 5410400), 2)
  line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts), crs = 32612))

  raster <- terra::rast()

  expect_error(expect_equal(sf::st_crs(line)[[2]], sf::st_crs(raster)[[2]]))

  raster <- terra::project(raster, sf::st_crs(line)[[1]])

  expect_equal(sf::st_crs(line)[[2]], sf::st_crs(raster)[[2]])


})


test_that("Checking Missing CRS", {

  pts = matrix(c(170800,172000, 5410500, 5410400), 2)
  line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts)))

  expect_equal(is.na(sf::st_crs(line)), TRUE)

  raster <- terra::rast()

  terra::crs(raster) <- ''

  expect_equal(nchar(terra::crs(raster))==0, TRUE)

  expect_equal(nchar(terra::crs(raster))==0, is.na(sf::st_crs(line)))

})

test_that("Change CRS between Raster and SF", {

  # check missing raster crs
  pts = matrix(c(170800,172000, 5410500, 5410400), 2)
  line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts)), crs = 32612)

  raster <- terra::rast()

  terra::crs(raster) <- ''

  expect_equal(nchar(terra::crs(raster))==0, TRUE)

  terra::crs(raster) <- sf::st_crs(line)[[1]]

  expect_equal(sf::st_crs(raster)[[2]], sf::st_crs(line)[[2]])

  # check missing line crs

  pts = matrix(c(170800,172000, 5410500, 5410400), 2)
  line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts)))

  raster <- terra::rast()

  raster <- terra::project(raster, 'EPSG:32612')

  expect_equal(is.na(sf::st_crs(line)), TRUE)

  line <- line %>% sf::st_set_crs(sf::st_crs(raster)[[2]])

  expect_equal(sf::st_crs(raster)[[2]], sf::st_crs(line)[[2]])

})





