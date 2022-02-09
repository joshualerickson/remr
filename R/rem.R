

#' @title Get Transects
#'
#' @param linestring A data.frame of sf_LINESTRING
#' @param raster A raster (digital elevation model (DEM))
#' @param distance A \code{numeric} indicating distance between transects
#' @param length A \code{numeric} indicating the length of the transect
#'
#' @return A sf_POINT data.frame with elevations along transects (\code{elevation_adj}) and trend line (\code{elevation_main}).
#' @note Be aware of your crs projection as this will affect the length and distance arguments.
#' @examples
#' \dontrun{
#' pts = matrix(c(170800,172000, 5410500, 5410400), 2)
#' line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts), crs = 32612))
#'
#' ele <- elevatr::get_elev_raster(line,
#'                                 z = 13,
#'                                 prj = '+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs')
#'
#' rem <- get_rem(line, ele, distance = 100, length = 500)
#'
#' ele_crop <- terra::crop(terra::rast(ele), terra::vect(sf::st_buffer(line, 200)))
#' terra::plot(ele_crop)
#' plot(rem$geometry)
#' }
#' @export
#'
get_rem <- function(linestring, raster, distance, length){

  if(!inherits(raster, "SpatRaster")){
    raster = terra::rast(raster)
  }

  # potential error catching for and changing crs but for now just stop function
  # if(nchar(terra::crs(raster))==0 && !is.na(sf::st_crs(linestring))) {
  #   stop("No CRS specified for raster")
  # } else if(is.na(sf::st_crs(linestring)) && nchar(terra::crs(raster))!=0) {
  #   stop("No CRS specified for line")
  # } else if(sf::st_crs(linestring)[[2]] != sf::st_crs(raster)[[2]]) {
  #   stop("Points and raster CRS are not equal")
  # }

  pts_and_transects <- get_pts_and_transects(linestring, distance, length)

  init_crs <- sf::st_crs(linestring)

  transect <- purrr::map(pts_and_transects, ~.x[['transects']])
  transect <- unlist(transect,recursive=FALSE)

  transect <- transect %>% purrr::map(~sf::st_as_sf(.)) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(group = dplyr::row_number()) %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(init_crs)

  class(transect) <- c("sf", "data.frame")

  sf_pt    <- transect %>%
    split(.$group) %>%
    purrr::map(~pts_on_transects(., distance))

  sf_pt    <- sf_pt %>%
    data.table::rbindlist() %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(init_crs)

  class(sf_pt) <- c("sf", "data.frame")

  #points adjacent to linestring
  elev_values <- terra::extract(raster, terra::vect(sf_pt))
  sf_pt$elevation_adj <- elev_values[,2]

  #points on linestring


  sf_pts_main <- purrr::map(pts_and_transects, ~.x[['points']])

  sf_pts_main <- unlist(sf_pts_main, recursive = FALSE)

  sf_pts_main <- purrr::map(sf_pts_main,~sf::st_as_sf(.)) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(group = dplyr::row_number()) %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(init_crs)

  class(sf_pts_main) <- c('sf', 'data.frame')

  elevation_main <- terra::extract(raster, terra::vect(sf_pts_main))

  sf_pts_main$elevation_main <- elevation_main[,2]

  dplyr::left_join(sf_pt, sf::st_drop_geometry(sf_pts_main), by = 'group') %>%
    sf::st_set_crs(init_crs)

}


#' @title REM Raster
#'
#' @param rem A previously created \code{get_rem()} object
#' @param raster Raster used to generate \code{get_rem()}
#' @param fun A function to use in \code{focal()} for NA's
#' @param window Window size for \code{focal()}
#' @param ... Additional arguments for \code{focal()}
#'
#' @return A terra raster object.
#' @note The raw output of 'get_rem()' contains holes when converting to a raster. Thus, a
#' focal function is used to clean up the holes.
#' @export
#'
#' @examples
#' \dontrun{
#' pts = matrix(c(170800,172000, 5410500, 5410400), 2)
#' line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts), crs = 32612))
#'
#' ele <- elevatr::get_elev_raster(line,
#'                                 z = 13,
#'                                 prj = '+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs')
#'
#' rem <- get_rem(line, ele, distance = 100, length = 500)
#'
#' rem_rast <- rem_raster(rem, ele, fun = 'mean', window = 3, na.rm = TRUE)
#'
#' }
#'

rem_raster <- function(rem, raster, fun = 'mean', window = 3, ...){

  rem$rem <- rem$elevation_main-rem$elevation_adj

  rem_rast <- terra::rasterize(terra::vect(rem), terra::rast(raster), field = 'rem')

  rem_rast <- terra::focal(rem_rast, fun = fun, ...)


}


