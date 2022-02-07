

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

  if(nchar(terra::crs(raster))==0 && !is.na(sf::st_crs(linestring))) {
    warning("No CRS specified for raster; assuming it has the same CRS as the polygons.")
    terra::crs(raster) <- sf::st_crs(linestring)[[1]]

  } else if(is.na(sf::st_crs(linestring)) && nchar(terra::crs(raster))!=0) {
    warning("No CRS specified for line; assuming they have the same CRS as the raster.")
    linestring <- sf::st_set_crs(sf::st_crs(raster)[[2]])

  } else if(sf::st_crs(linestring)[[2]] != sf::st_crs(raster)[[2]]) {
    warning("Points transformed to raster CRS")
    linestring <- sf::st_set_crs(sf::st_crs(raster)[[2]])
  }


  sf_crs <- sf::st_crs(linestring)

  pts_and_transects <- get_pts_and_transects(linestring, distance, length)

  points <- purrr::map(pts_and_transects[['transects']], ~geos::geos_interpolate(geom = ., distance = seq(0, geos::geos_length(.), by = distance)))

  sf_pt <- purrr::map(points, ~sf::st_as_sf(.))

  names(sf_pt) <- 1:length(sf_pt)

  for(i in 1:length(sf_pt)){
    sf_pt[[i]] <- sf_pt[[i]] %>%
      dplyr::mutate(group = as.numeric(names(sf_pt)[[i]]))
  }

  sf_pt <- dplyr::bind_rows(sf_pt)

  #points adjacent to linestring
  elev_values <- terra::extract(raster, terra::vect(sf_pt))
  sf_pt$elevation_adj <- elev_values[,2]

  #points on linestring
  sf_pts_main <- purrr::map(points, ~sf::st_as_sf(.))

  sf_pts_main <-  dplyr::bind_rows(sf_pts_main) |>
    dplyr::mutate(group = dplyr::row_number())

  elevation_main <- terra::extract(raster, terra::vect(sf_pts_main))

  sf_pts_main$elevation_main <- elevation_main[,2]

  dplyr::left_join(sf_pt, sf::st_drop_geometry(sf_pts_main), by = 'group') |> sf::st_set_crs(sf_crs)


}


#' @title REM Raster
#'
#' @param rem A previously created \code{get_rem()} object
#' @param raster Raster used to generate \code{get_rem()}
#' @param fun A function to use in \code{focal()} for NA's
#' @param ... Additional arguments for \code{focal()}
#'
#' @return A terra raster object.
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
rem_raster <- function(rem, raster, fun = 'mean', ...){


  rem$rem <- rem$elevation_main-rem$elevation_adj

  rem_rast <- terra::rasterize(terra::vect(rem), terra::rast(raster), field = 'rem')

  rem_rast <- terra::focal(rem_rast, fun = fun, ...)


}
