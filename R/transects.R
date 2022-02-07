#' @title Get Transects
#'
#' @param linestring A data.frame of sf_LINESTRING
#' @param distance A \code{numeric} indicating distance between transects
#' @param length A \code{numeric} indicating the length of the transect
#'
#' @return A list with geos_geometry LINSTRINGS split by distance.
#' @export
#' @importFrom dplyr `%>%` bind_rows mutate left_join
#' @importFrom geos as_geos_geometry geos_interpolate_normalized geos_interpolate geos_length geos_nearest
#' @importFrom vctrs vec_c
#' @importFrom terra rast extract vect
#' @importFrom wk wk_vertices wk_linestring
#' @importFrom purrr map
#' @importFrom sf st_as_sf st_drop_geometry
#'
#' @examples
#' \dontrun{
#' pts = matrix(c(170800,172000, 5410500, 5410400), 2)
#' line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts), crs = 32612))
#'
#' transects <- get_transects(line, distance = 100, length = 500)
#'
#' ele_crop <- terra::crop(terra::rast(ele), terra::vect(sf::st_buffer(line, 200)))
#' terra::plot(ele_crop)
#' plot(transects)
#' }
#'
get_transects <- function(linestring, distance, length){


  transect <- get_pts_and_transects(linestring, distance, length)

  transect[['transects']]
}

#' @title Get Points along Transects
#'
#' @param linestring A data.frame of sf_LINESTRING
#' @param distance A \code{numeric} indicating distance between transects
#' @param length A \code{numeric} indicating the length of the transect
#'
#' @return A list of geos_geometry POINTS along transects
#' @export
#' @examples
#' \dontrun{
#' pts = matrix(c(170800,172000, 5410500, 5410400), 2)
#' line = sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts), crs = 32612))
#'
#' points <- get_transects(line, distance = 100, length = 500)
#'
#' ele_crop <- terra::crop(terra::rast(ele), terra::vect(sf::st_buffer(line, 200)))
#' terra::plot(ele_crop)
#' plot(points)
#' }
get_transect_points <- function(linestring, distance, length){

  pts_and_transects <- get_pts_and_transects(linestring, distance, length)

  points <- purrr::map(pts_and_transects[['transects']], ~geos::geos_interpolate(geom = ., distance = seq(0, geos::geos_length(.), by = distance)))

  sf_pt <- purrr::map(points, ~sf::st_as_sf(.))

  names(sf_pt) <- 1:length(sf_pt)

  for(i in 1:length(sf_pt)){
    sf_pt[[i]] <- sf_pt[[i]] %>%
      dplyr::mutate(transect = as.numeric(names(sf_pt)[[i]]))
  }

  sf_pt <- dplyr::bind_rows(sf_pt)


}


#' @title Get points and transects
#' @description set-up function
#' @param linestring A data.frame of sf_LINESTRING
#' @param distance A \code{numeric} indicating distance between transects
#' @param length A \code{numeric} indicating the length of the transect
#'
get_pts_and_transects <- function(linestring, distance, length){

  line <- geos::as_geos_geometry(linestring)

  vertices <- wk::wk_vertices(line)

  edges <- geos::as_geos_geometry(
    wk::wk_linestring(
      vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
      feature_id = rep(seq_len(length(vertices) - 1), each = 2)
    )
  )

  edges_midpoint <- geos::geos_interpolate_normalized(edges[[1]], 0.5)

  points     <- geos::geos_interpolate(line, seq(0, geos::geos_length(line), by = distance))
  which_edge <- geos::geos_nearest(points, edges[[1]])
  transects  <- vctrs::vec_c(!!!Map(line_perpendicular_to_edge, points, which_edge, length = length, edges_midpoint, edges[[1]]))

  pts_and_transects <- list(points, transects)

  names(pts_and_transects) <- c('points', 'transects')

  pts_and_transects
}

#' @title Edge Transform Normal
#' @param i iterator
#' @param edges_midpoint midpoint of edges
#' @param edges edges to process
#' @importFrom geos geos_length geos_x geos_y
#' @importFrom wk wk_affine_compose wk_affine_translate wk_affine_scale

edge_transform_normal <- function(i, edges_midpoint, edges) {
  midpoint <- edges_midpoint[i]
  length  <- geos::geos_length(edges[i])

  wk::wk_affine_compose(
    wk::wk_affine_translate(dx = -geos::geos_x(midpoint), dy = -geos::geos_y(midpoint)),
    wk::wk_affine_scale(1 / length, 1 / length),
    wk::wk_affine_rotate(90)
  )
}


#' @title Line Perpendicular to Edge
#' @param point sf POINT object
#' @param which_edge which edge of `edges` to process
#' @param length length of scale_x and scale_y in affline scale
#' @param edges_midpoint midpoint of edges
#' @param edges edges to process
#' @importFrom geos geos_x geos_y
#' @importFrom wk wk_transform wk_affine_compose wk_affine_scale wk_affine_translate

line_perpendicular_to_edge <- function(point, which_edge, length = 1, edges_midpoint, edges) {
  normalized_edge <- wk::wk_transform(edges[which_edge], edge_transform_normal(which_edge, edges_midpoint, edges))
  wk::wk_transform(
    normalized_edge,
    wk::wk_affine_compose(
      wk::wk_affine_scale(length, length),
      wk::wk_affine_translate(geos::geos_x(point), geos::geos_y(point))
    )
  )
}
