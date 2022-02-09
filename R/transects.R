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

  init_crs <- sf::st_crs(linestring)

  transect <- purrr::map(transect, ~.x[['transects']])
  transect <- unlist(transect,recursive=FALSE)

  transect <- transect %>% purrr::map(~sf::st_as_sf(.)) %>%
    data.table::rbindlist() %>%
    dplyr::mutate(group = dplyr::row_number()) %>%
    sf::st_as_sf() %>%
    sf::st_set_crs(init_crs)

  class(transect) <- c("sf", "data.frame")

  transect
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
#'
get_transect_points <- function(linestring, distance, length){

  pts_and_transects <- get_transects(linestring, distance, length)

  init_crs <- sf::st_crs(linestring)

  sf_pt    <- pts_and_transects %>%
              split(.$group) %>%
              purrr::map(~pts_on_transects(., distance))

  sf_pt    <- sf_pt %>%
              data.table::rbindlist() %>%
              sf::st_as_sf() %>%
              sf::st_set_crs(init_crs)

  class(sf_pt) <- c("sf", "data.frame")

  sf_pt
}

#' @title Points on transects
#'
#' @param pts_and_transects created with get_pts_and_transects
#' @param distance A \code{numeric} indicating distance between transects
#'
#' @return Lists of sf_POINT data.frames
pts_on_transects <- function(pts_and_transects, distance){

    pts <- purrr::map(geos::as_geos_geometry(pts_and_transects), ~geos::geos_interpolate(geom = ., distance = seq(0, geos::geos_length(.), by = distance)))

    pts <- purrr::map(pts, ~sf::st_as_sf(.)) %>%
      data.table::rbindlist() %>%
      dplyr::mutate(group = pts_and_transects$group)

    sf_pt <- sf::st_as_sf(pts)
}

#' @title Get points on line
#' @description set-up function for getting the points on the linestring
#' where the transects cross
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

  pts_and_transects <- purrr::map(edges, ~ps_and_ts(., distance, length, line))

  pts_and_transects
}


#' ps and ts
#'
#' @param edges edges to process
#' @param distance A \code{numeric} indicating distance between transects
#' @param length A \code{numeric} indicating the length of the transect
#' @param line A linestring
#'
ps_and_ts <- function(edges, distance, length, line){

  points <- list()
  transects <- list()

  for(i in 1:length(edges)){
  edges_midpoint <- geos::geos_interpolate_normalized(edges[[i]], 0.5)
  pts     <- geos::geos_interpolate(edges[[i]], seq(0, geos::geos_length(line), by = distance))
  which_edge <- geos::geos_nearest(pts, edges[[i]])

  tran  <- vctrs::vec_c(!!!Map(line_perpendicular_to_edge, pts, which_edge, length = length, edges_midpoint, edges[[i]]))

  transects <- append(transects, list(tran))
  points <- append(points, list(pts))

  }

  pts_and_transects <- list('points' = points, 'transects' = transects)

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
