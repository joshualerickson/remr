


#' Get Transects
#'
#' @param linestring A data.frame of sf_LINESTRING
#' @param raster A raster of area
#' @param distance A \code{numeric} indicating distance between transects
#' @param length A \code{numeric} indicating distance per point per transect
#'
#' @return A sfc_POINT data.frame with elevations along the transect as well as the linestring.
#' @export
#' @note Be aware of your crs as this will affect the length and distance arguments.
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' line <- mapedit::drawFeatures() %>%
#' sf::st_transform(32612)
#' ele <- elevatr::get_elev_raster(line, z = 13)
#' terra::crs(ele) <- '+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs'
#'
#' rem <- get_transects(line, ele, distance = 100, length = 500)
#'
#' ele_crop <- terra::crop(ele, terra::vect(sf::st_buffer(line, 200)))
#' terra::plot(ele_crop)
#' plot(line$geometry, add = TRUE)
#' plot(rem$geometry, add = TRUE)
#' }
#'
get_transects <- function(linestring, raster, distance, length){

  line <- geos::as_geos_geometry(linestring)

  vertices <- wk::wk_vertices(line)

  edges <- geos::as_geos_geometry(
    wk::wk_linestring(
      vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
      feature_id = rep(seq_len(length(vertices) - 1), each = 2)
    )
  )

  edges_midpoint <- geos::geos_interpolate_normalized(edges, 0.5)


  points <- geos::geos_interpolate(line, seq(0, geos::geos_length(line), by = distance))
  which_edge <- geos::geos_nearest(points, edges)
  transects <- vctrs::vec_c(!!!Map(line_perpendicular_to_edge, points, which_edge, length = length, edges_midpoint, edges))

  points <- purrr::map(transects, ~geos::geos_interpolate(geom = ., distance = seq(0, geos::geos_length(.), by = distance)))

  sf_pt <- purrr::map(points, ~sf::st_as_sf(.))

  names(sf_pt) <- 1:length(sf_pt)

  for(i in 1:length(sf_pt)){
    sf_pt[[i]] <- sf_pt[[i]] %>%
      dplyr::mutate(group = as.numeric(names(sf_pt)[[i]]))
  }

  sf_pt <- sf_pt %>% plyr::rbind.fill() %>% sf::st_as_sf()

  #points adjacent to linestring
  elev_values <- terra::extract(raster, terra::vect(sf_pt))
  sf_pt$elevation_adj <- elev_values[,2]

  #points on linestring
  sf_pts_main <- purrr::map(points, ~sf::st_as_sf(.))

  sf_pts_main <- sf_pts_main %>%
    plyr::rbind.fill() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(group = dplyr::row_number())


  elev_main <- terra::extract(raster, terra::vect(sf_pts_main))

  sf_pts_main$elevation_main <- elev_main[,2]

  elev_final <- dplyr::left_join(sf_pt, sf_pts_main %>%
                            sf::st_drop_geometry(), by = 'group')


}


edge_transform_normal <- function(i, edges_midpoint, edges) {
  midpoint <- edges_midpoint[i]
  length <- geos::geos_length(edges[i])

  wk::wk_affine_compose(
    wk::wk_affine_translate(dx = -geos::geos_x(midpoint), dy = -geos::geos_y(midpoint)),
    wk::wk_affine_scale(1 / length, 1 / length),
    wk::wk_affine_rotate(90)
  )
}

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
