# Function to extract lon and lat from the geometry listcol
# From https://github.com/r-spatial/sf/issues/231#issuecomment-290817623

sfc_as_cols <- function(x, names = c("lon","lat")) {
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}