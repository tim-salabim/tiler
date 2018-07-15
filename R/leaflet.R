#' Add a tiled raster image to a leaflet map
#'
#' @description
#' Create tiled image data and add to a leaflet or mapview map. The image data
#' can be in the form of a character vector pointing to a file on disk such as
#' .png, .jpg, .bmp, .tif files or a RasterLayer from the \code{raster} package. \cr
#' \cr
#' \strong{NOTE:} \cr In case you want to save your map with tiled imagery using
#' \code{htmlwidgets::saveWidget} or \code{mapview::mapshot} make sure you set
#' \code{libdir = "lib"} as otherwise tiles won't render in your .html.
#'
#' @details
#' add non-projected/non-geographical tiled images to a non-projected
#' leaflet map for better performance. The map should be set up with
#' \code{leafletCRS(crsClass = "L.CRS.Simple")} - see example below. \cr
#' \cr
#' The use of these functions requires some system libraries to be installed.
#' First and foremost, GDAL needs to be available on your system. Furthermore,
#' you will need to have the \code{python-gdal} bindings. Installing GDAL
#' is pretty straight forward for most systems and chances are you have it already,
#' given that you were able to install \code{mapview}. The \code{gdal-python} bindings
#' require some extra work. \cr
#' \cr
#' If you're useing a debian \strong{linux} system \cr
#' \cr
#' \code{sudo apt-get install python-gdal} \cr
#' \cr
#' should be sufficient.\cr
#' \cr
#' On \strong{windows} (as usual) things are a more involved. \cr
#' The following describes how to get things set up to get these functions working
#' under windows:
#'
#' \enumerate{
#'   \item{
#'     Download and install Python from \url{https://www.python.org/ftp/python},
#'     e.g. python-2.7.9.amd64.msi for Python 2.7 64-bit (assumed in the following)
#'   }
#'   \item{
#'     From \url{http://www.gisinternals.com/release.php}, download and install \cr
#'       1. GDAL core, e.g. gdal-203-1911-x64-core.msi, \cr
#'       2. then GDAL Python bindings, e.g. GDAL-2.3.0.win-amd64-py2.7.msi
#'   }
#'   \item{
#'     Add required GDAL paths to system variables: \cr
#'       1. PATH: C:\\Program Files (x86)\\GDAL \cr
#'       2. GDAL_DATA: C:\\Program Files (x86)\\GDAL\\gdal-data \cr
#'       3. GDAL_DRIVER_PATH: C:\\Program Files (x86)\\GDAL\\gdalplugins \cr
#'   }
#'   \item{
#'     Download and install Microsoft Visual C++ Compiler for Python 2.7 from
#'     \url{https://www.microsoft.com/en-us/download/confirmation.aspx?id=44266}
#'     (required for installation of numpy package)
#'   }
#'   \item{
#'     Download and extract numpy (e.g. numpy-1.14.3.zip from
#'     \url{https://files.pythonhosted.org/packages/b0/2b/497c2bb7c660b2606d4a96e2035e92554429e139c6c71cdff67af66b58d2/numpy-1.14.3.zip}).
#'     On the command line, navigate to decompressed folder and run \cr
#'     \cr
#'       >>> python setup.py install
#'     \cr
#'   }
#'   \item{
#'     To verify, run the following import statements in the Python shell on the command line: \cr
#'       \cr
#'       >>> import gdal \cr
#'       >>> import ogr \cr
#'       >>> import osr \cr
#'       >>> import gdalnumeric \cr
#'       >>> import gdalconst \cr
#'   }
#' }
#'
#' See also \cr
#' \itemize{
#'   \item{\url{https://pypi.org/project/GDAL/}} and \cr
#'   \item{\url{https://sandbox.idre.ucla.edu/sandbox/tutorials/installing-gdal-for-windows}} \cr
#' }
#' for further details.
#'
#' On a \strong{mac} you're on your own for now (we don't have testing opportunities).
#' Though we would appreciate a PR if you feel confident you have a solution for
#' installing all needed dependencies/prerequisites.
#'
#' @param map a leaflet map
#' @param x for \code{addTiledRasterImage} a RasterLayer, for \code{addTiledImage}
#' a file path to the image to be tiled.
#' @param minzoom minimum zoom of the map
#' @param maxzoom maximum zoom of the map. This will be set as the maximum zoom
#' during tile generation. If missing, it is calculated based on image dimensions.
#' @param color the color palette to be used
#' @param at optionally specify breaks
#' @param na.color color for NA values
#'
#' @examples
#' \dontrun{
#' library(leaflet)
#' library(raster)
#' library(tiler)
#'
#' # generate a large RasterLayer
#' rst = raster(nrows = 5000, ncols = 10000,
#'              xmn = 0, xmx = 10000,
#'              ymn = -5000, ymx = 0)
#' rst[] = runif(ncell(rst))
#'
#' # generate leaflet map with simple crs
#' map = leaflet(
#'   options = leaflet::leafletOptions(
#'     crs = leafletCRS(
#'       crsClass = "L.CRS.Simple"
#'     )
#'   )
#' )
#'
#' options(viewer = NULL)
#'
#' map %>%
#'   addTiledRasterImage(rst)
#' }
#'
#' @export
#'
#' @name addTiledRasterImage
#' @rdname addTiledRasterImage
addTiledRasterImage = function(map,
                               x,
                               minzoom = 0,
                               maxzoom,
                               color = sp::bpy.colors,
                               at,
                               na.color = "#BEBEBE") {

  png_dst = tempfile(fileext = ".png")

  maxpixels = raster::ncell(x) + 1
  png <- raster2PNG(x,
                    col.regions = color,
                    at = at,
                    na.color = na.color)

  png::writePNG(png, png_dst)

  if (missing(maxzoom)) {
    mxzm = calcMaxZoom(png_dst)
  } else {
    mxzm = maxzoom
  }

  mnzm = minzoom
  width = raster::ncol(x)
  height = raster::nrow(x)

  tiles_dst = tempfile("tiles")
  # gdal2tiles(png_dst, tiles_dst, mnzm, mxzm)
  tile(png_dst,
       tiles = tiles_dst,
       zoom = paste(mnzm, mxzm, sep = "-"),
       viewer = FALSE)

  Sys.sleep(1)
  map$dependencies <- c(map$dependencies,
                        tiledDataDependency(tiles_dst),
                        rastercoordsDependency())
  urlTemplate = paste0("lib/", basename(tiles_dst), "-0.0.1/{z}/{x}/{y}.png")

  leaflet::invokeMethod(map, leaflet::getMapData(map), 'rastercoords',
                        width, height, mxzm, urlTemplate)
}


#' @export
#'
#' @name addTiledImage
#' @rdname addTiledRasterImage
#' @importFrom utils glob2rx
addTiledImage = function(map,
                         x,
                         minzoom = 0,
                         maxzoom) {

  # png_dst = tempfile(fileext = ".png")

  if (missing(maxzoom)) {
    mxzm = calcMaxZoom(x)
  } else {
    mxzm = maxzoom
  }

  mnzm = minzoom

  info = strsplit(
    sf::gdal_utils(
      util = "info",
      source = x,
      quiet = TRUE
    ),
    split = "\n"
  )
  info = unlist(lapply(info, function(i) grep(glob2rx("Size is*"), i, value = TRUE)))
  width = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[1]
  height = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[2]

  tiles_dst = tempfile("tiles")
  # gdal2tiles(x, tiles_dst, mnzm, mxzm)
  tile(x,
       tiles = tiles_dst,
       zoom = paste(mnzm, mxzm, sep = "-"),
       viewer = FALSE)

  Sys.sleep(1)
  map$dependencies <- c(map$dependencies,
                        tiledDataDependency(tiles_dst),
                        rastercoordsDependency())
  urlTemplate = paste0("lib/", basename(tiles_dst), "-0.0.1/{z}/{x}/{y}.png")

  leaflet::invokeMethod(map, leaflet::getMapData(map), 'rastercoords',
                        width, height, mxzm, urlTemplate)
}


### helpers
tiledDataDependency <- function(tiles_dir) {
  list(
    htmltools::htmlDependency(
      name = basename(tiles_dir),
      version = "0.0.1",
      src = c(file = tiles_dir)
    )
  )
}


rastercoordsDependency = function() {
  list(
    htmltools::htmlDependency(
      "rastercoords",
      '0.0.1',
      system.file("htmlwidgets/lib/leaflet-rastercoords", package = "mapview"),
      script = c("rastercoords.js", 'rastercoords-binding.js')
    )
  )
}


calcMaxZoom = function(x) {
  if (inherits(x, "Raster")) {
    cols = ncol(x)
    rows = nrow(x)
  } else {
    info = strsplit(
      sf::gdal_utils(
        util = "info",
        source = x,
        quiet = TRUE
      ),
      split = "\n"
    )

    info = unlist(lapply(info, function(i) grep(glob2rx("Size is*"), i, value = TRUE)))
    cols = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[1]
    rows = as.numeric(strsplit(gsub("Size is ", "", info), split = ", ")[[1]])[2]
  }
  dm = max(cols, rows)
  ceiling(log2(dm/256))
}


raster2PNG <- function(x,
                       col.regions,
                       at,
                       na.color) {

  mat <- t(raster::as.matrix(x))

  if (missing(at)) at <- lattice::do.breaks(range(mat, na.rm = TRUE), 256)

  cols <- lattice::level.colors(mat,
                                at = at,
                                col.regions = col.regions)
  cols[is.na(cols)] = na.color
  cols = col2Hex(cols, alpha = TRUE)
  #cols <- clrs(t(mat))
  png_dat <- as.raw(grDevices::col2rgb(cols, alpha = TRUE))
  dim(png_dat) <- c(4, ncol(x), nrow(x))

  return(png_dat)
}


col2Hex <- function(col, alpha = FALSE) {

  mat <- grDevices::col2rgb(col, alpha = TRUE)
  if (alpha) {
    hx <- grDevices::rgb(mat[1, ]/255, mat[2, ]/255,
                         mat[3, ]/255, mat[4, ]/255)
  } else {
    hx <- grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3, ]/255)
  }
  return(hx)

}
