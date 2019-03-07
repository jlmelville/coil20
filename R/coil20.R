# Public Interface --------------------------------------------------------

#' Visualize COIL-20 or COIL-100 image.
#'
#' Display a COIL-20 or COIL-100 object pose.
#'
#' @param df Data frame containing the COIL-20 or COIL-100 dataframe.
#' @param object Index of the object to display. Should be an integer between
#' 1 and 20.
#' @param pose For COIL-20: the index of the pose to display. Should be an integer between
#' 0 and 71. For COIL-100: the angle of the pose. Should be an integer from 0
#' to 355, in five degree increments.
#' @examples
#' \dontrun{
#' # show the fourth pose of the fifth object of COIL-20
#' show_object(coil20, object = 5, pose = 4)
#' }
#' @export
show_object <- function(df, object, pose) {
  if (ncol(df) == 49153) {
    show_coil100(df, object, pose)
  }
  else {
    show_coil20(df, object, pose)
  }
}

#' Download COIL-20
#'
#' Download processed COIL-20 database of object images.
#'
#' Downloads and expands the zip file of the processed COIL-20 database of
#' object images and converts them to a data frame. There are 20 objects, each
#' represented by 72 poses, in a grayscale PNG format. Each image has a
#' resolution of 128 x 128.
#'
#' @format A data frame with 16385 variables:
#'
#' \describe{
#' \item{\code{x1_y1}, \code{x1_y2}, ... \code{x1_128}, \code{x2_y1} ...
#'   \code{x128_y128}}{Pixel values ranging from 0 to 1. The \code{x} index
#'   increases from left to right, and the \code{y} index increases from top to
#'   bottom, i.e. \code{x1_y1} is top left and \code{x128_y128} is the bottom
#'   right.}
#' \item{\code{Label}}{The id of the object represented by the image, in the
#'   range 1-20. Stored as a factor.}
#' }
#'
#' The name of each row in the data frame is in the form "<obj>_<pose>",
#' where <obj> is the id of the object (numbered from 1 to 20) and <pose>
#' is the pose id (numbered from 0 to 71). e.g. "5_4" is the fourth pose of the
#' fifth object.
#'
#' @seealso
#' For more information see
#'  \url{http://www.cs.columbia.edu/CAVE/software/softlib/coil-20.php}.
#'
#' @note
#' This function requires the \code{png} package
#' \url{https://cran.r-project.org/web/packages/png/} to be installed.
#' @param file name of file to download to. Defaults to \code{\link{tempfile}}.
#' @param cleanup if \code{TRUE}, then delete any downloaded files when
#'   finished.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @return Data frame containing the COIL-20 database.
#' @export
download_coil20 <- function(file = tempfile(), cleanup = TRUE,
                            verbose = FALSE) {
  base_coil_url <-
    "http://www.cs.columbia.edu/CAVE/databases/SLAM_coil-20_coil-100"

  coil_20_url <- paste(base_coil_url(), "coil-20", "coil-20-proc.zip",
                        sep = "/")
  coil_20 <- download_coil(url = coil_20_url, file = file, cleanup = cleanup,
                          verbose = verbose)
  colnames(coil_20)[1:(ncol(coil_20) - 1)] <- apply(
    rev(expand.grid(paste0("y", 1:128), paste0("x", 1:128))),
    1,
    paste, collapse = "_")
  coil_20
}

#' Download COIL-100
#'
#' Download processed COIL-100 database of object images.
#'
#' Downloads and expands the zip file of the processed COIL-100 database of
#' object images and converts them to a data frame. There are 100 objects, each
#' represented by 72 poses, in an RGB PNG format. Each image has a
#' resolution of 128 x 128.
#'
#' @format A data frame with 49153 variables (128 x 128 pixels for each
#' of the three channels):
#'
#' \describe{
#' \item{\code{r_x1_y1}, \code{r_x1_y2}, ... \code{r_x1_128}, \code{r_x2_y1} ...
#'   \code{r_x128_y128}}{Pixel values in the red channel ranging from 0 to 1.
#'   The \code{x} index increases from left to right, and the \code{y} index
#'   increases from top to bottom, i.e. \code{r_x1_y1} is top left and
#'   \code{r_x128_y128} is the bottom right.}
#' \item{\code{g_x1_y1}, \code{g_x1_y2}, ... \code{g_x1_128}, \code{g_x2_y1} ...
#'   \code{g_x128_y128}}{Pixel values in the green channel ranging from 0 to 1.
#'   The \code{x} index increases from left to right, and the \code{y} index
#'   increases from top to bottom, i.e. \code{g_x1_y1} is top left and
#'   \code{g_x128_y128} is the bottom right.}
#' \item{\code{b_x1_y1}, \code{b_x1_y2}, ... \code{b_x1_128}, \code{b_x2_y1} ...
#'   \code{b_x128_y128}}{Pixel values in the blue channel ranging from 0 to 1.
#'   The \code{x} index increases from left to right, and the \code{y} index
#'   increases from top to bottom, i.e. \code{b_x1_y1} is top left and
#'   \code{b_x128_y128} is the bottom right.}
#' \item{\code{Label}}{The id of the object represented by the image, in the
#'   range 1-100. Stored as a factor.}
#' }
#'
#' The name of each row in the data frame is in the form "<obj>_<angle>", where
#' <obj> is the id of the object (numbered from 1 to 20) and <angle> is the
#' viewing angle in degrees (numbered from 0 to 355). e.g. "5_150" is the fifth
#' object viewed at an angle of 150 degrees.
#'
#' @seealso
#' For more information see
#'  \url{http://www.cs.columbia.edu/CAVE/software/softlib/coil-100.php}.
#'
#' @note
#' This function requires the \code{png} package
#' \url{https://cran.r-project.org/web/packages/png/} to be installed.
#' @param file name of file to download to. Defaults to \code{\link{tempfile}}.
#' @param cleanup If \code{TRUE}, then delete any downloaded files when
#'   finished.
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @return Data frame containing the COIL-100 database.
#' @export
download_coil100 <- function(file = tempfile(), cleanup = TRUE,
                             verbose = FALSE) {
  coil_100_url <- paste(base_coil_url(), "coil-100", "coil-100.zip",
                        sep = "/")
  coil_100 <- download_coil(url = coil_100_url, file = file, cleanup = cleanup,
                            verbose = verbose)
  colnames(coil_100)[1:(ncol(coil_100) - 1)] <- apply(
    rev(expand.grid(
      paste0("y", 1:128),
      paste0("x", 1:128),
      c("r", "g", "b")
    )),
    1,
    paste, collapse = "_")
  coil_100
}

# File downloading/reading ------------------------------------------------

base_coil_url <- function() {
  "http://www.cs.columbia.edu/CAVE/databases/SLAM_coil-20_coil-100"
}

# download one of the COIL datasets and extract it to a data frame
download_coil <- function(url, file = tempfile(), cleanup = FALSE,
                          verbose = FALSE) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Library 'png' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!endsWith(file, ".zip")) {
    file <- paste0(file, ".zip")
  }
  if (verbose) {
    message("Downloading ", url, " to ", file)
    utils::flush.console()
  }
  utils::download.file(url, file, quiet = !verbose)
  df <- read_png_zip(file, cleanup = cleanup, verbose = verbose)

  if (cleanup) {
    if (verbose) {
      message("Deleting ", file)
    }
    unlink(file)
  }

  df
}

# Reads the zipped directory of PNG files, adding each one to a data frame
# and naming based on the file name.
read_png_zip <- function(zipfile, cleanup = FALSE, verbose = FALSE) {
  exdir <- dirname(zipfile)
  if (verbose) {
    message("Unzipping ", zipfile, " to ", exdir ,
            " (this can take a LONG time!)")
    utils::flush.console()
  }

  unzipped <- utils::unzip(zipfile, exdir = exdir)

  n <- length(unzipped)
  if (n > 0) {
    dir <- dirname(unzipped[1])
  }
  else {
    return(NULL)
  }

  df <- read_png_dir(dir = dir, verbose = verbose)

  if (cleanup) {
    if (verbose) {
      message("Deleting ", exdir)
    }
    unlink(exdir, recursive = TRUE)
  }

  df
}

read_png_dir <- function(dir, verbose = FALSE) {
  files <- list.files(dir, full.names = TRUE, pattern = "\\.png$")
  n <- length(files)

  df <- NULL
  labels <- vector(mode = "character", length = n)

  for (i in seq_along(files)) {
    file <- files[[i]]
    if (verbose) {
      message("Reading ", file, " (", i, " of ", n, ")")
      utils::flush.console()
    }

    # format is obj<num>__<pose>.png
    name_and_pose <- Filter(nchar,
                            strsplit(basename(file), "\\D+", perl = TRUE)[[1]])
    obj <- png::readPNG(file)
    npx <- prod(dim(obj))

    if (is.null(df)) {
      df <- data.frame(matrix(nrow = n, ncol = npx))
    }
    else {
      if (npx != ncol(df)) {
        stop("Images are of different sizes")
      }
    }
    df[i, ] <- as.vector(obj)
    labels[i] <- name_and_pose[1]
    row.names(df)[i] <- paste(name_and_pose, collapse = "_")
  }
  names(df) <- paste0("px", 1:ncol(df))
  df$Label <- as.factor(labels)
  df
}


# Images ------------------------------------------------------------------

show_coil100 <- function(df, object, pose) {
  name <- paste(object, pose, sep = "_")

  r <- as.matrix(df[name, 1:(ncol(df) - 1)])
  dim(r) <- c(128, 128, 3)
  img <- png::readPNG(png::writePNG(r))
  show_img(img)
}

show_coil20 <- function(df, object, pose) {
  name <- paste(object, pose, sep = "_")

  r <- as.matrix(df[name, 1:(ncol(df) - 1)])
  dim(r) <- c(128, 128)
  img <- png::readPNG(png::writePNG(r))
  show_img(img)
}

show_img <- function(img, x1 = 100, x2 = 250, y1 = 300, y2 = 450) {
  graphics::plot(c(x1, x2), c(y1, y2), type = "n", xlab = "", ylab = "",
                 axes = FALSE)
  graphics::rasterImage(img, x1, y1, x2, y2, interpolate = FALSE)
}
