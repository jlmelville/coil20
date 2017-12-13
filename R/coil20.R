#' Visualize COIL-20 image.
#'
#' Display a COIL-20 object pose.
#'
#' @param df Data frame containing COIL-20 objects.
#' @param object Index of the object to display. Should be an integer between
#' 1 and 20.
#' @param pose Index of the pose to display. Should be an integer between
#' 0 and 71.
#' @examples
#' \dontrun{
#' # show the fourth pose of the fifth digit
#' show_digit(coil20, object = 5, pose = 4)
#' }
#' @export
show_object <- function(df, object, pose) {
  name <- paste(object, pose, sep = "_")
  grid::grid.raster(matrix(as.numeric(df[name, 1:(128 ^ 2)]), nrow = 128))
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
#' \item{\code{px1}, \code{px2}, \code{px3} ... \code{px16384}}{Pixel values
#' ranging from 0 to 1.}
#' \item{\code{Label}}{The id of the object represented by the image, in the
#' range 1-20. Stored as a factor.}
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
#' @param verbose If \code{TRUE}, then download progress will be logged as a
#'   message.
#' @return Data frame containing the COIL-20 database.
#' @export
download_coil20 <- function(verbose = FALSE) {
  base_coil_url <-
    "http://www.cs.columbia.edu/CAVE/databases/SLAM_coil-20_coil-100"
  coil_20_url <- paste(base_coil_url, "coil-20", sep = "/")
  coil_20_proc_url <- paste(coil_20_url, "coil-20-proc.zip", sep = "/")

  temp <- tempfile()
  if (verbose) {
    message("Downloading ", coil_20_proc_url, " to ", temp)
    utils::flush.console()
  }
  utils::download.file(coil_20_proc_url, temp)
  df <- read_png_zip(temp, verbose = verbose)
  unlink(temp)
  df
}

# Reads the zipped directory of PNG files, adding each one to a data frame
# and naming based on the file name.
read_png_zip <- function(zipfile, verbose = FALSE) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("Library 'png' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (verbose) {
    message("Unzipping ", zipfile, " (this can take a LONG time!)")
    utils::flush.console()
  }
  unzipped <- utils::unzip(zipfile)
  n <- length(unzipped)
  df <- data.frame(matrix(nrow = n, ncol = 128 ^ 2))
  labels <- vector(mode = "character", length = n)
  for (i in 1:n) {
    file <- unzipped[i]
    if (verbose) {
      message("Reading ", file, " (", i, " of ", n, ")")
      utils::flush.console()
    }
    # format is obj<num>__<pose>.png
    name_and_pose <- Filter(nchar,
                            strsplit(basename(file), "\\D+", perl = TRUE)[[1]])
    obj <- png::readPNG(file)
    df[i, ] <- as.vector(obj)
    labels[i] <- name_and_pose[1]
    row.names(df)[i] <- paste(name_and_pose, collapse = "_")
    unlink(file)
  }
  names(df) <- paste0("px", 1:ncol(df))
  df$Label <- as.factor(labels)
  unlink("coil-20-proc", recursive = TRUE)
  df
}
