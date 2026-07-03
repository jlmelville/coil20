#' coil20: Download COIL object image datasets
#'
#' Helper functions to download the processed COIL-20 and COIL-100 object
#' image datasets and convert the PNG files into row-based R objects.
#'
"_PACKAGE"

coil_base_url <- "https://cave.cs.columbia.edu/old/databases/SLAM_coil-20_coil-100"

#' Visualize a COIL-20 or COIL-100 image
#'
#' Display a COIL-20 or COIL-100 object pose.
#'
#' @param df Data frame returned by [download_coil20()] or [download_coil100()].
#'   Matrix-list results returned with `as = "matrix"` are also supported.
#' @param object Object id to display. COIL-20 contains objects 1 to 20 and
#'   COIL-100 contains objects 1 to 100.
#' @param pose For COIL-20, the pose id to display, from 0 to 71. For COIL-100,
#'   the viewing angle, from 0 to 355 in five-degree increments.
#' @examples
#' \dontrun{
#' coil20 <- download_coil20()
#'
#' # show the fourth pose of the fifth object of COIL-20
#' show_object(coil20, object = 5, pose = 4)
#'
#' coil100 <- download_coil100()
#'
#' # COIL-100 uses viewing angles in degrees, in five-degree increments
#' show_object(coil100, object = 5, pose = 150)
#' }
#' @export
show_object <- function(df, object, pose) {
  features <- coil_feature_data(df)
  n_features <- ncol(features)
  name <- coil_row_name(object, pose)
  if (!(name %in% rownames(features))) {
    stop_missing_object_pose(name, n_features)
  }

  img_data <- as.numeric(unlist(
    features[name, , drop = FALSE],
    use.names = FALSE
  ))
  if (n_features == coil20_n_pixels()) {
    dim(img_data) <- c(128, 128)
  } else if (n_features == coil100_n_pixels()) {
    dim(img_data) <- c(128, 128, 3)
  } else {
    stop(
      "`df` must contain either ",
      coil20_n_pixels(),
      " or ",
      coil100_n_pixels(),
      " pixel columns",
      call. = FALSE
    )
  }

  show_img(img_data)
}

#' Download COIL-20
#'
#' Download the processed COIL-20 database of object images.
#'
#' Downloads and expands the zip file of the processed COIL-20 database of
#' object images and converts the PNG files to a data frame or matrix-list
#' result. There are 20 objects, each represented by 72 poses, in grayscale PNG
#' format. Each image has a resolution of 128 x 128.
#'
#' @format If `as = "data.frame"`, the default, a data frame with 16,385
#'   variables:
#'
#' * `x1_y1`, `x1_y2`, ... `x1_y128`, `x2_y1`, ... `x128_y128`: Pixel values
#'   ranging from 0 to 1. The `x` index increases from left to right, and the
#'   `y` index increases from top to bottom, so `x1_y1` is top left and
#'   `x128_y128` is bottom right.
#' * `Label`: The id of the object represented by the image, in the range 1 to
#'   20. Stored as a factor.
#'
#' The name of each row is `"<object>_<pose>"`, where `<object>` is the object
#' id and `<pose>` is the pose id, from 0 to 71. For example, `"5_4"` is the
#' fourth pose of the fifth object.
#'
#' For more information see <https://cave.cs.columbia.edu/repository/COIL-20>.
#'
#' @param file File path to download the zip archive to. If `NULL`, a file in a
#'   temporary work directory is used.
#' @param cleanup If `TRUE`, delete temporary download and extraction files
#'   before returning. If `file` is supplied, only that file and the dedicated
#'   extraction directory are removed.
#' @param verbose If `TRUE`, log download and extraction progress.
#' @param as Return format. Use `"data.frame"` for the original wide data frame
#'   shape, or `"matrix"` for a list with `data`, `labels`, `poses`, and `ids`.
#' @return If `as = "data.frame"`, a data frame containing the COIL-20 dataset.
#'   If `as = "matrix"`, a list containing a numeric matrix with one image per
#'   row, factor labels, integer poses, and row ids.
#' @export
download_coil20 <- function(
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  url <- coil_url("coil-20", "coil-20-proc.zip")
  download_coil(
    url = url,
    file = file,
    cleanup = cleanup,
    verbose = verbose,
    as = as,
    pixel_names = coil20_pixel_names()
  )
}

#' Download COIL-100
#'
#' Download the processed COIL-100 database of object images.
#'
#' Downloads and expands the zip file of the processed COIL-100 database of
#' object images and converts the PNG files to a data frame or matrix-list
#' result. There are 100 objects, each represented by 72 poses, in RGB PNG
#' format. Each image has a resolution of 128 x 128.
#'
#' @format If `as = "data.frame"`, the default, a data frame with 49,153
#'   variables:
#'
#' * `r_x1_y1`, `r_x1_y2`, ... `r_x128_y128`: Pixel values in the red channel.
#' * `g_x1_y1`, `g_x1_y2`, ... `g_x128_y128`: Pixel values in the green
#'   channel.
#' * `b_x1_y1`, `b_x1_y2`, ... `b_x128_y128`: Pixel values in the blue channel.
#' * `Label`: The id of the object represented by the image, in the range 1 to
#'   100. Stored as a factor.
#'
#' Pixel values range from 0 to 1. Within each channel, the `x` index increases
#' from left to right, and the `y` index increases from top to bottom.
#'
#' The name of each row is `"<object>_<angle>"`, where `<object>` is the object
#' id and `<angle>` is the viewing angle in degrees, from 0 to 355 in
#' five-degree increments. For example, `"5_150"` is the fifth object viewed at
#' an angle of 150 degrees.
#'
#' For more information see <https://cave.cs.columbia.edu/repository/COIL-100>.
#'
#' @param file File path to download the zip archive to. If `NULL`, a file in a
#'   temporary work directory is used.
#' @param cleanup If `TRUE`, delete temporary download and extraction files
#'   before returning. If `file` is supplied, only that file and the dedicated
#'   extraction directory are removed.
#' @param verbose If `TRUE`, log download and extraction progress.
#' @param as Return format. Use `"data.frame"` for the original wide data frame
#'   shape, or `"matrix"` for a list with `data`, `labels`, `poses`, and `ids`.
#' @return If `as = "data.frame"`, a data frame containing the COIL-100 dataset.
#'   If `as = "matrix"`, a list containing a numeric matrix with one image per
#'   row, factor labels, integer viewing angles, and row ids.
#' @export
download_coil100 <- function(
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  url <- coil_url("coil-100", "coil-100.zip")
  download_coil(
    url = url,
    file = file,
    cleanup = cleanup,
    verbose = verbose,
    as = as,
    pixel_names = coil100_pixel_names()
  )
}

coil_url <- function(...) {
  paste(coil_base_url, ..., sep = "/")
}

download_coil <- function(
  url,
  file = NULL,
  cleanup = TRUE,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  pixel_names = NULL
) {
  as <- match.arg(as)
  stop_if_not_installed("png")

  paths <- setup_coil_download_paths(url = url, file = file)
  if (cleanup) {
    on.exit(
      cleanup_owned_paths(paths$owned_paths, verbose = verbose),
      add = TRUE
    )
  }

  log_message(verbose, "Downloading ", url, " to ", paths$destfile)
  status <- utils::download.file(
    url,
    paths$destfile,
    quiet = !verbose,
    mode = "wb"
  )
  if (status != 0) {
    stop("Failed to download ", url, " with status ", status, call. = FALSE)
  }

  read_png_zip(
    paths$destfile,
    exdir = paths$exdir,
    verbose = verbose,
    as = as,
    pixel_names = pixel_names
  )
}

setup_coil_download_paths <- function(url, file = NULL) {
  if (is.null(file)) {
    workdir <- tempfile("coil-")
    dir.create(workdir)
    return(list(
      destfile = file.path(workdir, basename(url)),
      exdir = file.path(workdir, "extracted"),
      owned_paths = workdir
    ))
  }

  destfile <- add_zip_extension(file)
  exdir <- tempfile("coil-extract-", tmpdir = dirname(destfile))
  list(
    destfile = destfile,
    exdir = exdir,
    owned_paths = c(destfile, exdir)
  )
}

read_png_zip <- function(
  zipfile,
  exdir = tempfile("coil-unzip-"),
  cleanup = FALSE,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  pixel_names = NULL
) {
  as <- match.arg(as)
  if (cleanup) {
    on.exit(cleanup_owned_paths(exdir, verbose = verbose), add = TRUE)
  }

  dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
  entries <- zip_entries(zipfile)
  validate_zip_entries(entries)

  log_message(
    verbose,
    "Unzipping ",
    zipfile,
    " to ",
    exdir,
    " (this can take a long time)"
  )
  utils::unzip(zipfile, files = entries, exdir = exdir)

  read_png_dir(
    exdir,
    recursive = TRUE,
    verbose = verbose,
    as = as,
    pixel_names = pixel_names
  )
}

read_png_dir <- function(
  dir,
  recursive = FALSE,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  pixel_names = NULL
) {
  as <- match.arg(as)
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir, call. = FALSE)
  }

  files <- list.files(
    dir,
    full.names = TRUE,
    recursive = recursive,
    pattern = "\\.png$",
    ignore.case = TRUE
  )
  read_png_files(
    files,
    verbose = verbose,
    as = as,
    pixel_names = pixel_names
  )
}

read_png_files <- function(
  files,
  verbose = FALSE,
  as = c("data.frame", "matrix"),
  pixel_names = NULL
) {
  as <- match.arg(as)
  if (length(files) == 0) {
    stop("No PNG files found", call. = FALSE)
  }

  metadata <- coil_file_metadata(files)
  metadata <- metadata[order(metadata$object, metadata$pose), , drop = FALSE]
  files <- metadata$file

  first_img <- png::readPNG(files[1])
  n_pixels <- length(first_img)
  images <- matrix(0, nrow = length(files), ncol = n_pixels)

  for (i in seq_along(files)) {
    if (i %% 100 == 0) {
      log_message(verbose, "Reading file ", i, " of ", length(files))
    }

    img_data <- png::readPNG(files[i])
    if (length(img_data) != n_pixels) {
      stop(
        "Image dimensions differ from the first file. ",
        "All images must have the same shape.",
        call. = FALSE
      )
    }
    images[i, ] <- as.vector(img_data)
  }

  format_coil_result(
    images,
    objects = metadata$object,
    poses = metadata$pose,
    pixel_names = pixel_names,
    as = as
  )
}

format_coil_result <- function(
  images,
  objects,
  poses,
  pixel_names = NULL,
  as = c("data.frame", "matrix")
) {
  as <- match.arg(as)
  if (nrow(images) != length(objects) || length(objects) != length(poses)) {
    stop("Image, object, and pose counts must match", call. = FALSE)
  }

  if (is.null(pixel_names)) {
    pixel_names <- paste0("px", seq_len(ncol(images)))
  }
  if (length(pixel_names) != ncol(images)) {
    stop("Pixel name count must match image column count", call. = FALSE)
  }

  ids <- coil_row_name(objects, poses)
  labels <- factor(objects, levels = sort(unique(objects)))

  colnames(images) <- pixel_names
  rownames(images) <- ids

  if (as == "matrix") {
    return(list(
      data = images,
      labels = labels,
      poses = poses,
      ids = ids
    ))
  }

  df <- as.data.frame(images)
  df$Label <- labels
  rownames(df) <- ids
  df
}

coil_file_metadata <- function(files) {
  parsed <- lapply(files, parse_coil_filename)
  data.frame(
    file = files,
    object = vapply(parsed, `[[`, integer(1), "object"),
    pose = vapply(parsed, `[[`, integer(1), "pose"),
    stringsAsFactors = FALSE
  )
}

parse_coil_filename <- function(file) {
  match <- regmatches(
    basename(file),
    regexec("^obj([0-9]+)__([0-9]+)\\.png$", basename(file), ignore.case = TRUE)
  )[[1]]

  if (length(match) != 3) {
    stop(
      "PNG file names must match 'obj<object>__<pose>.png': ",
      basename(file),
      call. = FALSE
    )
  }

  list(
    object = as.integer(match[2]),
    pose = as.integer(match[3])
  )
}

zip_entries <- function(zipfile) {
  info <- utils::unzip(zipfile, list = TRUE)
  entries <- info$Name
  if (length(entries) == 0) {
    stop("Zip file contains no entries: ", zipfile, call. = FALSE)
  }
  entries
}

validate_zip_entries <- function(entries) {
  unsafe <- vapply(entries, is_unsafe_zip_entry, logical(1))
  if (any(unsafe)) {
    stop(
      "Zip file contains unsafe paths: ",
      paste(entries[unsafe], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(entries)
}

is_unsafe_zip_entry <- function(entry) {
  entry <- gsub("\\\\", "/", entry)
  if (entry == "" || grepl("^/", entry) || grepl("^[A-Za-z]:", entry)) {
    return(TRUE)
  }

  parts <- strsplit(entry, "/+", perl = TRUE)[[1]]
  any(parts == "..")
}

coil20_pixel_names <- function(width = 128, height = 128) {
  paste0(
    "x",
    rep(seq_len(width), each = height),
    "_y",
    rep(seq_len(height), times = width)
  )
}

coil100_pixel_names <- function(width = 128, height = 128) {
  n_channel_pixels <- width * height
  paste0(
    rep(c("r", "g", "b"), each = n_channel_pixels),
    "_x",
    rep(rep(seq_len(width), each = height), times = 3),
    "_y",
    rep(seq_len(height), times = width * 3)
  )
}

coil20_n_pixels <- function() {
  128 * 128
}

coil100_n_pixels <- function() {
  128 * 128 * 3
}

coil_row_name <- function(object, pose) {
  paste(object, pose, sep = "_")
}

coil_feature_data <- function(df) {
  if (is.list(df) && !is.null(df$data)) {
    data <- df$data
  } else if (is.matrix(df)) {
    data <- df
  } else if (is.data.frame(df)) {
    data <- df[, -ncol(df), drop = FALSE]
  } else {
    stop(
      "`df` must be a data frame, matrix, or matrix-list result",
      call. = FALSE
    )
  }

  if (is.null(rownames(data))) {
    stop("`df` must have row names in '<object>_<pose>' form", call. = FALSE)
  }
  data
}

stop_missing_object_pose <- function(name, n_features) {
  message <- paste0("No row with object_pose: ", name)
  if (n_features == coil100_n_pixels()) {
    message <- paste0(
      message,
      ". COIL-100 uses viewing angles for `pose`; valid values are ",
      "0, 5, 10, ..., 355."
    )
  } else if (n_features == coil20_n_pixels()) {
    message <- paste0(
      message,
      ". COIL-20 uses pose ids from 0 to 71."
    )
  }

  stop(message, call. = FALSE)
}

show_img <- function(img, x1 = 100, x2 = 250, y1 = 300, y2 = 450) {
  graphics::plot(
    c(x1, x2),
    c(y1, y2),
    type = "n",
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  graphics::rasterImage(img, x1, y1, x2, y2, interpolate = FALSE)
}

add_zip_extension <- function(file) {
  if (grepl("\\.zip$", file, ignore.case = TRUE)) {
    return(file)
  }
  paste0(file, ".zip")
}

cleanup_owned_paths <- function(paths, verbose = FALSE) {
  for (path in unique(paths)) {
    if (file.exists(path) || dir.exists(path)) {
      log_message(verbose, "Deleting ", path)
      unlink(path, recursive = TRUE)
    }
  }
}

stop_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Please install the '", pkg, "' package", call. = FALSE)
  }
}

log_message <- function(verbose, ...) {
  if (isTRUE(verbose)) {
    message(...)
    utils::flush.console()
  }
}
