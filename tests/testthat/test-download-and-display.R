file_url <- function(path) {
  paste0("file://", normalizePath(path, winslash = "/", mustWork = TRUE))
}

zip_is_available <- function() {
  nzchar(Sys.which("zip"))
}

make_png_zip <- function(zipfile) {
  srcdir <- tempfile()
  dir.create(srcdir)
  png::writePNG(
    matrix(c(0, 0.25, 0.5, 1), nrow = 2),
    target = file.path(srcdir, "obj1__0.png")
  )

  oldwd <- setwd(srcdir)
  on.exit(setwd(oldwd), add = TRUE)
  utils::zip(zipfile, files = "obj1__0.png", flags = "-q")
}

test_that("download_coil cleans an explicit destination file", {
  skip_if_not(zip_is_available(), "zip command is not available")

  zipfile <- tempfile(fileext = ".zip")
  make_png_zip(zipfile)
  destfile <- tempfile()

  df <- coil20:::download_coil(
    url = file_url(zipfile),
    file = destfile,
    cleanup = TRUE,
    pixel_names = coil20:::coil20_pixel_names(width = 2, height = 2)
  )

  expect_s3_class(df, "data.frame")
  expect_false(file.exists(paste0(destfile, ".zip")))
})

test_that("show_object plots data frames and matrix-list results", {
  images <- matrix(seq(0, 1, length.out = 128 * 128), nrow = 1)
  df <- coil20:::format_coil_result(
    images,
    objects = 1L,
    poses = 0L,
    pixel_names = coil20:::coil20_pixel_names()
  )
  mat <- coil20:::format_coil_result(
    images,
    objects = 1L,
    poses = 0L,
    pixel_names = coil20:::coil20_pixel_names(),
    as = "matrix"
  )

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(show_object(df, object = 1, pose = 0))
  expect_silent(show_object(mat, object = 1, pose = 0))
  expect_error(show_object(df, object = 1, pose = 1), "No row")
})

test_that("show_object explains COIL-100 poses are angles", {
  images <- matrix(seq(0, 1, length.out = 128 * 128 * 3), nrow = 1)
  df <- coil20:::format_coil_result(
    images,
    objects = 1L,
    poses = 0L,
    pixel_names = coil20:::coil100_pixel_names()
  )

  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_silent(show_object(df, object = 1, pose = 0))
  expect_error(
    show_object(df, object = 1, pose = 1),
    "COIL-100 uses viewing angles"
  )
})
