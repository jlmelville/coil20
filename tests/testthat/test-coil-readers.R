write_gray_png <- function(path, values = c(0, 0.25, 0.5, 1), size = 2) {
  png::writePNG(matrix(values, nrow = size), target = path)
}

write_rgb_png <- function(path, values = seq(0, 1, length.out = 12)) {
  png::writePNG(array(values, dim = c(2, 2, 3)), target = path)
}

test_that("COIL filename parser reads object and pose ids", {
  parsed <- coil20:::parse_coil_filename("obj5__150.png")

  expect_equal(parsed$object, 5L)
  expect_equal(parsed$pose, 150L)
  expect_error(
    coil20:::parse_coil_filename("not-a-coil-file.png"),
    "must match"
  )
})

test_that("pixel name helpers match documented order", {
  expect_equal(
    coil20:::coil20_pixel_names(width = 2, height = 3),
    c("x1_y1", "x1_y2", "x1_y3", "x2_y1", "x2_y2", "x2_y3")
  )
  expect_equal(
    coil20:::coil100_pixel_names(width = 2, height = 2),
    c(
      "r_x1_y1",
      "r_x1_y2",
      "r_x2_y1",
      "r_x2_y2",
      "g_x1_y1",
      "g_x1_y2",
      "g_x2_y1",
      "g_x2_y2",
      "b_x1_y1",
      "b_x1_y2",
      "b_x2_y1",
      "b_x2_y2"
    )
  )
})

test_that("read_png_dir returns sorted data frames and matrix lists", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_gray_png(file.path(tmpdir, "obj2__1.png"), values = c(1, 0.5, 0.25, 0))
  write_gray_png(file.path(tmpdir, "obj1__0.png"), values = c(0, 0.25, 0.5, 1))

  df <- coil20:::read_png_dir(
    tmpdir,
    pixel_names = coil20:::coil20_pixel_names(width = 2, height = 2)
  )
  mat <- coil20:::read_png_dir(
    tmpdir,
    as = "matrix",
    pixel_names = coil20:::coil20_pixel_names(width = 2, height = 2)
  )

  expect_s3_class(df, "data.frame")
  expect_equal(rownames(df), c("1_0", "2_1"))
  expect_equal(names(df), c("x1_y1", "x1_y2", "x2_y1", "x2_y2", "Label"))
  expect_equal(as.character(df$Label), c("1", "2"))
  expect_named(mat, c("data", "labels", "poses", "ids"))
  expect_equal(dim(mat$data), c(2L, 4L))
  expect_equal(mat$ids, c("1_0", "2_1"))
  expect_equal(as.character(mat$labels), c("1", "2"))
})

test_that("RGB PNGs can be read into documented channel order", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  write_rgb_png(file.path(tmpdir, "obj1__0.png"))

  df <- coil20:::read_png_dir(
    tmpdir,
    pixel_names = coil20:::coil100_pixel_names(width = 2, height = 2)
  )

  expect_equal(
    names(df)[1:12],
    coil20:::coil100_pixel_names(width = 2, height = 2)
  )
  expect_equal(rownames(df), "1_0")
  expect_equal(as.character(df$Label), "1")
})

test_that("read_png_dir reports missing files and inconsistent dimensions", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  expect_error(coil20:::read_png_dir(tmpdir), "No PNG files")

  write_gray_png(file.path(tmpdir, "obj1__0.png"))
  write_gray_png(file.path(tmpdir, "obj1__1.png"), values = rep(0, 9), size = 3)

  expect_error(
    coil20:::read_png_dir(tmpdir),
    "Image dimensions differ"
  )
})

test_that("zip entry validation rejects traversal and absolute paths", {
  expect_silent(coil20:::validate_zip_entries(c("coil/obj1__0.png")))
  expect_error(coil20:::validate_zip_entries(c("../obj1__0.png")), "unsafe")
  expect_error(coil20:::validate_zip_entries(c("/tmp/obj1__0.png")), "unsafe")
  expect_error(coil20:::validate_zip_entries(c("C:/tmp/obj1__0.png")), "unsafe")
})
