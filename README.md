# coil20

*July 16 2026*: This project has been archived. All functionality has been rolled into the [snedata](https://github.com/jlmelville/snedata) repo.

Download helpers for the
[COIL-20](https://cave.cs.columbia.edu/repository/COIL-20) and
[COIL-100](https://cave.cs.columbia.edu/repository/COIL-100) object image
datasets.

<!-- badges: start -->
[![R-CMD-check](https://github.com/jlmelville/coil20/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jlmelville/coil20/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## COIL-20

COIL-20 contains 1,440 grayscale 128 x 128 images: 20 objects with 72 poses
each. `download_coil20()` returns one row per image. Pixel values are in the
range 0 to 1, labels are stored in `Label`, and row names use
`<object>_<pose>`.

## COIL-100

COIL-100 contains 7,200 RGB 128 x 128 images: 100 objects with 72 viewing
angles each. `download_coil100()` stores red, green, and blue channel values in
separate pixel columns. Row names use `<object>_<angle>`, where angle is in
degrees from 0 to 355.

## Install

```R
install.packages("pak")
pak::pak("jlmelville/coil20")
```

## Examples

```R
library(coil20)

coil20 <- download_coil20(verbose = TRUE)

show_object(coil20, object = 4, pose = 0)

# To avoid a very wide data frame, return a list containing a pixel matrix.
coil20_list <- download_coil20(as = "matrix")
coil20_pixels <- coil20_list$data

pca <- prcomp(coil20[, 1:(128 ^ 2)], retx = TRUE)
plot(pca$x[, 1:2], type = "n")
text(pca$x[, 1:2], labels = coil20$Label, cex = 0.5,
  col = rainbow(length(levels(coil20$Label)))[coil20$Label])

save(coil20, file = "coil20.Rda")

# Takes a long time to process all 7,200 images
coil100 <- download_coil100(verbose = TRUE)
```

## See also

* [snedata](https://github.com/jlmelville/snedata), a related package with more
  datasets for dimensionality reduction examples.

## License

[GPLv2 or later](https://www.gnu.org/licenses/gpl-2.0.txt).
