# coil20

coil20 is an R package to download the processed images from the
[COIL-20 database](http://www.cs.columbia.edu/CAVE/software/softlib/coil-20.php),
and [COIL-100 database](http://www.cs.columbia.edu/CAVE/software/softlib/coil-100.php)
using the [R png package](https://cran.r-project.org/web/packages/png/).

*December 7th 2018* This package has now gained the ability to download the
COIL-100 database. Plus, it's better about where it extracts the zip file and
cleaning up after itself.

## COIL-20

1440 128 x 128 grayscale images (20 objects with 72 poses each), with 16,384
features (the value of the 128 x 128 pixels). The pixel values (reals in the
range 0-1) are in columns with name `x1_y1`, `x1_y2`, `x1_y3` etc. The label
representing the object is in the `Label` column (which is stored as a factor)
and the row names store the object id (an integer between 1 and 20) and the pose
id (an integer between 0 and 71) which represents the viewing angle of the
object.

### COIL-100

100 128 x 128 color images (100 objects with 72 poses each) and 49,152 features
(the value of the 128 x 128 pixels in the red, green and blue channels). The
pixel values (reals in the range 0-1) are in columns with name `r_x1_y1`,
`r_x1_y2`, `r_x1_y3` etc. for the red channel, `g_x1_y1` and so on for the green
channel, and `b_x1_y1` for the blue channel. The label representing the object
is in the `Label` column (which is stored as a factor) and the row names store
the object id and the view angle in degrees (an integer between 0 and 355 in
increments of 5). This is slightly different to the naming scheme for COIL-20,
but follows the nomenclature of the COIL-20 and COIL-100 filenames.

## Installing

```R
# install.packages("devtools")
devtools::install_github("jlmelville/coil20")
library(coil20)
```

## Examples

```R
# fetch the data set from the COIL-20 website
# takes a little while so you might want some indication of what's going on
coil20 <- download_coil20(verbose = TRUE)

# view the zeroth pose of the fourth object
show_object(coil20, object = 4, pose = 0)

# Example of use: PCA
pca <- prcomp(coil20[, 1:128 ^ 2], retx = TRUE)
# plot the scores of the first two components
plot(pca$x[, 1:2], type = 'n')
text(pca$x[, 1:2], labels = coil20$Label, cex = 0.5,
  col = rainbow(length(levels(coil20$Label)))[coil20$Label])

# save data set to disk
save(coil20, file = "coil20.Rda")

# Fetch COIL-100
# Takes a long time to process all 7,200 images (a couple of hours)
coil100 <- download_coil100(verbose = TRUE)
```

## See also

* I have a similar R package if you would like [more datasets](https://github.com/jlmelville/snedata).

## License

[GPLv2 or later](https://www.gnu.org/licenses/gpl-2.0.txt).
