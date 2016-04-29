# coil20

coil20 is an R package to download the processed images from the
[COIL-20 database](http://www.cs.columbia.edu/CAVE/software/softlib/coil-20.php),
using the [R png package](https://cran.r-project.org/web/packages/png/)

The entire dataset is returned as a single data frame of 1440 observations 
(20 objects with 72 poses each), and 16384 variables (The value of the 128 x 128 
pixels) The pixel values (reals in the range 0-1) are in columns with name 
`px1`, `px2`, `px3` etc. The label representing the object is in the `Label` 
column (which is stored as a factor) and the row names store the object id and 
the pose id.

Installing:
```R
# install.packages("devtools")
devtools::install_github("jlmelville/coil20")
```

Using:
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
```

## License
[GPLv2 or later](https://www.gnu.org/licenses/gpl-2.0.txt).
