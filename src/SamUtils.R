make_sample <- function (x) { UseMethod('make_sample') }
make_sample.numeric <- function (x) { function (i) x[i] }
make_sample.matrix <- function (x) { function (i) x[i,] }
make_sample.data.frame <- function (x) { function (i) x[i,] }

sample_size <- function (x) { UseMethod('sample_size') }
sample_size.numeric <- function (x) { length(x) }
sample_size.matrix <- function (x) { nrow(x) }
sample_size.data.frame <- function (x) { nrow(x) }
