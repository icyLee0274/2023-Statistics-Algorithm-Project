## Unils for handling different type of samples
source('SamUtils.R')
## Dependencies
require(tidyverse)

available.Jackknife.methods <- c('normal', 'student')

Jackknife.ci <- function (
  x, stat,
  level = 0.95,
  method = available.Jackknife.methods
) {
  n_ <- sample_size(x)
  x_ <- make_sample(x)
  ii <- 1:n_

  alpha  <- 1 - level
  method <- method[1]

  theta.hat  <- stat(x)
  theta.jk.s <- map_dbl(ii, ~stat(x_(ii[-.])))
  ps.jk      <- n_ * theta.hat - (n_ - 1) * theta.jk.s
  ps.jk.avg  <- mean(ps.jk)

  if (method == 'normal') {
    z     <- qnorm(alpha / 2)
    jk.sd <- sd(ps.jk) / sqrt(n_)
    lower <- ps.jk.avg + z * jk.sd
    upper <- ps.jk.avg - z * jk.sd
  } else if (method == 'student') {
    t     <- qt(alpha / 2, df = n_ - 1)
    jk.sd <- sd(ps.jk) / sqrt(n_)
    lower <- ps.jk.avg + t * jk.sd
    upper <- ps.jk.avg - t * jk.sd
  } else {
    stop('Unknown method: ', method)
  }

  list(est   = ps.jk.avg,
       lower = lower,
       upper = upper)
}

