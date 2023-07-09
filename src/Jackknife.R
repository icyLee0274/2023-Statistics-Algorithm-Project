## Unils for handling different type of samples
source('SamUtils.R')
## Dependencies
require(tidyverse)

Jackknife.ci <- function (
  x, stat,
  level = 0.95,
  method = 'normal'
) {
  n_ <- sample_size(x)
  x_ <- make_sample(x)
  ii <- 1:n_

  alpha  <- 1 - level
  method <- method[1]

  theta.hat     <- stat(x)
  theta.jk.s    <- map_dbl(ii, ~stat(ii[-.]))
  theta.jk.mean <- mean(theta.jk.s)
  theta.jk      <- n_ * theta.hat - (n_ - 1) * theta.jk.mean

  ps.jk     <- map_dbl(ii, ~(n_ * theta.hat - (n_ - 1) * stat(ii[-.])))
  ps.jk.avg <- mean(ps.jk)

  if (method == 'normal') {
    z     <- qnorm(alpha / 2)
    jk.sd <- sd(theta.jk.s) / sqrt(n_)
    lower <- theta.jk + z * jk.sd
    upper <- theta.jk - z * jk.sd
  } else {
    stop('Unknown method: ', method)
  }

  list(est   = theta.jk,
       lower = lower,
       upper = upper)
}
