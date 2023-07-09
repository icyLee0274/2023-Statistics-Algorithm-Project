## Unils for handling different type of samples
source('SamUtils.R')
## Dependencies
require(tidyverse)


available.bootstrap.methods <- c('normal', 'basic', 'percentile')

#######################################################
# Bootstrap resampling mathod                         #
#                                                     #
# x      - Samples, vector, matrix or data.frame      #
# stat   - Estimator to use, accepts the same type    #
#          of arguments as `x', and produces a number #
# size   - Resampling size                            #
# method - Method used to compute confidence interval #
#                                                     #
# @return - List of entries: est, lower and upper     #
#######################################################
bootstrap.ci <-
  function (
    x,
    stat,
    size = 100,
    level = 0.95,
    method = available.bootstrap.methods
  )
  {
    alpha <- 1 - level
    stopifnot(between(alpha, 0, 1))

    x_ <- make_sample(x)
    n_ <- sample_size(x)

    bs.x <- map_dbl(seq_len(size),
                    function (i) {
                      ii <- sample(n_, n_, TRUE)
                      stat(x_(ii))
                    })

    method <- method[1]
    if (method == 'normal') {
      bs.est   <- stat(x)
      bs.sd    <- sd(bs.x)
      z        <- qnorm(alpha / 2)
      bs.hlen  <- z * bs.sd
      bs.lower <- bs.est + bs.hlen
      bs.upper <- bs.est - bs.hlen
    } else if (method == 'basic') {
      bs.est   <- stat(x)
      qbsx     <- quantile(bs.x,
                           probs = c(alpha / 2,
                                     1 - alpha / 2),
                           names = FALSE)
      bs.lower <- 2 * bs.est - qbsx[2]
      bs.upper <- 2 * bs.est - qbsx[1]
    } else if (method == 'percentile') {
      bs.est   <- mean(bs.x)
      qbsx     <- quantile(bs.x,
                           probs = c(alpha / 2,
                                     1 - alpha / 2),
                           names = FALSE)
      bs.lower <- qbsx[1]
      bs.upper <- qbsx[2]
    } else {
      stop('Unknown method: ', method)
    }
    list(est   = bs.est,
         lower = bs.lower,
         upper = bs.upper)
  }


bootstrap.coverage <-
  function (r.fun, stat, par.true,
            n.sample = 100,
            n.mc = 100,
            n.bs = 100,
            levels = c(.90, .95, .99),
            methods = available.bootstrap.methods) {
    r.fun <- rlang::as_function(r.fun)

    pmap_dfr(
      expand_grid(level = levels, method = methods),
      function (level, method) {
        par.inside <- double(n.mc)
        lower.miss <- double(n.mc)
        upper.miss <- double(n.mc)
        walk(1:n.mc, function (ii) {
          x              <- r.fun(n.sample)
          ci             <- bootstrap.ci(x, stat, n.bs, level, method)
          par.inside[ii] <<- between(par.true, ci$lower, ci$upper)
          lower.miss[ii] <<- ci$lower > par.true
          upper.miss[ii] <<- ci$upper < par.true
        })
        c('Method'        = method,
          'Level'         = level,
          'Coverage Rate' = mean(par.inside),
          'Lower Miss'    = mean(lower.miss),
          'Upper Miss'    = mean(upper.miss))
      }
    )
  }




