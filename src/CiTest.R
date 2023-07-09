## Functions all we need
source('Bootstrap.R')
## Directory to save the result date frame
result.dir <- '../res/extrabs.csv'
## Number of repeatitions/sample size
n.rep      <- expand_grid(sample = 250, mc = 500, bs = c(100, 250, 500))
## Ramdom seed
TheSeed    <- 114514

## Distributions, description of distributions and true means
rfuns <- c(rnorm,
           ~rchisq(., df = 5),
           ~rchisq(., df = 20),
           ~rt(., df = 5),
           ~rt(., df = 20),
           ~rf(., 5, 20),
           ~rf(., 20, 20))
descs <- c('Z', 'chisq(5)', 'chisq(20)', 't(5)', 't(20)', 'F(5,20)', 'F(20, 20)')
means <- c(0, 5, 20, 0, 0, 20 / 18, 20 / 18)

if (require(doParallel)) {

  message('Running parallelly...')
  registerDoParallel(8)

  df <-
    foreach(rfun      = rfuns, desc = descs, partrue = means,
            .combine  = bind_rows, .multicombine = TRUE,
            .packages = c('tidyverse', 'rlang')) %dopar% {
      source('SamUtils.R')
      set.seed(TheSeed)
      rfun <- rlang::as_function(rfun)
      pmap_dfr(n.rep, function (sample, mc, bs) {
        bootstrap.coverage(rfun, mean, partrue,
                           n.sample = sample,
                           n.mc     = mc,
                           n.bs     = bs) |>
          mutate(`Sample Repeatition`      = sample,
                 `Monte Carlo Repeatition` = mc,
                 `Bootstrap Repeatition`   = bs)
      })|>
        mutate(Distribution = desc)
    }
  write_csv(df, result.dir)
  stopImplicitCluster()

} else {

  pmap_dfr(list(rfun = rfuns, desc = descs, partrue = means),
           function (rfun, desc, partrue) {
             set.seed(TheSeed)
             rfun <- rlang::as_function(rfun)
             pmap_dfr(n.rep, function (sample, mc, bs) {
               bootstrap.coverage(rfun, mean, partrue,
                                  n.sample = sample,
                                  n.mc     = mc,
                                  n.bs     = bs) |>
                 mutate(`Sample Repeatition`      = sample,
                        `Monte Carlo Repeatition` = mc,
                        `Bootstrap Repeatition`   = bs)
             })|>
               mutate(Distribution = desc)
           }) |>
    write_csv(result.dir)

}
