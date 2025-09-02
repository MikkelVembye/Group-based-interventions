## script: MWE of {metafor} and
## and {wildmeta} with and without parallel
## sessions
##
## The script uses the examples
## from ?metafor::rma.mv() and ?wildmeta::Wald_test_cwb()
## prelims
rm(list = ls())
gc()

RNGkind("L'Ecuyer-CMRG") 

## prepare data for
## metafor::rma.mv()
DT <- metafor::escalc(
  measure = "OR",
  ai = tpos,
  bi = tneg,
  ci = cpos,
  di = cneg,
  data = metadat::dat.bcg
)

## run model models
## compatible with wildmeta::Wald_test_cwb()
model_list <- list(
  univariate = metafor::rma.uni(
    yi,
    vi,
    data = DT,
    mods = ~ 0 + alloc
  ),
  multivariate = metafor::rma.mv(
    yi,
    vi,
    mods = ~ 0 + alloc,
    random = ~ 1 | trial,
    data = DT
  )
)

## sanity check on seed
## consistency across runs
## sequentially
sequential_run <- replicate(
  3,
  wildmeta::Wald_test_cwb(
    full_model = model_list$multivariate,
    constraints = wildmeta::constrain_equal(1:3),
    R = 10,
    seed = 1903L
  ),
  simplify = FALSE
)

stopifnot(
  all(
    vapply(
      sequential_run[-1],
      function(z) {
        identical(z, sequential_run[[1]])
      },
      logical(1)
    )
  )
)

## sanity check on seed
## consistency across runs
## in parallel with {future}
future::plan(
  future::multisession,
  workers = max(1L, future::availableCores() - 1L)
)
on.exit(future::plan(future::sequential), add = TRUE)

## generate helper-function
## for a parallel run with
## three repetitions
one_parallel_run <- function(model_list, DT) {
  output_list <- lapply(1:3, function(i) {
    future::future(
      {
        wildmeta::Wald_test_cwb(
          full_model = model_list$multivariate,
          constraints = wildmeta::constrain_equal(1:3),
          R = 10,
          seed = 1903L
        )
      },
      globals = list(model_list = model_list, DT = DT),
      seed = TRUE
    )
  })
  lapply(output_list, future::value)
}

## run each parallel execution
## three times and sotre as runs
runs <- replicate(
  3,
  one_parallel_run(model_list, DT),
  simplify = FALSE
)

## conduct sanity checks
## within each replication
## and across replications
within_check <- vapply(
  runs,
  function(run) {
    all(vapply(run[-1], function(z) identical(z, run[[1]]), logical(1)))
  },
  logical(1)
)

across_check <- all(vapply(
  runs[-1],
  function(r) identical(r, runs[[1]]),
  logical(1)
))

## stop script if
## either value is false
stopifnot(all(within_check), across_check)

## store all results
## locally conditional on whether the file
## exists already
if (!file.exists("local_runs.rds")) {
  saveRDS(
    object = runs,
    file = "local_runs.rds"
  )
} else {
  local_runs <- readRDS(
    "local_runs.rds"
  )
}

## check cloud runs
## against local
flatten <- function(x) {
  if (!inherits(x, "list")) {
    list(x)
  } else {
    unlist(c(lapply(x, flatten)), recursive = FALSE)
  }
}

## flatten the lists
## and compare local vs cloud
local_runs <- flatten(local_runs)
cloud_runs <- flatten(runs)

local_vs_cloud <- vapply(
  X = seq_along(local_runs),
  FUN = function(i) {
    identical(local_runs[[i]], cloud_runs[[i]])
  },
  FUN.VALUE = logical(1L)
)

stopifnot(all(local_vs_cloud))
