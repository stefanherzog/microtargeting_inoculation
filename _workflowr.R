### workflowr management




# Delete the stan model files?
# Change to `TRUE` if you want the MCMC models to be re-run.
# Beware: Depending on the machine this can take a couple of minutes or more.
delete_stan_model_files <- FALSE

if (delete_stan_model_files == TRUE) {
  stan_model_files <- list(
    "output/study1/models/mdl_exp1.rds",
    "output/study2/models/mdl_exp2.rds")
  lapply(stan_model_files, function(x) {
    if (file.exists(here::here(x))) {
      file.remove(here::here(x))
    }
  })
}


# project files
# (in their order of processing)
files <- c(
  "analysis/index.Rmd",
  "analysis/study1_make_data.Rmd",
  "analysis/study2_make_data.Rmd",
  "analysis/analyses.Rmd"
)



# Note: `dry_run` is set to `TRUE`
# that is, `wflow_build` and `wflow_publish` will only preview 
# what they would do id `dry_run` were set to `FALSE`


# Testing the project website (build locally)
wflow_build(
  files = files,
  republish = FALSE,
  update = FALSE,
  verbose = TRUE,
  delete_cache = TRUE,
  dry_run = TRUE
)


# Publishing/updating the project website (build locally & commit files)
wflow_publish(
  files = files,
  republish = FALSE,
  update = FALSE,
  verbose = TRUE,
  delete_cache = TRUE,
  dry_run = TRUE
)
