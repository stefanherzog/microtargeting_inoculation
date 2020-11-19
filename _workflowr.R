### workflowr management
#
# Note: `dry_run` is set to `TRUE`


# project files
# (in their order of processing)
files <- c(
  "analysis/index.Rmd",
  "analysis/study1_make_data.Rmd",
  "analysis/study2_make_data.Rmd",
  "analysis/analyses.Rmd"
)


# Testing the project website
wflow_build(
  files = files,
  republish = FALSE,
  update = FALSE,
  verbose = TRUE,
  delete_cache = TRUE,
  dry_run = TRUE
)


# Publishing/updating the project website
wflow_publish(
  files = files,
  republish = FALSE,
  update = FALSE,
  verbose = TRUE,
  delete_cache = TRUE,
  dry_run = TRUE
)
