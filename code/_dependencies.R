### Load packages ####
if (!require("pacman")) install.packages("pacman"); library(pacman)


p_load(
  here,
  sessioninfo,
  assertr,
  cowplot,
  scales,
  pROC,
  brms,
  knitr,
  testthat,
  tidyverse,
  tidybayes,
  tidymodels,
  #tidylog,
  glue
)

session_info() %>% 
  write_lines(here("output/session_info.txt"))


span_smooth <- 2.5


### ggplot theming ####
theme_set(theme_minimal_grid())
theme_update(
  strip.text = element_text(face = "bold")
)



# extraversion norms

# Norms are from
# 
# Srivastava, S., John, O. P., Gosling, S. D., & Potter, J. (2003). Development of personality in early and middle adulthood: Set like plaster or persistent change? Journal of Personality and Social Psychology, 84(5), 1041–1053. https://doi.org/10.1037/0022-3514.84.5.1041
#
# https://thehardestscience.com/2012/10/17/norms-for-the-big-five-inventory-and-other-personality-measures/
#
# Table below is from: https://web.archive.org/web/20190427124300/http://www.ocf.berkeley.edu/~johnlab/pdfs/BFI%20Comparison%20Samples%20(Ages%2021%20-%2060).doc

bfi_norms <- read_delim(
  'age N variable M SD
21 6076 Extraversion 3.25 0.90
22 5014 Extraversion 3.26 0.89
23 4828 Extraversion 3.30 0.89
24 4494 Extraversion 3.28 0.89
25 4499 Extraversion 3.31 0.91
26 3683 Extraversion 3.31 0.91
27 3529 Extraversion 3.28 0.91
28 3497 Extraversion 3.29 0.92
29 3213 Extraversion 3.29 0.91
30 3007 Extraversion 3.28 0.90
31 2307 Extraversion 3.31 0.90
32 2111 Extraversion 3.27 0.89
33 1907 Extraversion 3.26 0.92
34 1735 Extraversion 3.29 0.93
35 1760 Extraversion 3.29 0.91
36 1509 Extraversion 3.24 0.91
37 1541 Extraversion 3.26 0.92
38 1406 Extraversion 3.23 0.90
39 1269 Extraversion 3.23 0.91
40 1393 Extraversion 3.30 0.89
41 1115 Extraversion 3.25 0.91
42 1244 Extraversion 3.25 0.90
43 1064 Extraversion 3.22 0.93
44 1051 Extraversion 3.26 0.88
45 1135 Extraversion 3.22 0.89
46  900 Extraversion 3.23 0.91
47  856 Extraversion 3.25 0.89
48  809 Extraversion 3.24 0.91
49  735 Extraversion 3.21 0.89
50  791 Extraversion 3.26 0.90
51  600 Extraversion 3.29 0.94
52  563 Extraversion 3.30 0.87
53  456 Extraversion 3.25 0.92
54  328 Extraversion 3.17 0.91
55  346 Extraversion 3.25 0.85
56  317 Extraversion 3.26 0.85
57  246 Extraversion 3.12 0.91
58  210 Extraversion 3.18 0.89
59  161 Extraversion 3.13 0.89
60  162 Extraversion 3.10 0.85',
  delim = " "
) %>%
  type.convert()


fnc_find_extr_prctl <- function(extr, age) {
  age_int <- case_when(age > 60 ~ 60,
                       age < 21 ~ 21,
                       TRUE ~ age)
  
  suppressMessages({
    extr_norm <- filter(bfi_norms, age == age_int) %>%
      verify(nrow(.) == 1)
  })
  
  extr_prctl <- pnorm(q = extr,
                      mean = extr_norm$M,
                      sd = extr_norm$SD)
  
  extr_type <- case_when(extr_prctl > .5 ~ "extr",
                         extr_prctl <= .5 ~ "intr",
                         TRUE ~ "NA") %>% factor(levels = c("extr", "intr"))
  extr_pro <- abs(extr_prctl - .5)
  extr_pro_c <- extr_pro - 0.25 # centered

  return(
    tibble(
      extr_prctl,
      extr_type,
      extr_pro,
      extr_pro_c
    )
  )
}


# stimuli categories
tbl_stimuli <- tribble(
  ~stimuli, ~stimuli_target,
  1,       "intr",
  2,       "extr",
  3,       "intr",
  4,       "extr",
  5,       "intr",
  6,       "extr",
  7,       "intr",
  8,       "extr",
  9,       "intr",
  10,       "extr",
)



# education
df_education_infos <- tribble(
  ~education_num, ~education, ~education_coarse,
  1, "No formal qualifications", "Secondary school/GCSE or less",
  2, "Secondary school/GCSE", "Secondary school/GCSE or less",
  3, "College/A levels", "College",
  4, "Undergraduate degree (BA/BSc/other)", "Undergraduate degree",
  5, "Graduate degree (MA/MSc/MPhil/other)", "Graduate/doctorate degree",
  6, "Doctorate degree (PhD/MD/other)", "Graduate/doctorate degree",
) %>% 
  mutate(
    education = ordered(education, .$education),
    education_coarse = ordered(education_coarse,
                               unique(.$education_coarse))
  )



acc_str <- "Accuracy"
#acc_str <- "Accuracy (proportion correct)"


# Experiment 1
df_exp1_conditions <- tribble(~condition, ~ condition_str,
        "inoculation", "boosting",
        "control", "control",
) %>% 
  mutate(condition_str = factor(condition_str, unique(.$condition_str)))


# Experiment 2
df_exp2_conditions <- tribble(~questionnaire, ~questionnaire_str, ~relevance, ~relevance_str, ~boosting_str,
                              "without", "without", "control", "control", "control",
                              "with", "with", "control", "control", "control",
                              "without", "without", "boosting", "relevant", "boosting",
                              "with", "with", "boosting", "boosting",  "boosting",
) %>% 
  mutate(across(everything(), as_factor))



# as far as I can tell, there is no deviation coding (strict meaning) function in base R, where the sum of the contrast values sum to 1 and not to n.
contr.dev <- function(n,
                      contrasts = TRUE,
                      sparse = FALSE) {
  contr.sum(n, contrasts = TRUE, sparse = FALSE) / 2
}





### MCMC model setup

rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# total number of MCMC iterations (incl. burn-in)
iter <- 8000 # 4000 or more

# number of warmup MCMC iterations (aka burn-in)
warmup <- 0.5 * iter

# thinning
thin <- 1 # aim for a total of ca. 4,000 samples across chains





### CUSTOM FUNCTIONS

# check coding of a variable
fnc_check_coding <- function(x_fctr) {
  message("Contrast coding:")
  print(contrasts(x_fctr))
  test_that("is deviation coded",
            {
              expect_equal({
                x_fctr %>%
                  contrasts %>%
                  sum
              },
              0)
            })
}



# function that rounds numbers nicely
fnc_perc <- function(x) {
  # [adding up rounded percentages to equal 100%](http://dochoffiday.com/professional/adding-up-rounded-percentages-to-equal-100)
  
  # floor-round percentages
  perc_floor <- floor(100 * x)
  
  # calculate how many percentage points need to be topped up
  top_up <- 100 - sum(perc_floor)
  
  # order percentages according to their decimal value
  top_up_indices <-
    order(100 * x - perc_floor, decreasing = TRUE)[1:top_up]
  
  # top up the floor-rounded percentages
  perc <- perc_floor
  perc[top_up_indices] <- perc[top_up_indices] + 1
  
  # check
  expect_equal(sum(perc), 100)
  
  return(perc)
}
#fnc_perc(c(.405, .206, .389))
