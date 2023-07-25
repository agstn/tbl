# PACKAGES
pacman::p_load(tidyverse, rio)
pacman::p_load(gt, gtsummary)
pacman::p_load(labelled)
pacman::p_load(broom.helpers)
pacman::p_load(ordinal)

# Common statistical tests are linear models (or: how to teach stats)
# https://lindeloev.github.io/tests-as-linear/

# LOGISTIC
pairwise_logistic <- function(data, variable, by, ...) {
   rlang::inject(
      glm(!!rlang::sym(variable) ~ !!rlang::sym(by), data = data, family = binomial)) %>%
      tidy_and_attach() %>% 
      tidy_remove_intercept() %>% 
      tidy_add_pairwise_contrasts(keep_model_terms = FALSE,
                                  pairwise_reverse = FALSE,
                                  contrasts_adjust = "none") %>% 
      select(label = term, p.value) %>% 
      mutate(label = str_glue('**{label}**')) %>% 
      spread(label, p.value) %>% 
      mutate(m = 'G')
}
pairwise_logistic(trial, "death", "grade")

# REGRESSION
pairwise_reg <- function(data, variable, by, ...) {
   rlang::inject(
      lm(!!rlang::sym(variable) ~ !!rlang::sym(by), data = data)) %>%
      tidy_and_attach() %>% 
      tidy_remove_intercept() %>% 
      tidy_add_pairwise_contrasts(keep_model_terms = FALSE,
                                  pairwise_reverse = FALSE,
                                  contrasts_adjust = "none") %>% 
      select(label = term, p.value) %>% 
      mutate(label = str_glue('**{label}**')) %>% 
      spread(label, p.value) %>% 
      mutate(m = 'L')
}
pairwise_reg(trial, "death", "grade")

# RANK
pairwise_rank <- function(data, variable, by, ...) {
   rlang::inject(
      lm(rank(!!rlang::sym(variable)) ~ !!rlang::sym(by), data = data )) %>%
      tidy_and_attach() %>% 
      tidy_remove_intercept() %>% 
      tidy_add_pairwise_contrasts(keep_model_terms = FALSE,
                                  pairwise_reverse = FALSE,
                                  contrasts_adjust = "none") %>% 
      select(label = term, p.value) %>% 
      mutate(label = str_glue('**{label}**')) %>% 
      spread(label, p.value) %>% 
      mutate(m = 'R')
}
pairwise_rank(trial, "marker", "grade")

# CLM
pairwise_clm <- function(data, variable, by, ...) {
   rlang::inject(
      clm( factor(!!rlang::sym(variable)) ~ 
              !!rlang::sym(by), 
           data = data )) %>%
      tidy_and_attach() %>% 
      tidy_remove_intercept() %>% 
      tidy_add_pairwise_contrasts(keep_model_terms = FALSE,
                                  pairwise_reverse = FALSE,
                                  contrasts_adjust = "none") %>% 
      select(label = term, p.value) %>% 
      mutate(label = str_glue('**{label}**')) %>% 
      spread(label, p.value) %>% 
      mutate(m = 'O')
}
pairwise_clm(trial, "stage", "grade")

# THEME
theme_gtsummary_compact()

# FUNCTIONS
builder <- function(x, Limit){cells_body(columns = !!sym(x), rows = !!sym(x) < 0.05)}
names   <- c('**I - II**', '**I - III**', '**II - III**')

# TABLE
tbl_summary(data = trial,
            by = grade,
            missing = "no",
            include = c(trt, age, marker, stage, response, death, ttdeath) ,
            digits = list(age ~ c(1,1) ),
            statistic = list(age ~ c("{mean} ({sd})")),
            label = list(trt ~ 'Chemo. Treatment',
                         age ~ 'Age (yr.)') ) %>% 
   add_stat(fns = list(all_continuous()  ~ pairwise_reg,
                       'marker'          ~ pairwise_rank,
                       all_dichotomous() ~ pairwise_logistic,
                       c('trt','stage')  ~ pairwise_clm) ) %>% 
   add_overall() %>% 
   add_n() %>% 
   bold_labels() %>% 
   modify_header( label = "",
                  m = '',
                  all_stat_cols() ~ "**{level}**<br> N = {n} <br> ({style_percent(p)}%)") %>% 
   modify_fmt_fun( all_of(names) ~ function(x) style_pvalue(x, digits = 2) ) %>% 
   modify_spanning_header( c(all_of(names), 'm') ~ "**Pairwise Comparisons**") %>% 
   modify_column_alignment( c(all_of(names), 'm'), align = 'right') %>% 
   modify_caption('**Table I: Baseline Characterisitc w/ Pairwise Comparisons**') %>% 
   as_gt() %>% 
   tab_style(
      style = list( cell_text(weight = "bold"),
                    cell_fill(color = "lightgray", alpha = 0.5) ),
      locations = lapply( names, builder, Limit = 0.05)
   ) %>%
   tab_style(
      style = list( cell_text(size = "xx-small") ),
      locations = cells_body(
         columns = c('m'),
         rows = everything())
   )  %>%
   gtsave("trial_pairwise.html")

