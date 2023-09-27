library(simstudy)
library(smd)
library(tidyverse)
library(gt)
library(gtsummary)

# DATA
def <-  
   defData(varname = "rx", formula = "1;1", dist = "trtAssign") |>
   defData(varname = "x", formula = 0, variance = 10) |>
   defData(varname = "v1", formula = ".5;.3;.2", dist = "categorical")

dm <- 
   defMiss(varname = "x", formula = .10) |>
   defMiss(varname = "f2_v1", formula = '.05 + .05*(frx == "Control")')

set.seed(8312)

dd <- genData(1000, def)
dd <- genFactor(dd, "rx", labels = c("Control", "Treatment"), replace = TRUE)
dd <- genFactor(dd, "v1", prefix = "f1_")
dd <- genFactor(dd, "v1", prefix = "f2_", labels = c("red", "blue", "green"))

missMat <- genMiss(dd, dm, idvars = "id")
dobs <- genObs(dd, missMat, idvars = "id")

# TABLE
reset_gtsummary_theme()
theme_gtsummary_compact()
tbl_summary(data = dobs,
            by = frx,
            type = list(x  ~ 'continuous2',
                        v1 ~ 'continuous2'),
            statistic = list(x ~ c('{mean} ({sd})',
                                  '{N_miss} ({p_miss}%)'),
                             v1 ~ c('{mean} ({sd})')),
            digits = list(x ~ c(3, 2, 0, 1),
                          v1 ~ c(2, 3) ),
            missing = 'ifany',
            missing_text = 'Missing',
            include = c(x, v1, f1_v1, f2_v1)) %>%
   add_difference(everything() ~ "smd",
                  estimate_fun = list(everything() ~ function(x) style_sigfig(x, digits = 3) ) ) %>% 
   modify_column_hide(columns = ci) %>% 
   modify_header(estimate ~ '**SMD**',
                 label ~ '',
                 all_stat_cols() ~ "**{level}<br>(N={n})**") %>%
   modify_caption('**Table 1**') %>% 
   modify_footnote(all_stat_cols() ~ "Values are No. (%) unless otherwise noted. SD = standard deviation") %>% 
   modify_table_body(
      ~.x %>% 
         mutate(label = ifelse(label == "N missing (% missing)","Missing", label) )
   ) %>% 
   remove_row_type(x,
                   type = 'missing') %>% 
   bold_labels() %>% 
   as_gt() %>% 
   gtsave("simstudy_smd.html")


