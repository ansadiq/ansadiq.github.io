library(readxl)
library(gt)
library(tidyverse)
library(gtsummary)
library(kableExtra)
library(rmarkdown)
library(gtExtras)

total_words10 <- read_xlsx("/Users/aishatsadiq/Library/Mobile Documents/iCloud~md~obsidian/Documents/PhD/CCSS Data Fellow/relm_mergeimage/total_words10.xlsx")

total_words10$TAX_YEAR <- as.factor(total_words10$TAX_YEAR)
professional.lm <- lm(professional_perc ~ TAX_YEAR, data = total_words10)
summary(professional.lm)
corporate.lm <- lm(corporate_perc ~ TAX_YEAR, data = total_words10)
summary(corporate.lm)
public.lm <- lm(public_perc ~ TAX_YEAR, data = total_words10)
summary(public.lm)
community.lm <- lm(community_perc ~ TAX_YEAR, data = total_words10)
summary(community.lm)

tb37 <- tbl_regression(professional.lm) %>% add_significance_stars(hide_ci = TRUE, hide_p = FALSE, pattern = "{p.value}{stars}") 
tb38 <- tbl_regression(corporate.lm) %>% add_significance_stars(hide_ci = TRUE, hide_p = FALSE, pattern = "{p.value}{stars}")
tb39 <- tbl_regression(public.lm) %>% add_significance_stars(hide_ci = TRUE, hide_p = FALSE, pattern = "{p.value}{stars}")
tb40 <- tbl_regression(community.lm) %>% add_significance_stars(hide_ci = TRUE, hide_p = FALSE, pattern = "{p.value}{stars}")

lm.merge10 <- tbl_merge(
  tbls = list(tb37, tb38, tb39, tb40),
  tab_spanner = c("**Professional Logic**", "**Market Logic**","**Public Logic**","**Community Logic**")) %>% 
  as_gt() %>%
 gt::opt_table_lines("all")  %>%
tab_style(
    style = cell_borders(
        sides = c("left"),
        color = "red",
        style = "solid",
        weight = px(1.5)),
    locations =  list(
       cells_column_spanners(),
       cells_column_labels(),
       cells_body(
         columns = 
           c(estimate_1,estimate_2,estimate_3,estimate_4)
       )))%>%
tab_style(
    style = cell_borders(
        sides = c("left","right"),
        color = "red",
        style = "solid",
        weight = px(1.5)),
    locations =  list(
       cells_column_spanners(),
       cells_column_labels(),
       cells_body(
         columns = 
           c(p.value_1,p.value_2,p.value_3,p.value_4))))


%>%
tab_style(
    style = cell_borders(
        sides = c("right"),
        color = "blue",
        style = "solid",
        weight = px(1.5),
    locations =  cells_body(
      columns = 
      rows = "TAX_YEAR2010", "TAX_YEAR2011", "TAX_YEAR2012", "TAX_YEAR2013", "TAX_YEAR2014", "TAX_YEAR2015","TAX_YEAR2016","TAX_YEAR2017","TAX_YEAR2018","TAX_YEAR2019")))
lm.merge10    
     
   
%>%
  gt_add_divider(columns = "cyl", style = "solid")


#Borders before Beta and after P-value
 
             TAX_YEAR2010 TAX_YEAR2011 TAX_YEAR2012 TAX_YEAR2013 TAX_YEAR2014 TAX_YEAR2015 TAX_YEAR2016 
  "TAX_YEAR"       "2010"       "2011"       "2012"       "2013"       "2014"       "2015"       "2016" 
TAX_YEAR2017 TAX_YEAR2018 TAX_YEAR2019 
      "2017"       "2018"       "2019"   
  
      


lm.merge10 %>% 
  as_gt() %>%
  gt::gtsave(filename = "lm.merge.html") # use extensions .png, .html, .docx, .rtf, .tex, .ltx


# Junkyard ----
tb37 <- tbl_regression(professional.lm) %>% 
  add_significance_stars(hide_ci = TRUE, hide_p = FALSE, pattern = "{p.value}{stars}") %>% 
  gtExtras::gt_add_divider(Characteristic,
  sides = "all",
  color = "grey",
  style = "solid",
  weight = px(3),
  include_labels = TRUE
)


gtcars %>%
  dplyr::select(
    -mfr, -trim, bdy_style, drivetrain,
    -drivetrain, -trsmn, -ctry_origin
  ) %>%
  dplyr::slice(1:8) %>%
  gt(rowname_col = "model") %>%
  tab_spanner(
    label = "performance",
    columns = c(
      hp, hp_rpm, trq, trq_rpm,
      mpg_c, mpg_h
    )
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      weight = px(2)),
    locations = cells_body(
      columns = c(year, bdy_style, msrp)
      )
    )


<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="border-left-width: 1.5px; border-left-style: solid; border-left-color: red; border-right-width: 1.5px; border-right-style: solid; border-right-color: red;" scope="col" id="<strong>Characteristic</strong>"><strong>Characteristic</strong></th>
  
  
      locations = list(
      cells_body(
        columns = num,
        rows = is.na(num)
      ),
      cells_body(
        columns = currency,
        rows = is.na(currency)
      )
    )
  )

    )     
lm.merge10
     
