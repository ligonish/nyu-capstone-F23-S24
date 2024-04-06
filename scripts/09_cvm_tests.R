# -----------------------------------------------------------------------------
# Capstone Team 4
# Tract-Level Staggered Difference in Differences Analysis (Callaway & Sant'Anna)
# 2024-03-24
# SL
# -----------------------------------------------------------------------------

# Resources
# See https://bcallaway11.github.io/did/articles/did-basics.html (2023-08-29)

options(scipen=999) # removes scientific notation in GEOID field etc

# Dependent Packages ----------------------------------------------------------

library(did)  # Callaway & Sant'Anna's package implementing their staggered DiD
library(tidyverse) 


# Tract-Level Parallel Trends "Test"

cdp_tract <- conditional_did_pretest(yname = "n_violations_per_1k_units", 
                                     tname = "inspection_yr_mo_num", 
                                     idname = "geoid", 
                                     gname = "treat_yr_mo_num",
                                     control_group = "notyettreated",
                                     xformla = ~ evict_rate_17 + rs_rate_17 + pct_pov_17,
                                     data = violations)

summary(cdp_tract)


cdp_tract_maj_bl <- conditional_did_pretest(yname = "n_violations_per_1k_units", 
                                            tname = "inspection_yr_mo_num", 
                                            idname = "geoid", 
                                            gname = "treat_yr_mo_num",
                                            control_group = "notyettreated",
                                            xformla = ~ evict_rate_17 + rs_rate_17 + pct_pov_17,
                                            data = maj_bl)

summary(cdp_tract_maj_bl)

cdp_tract_pct_bl <- conditional_did_pretest(yname = "n_violations_per_1k_units", 
                                            tname = "inspection_yr_mo_num", 
                                            idname = "geoid", 
                                            gname = "treat_yr_mo_num",
                                            control_group = "notyettreated",
                                            xformla = ~ evict_rate_17 + rs_rate_17 + pct_pov_17 + pct_bl_17,
                                            data = violations)

summary(cdp_tract_pct_bl)


cdp_tract_pct_wh <- conditional_did_pretest(yname = "n_violations_per_1k_units", 
                                            tname = "inspection_yr_mo_num", 
                                            idname = "geoid", 
                                            gname = "treat_yr_mo_num",
                                            control_group = "notyettreated",
                                            xformla = ~ evict_rate_17 + rs_rate_17 + pct_pov_17 + pct_wh_17,
                                            data = violations)

summary(cdp_tract_pct_wh)

# Save as R objects

saveRDS(cdp_tract, file = "data_build/cdp_tract.Rds")
saveRDS(cdp_tract_maj_bl, file = "data_build/cdp_tract_maj_bl.Rds")
saveRDS(cdp_tract_pct_bl, file = "data_build/cdp_tract_pct_bl.Rds")
saveRDS(cdp_tract_pct_wh, file = "data_build/cdp_tract_pct_wh.Rds")

list.save(cdp_tract, "data_build/cdp_tract.Rds")

# Table output

cdp_three_table <- as.data.frame(do.call(cbind, cdp_tract_pct_bl)) %>%
  select(xformla, CvM, CvMcval, CvMpval) %>% 
  distinct() %>% 
  slice(2)

cdp_four_table <- as.data.frame(do.call(cbind, cdp_tract_maj_bl)) %>%  
  select(xformla, CvM, CvMcval, CvMpval) %>% 
  distinct() %>% 
  slice(2)

cdp_table <- as.data.frame(do.call(cbind, cdp_tract)) %>%  
  select(xformla, CvM, CvMcval, CvMpval) %>% 
  distinct() %>% 
  slice(2) %>% 
  bind_rows(cdp_three_table, cdp_four_table) %>% 
  mutate(Covariates = c('[All Tracts] 2017 eviction rate + 2017 rent stabilization rate + 2017 poverty rate', 
                        '[All Tracts] 2017 eviction rate + 2017 rent stabilization rate + 2017 poverty rate + 2017 proportion Black renters', 
                        '[Subset of Majority-Black Census Tracts] 2017 eviction rate + 2017 rent stabilization rate + 2017 poverty rate'), .before = xformla) %>% 
  select(-xformla)

cdp_table %>% 
  kable(
    digits = 2,
    caption = 'Table 4. Cramer von Mises Test Results',
    col.names = c('Covariates', 'Test Statistic', 'Critical Value', 'P-Value'),
    align = "l") %>% 
  add_footnote(c("Callaway and Sant'Anna propose the Cramer von Mises or Kolgorov-Smirnov-type integrated moments test as one tool for assessing whether the conditional parallel trends assumption might hold for all groups in all pre-treatment periods. See https://search.r-project.org/CRAN/refmans/did/html/conditional_did_pretest.html and Section 4 of Callaway & Sant'Anna (2020)."), notation = "none") %>% 
  kable_styling(full_width = F, html_font = "Roboto Condensed")