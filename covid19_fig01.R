
# IDEAS ------
# # porcentaje de asintomáticos 32% (fuente: https://twitter.com/cmyeaton/status/1241880859000610816)
# # delay entre sintomas y admisión/confirmación (fuente: https://www.reconlearn.org/solutions/real-time-response-2.html)


# REFERENCIAS papers --------
# #rompe supuesto de independencia

# REFERENCIAS graficas --------

# if(!require("remotes")) install.packages("remotes")
# if(!require("tidyverse")) install.packages("tidyverse")
# remotes::install_github("avallecam/covid19viz")

library(tidyverse)
library(covid19viz)

#jhu_sitrep_country_report(country_region = "Peru")

dat_jhu <- jhu_sitrep_all_sources(country_region="Peru") %>%
  jhu_sitrep_all_sources_tidy() %>%
  filter(confirmed_cumulative>0) %>% 
  arrange(desc(confirmed_cumulative))

dat_url <- rio::import("https://github.com/jincio/COVID_19_PERU/blob/master/docs/reportes_minsa.xlsx?raw=true")
dat_per <- dat_url %>%  
  as_tibble() %>% 
  janitor::clean_names() %>% 
  arrange(desc(dia)) %>% 
  mutate(dia=lubridate::date(dia)) %>% 
  #un registro por día
  group_by(dia) %>% 
  arrange(desc(hora)) %>% 
  ungroup() %>% 
  distinct(dia,.keep_all = T) %>% 
  #adaptar
  mutate(country_region="Peru") %>% 
  rename(confirmed_cumulative=positivos,
         date=dia) %>% 
  #crear
  arrange(confirmed_cumulative) %>% 
  mutate(confirmed_incidence=confirmed_cumulative-lag(confirmed_cumulative,default = 0)) %>% 
  arrange(desc(confirmed_cumulative))

#dat_per %>% count(dia,sort = T)

dat_jhu %>% glimpse()
dat_per %>% glimpse()

data_input <- dat_jhu
data_input <- dat_per

f1 <- data_input %>%
  who_sitrep_ggline(y_cum_value = confirmed_cumulative,#color = province_state,
                    n_breaks = 10) #+
#theme(legend.position="none")

f2 <- data_input %>%
  who_sitrep_ggbar(y_inc_value = confirmed_incidence,#fill = province_state,
                   n_breaks=10) #+
#theme(legend.position="none")

# FIG.A ACUMULADA DE DETECCIONES POR DÍA ---------

# FIG.B DETECCIONES POR DÍA ---------

library(patchwork)
f1 + f2 + plot_annotation(tag_levels = 'A')

