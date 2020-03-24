#' about delay
#' 
#' https://twitter.com/AdamJKucharski/status/1229708001243795458

# FIG.C DELAY SIMPTOMS-CONFIRMATION INTERNACIONAL ---------

library(tidyverse)
library(googlesheets4)
library(lubridate)
library(aweek)
library(rlang)

ncovdb_sheetname <- "https://docs.google.com/spreadsheets/d/1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/"


# ncovdb_in_hubei <- sheets_get(ss = ncovdb_sheetname) %>% 
#   read_sheet(sheet = "Hubei") %>% 
#   mutate(hubei="in") %>% 
#   mutate_at(.vars = vars(contains("date")),.funs = dmy)

ncovdb_out_hubei <- sheets_get(ss = ncovdb_sheetname) %>%
  read_sheet(sheet = "outside_Hubei") %>%
  mutate(hubei="out") %>%
  mutate_at(.vars = vars(contains("date")),.funs = dmy) %>% 
  # fecha de sintomas-coleccion-confimacion
  #mutate(fecha_de_obtencion=dmy(fecha_de_obtencion)) %>% 
  mutate(tiempo_obtencion_sintomas=
           interval(date_onset_symptoms, date_confirmation) / days(1)) %>% 
  mutate(fecha_ini_epiweek_w=date2week(date_onset_symptoms, 
                                       week_start = "Sunday", floor_day = TRUE),
         fecha_ini_epiweek_w_fct=as.factor(fecha_ini_epiweek_w)) %>% 
  filter(!is.na(tiempo_obtencion_sintomas)) %>% 
  filter(tiempo_obtencion_sintomas>=0) %>% 
  #mutate(country=)
  select(country,tiempo_obtencion_sintomas,fecha_ini_epiweek_w_fct) %>% 
  mutate(country=if_else(country=="China","China (out of Hubei)",country)) %>% 
  mutate(country=fct_infreq(f = country))

#ncovdb_out_hubei <- ncovdb_out_hubei

ncovdb_out_hubei %>% glimpse()
# ncovdb_out_hubei %>% 
#   select(contains("date")) %>% 
#   naniar::miss_var_summary()

ncovdb_out_hubei %>% 
  filter(as.numeric(country)<5) %>% 
  select(-fecha_ini_epiweek_w_fct) %>% 
  group_by(country) %>%
  skimr::skim_to_wide(tiempo_obtencion_sintomas) %>%
  ungroup() %>% 
  arrange(desc(as.numeric(n))) %>%
  select(-type,-variable,-missing,-complete,-hist) %>% 
  writexl::write_xlsx("figure/tab01-ncovdb_out_hubei-tiempo_obtencion_sintomas.xlsx")

ncovdb_out_hubei %>% 
  filter(as.numeric(country)<5) %>% 
  #select(-fecha_ini_epiweek_w_fct) %>% 
  group_by(fecha_ini_epiweek_w_fct,country) %>%
  skimr::skim_to_wide(tiempo_obtencion_sintomas) %>%
  ungroup() %>% 
  arrange(country,fecha_ini_epiweek_w_fct) %>%
  select(-type,-variable,-missing,-complete,-hist) %>% 
  writexl::write_xlsx("figure/tab01-ncovdb_out_hubei-tiempo_obtencion_sintomas-fecha_ini_epiweek_w_fct.xlsx")

ncovdb_out_hubei %>% 
  filter(as.numeric(country)<5) %>% 
  
  ggplot(aes(tiempo_obtencion_sintomas,fill=fecha_ini_epiweek_w_fct)) +
  geom_bar(width = 0.9) +
  scale_fill_viridis_d() +
  facet_wrap(~country,scales = "free") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  # scale_y_continuous(breaks = int_breaks_rounded) +
  # scale_x_continuous(breaks = int_breaks_rounded) +
  theme_minimal() +
  labs(title = "Tiempo desde el Inicio de Síntomas hasta la Confirmación del caso por Semana Epidemiológica",
       y="Número de casos",
       x = "Días entre inicio de síntomas y confirmación de caso",
       caption = "W: Week o Semana Epidemiológica",
       fill="Semana de\ninicio de\nsíntomas")

ggsave("figure/fig01-ncovdb_out_hubei-tiempo_obtencion_sintomas.png",dpi = "retina",width = 9,height = 5)
