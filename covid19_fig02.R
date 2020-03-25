#' about curve cumulative
#' https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(03)13335-1/fulltext
#' 
#' about delay
#' 
#' https://twitter.com/AdamJKucharski/status/1229708001243795458
#' https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30119-5/fulltext#sec1
#' 
#' abount modeling SIR
#' https://royalsocietypublishing.org/doi/full/10.1098/rspb.2015.0347
#' 
#' pendientes
#' - estimar pico de curva epidemica
#' - día de de la mediana de la curva de casos y confimación (días de retrazo en el pico)
#' - cambiar demora por tiempo en gráfico (y texto?)

# FIG.C DELAY SIMPTOMS-CONFIRMATION INTERNACIONAL ---------

library(tidyverse)
library(googlesheets4)
library(lubridate)
library(aweek)
library(rlang)
theme_set(theme_minimal())

ncovdb_sheetname <- "https://docs.google.com/spreadsheets/d/1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/"
ncovdb_githuname <- "https://raw.github.com/beoutbreakprepared/nCoV2019/master/latest_data/latestdata.csv"

# ncovdb_in_hubei <- sheets_get(ss = ncovdb_sheetname) %>% 
#   read_sheet(sheet = "Hubei") %>% 
#   mutate(hubei="in") %>% 
#   mutate_at(.vars = vars(contains("date")),.funs = dmy)

ncovdb_out_hubei <- 
  read_csv(ncovdb_githuname) %>% 
  filter((country=="China"&province!="Hubei")|country=="Japan") %>% 
  # sheets_get(ss = ncovdb_sheetname) %>%
  # read_sheet(sheet = "outside_Hubei") %>%
  # mutate(hubei="out") %>%
  mutate_at(.vars = vars(contains("date")),.funs = dmy) %>% 
  # fecha de sintomas-coleccion-confimacion
  #mutate(fecha_de_obtencion=dmy(fecha_de_obtencion)) %>% 
  mutate(tiempo_obtencion_sintomas=
           interval(date_onset_symptoms, date_confirmation) / days(1)) %>% 
  mutate(fecha_ini_epiweek_w=date2week(date_onset_symptoms, 
                                       week_start = "Sunday", floor_day = TRUE),
         fecha_ini_epiweek_w_fct=as.factor(fecha_ini_epiweek_w),
         fecha_conf_epiweek_w=date2week(date_confirmation, 
                                        week_start = "Sunday", floor_day = TRUE),
         fecha_conf_epiweek_w_fct=as.factor(fecha_conf_epiweek_w)
         ) %>% 
  filter(!is.na(tiempo_obtencion_sintomas)) %>% 
  filter(tiempo_obtencion_sintomas>=0) %>% 
  #mutate(country=)
  select(country,
         date_onset_symptoms, date_confirmation,
         tiempo_obtencion_sintomas,
         fecha_ini_epiweek_w_fct,
         fecha_conf_epiweek_w_fct
         ) %>% 
  mutate(country=case_when(
    country=="China"~"China (Fuera de Hubei)",
    country=="Japan"~"Japón",
    TRUE~country)) %>% 
  mutate(country=fct_infreq(f = country)) %>% 
  
  #fijar fecha de cierre para articulo
  filter(date_confirmation<ymd(20200324))

#ncovdb_out_hubei <- ncovdb_out_hubei

ncovdb_out_hubei %>% glimpse()
# ncovdb_out_hubei %>% 
#   select(contains("date")) %>% 
#   naniar::miss_var_summary()

ncovdb_out_hubei %>% 
  filter(as.numeric(country)<5) %>% 
  select(-fecha_ini_epiweek_w_fct,-fecha_conf_epiweek_w_fct,
         -date_onset_symptoms,-date_confirmation) %>% 
  group_by(country) %>%
  skimr::skim_to_wide(tiempo_obtencion_sintomas) %>%
  ungroup() %>% 
  arrange(desc(as.numeric(n))) %>%
  select(-type,-variable,-missing,-complete,-hist) %>% 
  writexl::write_xlsx("figure/tab01-ncovdb_out_hubei-tiempo_obtencion_sintomas.xlsx")

ncovdb_out_hubei %>% 
  filter(as.numeric(country)<5) %>% 
  select(-fecha_conf_epiweek_w_fct,
         -date_onset_symptoms,-date_confirmation) %>% 
  group_by(fecha_ini_epiweek_w_fct,country) %>%
  skimr::skim_to_wide(tiempo_obtencion_sintomas) %>%
  ungroup() %>% 
  arrange(country,fecha_ini_epiweek_w_fct) %>%
  select(-type,-variable,-missing,-complete,-hist) %>% 
  writexl::write_xlsx("figure/tab01-ncovdb_out_hubei-tiempo_obtencion_sintomas-fecha_ini_epiweek_w_fct.xlsx")


# delay sintomas-confirmacion ---------------------------------------------

f01 <- ncovdb_out_hubei %>% 
  filter(as.numeric(country)<3) %>% 
  
  ggplot(aes(tiempo_obtencion_sintomas,fill=fecha_ini_epiweek_w_fct)) +
  geom_bar(width = 0.9) +
  scale_fill_viridis_d() +
  facet_wrap(~country,scales = "free",nrow = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  # scale_y_continuous(breaks = int_breaks_rounded) +
  # scale_x_continuous(breaks = int_breaks_rounded) +
  labs(#title = "Demora entre Inicio de Síntomas y Confirmación\ndel caso por Semana Epidemiológica",
       y="Número de casos",
       x = "Demora de Inicio de síntomas a Confirmación del caso (días)",
       caption = "\nW: Week o Semana Epidemiológica\n\nFuente: Open COVID-19 Data Curation Group",
       fill="Semana de\ninicio de\nsíntomas")

#ggsave("figure/fig01-ncovdb_out_hubei-tiempo_obtencion_sintomas.png",dpi = "retina",width = 9,height = 5)


# datasets per day ----------------------------------------------------------------

date_onset_db <- ncovdb_out_hubei %>% 
  filter(as.numeric(country)<3) %>% 
  count(country,date_onset_symptoms) %>% 
  arrange(country,date_onset_symptoms) %>% 
  rename(case_incidence=n) %>% 
  group_by(country) %>% 
  mutate(case_cumulative=cumsum(case_incidence)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(case_incidence,case_cumulative),
               names_to = "case_type",
               values_to = "case_number") %>% 
  mutate(date_type="date_onset_symptoms") %>% 
  rename(date="date_onset_symptoms")

date_confirmed_db <- ncovdb_out_hubei %>% 
  filter(as.numeric(country)<3) %>% 
  count(country,date_confirmation) %>% 
  arrange(country,date_confirmation) %>% 
  rename(case_incidence=n) %>% 
  group_by(country) %>% 
  mutate(case_cumulative=cumsum(case_incidence)) %>% 
  ungroup() %>% 
  pivot_longer(cols = c(case_incidence,case_cumulative),
               names_to = "case_type",
               values_to = "case_number") %>% 
  mutate(date_type="date_confirmation") %>% 
  rename(date="date_confirmation")


# incidencia diaria -------------------------------------------------------

f02 <- union_all(date_onset_db,date_confirmed_db) %>% 
  mutate(date_type=fct_relevel(date_type,"date_onset_symptoms")) %>% 
  filter(case_type=="case_incidence") %>%
  mutate(date_type=fct_recode(
    .f = date_type,
    "Inicio de síntomas"="date_onset_symptoms",
    "Confirmación de caso"="date_confirmation")) %>% 
  ggplot(aes(x = date,y = case_number,fill=date_type)) +
  geom_col(position = position_dodge(preserve = "single")) +
  scale_fill_viridis_d(option = "cividis") +
  facet_wrap(~country,scales = "free",nrow = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_date(date_breaks = "7 day",date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        #legend.position = "bottom"
        ) +
  labs(fill="",x="Día calendario",y="Casos incidentes")


# incidencia acumulada ----------------------------------------------------

f03 <- union_all(date_onset_db,date_confirmed_db) %>% 
  mutate(date_type=fct_relevel(date_type,"date_onset_symptoms")) %>% 
  filter(case_type=="case_cumulative") %>%
  mutate(date_type=fct_recode(
    .f = date_type,
    "Inicio de síntomas"="date_onset_symptoms",
    "Confirmación de caso"="date_confirmation")) %>% 
  ggplot(aes(x = date,y = case_number,color=date_type)) +
  geom_line() +
  geom_point() + 
  scale_color_viridis_d(option = "cividis") +
  facet_wrap(~country,scales = "free",nrow = 1) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_date(date_breaks = "7 day",date_labels = "%b-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        #legend.position = "bottom"
        ) +
  labs(color="",x="Día calendario",y="Casos acumulados")

# enlazar -----------------------------------------------------------------

library(patchwork)

f02 / f03 / f01 + patchwork::plot_annotation(tag_levels = "A")
ggsave("figure/fig01-curve_day-incidence_cumulative_delay.png",height = 9,width = 7,dpi = "retina")


# doubling time -----------------------------------------------------------

date_onset_db_max <- date_onset_db %>% 
  filter(case_type=="case_incidence") %>% 
  group_by(country) %>% 
  filter(case_number==max(case_number)) %>% 
  select(country,date_max_inc=date)

union_all(date_onset_db,date_confirmed_db) %>% 
  left_join(date_onset_db_max) %>% 
  filter(date<date_max_inc) %>% 
  ggplot(aes(x = date,y = case_number,colour=case_type)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(date_type~country,scales = "free_y") +
  scale_y_log10()
