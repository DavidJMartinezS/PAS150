if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinydashboardPlus")) install.packages("shinydashboardPlus")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("shinyEffects")) install.packages("shinyEffects")
if (!require("shinybusy")) install.packages("shinybusy")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("bslib")) install.packages("bslib")
if (!require("bsplus")) install.packages("bsplus")
if (!require("bsicons")) install.packages("bsicons")
if (!require("dplyr")) install.packages("dplyr")
if (!require("purrr")) install.packages("purrr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("forcats")) install.packages("forcats")
if (!require("tibble")) install.packages("tibble")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("sf")) install.packages("sf")
if (!require("leaflet")) install.packages("leaflet")
if (!require("leaflet.extras")) install.packages("leaflet.extras")
if (!require("plotly")) install.packages("plotly")
if (!require("readxl")) install.packages("readxl")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("janitor")) install.packages("janitor")
if (!require("elevatr")) install.packages("elevatr")
if (!require("stars")) install.packages("stars")
if (!require("raster")) install.packages("raster")
if (!require("units")) install.packages("units")
if (!require("fresh")) install.packages("fresh")
if (!require("zip")) install.packages("zip")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("ggforce")) install.packages("ggforce")
if (!require("gt")) install.packages("gt")
if (!require("dipsaus")) install.packages("dipsaus")
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyEffects)
library(shinybusy)
library(shinyFiles)
library(shinyjs)
library(bslib)
library(bsplus)
library(bsicons)
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(tibble)
library(stringr)
library(stringi)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(readxl)
library(openxlsx)
library(janitor)
library(elevatr)
library(stars)
library(units)
library(fresh)
library(zip)
library(ggthemes)
library(ggforce)
library(gt)
library(dipsaus)

options(shiny.maxRequestSize=30*1024^2)

# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#35978F"
  ),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#d9e6eb",
    dark_hover_bg = "#7b92a8",
    dark_color = "#2E3440",
    dark_submenu_bg = ,
    dark_submenu_hover_color = ,
    dark_submenu_color = ,
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF",
    info_box_bg = "#D8DEE9"
  )
)

# Cargar Datos ----
comunas <- read_sf('N:/Dashboard PAS 150/COMUNAS/COMUNAS_v1.shp')

# Cargar modulos ----
source("Modules/leer_sf.R")

# Cargar funciones
source("Functions/Accordion_info.R")
source("Functions/carto_digital.R")
source("Functions/BD_Biodiversidad.R")
source("Functions/BD_Inventarios.R")
source("Functions/BD_fragmentacion.R")
source("Functions/Estadisticos.R")

get_cuenca <- function(uso_veg,crs){
  uso_veg <- uso_veg %>% st_union()
  cuencas <- read_sf('N:/Dashboard PAS 150/SubsubcuencasBNA/Subsubcuencas_BNA.shp') %>% 
    st_zm() %>% 
    st_transform(crs)
  cuenca <- cuencas[uso_veg,][which.max(st_area(st_intersection(cuencas,uso_veg))),] 
  return(cuenca)
}

get_BNP_cuenca <- function(uso_veg, sp, cuenca){
  uso_veg %>% 
    st_zm() %>% 
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>% 
    rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% 
    rename_at(vars(contains("ecc")), str_to_upper) %>% 
    filter(str_detect(F_ley20283, "preser") & str_detect(BNP_ECC,sp)) %>% 
    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2),
           NOM_SSUBC = cuenca$NOM_SSUBC) %>% 
    drop_units() %>% 
    select(NOM_SSUBC, Formacion, Tipo_for, Subtipo_fo, starts_with('ECC'), F_ley20283, BNP_ECC, Sup_ha) %>% 
    relocate(Sup_ha, .before = geometry) %>% 
    arrange(Sup_ha)
} 

