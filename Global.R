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
library(tibble)
library(stringr)
library(stringi)
library(sf)
library(forcats)
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
# devtools::install_github("DavidJMartinezS/dataPAS")
library(dataPAS)

options(shiny.maxRequestSize=150*1024^2, timeout = 100)

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

css <- HTML(
  "/* move logo to center */
    #logo {
        position: absolute;
        left: 50%;
        top: 50%;
        transform: translate(-50%, -50%);
    }
    /* remove hover effect */
    #logo > a:hover {
        background-color: transparent !important;
        color: transparent !important;
    }"
)

# Cargar Datos ----
comunas <- read_sf(system.file("Comunas.gdb", package = "dataPAS"))

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
  cuencas <- read_sf(system.file("Subsubcuencas.gdb", package = "dataPAS")) %>% 
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
    mutate(
      Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2),
      NOM_SSUBC = cuenca$NOM_SSUBC
    ) %>% 
    drop_units() %>% 
    select(NOM_SSUBC, Formacion, Tipo_for, Subtipo_fo, starts_with('ECC'), F_ley20283, BNP_ECC, Sup_ha) %>% 
    relocate(Sup_ha, .before = geometry) %>% 
    arrange(Sup_ha)
} 

