
shinyUI(
  dashboardPage(
    skin = "green",
    # Header ----
    dashboardHeader(
      title = "IE Dashboard",
      # dropdownMenuOutput(
      #   "menu"
      # ),
      userOutput("user")
    ),
    
    # Sidebar ----
    dashboardSidebar(
      # sidebarSearchForm(textId = "SearchText",buttonId = "ButtonSearch",label = "Buscar"),
      sidebarMenu(
        menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
        menuItem("Cartografía digital", tabName = "carto_digital", icon = icon("map")),
        menuItem("Info Cuenca", id = "info", icon = icon("info"),
          menuSubItem("Info subsubcuenca", tabName = "info_cuenca", icon = icon("circle-info")),
          menuSubItem("Uso suelo", tabName = "uso", icon = icon("map")),
          menuSubItem("Vegetación", tabName = "veg", icon = icon("leaf"))
        ),
        menuItem("BNP", id = "bnp", icon = icon("tree-deciduous", lib = "glyphicon"),
          menuSubItem("Inventarios forestales", tabName = "fore", icon = icon("tree")),
          menuSubItem("Intervención", tabName = "inter", icon = icon("person-digging")),
          menuSubItem("Biodiversidad", tabName = "bio", icon = bs_icon("flower2"))
        ),
        menuItem("Analisis de amenazas", tabName = "lsm", icon = icon("layer-group"))
        # menuItem("Link google", href = "https://www.google.com", icon = icon("google"))
        
      )
      # tags$div(
      #   class = "sidebar-footer",
      #   helpText("Agrega alguna nota de recordatorio"),
      #   actionButton("addItem", "Añadir nota",icon("plus"))
      # )
    ),
    
    # Body ----
    dashboardBody(
      # use_theme(mytheme),
      # use a bit of shinyEffects
      shinyEffects::setShadow(class = "dropdown-menu"),
      shinyEffects::setShadow(class = "box"),
      tags$head(tags$style(HTML(".small-box {height: 120px;}"))),
      tags$head(tags$style(
        ".progress-bar{background-color:#3c763d;}"
      )),
    #   tags$head(tags$script('
    #   // Define function to set height of "leaf_subusos" and "box_map_sub"
    #   setHeight = function() {
    #     var window_height = $(window).height();
    #     var header_height = $(".main-header").height();
    # 
    #     var boxHeight = window_height - header_height - 30;
    # 
    #     $("#box_tabla_subusos, #box_map_fore").height(boxHeight);
    #     $("#leaf_inv_fore").height(boxHeight - 30);
    #   };
    # 
    #   // Set input$box_height when the connection is established
    #   $(document).on("shiny:connected", function(event) {
    #     setHeight();
    #   });
    # 
    #   // Refresh the box height on every window resize event    
    #   $(window).on("resize", function(){
    #     setHeight();
    #   });
    # ')),
      tabItems(
        tabItem(
          tabName = "importante",
          fluidRow(
            box(
              title = "Uso adecuado del Dashboard",
              width = 12,
              info_dashboard()
            )
          )
        ),
        tabItem(
          tabName = "carto_digital",
          fluidRow(
            box(
              width = 4,
              title = "Inputs cartografía digital",
              solidHeader = TRUE,
              status = "success",
              h3("Datos del proyecto (opcionales)"),
              textInput("nom_proj", "Nombre del proyecto"),
              textInput("cod_proj", "Código del proyecto"),
              hr(),
              h3("Shapes y bases de datos de la cuenca"),
              leer_sfUI("uso_veg_id", "Ingrese cartografía de uso y vegetación"),
              selectInput(
                "sp",
                "Especie en categoría:",
                c("Porlieria chilensis", "Prosopis Chilensis", "Carica chilensis", "Echinopsis chilensis", "Kageneckia angustifolia", "Drimys winteri", "Crinodendron patagua", "Jubaea chilensis") %>% sort(),
                "Porlieria chilensis"
              ),
              radioButtons("alt_lgl", "¿Existe alteración de hábitat?", choices = c("Si","No"), selected = "No"),
              uiOutput("leer_alt"),
              leer_sfUI("obras_id", "Ingrese shapefile de obras"),
              leer_sfUI("censo_id", "Ingrese shapefile de censo"),
              fileInput("bd_flora_id", "Ingrese parcelas de inventario florístico", accept = c(".xlsx")),
              fileInput("bd_fore_id", "Ingrese parcelas de inventario forestal", accept = c(".xlsx")),
              # fileInput("bd_trans", "Ingrese parcelas de transectos lineales", accept = c(".xlsx")),
              uiOutput("ui_densidad"),
              uiOutput("down_carto_ui")
            ),
            tabBox(
              width = 8,
              title = "Check Uso-veg.",
              id = "tabcheck",
              tabPanel("Atributos por campo", verbatimTextOutput("check_1")),
              tabPanel("Usos, Subusos, Formacion, F_ley20283", verbatimTextOutput("check_2")),
              tabPanel("Uso, Tipo_for, Subtipo_fo, F_ley20283", verbatimTextOutput("check_3")),
              tabPanel("F_ley20283, ECC, BNP_ECC", verbatimTextOutput("check_4"))
            )
          ),
          fluidRow(
            box(
              width = 4,
              title = "Analisis de Biodiversidad e Inventarios forestales",
              solidHeader = T,
              status = "success",
              h3("Análisis de Biodiversidad"),
              radioButtons(
                "bd_bio_lgl",
                "¿Utilizar BD generada automáticamente o desea ingresar una?",
                choices = c("Automática, a partir de la BD ya ingresada", "Ingresar BD manual"),
                selected = "Automática, a partir de la BD ya ingresada"
              ),
              uiOutput("leer_bd_flora2"),
              downloadButton("down_bd_bio", 
                             "Descargar Excel",
                             style = "color: #fff; background-color: #27ae60; border-color: #fff"),
              hr(),
              h3("Análisis de Inventarios forestales"),
              radioButtons(
                "bd_fore_lgl",
                "¿Utilizar BD generada automáticamente o desea ingresar una?",
                choices = c("Automática, a partir de la BD ya ingresada", "Ingresar BD manual"),
                selected = "Automática, a partir de la BD ya ingresada"
              ),
              uiOutput("leer_bd_fore2"),
              downloadButton("down_bd_fore", 
                             "Descargar Excel",
                             style = "color: #fff; background-color: #27ae60; border-color: #fff")
            ),
            tabBox(
              width = 8,
              title = "Check Otros",
              id = "tabcheck_2",
              tabPanel("SHP Obras", verbatimTextOutput("check2_1")),
              tabPanel("SHP BNP Alterar", verbatimTextOutput("check2_2")),
              tabPanel("SHP Censos", verbatimTextOutput("check2_3")),
              tabPanel("BD Flora", verbatimTextOutput("check2_4")),
              tabPanel("BD Inv. Forestales", verbatimTextOutput("check2_5"))
            )
          )
        ),
        tabItem(
          tabName = "info_cuenca",
          column(
            width = 5,
            fluidRow(valueBoxOutput("vb_sp", width = 12), style = "height:150px;"), 
            fluidRow(valueBoxOutput("vb_nom_ssubc", width = 12), style = "height:150px;"),
            fluidRow(valueBoxOutput("vb_localizacion", width = 12), style = "height:150px;"), 
            fluidRow(valueBoxOutput("vb_sup_ssubc", width = 12), style = "height:150px;")
          ),
          column(
            width = 7,
            box(
              id = "box_map_cuenca",
              width = 12,
              title = "Mapa subsubcuenca",
              solidHeader = T,
              status = "success",
              leafletOutput("leaf_cuenca", height = "890px")
            )
          )
        ),
        tabItem(
          tabName = "uso",
          fluidRow(
            column(width = 6,
            box(
              id = "box_tabla_subusos",
              width = 12,
              solidHeader = T,
              status = "success",
              title = "Tabla usos y subusos de suelo",
              gt_output("gt_subusos")
            )
            ),
            column(width = 6,
            box(
              id = "box_map_sub",
              width = 12,
              solidHeader = T,
              status = "success",
              title = "Mapa subusos de suelo",
              leafletOutput("leaf_subusos", height = "890px")
            )
            )
          )
        ),
        tabItem(
          tabName = "veg",
          fluidRow(
            column(
              4,
              valueBoxOutput("vb_sup_bnp", width = 12),
              valueBoxOutput("vb_sup_fx", width = 12),
              valueBoxOutput("vb_n_parc", width = 12),
              # fluidRow(
              #   div(
              #     infoBoxOutput("ib_n_parc", width = 6),
              #     infoBoxOutput("ib_n_spp", width = 6),
              #     style = "left: 15px; margin-right:30px; position: relative;"
              #   )
              # ),
              tabBox(
                width = 12,
                title = "Plotly",
                height = "510px",
                tabPanel("Habito", plotlyOutput("plt_habito")),
                tabPanel("Origen", plotlyOutput("plt_origen"))
              )
            ),
            column(
              8,
              box(
                title = "Tipos forestales",
                width = 12,
                solidHeader = T,
                collapsible = T,
                status = "success",
                gt_output("gt_tf")
              ),
              box(
                width = 12, 
                solidHeader = T, 
                status = "success",
                title = "Mapa",
                leafletOutput("leaf_bnp", height = "600px")
              )
            )
          )
        ),
        tabItem(
          tabName = "fore",
          column(
            width = 5,
            box(
              width = 12,
              height = "450px",
              title = "Tabla IVI",
              solidHeader = T,
              collapsible = T,
              status = "success",
              gt_output("gt_ivi")
            ),
            box(
              width = 12,
              height = "450px",
              title = "Estadísticos",
              solidHeader = T,
              collapsible = T,
              status = "success",
              gt_output("gt_estadisticos")
            )
          ),
          column(
            width = 7,
            box(
              width = 12,
              height = "380px",
              title = "Intervalos de confianza de la densidad media por estado de desarrollo",
              solidHeader = T,
              collapsible = T,
              status = "success",
              gt_output("gt_IC_prop")
            ),
            box(
              id = "box_map_fore",
              width = 12,
              height = "550px",
              title = "Mapa BNP y parcelas forestales",
              solidHeader = T,
              collapsible = T,
              status = "success",
              leafletOutput("leaf_inv_fore", height = "540px")
            )
          )
        ),
        tabItem(
          tabName = "inter",
          tabBox(
            width = 12,
            title = "N° individuos y superficie a intervenir y alterar",
            tabPanel("Censo a Intervenir", gt_output("gt_c_inter")),
            tabPanel("Censo a Alterar", gt_output("gt_c_alter")),
            tabPanel("Superficies", gt_output("gt_sup_inter"))
          )
        ),
        tabItem(
          tabName = "bio",
          fluidRow(
            # width = 6,
            box(
              width = 6,
              # height = 550,
              title = "Composición BNP por piso",
              solidHeader = T,
              collapsible = T,
              status = "success",
              gt_output("gt_compo_x_piso")
            ),
            box(
              width = 6,
              # height = 550,
              title =  p(
                "Riqueza por sector",
                downloadButton(
                  "down_riqueza",
                  "",
                  icon = icon("download"),
                  class = "btn-s",
                  style = "position: absolute; right: 40px"
                ),
              ),
              solidHeader = T,
              collapsible = T,
              status = "success",
              plotOutput("plot_riqueza",height = 600)
            )
          ),
          fluidRow(
            # width = 6,
            box(
              width = 6,
              height = 350,
              title = "Frecuencia de especies acompañantes",
              solidHeader = T,
              collapsible = T,
              status = "success",
              gt_output("gt_spp_acomp")
            ),
            box(
              width = 6,
              height = 350,
              title = "Índices de biodiversidad por sector",
              solidHeader = T,
              collapsible = T,
              status = "success",
              gt_output("gt_indices_x_sector")
            )
          )
        ),
        tabItem(
          tabName = "lsm",
          shinyjs::useShinyjs(),
          box(
            title = "Inputs & Outputs",
            width = 12,
            solidHeader = T, 
            collapsible = T,
            status = "success",
            shinyWidgets::pickerInput(
              inputId = "select_subusos",
              label = "Seleccionar subusos que no sean de vegetación (incluir terrenos agrícolas)", 
              choices = NULL,
              multiple = TRUE
            ),
            p("Ingrese XLS con los resultados del FragStats. La primera y segunda hoja del documento, 
              deben corresponder a las condiciones ANTES y DESPUES del proyecto, respectivamente.",
              style = "font-weight: bold;"),
            div(
              style = "display:inline-block;text-align: left;",
              splitLayout(
                cellWidths = c("40%", "10%","50%"),
                fileInput(
                  "input_lsm",
                  "",
                  # width = "500px",
                  accept = c(".xls")
                ),
                actionButtonStyled(
                  "apply_lsm",
                  label = "Generar análisis",
                  style = "margin-top:20px; margin-left:25px;",
                  # icon = icon("download"),
                  disabled = T
                ),
                downloadButton(
                  "down_lsm",
                  "Descargar Excel",
                  style = "margin-top:20px; color: #fff; background-color: #27ae60; border-color: #fff"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              box(
                width = 12,
                title = "Matriz del paisaje",
                solidHeader = T, 
                collapsible = T,
                status = "success",
                gt_output("gt_lsm_matriz")
              ),
              box(
                width = 12,
                title = "Parámetros de fragmentación",
                solidHeader = T, 
                collapsible = T,
                status = "success",
                gt_output("gt_lsm_frag")
              )
            ),
            column(
              width = 6,
              box(
                width = 12,
                title = "Resultados análisis de amenazas",
                solidHeader = T, 
                collapsible = T,
                status = "success",
                gt_output("gt_lsm_resultados")
              )
            )
          )
        )
      )
    )
  )
)
