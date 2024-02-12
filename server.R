
shinyServer(function(input,output, session){
  
  # Add notifications ----
  # notification <-  reactiveValues()
  # output$menu <- renderMenu({
  #   items <- lapply(notification, function(el) {
  #     notificationItem(text = el$text,icon = icon("caret-right"))
  #   })
  #   dropdownMenu(
  #     type = "notification", badgeStatus = "warning",icon = icon("circle-exclamation"),
  #     .list = items
  #   )
  # })
  # 
  # observeEvent(input$addItem, {
  #   showModal(modalDialog(title = "Añada alguna nota a sus notificaciones",
  #                         textInput(paste0("text", input$addItem), "Nota"),
  #                         actionButton(paste0("go", input$addItem), "Añadir notificación"),
  #                         easyClose = TRUE, footer = NULL
  #   ))
  #   
  #   observeEvent(input[[paste0("go", input$addItem)]], {
  #     notification[[paste0(input$addItem)]] <- list(
  #       text = input[[paste0("text", input$addItem)]]
  #     )
  #     removeModal()
  #   })
  # })
  
  # outputs page 'Cartografía digital' ----
  uso_veg <- callModule(module = leer_sf, id = "uso_veg_id")
  observeEvent(input$alt_lgl,{
    output$leer_alt <- renderUI({
      if (input$alt_lgl == "Si") {
        leer_sfUI("bnp_alterar_id", "Ingrese shapefile de alteración de hábitat")
      }
    })
  })
  BNP_alterar <- callModule(module = leer_sf, id = "bnp_alterar_id")
  
  observeEvent(input$bd_bio_lgl,{
    output$leer_bd_flora2 <- renderUI({
      if (input$bd_bio_lgl == "Ingresar BD manual") {
        fileInput("bd_xlsx_bio", "Ingrese BD de flora a utilizar", accept = c(".xlsx"))
      }
    })
  })
  BD_xlsx_bio <- reactive({
    read.xlsx(input$bd_xlsx_bio$datapath)
  }) 
  
  observeEvent(input$bd_fore_lgl,{
    output$leer_bd_fore2 <- renderUI({
      if (input$bd_fore_lgl == "Ingresar BD manual") {
        fileInput("bd_xlsx_fore", "Ingrese BD de inventarios forestales a utilizar", accept = c(".xlsx"))
      }
    })
  })
  BD_xlsx_fore <- reactive({
    read.xlsx(input$bd_xlsx_fore$datapath)
  }) 
  
  obras <- callModule(module = leer_sf, id = "obras_id")
  censo <- callModule(module = leer_sf, id = "censo_id")
  output$check_1 <- renderPrint({
    if (c("uso", "subuso","formacion","tipo_for","subtipo_fo","f_ley20283","bnp_ecc") %in% names(
      uso_veg() %>% rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII")))
    ) %>% sum() == 7) {
      return(
        uso_veg() %>%
          rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII"))) %>%
          dplyr::select("uso", "subuso","formacion","tipo_for","subtipo_fo","f_ley20283","bnp_ecc") %>% 
          st_drop_geometry() %>% 
          sapply(unique)
      ) 
    } else {
      return(
        paste("Por favor utilizar los campos que se mencionan en las instrucciones del dashboard")
      )
    }
  })
  output$check_2 <- renderPrint({
    if (c("uso", "subuso","formacion","tipo_for","subtipo_fo","f_ley20283","bnp_ecc") %in% names(
      uso_veg() %>% rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII")))
    ) %>% sum() == 7) {
      return(
        uso_veg() %>%
          rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII"))) %>%
          st_drop_geometry() %>% 
          group_by(uso, subuso, formacion, f_ley20283) %>% 
          tally()
      ) 
    } else {
      return(
        paste("Por favor utilizar los campos que se mencionan en las instrucciones del dashboard")
      )
    }
  })
  output$check_3 <- renderPrint({
    if (c("uso", "subuso","formacion","tipo_for","subtipo_fo","f_ley20283","bnp_ecc") %in% names(
      uso_veg() %>% rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII")))
    ) %>% sum() == 7) {
      return(
        uso_veg() %>%
          rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII"))) %>%
          st_drop_geometry() %>% 
          group_by(uso, tipo_for, subtipo_fo, f_ley20283) %>% 
          tally()
      ) 
    } else {
      return(
        paste("Por favor utilizar los campos que se mencionan en las instrucciones del dashboard")
      )
    }
  })
  output$check_4 <- renderPrint({
      if (c("uso", "subuso","formacion","tipo_for","subtipo_fo","f_ley20283","bnp_ecc") %in% names(
        uso_veg() %>% rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII")))
      ) %>% sum() == 7) {
        return(
          uso_veg() %>%
            rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII"))) %>%
            st_drop_geometry() %>% 
            group_by(uso, f_ley20283, across(starts_with("ecc")), bnp_ecc) %>% 
            tally()
        ) 
      } else {
        return(
          paste("Por favor utilizar los campos que se mencionan en las instrucciones del dashboard")
        )
      }
    })
  BD_flora <- reactive({
    req(input$bd_flora_id)
    read.xlsx(input$bd_flora_id$datapath) %>% as_tibble()
  }) 
  BD_fore <- reactive({
    req(input$bd_fore_id)
    read.xlsx(input$bd_fore_id$datapath) %>% as_tibble()
  }) 
  # BD_trans <- reactive({
  #   read.xlsx(input$bd_trans_id$datapath) %>% as_tibble()
  # }) 
  output$check2_1 <- renderPrint({
    if (c("obra") %in% names(
      obras() %>% rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII")))
    )) {
      return(
        "Bien, contiene el campo 'Obra'"
      ) 
    } else {
      return(
        "El shp debe contener al menos el campo 'Obra', como se señala en las instrucciones"
      )
    }
  })
  output$check2_2 <- renderPrint({
    if (c("uso", "subuso","formacion","tipo_for","subtipo_fo","f_ley20283","bnp_ecc") %in% names(
      uso_veg() %>% rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII")))
    ) %>% sum() == 7) {
      return(
        "Bien, contiene el campo 'Obra'"
      ) 
    } else {
      return(
        "El shp debe contener al menos el campo 'Obra', como se señala en las instrucciones"
      )
    }
  })
  output$check2_3 <- renderPrint({
    if ("especie" %in% names(
      censo() %>% rename_all(~str_to_lower(stri_trans_general(.,"Latin-ASCII")))
    )) {
      return(
        "Bien, contiene el campo 'Especie'"
      ) 
    } else {
      return(
        "El shp debe contener al menos el campo 'Especie', como se señala en las instrucciones"
      )
    }
  })
  output$check2_4 <- renderPrint({
    if (c('parcela', 'sup_parcela', 'utm_e', 'utm_n', 'especie', 'n_ind', 'habito', 'origen', 'rce', 'ds_68') %in% names(
      BD_flora() %>%
      rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII")))
    ) %>% all() &
    BD_flora() %>%
    rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII"))) %>%
    summarise_all(is.numeric) %>%
    dplyr::select('sup_parcela', 'utm_e', 'utm_n', 'n_ind') %>% 
    all()) {
      return(
        list(
          "Bien, contiene los campos requeridos con sus formatos correctos",
          BD_flora() %>% str()
        )
      ) 
    } else if (c('parcela', 'sup_parcela', 'utm_e', 'utm_n', 'especie', 'n_ind', 'habito', 'origen', 'rce', 'ds_68') %in% names(
      BD_flora %>%
      rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII")))
    ) %>% all() &
    BD_flora %>%
    rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII"))) %>%
    summarise_all(is.numeric) %>%
    dplyr::select('sup_parcela', 'utm_e', 'utm_n', 'n_ind') %>% 
    all() == F) {
      return(
        list(
          "Los campos 'Sup_parcela', 'UTM_E', 'UTM_N', 'N_ind' deben ser de tipo numéricos",
          BD_flora() %>% str()
        )
      )
    } else {
      return(
        list(
        "Por favor utilizar los campos que se mencionan en las instrucciones del dashboard",
        nombres = BD_flora() %>% names()
        )
      )
    }
  })
  output$check2_5 <- renderPrint({
    if (c('parcela', 'sup_parcela', 'utm_e', 'utm_n', 'especie', 'estado', 'dap', 'altura', 'n_ind') %in% names(
      BD_fore() %>%
      rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII")))
    ) %>% all() &
    BD_fore() %>%
    rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII"))) %>%
    summarise_all(is.numeric) %>%
    dplyr::select('sup_parcela', 'utm_e', 'utm_n', 'dap', 'altura', 'n_ind') %>% 
    all()) {
      return(
        list(
          "Bien, contiene los campos requeridos con sus formatos correctos",
          BD_fore() %>% str()
        )
      ) 
    } else if (c('parcela', 'sup_parcela', 'utm_e', 'utm_n', 'especie', 'estado', 'dap', 'altura', 'n_ind') %in% names(
      BD_fore() %>%
      rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII")))
    ) %>% all() &
    BD_fore() %>%
    rename_all( ~ str_to_lower(stri_trans_general(., "Latin-ASCII"))) %>%
    summarise_all(is.numeric) %>%
    dplyr::select('sup_parcela', 'utm_e', 'utm_n', 'dap', 'altura', 'n_ind') %>% 
    all() == F) {
      return(
        list(
          "Los campos 'Sup_parcela', 'UTM_E', 'UTM_N', 'DAP', 'Altura', 'N_ind' deben ser de tipo numéricos",
          BD_fore() %>% str()
        )
      )
    } else {
      return(
        list(
          "Por favor utilizar los campos que se mencionan en las instrucciones del dashboard",
          nombres = BD_fore() %>% names()
        )
      )
    }
  })
  
  densidad <- reactive({
    req(Inventarios())
    Inventarios()$IVI[Inventarios()$IVI$Especie == input$sp,][[2]]
  })
  observeEvent(Inventarios(),{
    output$ui_densidad <- renderUI({
      tags$div(
        tags$p("Densidad a partir de los datos: ", densidad()),
        shinyWidgets::numericInputIcon(
          inputId = "densidad",
          label = "Ingrese densidad de la especie ",
          value = densidad(),
          icon = list(NULL, "Ind/ha")
        ),
        actionButton("get_carto_btn", "Generar cartografía digital", class = "btn-primary",icon = icon("map")),
      )
    })
    # updateNumericInput(inputId = "densidad", value = Inventarios()$IVI[Inventarios()$IVI$Especie == input$sp,][[2]])
  })
  
  carto_digital <- eventReactive(input$get_carto_btn,{
    req(c(uso_veg(),obras(),censo(),BD_flora(),BD_fore(), Inventarios()))
    if (input$alt_lgl == "Si") {
      get_carto_digital(
        uso_veg = uso_veg(),
        BNP_alt = BNP_alterar(),
        obras = obras(),
        censo = censo(),
        sp = input$sp,
        BD_flora = BD_flora(),
        BD_fore = BD_fore(),
        densidad = input$densidad
      )
    } else {
      get_carto_digital(
        uso_veg = uso_veg(),
        obras = obras(),
        censo = censo(),
        sp = input$sp,
        BD_flora = BD_flora(),
        BD_fore = BD_fore(),
        densidad = input$densidad
      )
    }
  })
  observeEvent(input$get_carto_btn,{
    req(c(uso_veg(),obras(),censo(),BD_flora(),BD_fore()))
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Generando cartografía digital.",br()," Por favor espere, esto puede tardar unos minutos"))
    )
    req(carto_digital())
    remove_modal_spinner()
    output$down_carto_ui <- renderUI({
      if (input$get_carto_btn){
        tags$div(
          tags$hr(),
          tags$h5("Ya puede descargar su cartografía digital!! Seleccione el directorio y luego descargue su cartografía", 
                  style = "font-weight: bold;"),
          splitLayout(
            shinyDirButton(
              "directory",
              label = NULL,
              title = "Select directory",
              multiple = FALSE,
              icon = icon("folder"),
              viewtype = "detail",
              style = "padding: 10px 12px;background-color: #008CBA;border-radius: 12px;"
            ), 
            actionButtonStyled(
              "down_carto_btn",
              label = NULL,
              style = "padding: 10px 15px;border-radius: 50%;background-color: #04AA6D;",
              icon = icon("download"),
              disabled = T
            ), 
            verbatimTextOutput("dir_out_output"), 
            cellWidths = c("10%","10%", "80%")
          )
        )
      }
    })
  })
  
  roots <- c(wd = path.expand("~"))
  shinyDirChoose(
    input,
    id = "directory",
    roots = roots,
    updateFreq = 0,
    session,
    defaultPath = "",
    defaultRoot = NULL,
    allowDirCreate = TRUE
  )
  
  observeEvent(input$directory, {
    updateActionButtonStyled(
      session,
      "down_carto_btn", 
      disabled = c(TRUE, FALSE)[((all(c("root", "path") %in% names(input$directory))) %% 2) + 1])
  })
  
  directorio <- reactive({
    if(all(c("root", "path") %in% names(input$directory))){
      selected_path <- do.call(file.path, c(roots[input$directory$root], input$directory$path))
    } else {
      selected_path <- nullfile()
    }
    return(selected_path)
  })
  
  output$dir_out_output <- renderPrint({
    if (dir.exists(directorio())) {
      directorio()
    } else {
      "Directorio no seleccionado"
    }
  })
  
  observeEvent(input$down_carto_btn,{
    req(directorio())
    temp_dir <- tempdir()
    
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Descargando cartografía digital.",br()," Por favor espere, esto puede tardar unos segundos"))
    )
    down_carto_digital(carto_digital(), temp_dir, input)
    remove_modal_spinner()
    
    zip_file <- file.path(temp_dir, "Compilado_Carto_digital.zip")
    list_files <- list.files(temp_dir,
                             ".dbf$|.prj$|.shp$|.shx$|.kml&|.xlsx$",
                             full.names = TRUE)
    zip::zipr(zipfile = zip_file, files = Sys.glob(list_files))
    file.copy(zip_file, directorio(),overwrite = T)
    if (length(Sys.glob(list_files)) > 0) file.remove(Sys.glob(list_files))
    shinyalert::shinyalert(
      title = "Listo!", 
      text = paste0("Su cartografía digital ha sido descargada en:\n", directorio()),
      type = "success",
      closeOnEsc = T, 
      showConfirmButton = T,
      animation = TRUE
    )
  })
  
  crs_epsg <- reactive({
    req(uso_veg())
    if_else(
      uso_veg() %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] >= -72,
      32719, 
      32718
    )
  })
  cuenca <- reactive({
    req(uso_veg())
    get_cuenca(uso_veg(),crs_epsg())
  })
  BNP_cuenca <- reactive({
    req(c(uso_veg(), cuenca()))
    get_BNP_cuenca(uso_veg = uso_veg(), sp = input$sp, cuenca = cuenca())
  })
  
  # BD Biodiversidad ----
  Biodiversidad <- reactive({
    req(c(BD_flora(), obras(), BNP_cuenca(), input$nom_proj))
    get_BD_Diversidad(
      BD_flora = BD_flora(), 
      Obras = obras(), 
      BNP_cuenca = BNP_cuenca(), 
      input = input
    )
  })
  output$down_bd_bio <- downloadHandler(
    filename = function(){
      paste0(paste(input$cod_proj,"BD_Biodiversidad",substr(input$sp,1,4), sep = "_"),".xlsx")
    },
    content = function(file){
      req(Biodiversidad())
      saveWorkbook(Biodiversidad()$wb, file, overwrite = T)
    }
  )
  # BD Inventarios forestales ----
  Inventarios <- reactive({
    req(c(BD_fore(), cuenca(), BNP_cuenca, input$nom_proj))
    BD_inv_for(
      BD_fore = BD_fore(),
      BNP_cuenca = BNP_cuenca(),
      cuenca = cuenca(),
      input = input
    )
  })
  output$down_bd_fore <- downloadHandler(
    filename = function(){
      paste0(paste(input$cod_proj,"BD_Inv.Forestal_",substr(input$sp,1,4), sep = "_"),".xlsx")
    },
    content = function(file){
      req(Inventarios())
      saveWorkbook(Inventarios()$wb, file, overwrite = T)
    }
  )
  
  # outputs page 'Info cuenca' ----
  out_info <- reactive({
    req(c(uso_veg(), cuenca()))
    comunas <- comunas %>% st_transform(st_crs(cuenca()))
    nom_cuenca_vb <- cuenca() %>% pull(NOM_SSUBC)
    info_cuenca <-
      st_intersection(cuenca(), comunas) %>%
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% drop_units() %>%
      group_by(REGION, PROVINCIA, COMUNA) %>% summarise(Sup_ha = sum(Sup_ha)) %>%
      ungroup() %>%
      mutate(label_div = pmap_chr(list(REGION, PROVINCIA, COMUNA), function(.x, .y, .z) str_c(.x, .y, .z, sep = "-")))
    
    sup_cuenca_vb <-
      str_c((uso_veg()$Sup_ha %>% sum()) %>% format(decimal.mark = ",", big.mark = "."),
            " (",
            ((uso_veg()$Sup_ha %>% sum()) /
               (
                 comunas %>%
                   filter(REGION == info_cuenca %>% slice_max(Sup_ha) %>% pull(REGION)) %>%
                   st_make_valid() %>%
                   group_by(REGION) %>%
                   tally() %>%
                   mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>%
                   drop_units() %>%
                   st_drop_geometry() %>%
                   pull(Sup_ha)
               )
            ) %>%
              `*`(100) %>% round(2) %>% format(decimal.mark = ",", big.mark = "."),
            "%)"
      )
    colf_com <- colorFactor(ggthemes_data$tableau$`color-palettes`$regular$`Tableau 20`$value[1:(info_cuenca$COMUNA %>% length())],info_cuenca$COMUNA)
    return(
      list(
        nom_cuenca_vb = nom_cuenca_vb,
        info_cuenca = info_cuenca,
        sup_cuenca_vb = sup_cuenca_vb,
        colf_com = colf_com
      )
    )
  })
  output$vb_nom_ssubc <- renderValueBox({
    req(out_info())
    valueBox(
      tags$p(out_info()$nom_cuenca_vb, style = "font-size: 20px;"),
      "Nombre de la cuenca",
      icon = icon("book-atlas"),
      color = "teal"
    )
  })
  output$vb_sp <- renderValueBox({
    req(input$sp)
    valueBox(
      tags$p(input$sp, style = "font-size: 20px;"),
      "Especie",
      icon = icon("leaf"),
      color = "olive"
    )
  })
  output$vb_localizacion <- renderValueBox({
    req(out_info())
    valueBox(
      tags$p(out_info()$info_cuenca %>% slice_max(Sup_ha) %>% pull(label_div), style = "font-size: 20px;"),
      "División Politico-Administrativa",
      icon = icon("location-crosshairs"),
      color = "blue"
    )
  })
  output$vb_sup_ssubc <- renderValueBox({
    req(out_info())
    valueBox(
      tags$p(out_info()$sup_cuenca_vb, style = "font-size: 20px;"),
      "Superficie de la cuenca (ha) - Porcentaje región (%)",
      icon = icon("book-atlas"),
      color = "light-blue"
    )
  })
  output$leaf_cuenca <- renderLeaflet({
    out_info()$info_cuenca %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>% 
      addProviderTiles(providers$OpenStreetMap, group ="OpenStreetMap") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>% 
      addProviderTiles(providers$Esri.DeLorme, group ="Esri.DeLorme") %>% 
      addPolygons(
        fillColor = ~out_info()$colf_com(COMUNA),
        fillOpacity = .5,
        group = "Comunas",
        weight = 1,
        color = "black",
        label = ~ COMUNA,
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2.5,
          bringToFront = TRUE
        )
      ) %>% 
      addLayersControl(
        overlayGroups = c("Comunas"),
        baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      addResetMapButton() %>% 
      addMiniMap(
        position = "topright",
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        minimized = FALSE
      ) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters"
      ) %>% 
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(imperial = FALSE)
      )
  })
  
  # outputs page 'USOS' ----
  output$gt_subusos <- render_gt({
    req(uso_veg())
    uso_veg <- uso_veg() %>% 
      st_zm() %>% 
      st_make_valid() %>%
      st_collection_extract("POLYGON") %>% 
      rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% 
      rename_at(vars(contains("ecc")), str_to_upper)
    
    uso_veg %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
      drop_units() %>% 
      group_by(Uso,Subuso) %>% 
      summarise(Superficie_ha = sum(Sup_ha),.groups = "drop") %>% 
      mutate(Representatividad = Superficie_ha/sum(Superficie_ha)) %>%  
      st_drop_geometry() %>% 
      bind_rows(
        uso_veg %>% 
          mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
          drop_units() %>% 
          group_by(Uso) %>% 
          summarise(Superficie_ha = sum(Sup_ha),.groups = "drop") %>% 
          mutate(Representatividad = Superficie_ha/sum(Superficie_ha),
                 Subuso = str_c('Subtotal ',Uso)) %>% 
          st_drop_geometry()
      ) %>% 
      bind_rows(
        uso_veg %>%
          mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>%
          drop_units() %>%
          summarise(Superficie_ha = sum(Sup_ha)) %>%
          mutate(Representatividad = Superficie_ha/sum(Superficie_ha),
                 Uso = "Total",
                 Subuso = "Total") %>%
          st_drop_geometry()
      ) %>%
      mutate_if(is.character,stri_trans_general,"Latin-ASCII") %>% 
      arrange(Uso) %>% 
      # gt table
      gt() %>% 
      fmt_percent(
        columns = Representatividad,
        decimals = 2,
        dec_mark = ','
      ) %>% 
      fmt_number(
        columns = Superficie_ha,
        sep_mark = '.',
        dec_mark = ','
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold"
        ),
        locations = list(
          cells_body(
            rows = str_detect(str_to_lower(Subuso), 'total')
          )
        )
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = cells_column_labels()
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "medium"),
        locations = list(cells_body(),cells_column_labels())
      ) %>% 
      cols_label(
        Uso = "Uso de la tierra",
        Subuso = 'Sub-uso',
        Superficie_ha = "Superficie (ha)",
        Representatividad = md("Representatividad<br>en la cuenca (%)")
      ) 
  })
  output$leaf_subusos <- renderLeaflet({
    req(uso_veg())
    colf_sub <-
      colorFactor(
        ggthemes_data$tableau$`color-palettes`$regular$`Tableau 20`$value[1:(uso_veg()$Subuso %>% unique() %>% length())],
        uso_veg()$Subuso
      )
    uso_veg() %>% 
      st_transform(4326) %>% 
      leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>% 
      addProviderTiles(providers$OpenStreetMap, group ="OpenStreetMap") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>% 
      addProviderTiles(providers$Esri.DeLorme, group ="Esri.DeLorme") %>% 
      addPolygons(
        fillColor = ~colf_sub(Subuso),
        fillOpacity = 0.9,
        group = "Subusos",
        weight = 1,
        color = "black",
        label = ~ Subuso,
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2.5,
          bringToFront = TRUE
        )
      ) %>% 
      addResetMapButton() %>% 
      addMiniMap(
        position = "topright",
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        minimized = FALSE
      ) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(imperial = FALSE)
      ) %>% 
      addLegend(
        title = "Subusos",
        pal = colf_sub, 
        values = ~Subuso, 
        opacity = 1,
        position =  "bottomright",
        group = 'legend'
      ) %>% 
      addLayersControl(
        overlayGroups = c("Subusos","legend"),
        baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
        options = layersControlOptions(collapsed = T)
      ) 
  })
  
  # Vegetación ----
  out_veg <- reactive({
    req(uso_veg())
    sup_cuenca <- uso_veg()$Sup_ha %>% sum()
    TF_BN <- uso_veg() %>% 
      st_transform(crs = crs_epsg()) %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
      drop_units() %>% 
      st_drop_geometry() %>% 
      rename_all(~str_to_sentence(stri_trans_general(.,"Latin-ASCII"))) %>% 
      filter(F_ley20283 == 'Bosque nativo') %>% 
      group_by(Tipo_for) %>% 
      summarise(Sup_ha_BN = sum(Sup_ha),.groups = 'drop') %>% 
      mutate(P_BN = Sup_ha_BN/sup_cuenca)
    
    TF_BNP <- uso_veg() %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
      drop_units() %>% 
      st_drop_geometry() %>% 
      rename_all(~str_to_sentence(stri_trans_general(.,"Latin-ASCII"))) %>% 
      filter(!Tipo_for == 'No aplica', 
             F_ley20283 %>% str_detect('preser')) %>% 
      mutate(BNP = if_else(Bnp_ecc %>% str_detect(input$sp), str_sub(input$sp,1,4),'Otras')) %>% 
      group_by(Tipo_for,BNP) %>% 
      summarise(Sup_ha = sum(Sup_ha),.groups = 'drop') %>% 
      mutate(P = Sup_ha/sup_cuenca) 
    
    if ('Otras' %in% c(TF_BNP$BNP)) {
      TF_BNP <- TF_BNP %>%
        pivot_wider(id_cols = Tipo_for, names_from = BNP,values_from = c(Sup_ha,P),names_sep = "_") %>% 
        dplyr::select(Tipo_for,ends_with(str_sub(input$sp,1,4)),ends_with('Otras'))
    } else {
      TF_BNP <- TF_BNP %>%
        bind_rows(uso_veg() %>% 
                    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
                    drop_units() %>% 
                    st_drop_geometry() %>% 
                    rename_all(~str_to_sentence(stri_trans_general(.,"Latin-ASCII"))) %>%  
                    filter(!Tipo_for == 'No aplica', 
                           F_ley20283 %>% str_detect('preser')) %>% 
                    group_by(Tipo_for) %>% 
                    tally() %>% select(-n) %>% 
                    mutate(BNP = 'Otras',Sup_ha = 0, P = 0)
        ) %>% 
        pivot_wider(id_cols = Tipo_for, names_from = BNP,values_from = c(Sup_ha,P),names_sep = "_") %>% 
        dplyr::select(Tipo_for,ends_with(str_sub(input$sp,1,4)),ends_with('Otras'))
    }
    
    TF_Total <- uso_veg() %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
      drop_units() %>% 
      st_drop_geometry() %>% 
      rename_all(~str_to_sentence(stri_trans_general(.,"Latin-ASCII"))) %>% 
      filter(str_to_sentence(F_ley20283) %>% str_detect('Bosque nativo')) %>% 
      group_by(Tipo_for) %>% 
      summarise(Sup_Total = sum(Sup_ha)) %>% 
      mutate(P_Total = Sup_Total/sup_cuenca)
    
    TF_tabla <- inner_join(TF_BN,TF_BNP) %>% 
      inner_join(TF_Total) %>% 
      gt(rowname_col = "Tipo_for") %>% 
      tab_header(
        title = "Superficie (ha) según tipo forestal, bosque nativo y bosque nativo de preservación en la cuenca"
      ) %>% 
      tab_stubhead(label = "Tipo Forestal") %>% 
      tab_footnote(
        footnote = paste0("Porcentajes respecto del de la superficie total de la cuenca (",
                          sup_cuenca %>% format(decimal.mark = ",",big.mark = "."),
                          " ha)")
      ) %>% 
      tab_spanner(
        label = md(str_c("*",word(input$sp,1) %>% str_sub(1,1) %>% str_c('. '),word(input$sp,2),"*")),
        columns = 4:5,
        id = 'ecc') %>% 
      tab_spanner(label = "Otras",columns = 6:7,id = 'otras') %>% 
      tab_spanner(label = "Bosque nativo de preservación",spanners = c("ecc","otras")) %>% 
      tab_spanner(label = "Bosque nativo",columns = 2:3,level = 1) %>%  
      tab_spanner(label = "Total",columns = 8:9) %>% 
      cols_label(
        matches("Sup") ~ "Sup (ha)",
        starts_with("P_") ~ "%"
      ) %>% 
      cols_align(align = "center") %>% 
      fmt_percent(
        columns = seq(3,9,2),
        decimals = 2,
        dec_mark = ','
      ) %>% 
      fmt_number(
        columns = seq(2,8,2),
        sep_mark = '.',
        dec_mark = ','
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.2),
        locations = list(
          cells_column_labels(everything()),
          cells_column_spanners(everything()),
          cells_stubhead()
        )
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold"
        ),
        locations = list(
          cells_column_labels(everything()),
          cells_column_spanners(everything()),
          cells_stubhead()
        )
      ) %>% 
      tab_style(
        style = cell_text(size = "medium"),
        locations = list(cells_title(),cells_column_labels(),cells_column_spanners(),cells_body(),cells_stubhead(),cells_stub())
      ) %>% 
      tab_style(
        style = cell_text(size = "small"),
        locations = list(cells_footnotes())
      )
    
    bnp <- uso_veg() %>% 
      st_transform(crs_epsg()) %>% 
      st_make_valid() %>% 
      rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% 
      rename_at(vars(contains("ecc")), str_to_upper) %>% 
      filter(str_detect(F_ley20283, "preser") & str_detect(BNP_ECC,input$sp)) %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>%
      drop_units() %>%
      dplyr::select(F_ley20283, BNP_ECC, Sup_ha) %>% 
      relocate(Sup_ha, .before = geometry) %>% 
      arrange(Sup_ha)
    
    return(
      list(
        TF_tabla = TF_tabla,
        bnp = bnp,
        sup_cuenca = sup_cuenca
      )
    )
  })
  output$vb_sup_bnp <- renderValueBox({
    valueBox(
      out_veg()$bnp$Sup_ha %>% 
        sum() %>% 
        format(decimal.mark = ",", big.mark = ".") %>% 
        str_c(" ha ",((out_veg()$bnp$Sup_ha %>% sum())/(uso_veg()$Sup_ha %>% sum())*100) %>% round(2) %>% str_c("(",.," %)")),
      "Superficie de BNP de la especie (ha)",
      icon = icon("pagelines"),
      color = "light-blue"
    )
  })
  output$vb_sup_fx <- renderValueBox({
    req(uso_veg())
    valueBox(
      uso_veg() %>% 
        rename_all(str_to_sentence) %>% 
        filter(str_detect(str_to_lower(F_ley20283),"xero")) %>% 
        pull(Sup_ha) %>% 
        sum() %>% 
        format(decimal.mark = ",", big.mark = ".") %>% 
        str_c(" ha ",
              ((uso_veg() %>% 
                  rename_all(str_to_sentence) %>% 
                  filter(str_detect(str_to_lower(F_ley20283),"xero")) %>% 
                  pull(Sup_ha) %>% sum())/(uso_veg()$Sup_ha %>% sum())*100) %>% 
                round(2) %>% 
                format(decimal.mark = ",") %>% 
                str_c("(",.," %)")),
      "Superficie formaciones xerofíticas (ha)",
      icon = icon("sun-plant-wilt"),
      color = "purple"
    )
  })
  output$vb_n_parc <- renderValueBox({
    req(BD_flora())
    valueBox(
      BD_flora() %>% 
        rename_all(str_to_lower) %>%
        group_by(utm_e,utm_n) %>%
        tally() %>%
        nrow(),
      "N° de parcelas floristicas en BNP",
      icon = icon("plant-wilt"),
      color = "olive"
    )
  })
  output$plt_habito <- renderPlotly({
    req(BD_flora())
    habito <- BD_flora() %>%
      janitor::clean_names() %>% 
      mutate_at("habito",~str_to_sentence(stri_trans_general(str_trim(.),"Latin-ASCII"))) %>% 
      group_by(especie, habito) %>% 
      tally() %>% 
      group_by(habito) %>% 
      tally() %>% 
      filter(!habito %>% str_detect("-"))
    plot_ly(
      type = 'pie',
      labels = habito$habito,
      values = habito$n,
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    )
  })
  output$plt_origen <- renderPlotly({
    req(BD_flora())
    origen <- BD_flora() %>%
      janitor::clean_names() %>% 
      group_by(especie, origen) %>% 
      tally() %>% 
      group_by(origen) %>% 
      tally() %>% 
      filter(!origen %>% str_detect("-"))
    plot_ly(
      type = 'pie',
      labels = origen$origen,
      values = origen$n,
      textinfo = 'label+percent',
      insidetextorientation = 'radial'
    )
  })
  output$gt_tf <- render_gt({
    out_veg()$TF_tabla
  })
  output$leaf_bnp <- renderLeaflet({
    req(c(out_veg()), cuenca())
    uso_veg() %>%
      rename_all(str_to_sentence) %>%
      filter(str_detect(str_to_lower(F_ley20283),"xero")) %>%
      st_transform(4326) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>%
      addProviderTiles(providers$OpenStreetMap, group ="OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>%
      addProviderTiles(providers$Esri.DeLorme, group ="Esri.DeLorme") %>%
      addPolygons(
        fillColor = "#FFDC91FF",
        fillOpacity = 0.9,
        group = "FX",
        weight = 1,
        color = "gray40",
        highlightOptions = highlightOptions(
          color = "gray20",
          weight = 2.5,
          bringToFront = TRUE,
        )
      ) %>%
      addPolygons(
        data = uso_veg() %>%
          rename_all(str_to_sentence) %>%
          filter(!str_detect(str_to_lower(F_ley20283),"preser") &
                   str_detect(str_to_lower(F_ley20283),"bosque")) %>%
          st_transform(4326),
        fillColor = "#84BD00FF",
        fillOpacity = 0.9,
        group = "BN",
        weight = 1,
        color = "gray40",
        highlightOptions = highlightOptions(
          color = "gray20",
          weight = 2.5,
          bringToFront = TRUE,
        )
      ) %>%
      addPolygons(
        data = uso_veg() %>%
          rename_all(str_to_sentence) %>%
          filter(str_detect(str_to_lower(F_ley20283),"preser")) %>%
          st_transform(4326),
        fillColor = "#20854EFF",
        fillOpacity = 0.9,
        group = "BNP",
        weight = 1,
        color = "gray40",
        highlightOptions = highlightOptions(
          color = "gray20",
          weight = 2.5,
          bringToFront = TRUE,
        )
      ) %>%
      addPolygons(
        data = cuenca() %>% st_transform(4326),
        fillColor = "transparent",
        weight = 1,
        color = "#00A087FF",
        dashArray = "10,5"
      ) %>%
      addFullscreenControl(position = "topleft") %>%
      addResetMapButton() %>%
      addMiniMap(
        position = "topright",
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        minimized = FALSE
      ) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(imperial = FALSE)
      ) %>%
      addLegend(
        title = "Legend",
        colors = c("#FFDC91FF", "#84BD00FF","#20854EFF","#00A087FF"),
        labels = c("Formaciones Xerofíticas", "Bosque Nativo","Bosque Nativo de Preservación","Cuenca"),
        opacity = c(1,1,1,1),
        group = 'legend',
        position = "bottomleft"
      ) %>%
      addLayersControl(
        overlayGroups = c("FX","BN","BNP","legend"),
        baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  # BNP ----
  # Inventarios forestales ----
  output$gt_ivi <- render_gt({
    req(Inventarios())
    Inventarios()$IVI %>% 
      rename_at(5:7, str_replace, "_", " ") %>% 
      gt %>% 
      fmt_number(
        columns = 3,
        decimals = 3,
        sep_mark = '.',
        dec_mark = ','
      ) %>% 
      fmt_number(
        columns = 4:8,
        decimals = 1,
        sep_mark = '.',
        dec_mark = ','
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold"
        ),
        locations = list(
          cells_body(
            columns = everything(),
            rows = str_detect(Especie, 'Total')
          ),
          cells_column_labels(everything())
        )
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "small"),
        locations = list(cells_body(),cells_column_labels())
      )
  })
  estadisticos <- reactive({
    req(Inventarios(), BD_fore(), BNP_cuenca(), input$sp)
    Estadisticos_fun(
      a = BD_fore() %>% group_by(Parcela) %>% slice_head() %>% ungroup() %>% count(Sup_parcela) %>% slice_max(n) %>% .[[1]] %>% .[]/10000,
      A = BNP_cuenca()$Sup_ha %>% sum(),
      data = Inventarios()$BD_Nha,
      sp = input$sp
    ) 
  })
  output$gt_estadisticos <- render_gt({
    req(estadisticos())
    estadisticos() %>% 
      mutate_all(as.character) %>% 
      `names<-`(c("Variable","Rango (ind/ha)","Tamaño de la muestra (n)","Nha (ind/ha)","Varianza", "Coeficiente de variación (%)", "Error absoluto (ind/ha)","Error relativo (%)","Intervalo de confianza (95%) (ind/ha)")) %>% 
      pivot_longer(names_to = "Parámetros", values_to = "Val",cols = -Variable) %>% 
      mutate_at("Parámetros", as_factor) %>% 
      spread(key = Variable, value = Val) %>% 
      rename("Conjunto de especies arbóreas" = Nha_Total) %>% 
      rename_at(3, str_replace,"\\.", " ") %>% 
      gt() %>% 
      cols_align(align = "center",columns = 2:3) %>% 
      cols_align(align = "left",columns = 1) %>% 
      fmt_number(
        columns = 2:3,
        sep_mark = '.',
        dec_mark = ','
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "small"),
        locations = list(cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold", align = "center"
        ),
        locations = list(
          cells_column_labels(everything())
        )
      )
  })
  output$gt_IC_prop <- render_gt({
    req(estadisticos(), Inventarios(), uso_veg())
    estadisticos() %>% 
      filter(Variable == input$sp) %>% 
      dplyr::select(Variable, Promedio, E_rel, Int_conf) %>% 
      separate_wider_delim(Int_conf, delim = " - ",names = c("Int_inf","Int_sup")) %>% 
      mutate(Individuos_totales = Inventarios()$prop$Individuos_totales %>% sum(),
             param = "Densidad media") %>%
      mutate_at(vars(starts_with("Int")), as.double) %>% 
      dplyr::select(Variable, param, Promedio, E_rel, Int_inf, Int_sup, Individuos_totales) %>% 
      bind_rows(
        Inventarios()$prop %>% 
          mutate(
            param = str_c("Proporción (p=", Proporciones, ")"),
            Promedio = (estadisticos()[estadisticos()$Variable == input$sp,][["Promedio"]] *
                          Proporciones) %>% round(),
            E_rel = estadisticos()[estadisticos()$Variable == input$sp,][["E_rel"]],
            Int_inf = round(Promedio-Promedio*E_rel/100),
            Int_sup = round(Promedio+Promedio*E_rel/100)
          ) %>% 
          rename(Variable = Estado) %>% 
          dplyr::select(Variable, param, Promedio, E_rel, Int_inf, Int_sup, Individuos_totales)
      ) %>% 
      `names<-`(c("ECC/Estado de desarrollo","Parámetro poblacional estimado","Estimación (ind/ha)","Error relativo (%)","Inf", "Sup","Total poblacional* (individuos)")) %>% 
      gt() %>% 
      cols_width(
        1 ~ px(120),
        2 ~ px(150),
        c(3:4,7) ~ px(80),
        5:6 ~ px(70)
      ) %>% 
      tab_header(
        title = "Estimación puntual e intervalos de confianza para la densidad media de la especie en categoría de conservación"
      ) %>% 
      tab_footnote(
        footnote = paste0("* Total obtenido multiplicando la densidad por la superficie total de la cuenca (",
                          uso_veg()$Sup_ha %>% sum() %>% format(decimal.mark = ",",big.mark = "."),
                          " ha)")
      ) %>% 
      tab_spanner(
        label = "Intervalos de confianza (ind/ha)",
        columns = 5:6) %>% 
      fmt_number(
        columns = c(4,7),
        sep_mark = '.',
        dec_mark = ',',
        drop_trailing_zeros = T
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels(),cells_column_spanners())
      ) %>% 
      tab_style(
        style = cell_text(size = "small"),
        locations = list(cells_title(),cells_body(),cells_column_labels(),cells_column_spanners())
      ) %>% 
      tab_style(
        style = cell_text(size = "x-small"),
        locations = list(cells_footnotes())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold", align = "center"
        ),
        locations = list(
          cells_column_labels(everything()),cells_column_spanners()
        )
      )
  })
  output$leaf_inv_fore <- renderLeaflet({
    req(BNP_cuenca(), cuenca(), Inventarios())
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group ="CartoDB.Positron") %>%
      addProviderTiles(providers$OpenStreetMap, group ="OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group ="Esri.WorldImagery") %>%
      addProviderTiles(providers$Esri.DeLorme, group ="Esri.DeLorme") %>%
      addPolygons(
        data = cuenca() %>% st_transform(4326),
        fillOpacity = 0,
        weight = 2,
        color = "#00A087FF",
        dashArray = "10,5"
      ) %>%
      addPolygons(
        data = BNP_cuenca() %>% st_transform(4326),
        fillColor = "#84BD00FF",
        fillOpacity = 0.8,
        group = "BNP",
        weight = 2,
        color = "snow",
      ) %>%
      addCircles(
        data = Inventarios()$BD_Nha %>% 
          dplyr::select(1,3:4,input$sp, Nha_Total) %>% 
          rename(ECC = 4) %>% 
          mutate(popup = paste0("<h2 style='font-weight: bold;text-align: center;'>",Parcela,"</h2>",
                                "UTM E: ", round(UTM_E), "<br/>",
                                "UTM N: ", round(UTM_N), "<br/>",
                                "NHA ECC: ", ECC, "<br/>",
                                "NHA Total: ", Nha_Total)) %>% 
          st_as_sf(coords = c("UTM_E", "UTM_N"), crs = 32719, remove = F) %>% 
          st_intersection(st_union(BNP_cuenca())) %>% 
          st_transform(4326),
        group = "PTS",
        color = "#920000",
        popup = ~popup
      ) %>%
      leaflet.extras::addFullscreenControl(position = "topleft") %>%
      addResetMapButton() %>%
      addMiniMap(
        position = "topright",
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE,
        minimized = FALSE
      ) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters"
      ) %>%
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(imperial = FALSE)
      ) %>%
      addLegend(
        title = "Legend",
        colors = c("#84BD00FF","#920000","#00A087FF"),
        labels = c(str_c("Bosque Nativo de Preservación de ", input$sp),"Unidades de muestreo","Cuenca"),
        opacity = c(1,1,1),
        group = 'legend',
        position = "bottomleft"
      ) %>%
      addLayersControl(
        overlayGroups = c("BNP","PTS","legend"),
        baseGroups = c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Esri.DeLorme"),
        options = layersControlOptions(collapsed = T)
      )
  })
  
  # intervencion ----
  output$gt_c_inter <- render_gt({
    req(carto_digital())
    carto_digital()$ECC_int %>% 
      st_drop_geometry() %>% 
      group_by(Obra, Especie, Afectacion) %>% 
      tally() %>% 
      ungroup() %>% 
      bind_rows(
        carto_digital()$BNP_int_sin_pto %>% 
          st_drop_geometry() %>% 
          dplyr::select(Obra, Especie, Afectacion, Ind_Interv) %>% 
          rename(n = 4)
      ) %>% 
      group_by(Obra, Especie, Afectacion) %>% 
      summarise(n = sum(n)) %>% 
      group_by(Obra, Especie, Afectacion) %>% 
      tally() %>% 
      ungroup() %>% 
      janitor::adorn_totals(name = "Total individuos a intervenir") %>%
      gt() %>% 
      cols_width(
        3 ~ px(230),
        4 ~ px(130)
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "large"),
        locations = list(cells_title(),cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels(everything()))) %>% 
      tab_style(
        style = cell_text(style = "italic"),
        locations = list(cells_body(columns = 2))
      ) %>% 
      cols_label(
        Afectacion = "Tipo (corta, eliminación, destrucción, descepado)",
        n = "N° individuos por intervenir"
      )
  })
  output$gt_c_alter <- render_gt({
    req(carto_digital())
    carto_digital()$ECC_alt %>% 
      st_drop_geometry() %>% 
      group_by(Obra, Especie) %>% 
      tally() %>% 
      ungroup() %>% 
      bind_rows(
        carto_digital()$BNP_alterar_sin_pto %>% 
          st_drop_geometry() %>% 
          dplyr::select(Obra, Especie, Ind_alterar) %>% 
          rename(n = 3)
      ) %>% 
      group_by(Obra, Especie) %>% 
      summarise(n = sum(n)) %>%
      ungroup() %>% 
      mutate(Componente = "") %>% 
      janitor::adorn_totals(name = "Total individuos a alterar su hábitat") %>% 
      gt() %>% 
      cols_width(
        3:4 ~ px(180)
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "large"),
        locations = list(cells_title(),cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels(everything()))) %>% 
      tab_style(
        style = cell_text(style = "italic"),
        locations = list(cells_body(columns = 2))
      ) %>% 
      cols_label(
        Componente = "Componente del ambiente alterado",
        n = "N° individuos cuyo hábitat será alterado"
      )
  })
  output$gt_sup_inter <- render_gt({
    req(carto_digital())
    obras() %>% 
      st_drop_geometry() %>% 
      group_by(Obra) %>% 
      summarise(Sup_obra = sum(Sup_ha)) %>% 
      ungroup() %>% 
      left_join(
        carto_digital()$BNP_inter %>% 
          st_drop_geometry() %>% 
          group_by(Obra) %>% 
          summarise(Sup_bn_obra = sum(Sup_ha)) %>% 
          ungroup() 
      ) %>% 
      left_join(
        carto_digital()$BNP_inter %>% 
          st_drop_geometry() %>% 
          group_by(Obra) %>% 
          summarise(Sup_int = sum(Sup_ha)) %>% 
          ungroup() 
      ) %>% 
      left_join(
        carto_digital()$BNP_alterar %>% 
          st_drop_geometry() %>% 
          group_by(Obra) %>% 
          summarise(Sup_int = sum(Sup_ha)) %>% 
          ungroup() 
      ) %>% 
      mutate(Sup_alt = as.double(NA)) %>%
      mutate(Obra = case_when(if_all(c("Sup_bn_obra", "Sup_int", "Sup_alt"), ~is.na(.)) ~ "Otras obras",
                              .default = Obra)) %>% 
      group_by(Obra) %>% 
      summarise_all(sum) %>% 
      arrange(Obra == "Otras obras") %>% 
      janitor::adorn_totals() %>%   
      mutate_at(2:5, ~if_else(is.na(.), 0, .)) %>% 
      gt %>% 
      cols_width(
        3:5 ~ px(190)
      ) %>% 
      fmt_number(
        columns = c(2:5),
        sep_mark = '.',
        dec_mark = ',',
        decimals = 2
      ) %>% 
      tab_spanner(
        label = "Obra o actividad",
        columns = 1:2) %>% 
      tab_spanner(
        label = "Bosque nativo de preservación (ha)",
        columns = 3:5) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels(), cells_column_spanners())
      ) %>% 
      tab_style(
        style = cell_text(size = "large"),
        locations = list(cells_title(),cells_body(),cells_column_labels(), cells_column_spanners())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels(), cells_column_spanners())) %>% 
      cols_label(
        Obra = "Nombre de la obra",
        Sup_obra = "Superficie (ha)",
        Sup_bn_obra = "Superficie en la obra o actividad",
        Sup_int = "Superficie de intervención",
        Sup_alt = "Superficie de alteración de hábitat"
      ) 
  })
  
  # Biodiversidad ----
  output$gt_compo_x_piso <- render_gt(height = 600,{
    req(Biodiversidad())
    Biodiversidad()$Tabla_resumen %>% 
      gt() %>% 
      cols_width(
        2:(last_col()-1) ~ px(190)
      ) %>% 
      fmt_number(
        columns = c(2:last_col()),
        sep_mark = '.',
        dec_mark = ',',
        decimals = 2,
        drop_trailing_zeros = T
      ) %>% 
      tab_spanner(
        label = "Pisos de vegetación",
        columns = 2:(last_col()-1)) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels(), cells_column_spanners())
      ) %>% 
      tab_style(
        style = cell_text(size = "small"),
        locations = list(cells_title(),cells_body(),cells_column_labels(), cells_column_spanners())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels(), cells_column_spanners()))
  })
  output$gt_spp_acomp <- render_gt(height = 350,{
    req(Biodiversidad())
    Biodiversidad()$spp_acomp %>% 
      gt() %>% 
      fmt_percent(
        columns = 2,
        decimals = 1,
        dec_mark = ","
      ) %>% 
      tab_style(
        style = cell_text(style = "italic"),
        locations = list(cells_body(columns = 1))
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "medium"),
        locations = list(cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels())) %>% 
      tab_style(
        style = cell_text(align = "center"),
        locations = cells_body(columns = 2:3)
      ) %>% 
      cols_label(
        especie = "Especies acompañantes (cuenca)",
        Frec = "Frecuencia",
        presencia = "Presencia en el área de proyecto"
      )
  })
  output$gt_indices_x_sector <- render_gt(height = 350,{
    req(Biodiversidad())
    Biodiversidad()$D.alfa_index %>% 
      inner_join(
        Biodiversidad()$compo_x_sector_resumen 
      ) %>% 
      mutate_at(
        1,
        ~ case_when(
          . == "AI" ~ "BNP en área de proyecto",
          . == "FAI" ~ "BNP fuera del área de proyecto",
          TRUE ~ "BNP en la cuenca"
        )
      ) %>% 
      dplyr::select(SECTOR, N, Shannon, Div.Simpson) %>% 
      gt() %>% 
      cols_label(
        SECTOR = "Sector",
        N = "Riqueza específica",
        Shannon = "Shannon (H')",
        Div.Simpson = "Simpson (1-𝝀)"
      ) %>% 
      fmt_number(
        columns = 3:4,
        decimals = 2,
        sep_mark = '.',
        dec_mark = ","
      ) %>% 
      tab_header(
        title = "Índices de diversidad del bosque nativo de preservación en la cuenca"
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "medium"),
        locations = list(cells_title(),cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels()))
  })
  output$plot_riqueza <- renderPlot({
    req(Biodiversidad())
    Biodiversidad()$plot
    # print(Biodiversidad()$plot, vb = grid::viewport(width = unit(10, "in"), height = unit(7.3, "in")))
  })
  output$down_riqueza <- downloadHandler(
    filename = function(){
      paste0("Riqueza_x_sector.png")
    },
    content = function(file){
      req(Biodiversidad())
      ggsave(
        Biodiversidad()$plot,
        filename = file,
        width = 8,
        height = 5.8,
        dpi = 500
      )
    }
  )
  
  # Analisis de Fragmentación
  observeEvent(uso_veg(),{
    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "select_subusos",
      label = "Seleccionar subusos que no sean de vegetación (incluir terrenos agrícolas)", 
      choices = sort(unique(uso_veg()$Subuso))
    )
  })
  observeEvent(input$input_lsm, {
    updateActionButtonStyled(
      session,
      "apply_lsm", 
      disabled = c(T, F)[(file.exists(input$input_lsm$datapath) %% 2) + 1])
  })
  lsm_output <- eventReactive(input$apply_lsm,{
    req(c(uso_veg(),obras(),Biodiversidad(),Inventarios(),estadisticos(),input$select_subusos))
    get_lsm_analisis(
      uso_veg = uso_veg(), 
      obras = obras(), 
      input = input, 
      alteracion = (input$alt_lgl == "Si"), 
      spp_acomp = Biodiversidad()$spp_acomp, 
      estadisticos = estadisticos(), 
      prop = Inventarios()$prop
    )
  })
  observeEvent(input$apply_lsm,{
    req(c(uso_veg(),obras(),Biodiversidad(),Inventarios(),estadisticos(),input$select_subusos))
    show_modal_spinner(
      spin = "flower",
      color = "#35978F",
      text = div(br(),p("Generando análisis de fragmentación",br()," Por favor espere, esto puede tardar unos minutos"))
    )
    req(lsm_output())
    shinyjs::enable("down_lsm")
    remove_modal_spinner()
  })
  output$down_lsm <- downloadHandler(
    filename = function() {
      paste0(paste(input$cod_proj,'BD_Tabla_Analisis_fragmentacion',substr(input$sp,1,4), sep = "_"),'.xlsx')
    },
    content = function(file) {
      saveWorkbook(wb = lsm_output()$wb, file = file, overwrite = T)
    }
  )
  shinyjs::disable("down_lsm")
  output$gt_lsm_matriz <- render_gt({
    req(lsm_output())
    lsm_output()$matriz_paisaje %>% 
      gt() %>% 
      fmt_number(
        columns = 2:5,
        decimals = 2,
        sep_mark = '.',
        dec_mark = ","
      ) %>% 
      tab_spanner(
        label = "Superficie (ha)",
        columns = 2:4
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_spanners(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "medium"),
        locations = list(cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_spanners(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = Subuso %>% str_detect("Total"))
      ) %>% 
      tab_style(
        style = cell_fill("yellow"),
        locations = cells_body(columns = 5, rows = Subuso %>% str_detect("Total veg"))
      )
  })
  output$gt_lsm_frag <- render_gt({
    req(lsm_output())
    lsm_output()$df %>% 
      gt() %>% 
      fmt_number(
        columns = 3:4,
        decimals = 2,
        sep_mark = '.',
        dec_mark = ",",
        drop_trailing_zeros = T
      ) %>% 
      fmt_number(
        columns = 5,
        decimals = 2,
        sep_mark = '.',
        dec_mark = ",",
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "x-small"),
        locations = list(cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels())
      )
  })
  output$gt_lsm_resultados <- render_gt({
    req(lsm_output())
    lsm_output()$tabla_eval %>% 
      mutate_at(4, ~str_replace(.,"\\.",",")) %>% 
      janitor::adorn_totals() %>% 
      gt() %>% 
      fmt_number(
        columns = 5,
        decimals = 2,
        sep_mark = '.',
        dec_mark = ",",
        drop_trailing_zeros = T
      ) %>% 
      tab_style(
        style = cell_fill(color = "#A0968C",alpha = 0.1),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(size = "small"),
        locations = list(cells_body(),cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(
          weight = "bold",
          align = "center",
          v_align = "middle"
        ),
        locations = list(cells_column_labels())
      ) %>% 
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(rows = Parámetro %>% str_detect("Total"))
      ) 
  })
  
  # Author ----
  output$user <- renderUser({
    dashboardUser(
      name = "David Martínez",
      image = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
      title = "IE Dashboard",
      subtitle = "Autor",
      footer = fluidRow(
        tags$p("Mi Github", socialButton(href = "https://github.com/DavidJMartinezS", icon = icon("github")), class = "text-center"),
        tags$p("Geobiota", socialButton(href = "https://google.com", icon = icon("globe")), class = "text-center"),
      ),
      "Especialista en plantas de Geobiota"
    )
  })

  
})
