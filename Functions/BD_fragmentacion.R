# Determinar para todas las especies su sistema reproductivo, es decir si son dioicas o monoicas

get_lsm_analisis <- function(uso_veg, obras, input, alteracion, spp_acomp, estadisticos, prop){
  results_antes <- readxl::read_xls(input$input_lsm$datapath,sheet = 1)
  results_despues <- readxl::read_xls(input$input_lsm$datapath,sheet = 2)
  
  add_total_lsm <- function(x, name_tot){
    x %>% 
      mutate(CA = AREA %>% round(2),
             NP = 1,
             PID = as.character(PID)) %>%
      select(PID, NP, CA, AREA:ENN) %>% 
      bind_rows(
        x %>% 
          mutate(CA = AREA %>% round(2),
                 NP = 1) %>% 
          summarise_at(vars(NP, CA, CORE, NCORE), .funs = sum) %>% 
          bind_cols(
            x %>% 
              mutate(CA = AREA %>% round(2),
                     NP = 1) %>% 
              summarise_at(vars(AREA, SHAPE, FRAC, PROX, ENN), .funs = mean) %>% 
              mutate(PID = name_tot) 
          ) %>% 
          select(PID, NP, CA, AREA, SHAPE, FRAC, CORE, NCORE, PROX, ENN)
      ) %>% 
      mutate_at(vars(AREA, CA, CORE),round, 2) %>% 
      mutate_at(vars(ENN), round, 1) %>% 
      mutate_at(vars(SHAPE, FRAC, PROX), round, 3)
  }
  
  antes <- results_antes %>% add_total_lsm(name_tot = 'Total Antes')
  despues <- results_despues %>% add_total_lsm(name_tot = 'Total Después')
  
  eval_frag <- function(SIGLA,DIFF,CLASE){
    DIFF <- abs(DIFF)
    if(SIGLA == 'NP') val <- ifelse(CLASE == "Aumento",ifelse(DIFF>10,1,ifelse(DIFF>5,2,ifelse(DIFF>1,4,8))),8)
    if(SIGLA %in%  c('CA','PROX')) val <- ifelse(CLASE == "Reducción", ifelse(DIFF>5,1,ifelse(DIFF>1,2,ifelse(DIFF<1,3,4))),4)
    if(SIGLA == 'AREA') val <- ifelse(CLASE == "Reducción", ifelse(DIFF>5,1,ifelse(DIFF>1,2,ifelse(DIFF<1,2,4))),4)
    if(SIGLA %in% c('SHAPE','FRAC','NCORE', 'ENN')) val <- ifelse(CLASE == "Aumento", ifelse(DIFF>5,1,ifelse(DIFF>1,2,ifelse(DIFF<1,3,4))),4)
    if(SIGLA == 'CORE') val <- ifelse(CLASE == "Reducción", ifelse(DIFF>5,1,ifelse(DIFF>1,2,ifelse(DIFF<1,3,6))),6)
    return(val)
  }
  
  tabla_guia <- read_xlsx('./www/tabla_guia_fragmentación.xlsx')
  
  df <- data.frame(
    SIGLA = names(antes)[-1],
    ANTES = antes %>% slice_tail() %>% as.vector() %>% unlist() %>% unname() %>% .[-1],
    DESPUES = despues %>% slice_tail() %>% as.vector() %>% unlist() %>% unname() %>% .[-1]
  ) %>%
    mutate_at(2:3, as.numeric) %>%
    mutate(
      `DIFERENCIA(%)` = (DESPUES - ANTES)/ANTES * 100,
      CLASE = if_else(ANTES < DESPUES, "Aumento", "Reducción")
    ) %>% 
    mutate_at(2:3,round,3) %>% mutate_at(4,round,2) %>% 
    mutate(
      VALOR = pmap_dbl(list(SIGLA,`DIFERENCIA(%)`,CLASE), eval_frag)
    ) %>% 
    rename(Valoración = VALOR, Sigla = SIGLA) %>% 
    inner_join(tabla_guia[3:6]) %>% 
    relocate(Parámetro, .after = Sigla) %>% 
    rename_all(str_to_upper)
  
  # Matriz de paisaje
  Usos <- uso_veg %>% 
    st_zm() %>% 
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>% 
    rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% 
    rename_at(vars(contains("ecc")), str_to_upper) %>% 
    mutate(Subuso = if_else(
      Subuso %in% c(input$select_subusos),
      'Otros usos sin vegetación',
      Subuso
    ))   
  
  matriz_paisaje <- Usos %>% 
    filter(Subuso != 'Otros usos sin vegetación') %>% 
    group_by(Subuso) %>% 
    tally() %>% 
    mutate(Antes = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
    drop_units() %>% 
    select(-n) %>% 
    st_drop_geometry() %>% # sup antes por subuso
    merge(
      Usos %>% 
        st_intersection(st_union(obras)) %>% 
        filter(Subuso != 'Otros usos sin vegetación') %>% 
        group_by(Subuso) %>% 
        tally() %>% 
        mutate(Intervención = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
        drop_units() %>% 
        select(-n) %>% 
        st_drop_geometry()
      , 
      all =T
    ) %>% # añadir sup a intervenir por subuso
    mutate_at('Intervención', ~ ifelse(is.na(.), 0, .)) %>% 
    mutate_at(2:3,round,2) %>% 
    mutate(Despues = Antes - Intervención %>% round(2)) %>% # añadir sup despues
    adorn_totals(name = 'Total vegetación') %>% # añadir fila total vegetacion
    rbind(
      Usos %>% 
        filter(Subuso == 'Otros usos sin vegetación') %>% 
        group_by(Subuso) %>% 
        tally() %>% 
        mutate(Antes = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
        drop_units() %>% 
        select(-n) %>% 
        st_drop_geometry() %>% 
        merge(
          Usos %>% 
            st_intersection(st_union(obras)) %>% 
            filter(Subuso == 'Otros usos sin vegetación') %>% 
            group_by(Subuso) %>% 
            tally() %>% 
            mutate(Intervención = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
            drop_units() %>% 
            select(-n) %>% 
            st_drop_geometry()
        ) %>% 
        mutate_at(2:3,round,2) %>% 
        mutate(Despues = Antes - Intervención %>% round(2))
    ) %>% # añadir sup de otros usos
    rbind(
      data.frame(Subuso = 'Total',
                 Antes = st_area(st_union(Usos)) %>% set_units(ha) %>% round(2) %>% drop_units(),
                 Intervención = st_area(st_union(st_intersection(Usos,obras))) %>% set_units(ha) %>% round(2) %>% drop_units()) %>% 
        mutate(Despues = Antes - Intervención %>% round(2))
    ) %>% # añadir sup total
    mutate(Tasa = (log(Despues/Antes)*100) %>% round(2)) # añadir tasa
  
  densidades_prop <- prop %>% 
    mutate(
      Promedio = (estadisticos[estadisticos$Variable == input$sp,][["Promedio"]] *
                    Proporciones) %>% round(),
      E_rel = estadisticos[estadisticos$Variable == input$sp,][["E_rel"]],
      Int_inf = round(Promedio-Promedio*E_rel/100),
      Int_sup = round(Promedio+Promedio*E_rel/100)
    ) %>% 
    select(Estado, Int_inf)
  
  # Tabla Evaluación
  eval_param <- function(param, valor){
    val <- valor
    if(param == 'Matriz del paisaje (ha)'){
      val <- ifelse(abs(matriz_paisaje[matriz_paisaje$Subuso == 'Total vegetación', 'Tasa']) > 5, 1,
                    ifelse(abs(matriz_paisaje[matriz_paisaje$Subuso == 'Total vegetación', 'Tasa']) > 1, 3, 6))
    } 
    if(param == 'Hábitat natural') val <- 8
    if(param == 'Riqueza de especies'){
      val <- if_else(nrow(spp_acomp[spp_acomp$presencia == "Si",]) == 0, 3,
                     if_else(nrow(spp_acomp[spp_acomp$presencia == "Si",]) == 1, 2, 1))
    } 
    if(param == 'Abundancia de especies') val <- if_else(nrow(spp_acomp[spp_acomp$presencia == "Si",]) >= 1, 1, 3)
    if(param == 'Regeneración') {
      val <- if_else(densidades_prop[densidades_prop$Estado == "Regeneración","Int_inf"] > 300, 10,
                     if_else(densidades_prop[densidades_prop$Estado == "Regeneración","Int_inf"] > 100, 5, 1))
    }
    if(param == 'Brinzales') {
      val <- if_else(densidades_prop[densidades_prop$Estado == "Brinzal","Int_inf"] > 250, 9,
                     if_else(densidades_prop[densidades_prop$Estado == "Brinzal","Int_inf"] > 75, 5, 1))
    }
    if(param == 'Árboles adultos') {
      val <- if_else(densidades_prop[densidades_prop$Estado == "Adulto","Int_inf"] > 150, 9,
                     if_else(densidades_prop[densidades_prop$Estado == "Adulto","Int_inf"] > 50, 5, 1))
    }
    if(param == 'Alteración') val <- ifelse(alteracion, 1, 2)
    if(param == 'Extensión de la presencia') val <- 3
    if(param == 'Sistema reproductivo') val <- ifelse(input$sp == 'Carica chilensis', 2, 4)
    return(val)
  }
  
  tabla_eval <- tabla_guia %>% 
    group_by(N, Escala_espacial, Parámetro) %>% 
    tally() %>% select(-n) %>% ungroup() %>% 
    rename_all(str_to_upper) %>% 
    merge(df[,c(2,7:8)],by='PARÁMETRO',all=T) %>% 
    mutate(PONDERACIÓN = ifelse(ESCALA_ESPACIAL == 'PAISAJE',0.2,ifelse(ESCALA_ESPACIAL == 'HÁBITAT', 0.5,0.3))) %>% 
    select(N, ESCALA_ESPACIAL, PARÁMETRO, VALORACIÓN, PONDERACIÓN) %>% 
    arrange(N) %>% 
    as_tibble() %>% 
    mutate(VALORACIÓN = map2_dbl(PARÁMETRO, VALORACIÓN, eval_param)) %>% 
    inner_join(tabla_guia[,c(3,5,6)] %>% rename_all(str_to_upper), by = c('PARÁMETRO','VALORACIÓN')) %>% 
    rename_all(str_to_sentence) %>% 
    rename(`Categoría observada` = Categorías, Valores = Valoración) %>% 
    mutate(`Valor ponderado` = Valores * Ponderación) %>% 
    select(Parámetro, `Categoría observada`, Valores, Ponderación, `Valor ponderado`) 
  
  ### EXCEL
  wb <- createWorkbook() # crear workbook excel
  
  # Portada
  addWorksheet(wb, "Portada", gridLines = FALSE)
  setColWidths(wb, 1, cols = 1:6, widths = 14)
  pageSetup(wb, 1, scale = 100, paperSize = 1)
  
  nom_proj <- if_else(is.null(input$nom_proj), "ingrese nombre del proyecto", input$nom_proj)
  writeData(wb, 1, x = str_to_upper(nom_proj), startCol = 1,startRow = 14+3)
  mergeCells(wb, 1, cols = 1:6, rows = 14:15+3)
  addStyle(wb, 1, style = createStyle(textDecoration = c("bold"), halign = "center", valign = "center",fontSize = 16),rows = 14+3,cols = 1,gridExpand = T)
  writeData(wb, 1, x = "ESTUDIO DE IMPACTO AMBIENTAL", startCol = 2, startRow = 16+3)
  mergeCells(wb, 1, cols = 2:5, rows = 16+3)
  addStyle(wb, 1, style = createStyle(textDecoration = c("bold"), halign = "center", valign = "center",fontSize = 14),rows = 16+3,cols = 2, gridExpand = T)
  writeData(wb, 1 , x = "Elaborado para:", startCol = 3, startRow = 28)
  mergeCells(wb, 1, cols = 3:4, rows = 28)
  addStyle(wb, 1, style = createStyle(textDecoration = c("bold"), halign = "center", valign = "center"), rows = 28, cols = 3, gridExpand = T)
  insertImage(wb, 1, file = "./www/image.png", startRow = 30, startCol = 3, width = 135.2899, height = 141.7323, units = 'px', dpi = 72)
  date <- Sys.Date() %>% format('%d-%m-%Y')
  tabla_portada <- tibble(Rev = as.numeric(c(NA,NA)),`Elaborado por`=c('Geobiota',date),`Revisado por`= c(NA,NA), `Aprovado por` = c(NA,NA))
  writeData(wb, 1, tabla_portada, startCol = 2, startRow = 41)
  for (i in c(2,4,5)) {
    mergeCells(wb, 1, cols = i, rows = 42:43)
  }
  for (i in c(2:5)) {
    addStyle(wb, 1, style = createStyle(textDecoration = c("bold"), fgFill = '#D9D9D9',border = 'TopBottomLeftRight', borderStyle = "thick", halign = "center", valign = "center"),rows = 41,cols = i)
    addStyle(wb, 1, style = createStyle(border = 'TopBottomLeftRight', borderStyle = "thick", halign = "center", valign = "center"), rows = 42:43, cols = i,gridExpand = T)
  }
  mes <- Sys.Date() %>% format('%B') %>% str_to_sentence()
  yr <- Sys.Date() %>% format('%Y') %>% str_to_sentence()
  writeData(wb, 1, x = str_c(mes,', ',yr),startCol = 3, startRow = 45)
  mergeCells(wb, 1, cols = 3:4, rows = 45)
  addStyle(wb, 1, style = createStyle(halign = 'center',valign = 'center'),rows = 45,cols = 3, gridExpand = T)
  
  # Presentación
  addWorksheet(wb, "Presentación", gridLines = FALSE)
  setColWidths(wb, 2, cols = 1:2, widths = c(17,70))
  pageSetup(wb, 2, scale = 100, paperSize = 1)
  
  table.fun <- function(x){
    x %>% 
      str_split(pattern = '\n',simplify = T) %>% t() %>% str_trim() %>% as_tibble() %>% 
      mutate(Campo = map_chr(value,~str_split_1(.,':') %>% .[1] %>% str_c(':')),
             Desc = map_chr(value,~str_split_1(.,':') %>% .[2])) %>% 
      mutate_all(str_trim) %>% select(-value)
  }
  desc_general <- c("Análisis de fragmentación y métricas de paisaje","La siguiente memoria de calculos presenta los resultados  de las métricas de paisajes obtenidas por el software FragStats, junto con el análisis de fragmentación de acuerdo a la 'Guía para la solicitud de excepcionalidad del artículo 19 de la Ley N.º 20.283 sobre Recuperación del Bosque Nativo y Fomento Forestal'. A continuación el detalle sobre el contenido de cada una de las hojas de este Excel.") %>% as_tibble_col()
  writeData(wb, 2, desc_general,startCol = 1,startRow = 1,colNames = F)
  for (i in 1:2) {
    mergeCells(wb, 2, cols = 1:2, rows = i)
  }
  desc_h1 <- c(
    "Resultados_FragStats: Corresponde a los resultados de lás métricas de parche arrojadas por el software FragStats anterior y posterior al proyecto, con sus totales. Además se presenta una tabla con la valoración obtenida de acuerdo a la Guía antes mencionada. Los detalles de algunos campos a continuación.
SIGLA: Siglas de los parámetros indicados en el campo PARÁMETRO
ANTES: Cifras totales de los resultados de fragmentación antes del proyecto
DESPUES: Cifras totales de los resultados de fragmentación después del proyecto
DIFERENCIA(%): Porcentaje de cambio. Calculado como (DESPUES-ANTES)/ANTES
CLASE: Campo que indica si hubo un aumento o reducción de la metrica luego del proyecto
VALORACIÓN; CATEGORÍAS: Valores con sus respectivas categorías de acuerdo con la indicado en la Guía antes mencionada"
  ) %>% table.fun()
  writeData(wb, 2, desc_h1, startCol = 1,startRow = 4, colNames = F)
  nota <- "*Los calculos de los totales varían entre la suma o el promedio, de acuerdo a la métrica de fragmentación. Suma: NP, CA, CORE, NCORE. Promedio: AREA, SHAPE, FRAC, PROX, ENN."
  writeData(wb, 2, nota, startCol = 1, startRow = 11)
  mergeCells(wb, 2, cols = 1:2, rows = 11)
  addStyle(wb, 2, style = createStyle(fontSize = 9,textDecoration = 'italic'),rows = 11,cols = 1)
  desc_h2 <- c(
    "Matriz de Paisaje: En esta pestaña se presentan las superficies por subuso del suelo en la cuenca, antes y despues del proyecto, a partir de los cuales se obtiene una tasa de pérdida detallada a continuación.
Tasa: Porcentaje de pérdida de superficie. Calculado como log(Despues/Antes)*100."
  ) %>% table.fun()
  writeData(wb, 2, desc_h2, startCol = 1,startRow = 13, colNames = F)
  desc_h3 <- c(
    "Tabla de Evaluación: En esta pestaña se muestra la tabla de evaluación de cada uno de los 19 parámetros indicados en la Guía, señalando el valor del parámetro, la ponderación, y la suma ponderada."
  ) %>% table.fun()
  writeData(wb, 2, desc_h3, startCol = 1,startRow = 16, colNames = F)
  
  addStyle(wb, 2, style = createStyle(textDecoration = c('bold','underline'),valign = 'center', halign = 'center',fontSize = 12,fgFill = "#D9D9D9"),rows = c(1,4,13,16),cols = 1)
  addStyle(wb, 2, style = createStyle(valign = 'center', halign = 'center',fgFill = "#D9D9D9"),rows = c(4,13,16),cols = 2)
  addStyle(wb, 2, style = createStyle(textDecoration = 'bold',valign = 'center'),rows = c(5:10,14),cols = 1)
  addStyle(wb, 2, style = createStyle(wrapText = T),stack = T, rows = 1:20,cols = 1:2,gridExpand = T)
  
  # Styles
  titlestyle <- createStyle(
    fgFill  = "cadetblue3",       # Color de fondo
    fontColour = "black",  # Color del texto
    halign = "center",     # Alineación horizontal
    valign = "center"     # Alineación vertical
  )
  headerstyle <- createStyle(
    textDecoration = c("bold"),
    halign = "center", 
    valign = "center",
    border = 'TopBottomLeftRight'
  )
  bodystyle <- createStyle(
    border = 'TopBottomLeftRight'
  )
  # Resultados fragstats
  addWorksheet(wb, "Resultados_FragStats",zoom = 90)
  setColWidths(wb, 3, cols = 1:25, widths = "auto",ignoreMergedCells = T)
  writeData(wb, 3, "ANTES (SIN PROYECTO)",startCol = 1,startRow = 1)
  mergeCells(wb, 3, cols = 1:ncol(antes), rows = 1)
  writeData(wb, 3, antes,startCol = 1,startRow = 2)
  addStyle(wb, 3, style = bodystyle, cols = 1:ncol(antes), rows = 2:(nrow(antes)+2), gridExpand = TRUE)
  addStyle(wb, 3, style = headerstyle, cols = 1:ncol(antes), rows = 2)
  writeData(wb, 3, "DESPUÉS (CON PROYECTO)",startCol = ncol(antes)+2,startRow = 1)
  mergeCells(wb, 3, cols = 1:ncol(despues)+ncol(antes)+1,rows = 1)
  writeData(wb, 3, despues, startCol = ncol(antes)+2,startRow = 2)
  addStyle(wb, 3, style = bodystyle, cols = 1:ncol(despues)+ncol(antes)+1, rows = 2:(nrow(despues)+2), gridExpand = TRUE)
  addStyle(wb, 3, style = headerstyle, cols = 1:ncol(despues)+ncol(antes)+1, rows = 2)
  
  addStyle(wb, 3, style = titlestyle, rows = 1, cols = c(1, ncol(antes)+2))
  
  writeData(wb, 3, df, startCol = 1, startRow = nrow(antes)+4)
  addStyle(wb, 3, style = bodystyle, cols = 1:ncol(df), rows = (nrow(antes)+4):(nrow(antes)+4+nrow(df)), gridExpand = TRUE)
  addStyle(wb, 3, style = headerstyle, cols = 1:ncol(df), rows = nrow(antes)+4)
  
  # Matriz de paisaje
  addWorksheet(wb, "Matriz de Paisaje")
  setColWidths(wb, 4, cols = 1, widths = 24)
  writeData(wb, 4, "Superficies (ha) x Subuso de suelo",startCol = 1,startRow = 1)
  mergeCells(wb, 4, cols = 1:ncol(matriz_paisaje),rows = 1)
  addStyle(wb, 4, style = titlestyle, cols = 1, rows = 1)
  writeData(wb, 4, matriz_paisaje,startCol = 1,startRow = 2)
  addStyle(wb, 4, style = headerstyle,rows = 2,cols = 1:ncol(matriz_paisaje))
  addStyle(wb, 4, style = bodystyle, rows = 1:nrow(matriz_paisaje)+2,cols = 1:ncol(matriz_paisaje), gridExpand = T)
  addStyle(wb, 4, style = createStyle(textDecoration = 'bold'), rows = (which(grepl('Total', matriz_paisaje$Subuso))+2) %>% rep(each = ncol(matriz_paisaje)), cols = rep(1:ncol(matriz_paisaje), 2),stack = T)
  addStyle(wb, 4, style = createStyle(fgFill = 'yellow'),rows = which(grepl('Total vegetación', matriz_paisaje$Subuso))+2, cols = ncol(matriz_paisaje),stack = T)
  
  # Tabla de Evaluación
  addWorksheet(wb, "Tabla de Evaluación")
  setColWidths(wb, 5, cols = c(1:5),widths = c(44,95,10,15,19))
  writeData(wb, 5, tabla_eval)
  addStyle(wb, 5, style = headerstyle, rows = 1, cols = 1:ncol(tabla_eval))
  addStyle(wb, 5, style = bodystyle, rows = 1:nrow(tabla_eval)+1,cols = 1:ncol(tabla_eval),gridExpand = T)
  writeData(wb, 5, "Suma ponderada", startCol = 1, startRow = nrow(tabla_eval)+2)
  writeData(wb, 5, tabla_eval$`Valor ponderado` %>% sum(), startCol = ncol(tabla_eval),startRow = nrow(tabla_eval)+2)
  writeData(wb, 5, tabla_eval$Valores %>% sum(), startCol = ncol(tabla_eval)-2,startRow = nrow(tabla_eval)+2)
  mergeCells(wb, 5, cols = 1:2, rows = nrow(tabla_eval)+2)
  addStyle(wb, 5, style = createStyle(textDecoration = 'bold',halign = 'center',border = 'TopBottomLeftRight'),rows = nrow(tabla_eval)+2,cols = 1:ncol(tabla_eval),stack = T)
  
  return(
    list(
      df = df,
      matriz_paisaje = matriz_paisaje,
      tabla_eval = tabla_eval,
      wb = wb
    )
  )
}


