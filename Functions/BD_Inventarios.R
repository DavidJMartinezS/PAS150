BD_inv_for <- function(BD_fore, BNP_cuenca, cuenca, input){
  BD_fore <- BD_fore %>% 
    rename_all(~ str_to_sentence(stri_trans_general(.,"Latin-ASCII"))) %>% 
    rename_at(vars(contains("utm"),contains("dap")), str_to_upper)
  pto_bnp <- BD_fore %>% 
    as_tibble() %>% 
    group_by(Parcela, UTM_E, UTM_N) %>% tally() %>% 
    st_as_sf(coords = c("UTM_E","UTM_N"),crs=32719,remove=F) %>% 
    st_intersection(st_union(BNP_cuenca)) %>% 
    .$Parcela
  clase_corte <- BD_fore %>% 
    .$DAP %>% quantile(c(.99)) %>% plyr::round_any(.,accuracy = 10,f = ceiling)
  clase_max <- BD_fore %>% 
    .$DAP %>% max() %>% plyr::round_any(.,accuracy = 10,f = ceiling)
  BD <- BD_fore %>% 
    as_tibble() %>%
    filter(Parcela %in% pto_bnp) %>% 
    mutate_at(c("Especie","Estado"),~str_to_sentence(str_trim(.))) %>% 
    rowid_to_column('Correlativo') %>% 
    mutate(Clase = cut(DAP,c(seq(0,clase_corte,10),clase_max),include.lowest = F, right = F) %>% fct_explicit_na("sin información"),
           Clase_l1 = map_int(Clase, function(x) {
             if (is.na(x))
               return(NA_integer_)
             else
               return(as.numeric(str_remove_all(
                 str_split_1(as.character(x), ','), "[:punct:]"
               ))[1])
           }),
           Clase_l2 = map_int(Clase, function(x) {
             if (is.na(x))
               return(NA_integer_)
             else
               return(as.numeric(str_remove_all(
                 str_split_1(as.character(x), ','), "[:punct:]"
               ))[2])
           }),
           Marca_clase = (Clase_l1 + Clase_l2) / 2,
           Cod_ssubc = cuenca$COD_SSUBC) %>% 
    select(-c(Clase_l1,Clase_l2))
  
  n_par <- BD$Parcela %>% unique() %>% length()
  FE <- 10000/(BD %>% group_by(Parcela, Sup_parcela) %>% tally() %>% .$Sup_parcela %>% sum())
  
  Nha <- BD %>%
    group_by(Clase, Especie) %>%
    summarise(n = sum(N_ind, na.rm = T)) %>%
    mutate(n = (n * FE)) %>%
    pivot_wider(names_from = Especie, values_from = n) %>%
    ungroup() %>%
    adorn_totals(name = 'Nha_Total') %>%
    as_tibble() %>% 
    mutate(across(where(is.numeric), ~ if_else(Clase == "Nha_Total", round(.), .))) %>% 
    mutate(across(where(is.numeric), ~ if_else(Clase == "Nha_Total" & . == 0, 1, .))) %>% 
    mutate(Nha_Total = rowSums(select(.,-1), na.rm = TRUE)) %>%
    as_tibble() 
  
  H <- BD %>% 
    group_by(Clase, Especie) %>% 
    summarise(H = mean(Altura,na.rm = T) %>% round(1)) %>% 
    pivot_wider(names_from = Especie, values_from = H) %>% 
    ungroup() %>% 
    mutate_at('Clase', as.character) %>% 
    add_row(Clase = 'mean', !!! colMeans(.[-1], na.rm=T)) %>% 
    mutate(H_Promedio = rowMeans(select(., -1), na.rm = TRUE)) %>% 
    mutate_if(is.numeric,round,1)
  
  frec <- BD %>% 
    group_by(Especie, Parcela) %>% 
    summarise(n = sum(N_ind, na.rm = T)) %>% 
    pivot_wider(names_from = Parcela, values_from = n) %>% 
    ungroup() %>% 
    cbind(BD %>%
            group_by(Especie, Parcela) %>%
            summarise(n = sum(N_ind, na.rm = T)) %>%
            group_by(Especie) %>%
            count(name = 'Count') %>%
            ungroup() %>%
            mutate(
              Frec_abs = Count / n_par * 100,
              Frec_rel = Count / sum(Count) * 100
            ) %>% 
            select(-1)) %>% 
    arrange(Especie) %>% 
    as_tibble()
  
  Gha <- BD %>% 
    group_by(Clase, Marca_clase, Especie) %>% 
    summarise(n = sum(N_ind, na.rm = T)) %>%
    ungroup() %>%
    mutate(
      n = n * FE,
      Gha = (pi * (Marca_clase / 200) ^ 2 * n) %>% round(3)
    ) %>%
    select(-c(n,Marca_clase)) %>% 
    pivot_wider(names_from = Especie, values_from = Gha) %>% 
    mutate(Gha_Total =rowSums(select(., -1), na.rm = TRUE)) %>% 
    adorn_totals(name = 'Gha_Total') %>% 
    as_tibble()
  
  IVI_sp <- Nha %>% slice_tail() %>% rename_with(~ 'Total', last_col()) %>% 
    bind_rows(Gha %>% slice_tail() %>% rename_with(~ 'Total', last_col())) %>% 
    bind_rows(H %>% slice_tail() %>% rename_with(~ 'Total', last_col())) %>% 
    pivot_longer(cols = -Clase, names_to = "Especie", values_to = "Valor") %>% 
    pivot_wider(names_from = Clase, values_from = Valor) %>% 
    `names<-`(c('Especie', 'Nha', 'Gha (m2/ha)', 'H (m)')) %>% 
    mutate_at('Nha',~ifelse(.==0,1,.)) %>%
    mutate(Densidad_relativa = Nha /sum(Nha[!Especie == 'Total']) * 100 %>% round(),
           Dominancia_relativa = `Gha (m2/ha)` / sum(`Gha (m2/ha)`[!Especie == 'Total']) * 100) %>% 
    arrange(Especie) %>% 
    bind_cols(frec %>% 
                select(1,length(frec)) %>% 
                adorn_totals(name = 'Total') %>%
                select(2) %>% 
                `names<-`(c('Frecuencia_relativa'))) %>%
    mutate(
      IVI = (Densidad_relativa + Dominancia_relativa + Frecuencia_relativa) %>% round(1),
      `Gha (m2/ha)` = round(`Gha (m2/ha)`, 3),
    ) %>% 
    mutate_at(vars(contains('relativa')), round,1) %>% 
    .[-nrow(.),] %>% 
    arrange(desc(Nha))
  
  IVI_tot <- Nha %>% slice_tail() %>% rename_with(~ 'Total', last_col()) %>% 
    bind_rows(Gha %>% slice_tail() %>% rename_with(~ 'Total', last_col())) %>% 
    bind_rows(H %>% slice_tail() %>% rename_with(~ 'Total', last_col())) %>% 
    pivot_longer(cols = -Clase, names_to = "Especie", values_to = "Valor") %>% 
    pivot_wider(names_from = Clase, values_from = Valor) %>% 
    `names<-`(c('Especie', 'Nha', 'Gha (m2/ha)', 'H (m)')) %>% 
    mutate_at('Nha',~ifelse(.==0,1,.)) %>%
    mutate(Densidad_relativa = Nha /sum(Nha[!Especie == 'Total']) * 100 %>% round(),
           Dominancia_relativa = `Gha (m2/ha)` / sum(`Gha (m2/ha)`[!Especie == 'Total']) * 100) %>% 
    arrange(Especie) %>% 
    bind_cols(frec %>% 
                select(1,length(frec)) %>% 
                adorn_totals(name = 'Total') %>%
                select(2) %>% 
                `names<-`(c('Frecuencia_relativa'))) %>%
    mutate(
      IVI = (Densidad_relativa + Dominancia_relativa + Frecuencia_relativa) %>% round(1),
      `Gha (m2/ha)` = round(`Gha (m2/ha)`, 3),
    ) %>% 
    mutate_at(vars(contains('relativa')), round,1) %>% 
    slice_tail()
  
  IVI <- bind_rows(IVI_sp,IVI_tot)
  
  BD_Nha <- BD %>% 
    group_by(Parcela, Sup_parcela, UTM_E, UTM_N, Especie) %>% 
    summarise(n = sum(N_ind, na.rm = T)) %>%
    ungroup() %>% 
    mutate(n = (n * 10000 / Sup_parcela)) %>% 
    pivot_wider(names_from = Especie, values_from = n) %>% 
    mutate(Nha_Total = rowSums(select(., -c(1:4)), na.rm = TRUE)) %>% 
    mutate_at(-c(1:4), ~ ifelse(is.na(.), 0, .))
  
  prop <- BD %>% 
    filter(Especie %in% input$sp) %>% 
    group_by(Parcela, Sup_parcela, UTM_E, UTM_N, Estado) %>%
    summarise(n = sum(N_ind, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(n = (n * 10000 / Sup_parcela)) %>% 
    pivot_wider(names_from = Estado, values_from = n) %>% 
    mutate(Total = rowSums(select(., -c(1:4)), na.rm = TRUE)) %>% 
    mutate(
      across(-c(Parcela, Sup_parcela, UTM_E, UTM_N, Total), 
             ~ . / Total, 
             .names = "{col}_rel")
    ) %>% 
    mutate_at(-c(1:4), ~ ifelse(is.na(.), 0, .))
  
  area_x_meanTot <- st_area(st_union(BNP_cuenca)) %>% set_units(ha) %>% round(2) %>% drop_units() * IVI[IVI$Especie == input$sp,'Nha'] %>% pull()
  
  prop_2 <- prop %>% 
    pivot_longer(names_to = 'Estado',values_to = 'val',ends_with('_rel')) %>% 
    group_by(Estado) %>% 
    summarise(Proporciones = mean(val) %>% round(2)) %>% 
    mutate(Individuos_totales = (Proporciones*area_x_meanTot) %>% round()) %>% 
    mutate_at('Estado',str_remove,'_rel')
  
  nom_ssubc <- BNP_cuenca$NOM_SSUBC %>% unique()
  
  loca <- BD %>% 
    group_by(Parcela, UTM_E, UTM_N) %>% 
    tally() %>% 
    mutate(NOM_SSUBC = nom_ssubc) %>% 
    select(Parcela, NOM_SSUBC, UTM_E, UTM_N)
  
  #### Guardar datos en excel
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
  writeData(wb, 1 , x = "Elaborado para:", startCol = 3, startRow = 26)
  mergeCells(wb, 1, cols = 3:4, rows = 26)
  addStyle(wb, 1, style = createStyle(textDecoration = c("bold"), halign = "center", valign = "center"), rows = 26, cols = 3, gridExpand = T)
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
  desc_general <- c("Base de dato de Inventario forestal - Tabla de Rodal de la cuenca de estudio",paste0("La siguiente base de datos presenta los datos, conceptos y cálculos empleados para estimar la media poblacional del número de ejemplares de ",input$sp," en la superficie cubierta con bosque nativo de preservación de la especie en la cuenca de estudio. Para ello en cada una de las pestañas se emplea los siguientes conceptos:")) %>% 
    as_tibble_col()
  writeData(wb, 2, desc_general,startCol = 1,startRow = 1,colNames = F)
  for (i in 1:2) {
    mergeCells(wb, 2, cols = 1:2, rows = i)
  }
  desc_h1 <- c("BD_Inv.For:	Corresponde a la base de datos de inventario forestal registrada para cada una de las parcelas realizadas en las formaciones de bosque nativo de preservación, donde:
Parcela:	Nombre de la unidad de muestreo.
UTM_E:	Coordenada central Este de la parcela de muestreo en el sistema geodésico WGS84.
UTM_N:	Coordenada central Norte de la parcela de muestreo en el sistema geodésico WGS84.
Clase:	Clase diamétrica del Diametro equivalente
Cod_ssubc:	Código de la DGA para la subsubcuenca."
  ) %>% table.fun()
  writeData(wb, 2, desc_h1, startCol = 1,startRow = nrow(desc_general)+2, colNames = F)
  desc_h2 <- c('Nha, GHa, H, Frec: Corresponde a las tablas resumenes de la base de datos de inventario forestal de las cuales se extraen los valores para el calculo del índice IVI en la hoja "TABLA_IVI".  En esta se presenta el número de árboles por hectarea (Nha), el área basal por hectárea (Gha) y la altura promedio por clase diamétrica y especie arbórea. Además, se muestra una tabla con las frecuencias (n° de repeticiones) de cada especie arbórea. En esta ultima se calculan los siguientes datos por especie:
Count: Indica el número de parcelas donde se registró la especie.
Frec_abs: Frecuencia absoluta. Calculada como Count / n° total de parcelas * 100.
Frec_rel: Frecuencia relativa. Calculada como Frec_abs / suma total(Count) * 100.') %>% table.fun()
  writeData(wb, 2, desc_h2, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1, colNames = F)
  desc_h3 <- c("TABLA_IVI: En esta sección, se presenta una tabla resumen para el calculo del índice del valor de importancia (IVI) de cada especie arbórea. Las columnas Nha, Gha, H y Frecuencia relativa, se obtienen a partir de las tablas de la hoja 'Nha, GHa, H, Frec'. El resto de los campos se detallan a continuación:
Densidad_relativa: Frecuencia relativa del Nha. Nha/suma(Nha)*100.
Dominancia_relativa: Frecuencia relativa del Gha. Gha/suma(Gha)*100.
IVI: Valor de importancia.") %>% table.fun()
  writeData(wb, 2, desc_h3, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1, colNames = F)
  desc_h4 <- paste0("BD_Nha: Corresponde a la base de datos del número de árboles por hectárea por cada parcela y especie. En ella se utilizan conceptos previamente descritos en las pestañas anteriores") %>% table.fun()
  writeData(wb, 2, desc_h4, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1, colNames = F)
  desc_h5 <- c("Proporciones: Este segmento se refiere a las proporciones relacionadas con los distintos estados de desarrollo de la especie en cuestión, tal como se ha detallado en el informe de expertos. Esta hoja presenta una tabla con las proporciones de los estados de desarrollo de la especie, por cada parcela de inventario, las cuales se calcularon a partir del Nha por cada estado de desarrollo. Aparte se muestra una tabla con las proporciones promedio de todas las parcelas y su extrapolación a la superficie total de BNP.") %>% table.fun()
  writeData(wb, 2, desc_h5, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1, colNames = F)
  desc_h6 <- c("Localización UTM: Corresponde a la ubicación con las coordenadas del centro de cada pacela de inventario forestal.") %>% table.fun()
  writeData(wb, 2, desc_h6, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1, colNames = F)
  
  addStyle(
    wb,
    2,
    style = createStyle(
      textDecoration = c('bold', 'underline'),
      valign = 'center',
      halign = 'center',
      fontSize = 12,
      fgFill = "#D9D9D9"
    ),
    rows = c(
      1, 
      nrow(desc_general)+2,
      nrow(desc_general)+2+nrow(desc_h1)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1
    ),
    cols = 1
  )
  addStyle(
    wb,
    2,
    style = createStyle(
      valign = 'center',
      halign = 'center',
      fgFill = "#D9D9D9"
    ),
    rows = c(
      nrow(desc_general)+2,
      nrow(desc_general)+2+nrow(desc_h1)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1
    ),
    cols = 2
  )
  addStyle(
    wb,
    2,
    style = createStyle(textDecoration = 'bold', valign = 'center'),
    rows = as.numeric(list(desc_h1, desc_h2, desc_h3, desc_h4, desc_h5, desc_h6) %>% 
                        map(function(x){
                          x %>% add_row() %>% mutate(n = row_number())
                        }) %>% bind_rows() %>% rownames_to_column("row") %>% 
                        filter(n != 1 & !is.na(Campo)) %>% pull(row)) + 3,
    cols = 1
  )
  addStyle(wb, 2, style = createStyle(wrapText = T),stack = T, rows = 1:40, cols = 1:2, gridExpand = T)
  
  titlestyle <- createStyle(
    fgFill  = "cadetblue3",       # Color de fondo
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
  
  addWorksheet(wb, "BD_Inv.For")
  writeData(wb, 3, BD)
  addStyle(wb, 3, style = bodystyle, cols = 1:ncol(BD), rows = 1:nrow(BD)+1, gridExpand = TRUE)
  addStyle(wb, 3, style = headerstyle, cols = 1:ncol(BD), rows = 1)
  
  addWorksheet(wb, "Nha, GHa, H, Frec")
  setColWidths(wb, 4, cols = 1:8, widths = 'auto')
  writeData(wb, 4, x = "Nha x Especies", startCol = 1, startRow = 1)
  mergeCells(wb, 4, cols = 1:ncol(Nha), rows = 1)
  writeData(wb, 4, Nha, startRow = 2)
  
  writeData(wb, 4, x = "Gha x Especies", startCol = 1, startRow = nrow(Nha) + 4)
  mergeCells(wb, 4, cols = 1:ncol(Gha), rows = nrow(Nha) + 4)
  writeData(wb, 4, Gha, startRow = nrow(Nha) + 4 + 1)
  
  writeData(wb, 4, x = "Altura promedio", startCol = 1, startRow = nrow(Nha) + 3 + nrow(Gha) + 4)
  mergeCells(wb, 4, cols = 1:ncol(H), rows = nrow(Nha) + 3 + nrow(Gha) + 4)
  writeData(wb, 4, H, startRow = nrow(Nha) + 3 + nrow(Gha) + 4 + 1)
  
  writeData(wb, 4, x = "Frecuencias", startCol = 1, startRow = nrow(Nha) + 3 + nrow(Gha) + 3 + nrow(H) + 4)
  mergeCells(wb, 4, cols = 1:ncol(H), rows = nrow(Nha) + 3 + nrow(Gha) + 3 + nrow(H) + 4)
  writeData(wb, 4, frec, startRow = nrow(Nha) + 3 + nrow(Gha) + 3 + nrow(H) + 4 + 1)
  
  addStyle(wb, 4, style = titlestyle, cols = 1, rows = c(1,nrow(Nha) + 4,nrow(Nha) + 3 + nrow(Gha) + 4,nrow(Nha) + 3 + nrow(Gha) + 3 + nrow(H) + 4))
  
  addWorksheet(wb, "TABLA IVI")
  setColWidths(wb, 5, cols = 1:200, widths = 'auto')
  writeData(wb, 5, x = "Tabla Índice IVI", startRow = 1)
  mergeCells(wb, 5, cols = 1:ncol(IVI), rows = 1)
  writeData(wb, 5, IVI, startRow = 2)
  addStyle(wb, 5, style = titlestyle, cols = 1,rows = 1)
  addStyle(wb, 5, style = headerstyle, cols = 1:ncol(IVI),rows = 2)
  addStyle(wb, 5, style = bodystyle, cols = 1:ncol(IVI),rows = 1:nrow(IVI)+2,gridExpand = T)
  
  addWorksheet(wb, "BD_Nha")
  writeData(wb, 6, BD_Nha)
  addStyle(wb, 6, style = bodystyle, cols = 1:ncol(BD_Nha), rows = 1:nrow(BD_Nha)+1, gridExpand = TRUE)
  addStyle(wb, 6, style = headerstyle, cols = 1:ncol(BD_Nha), rows = 1)
  
  addWorksheet(wb, "Proporciones")
  setColWidths(wb, 7, cols = 1:20, widths = 'auto')
  writeData(wb, 7, prop)
  addStyle(wb, 7, style = bodystyle, cols = 1:ncol(prop), rows = 1:nrow(prop)+1, gridExpand = TRUE)
  addStyle(wb, 7, style = headerstyle, cols = 1:ncol(prop), rows = 1)
  
  writeData(wb, 7, x = "Proporciones e individuos totales x estado de desarrollo", startCol = ncol(prop)+2, startRow = 1)
  addStyle(wb, 7, style = titlestyle, cols = ncol(prop)+2, rows = 1)
  mergeCells(wb, 7, cols = 1:ncol(prop_2)+ncol(prop)+1, rows = 1)
  writeData(wb, 7, prop_2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all", startCol = ncol(prop)+2, startRow = 2)
  
  addWorksheet(wb, "Localización UTM")
  setColWidths(wb, 8, cols = 1:5, widths = 'auto')
  writeData(wb, 8, loca)
  addStyle(wb, 8, style = bodystyle, cols = 1:ncol(loca), rows = 1:nrow(loca)+1, gridExpand = TRUE)
  addStyle(wb, 8, style = headerstyle, cols = 1:ncol(loca), rows = 1)
  
  return(
    list(
      IVI = IVI,
      BD_Nha = BD_Nha,
      prop = prop_2,
      wb = wb
    )
  )
}

