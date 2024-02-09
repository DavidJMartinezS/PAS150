get_BD_Diversidad <- function(BD_flora, Obras, BNP_cuenca, input){
  wb <- createWorkbook()
  titlestyle <- createStyle(
    fgFill  = "cadetblue3",       # Color de fondo
    halign = "center",     # Alineación horizontal
    valign = "center"     # Alineación vertical
  )
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
  desc_general <- c("Base de dato de flora y biodiversidad en el área del proyecto","La siguiente base de datos expone los criterior y calculos para cuantificar la biodiversidad de la cuenca de estudio en las áreas de bosque nativo de preservación. Para poder lograr dicho objetivo, el proceso consta de varios procesos, explicados en las siguientes pestañas de cáculo:") %>% 
    as_tibble_col()
  writeData(wb, 2, desc_general,startCol = 1,startRow = 1,colNames = F)
  for (i in 1:2) {
    mergeCells(wb, 2, cols = 1:2, rows = i)
  }
  desc_h1 <- paste0("BD_Flora: Corresponde a la base de datos flora para la subsubcuenca de estudio, donde se detalla la categoría taxonómica de cada una de las especies junto con su RCE y piso vegetacional:
parcela: Parcela de muestreo.
utm_e: Coordenada central Este de la parcela de muestreo en el sistema geodésico WGS84.
utm_n: Coordenada central Norte de la parcela de muestreo en el sistema geodésico WGS84.
n_individuos: Número de ejemplares encontrados en la parcela de 1000 m2.
cob_bb: Cobertura vegetacional según Braun-Blanquet.
piso_eco: Piso vegetacional según Luebert y Pliscoff.
rce: Reglamento para Clasificar Especies según estado de conservación.
ds_68: Indica si se encuentra en el listado de especies dentro del DS 68.
sector: Se refiere a si el registro se presenta dentro del área de influencia del proyecto (AI) o fuera del área de influencia del proyecto (FAI)."
  ) %>% table.fun()
  writeData(wb, 2, desc_h1, startCol = 1,startRow = nrow(desc_general)+2, colNames = F)
  desc_h2 <- c('Localización UTM: Corresponde a la localización de cada una de las parcelas de inventario florístico levantado en cada una de las unidades de bosque nativo de preservación') %>% table.fun()
  writeData(wb, 2, desc_h2, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1, colNames = F)
  desc_h3 <- c("Composición BNP Cuenca: Corresponde a un resumen de la base de datos de flora, donde se muestra para cada una de las especie, el hábito, origen, categoría de conservación y decretos que la sustentan.") %>% table.fun()
  writeData(wb, 2, desc_h3, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1, colNames = F)
  desc_h4 <- c(
    "Composición BNP x Piso: En esta pestaña se muestra el conteo de las especies según su origen para cada piso vegetacional, a partir de la cual se obtiene la riqueza de especies. Por otro lado se muestran las proporciones en que se encuentra cada especie según la cuenta presentada en las parcelas de 500 m2. A partir de las proporciones se puso obtener el índice de biodiversidad de Simpson de cada piso de vegetación. Por último, se presenta una tabla resumen con la riqueza de especies por hábito y origen, índice de Simpson y superficie de BNP por cada piso de vegetación.
  n: Total de indivduos muestreados de la especie (Suma Count_500m2 x especie).
  p: Proporción de la presencia de la especie (n/suma total(n)).") %>% table.fun()
  writeData(wb, 2, desc_h4, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1, colNames = F)
  desc_h5 <- c("Composición BNP x Sector: Esta pestaña muestra el número de especies por sector. Las que se presentan solo en el área de influencia, solo fuera del área de influencia, y las que están en ambos sectores.") %>% table.fun()
  writeData(wb, 2, desc_h5, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1, colNames = F)
  desc_h6 <- c(
    "Composición BNP x Sector_D.alfa: Esta pestaña muestra el calculo de los índices de Simpson y Shannon por cada sector, calculados a partir de la proporción de cada especie en la cuenca.
  n: Total de indivduos muestreados de la especie (Suma Count_500m2 x especie).
  p: Proporción de la presencia de la especie (n/suma total(n)).
  Ln(p): Logaritmo natural de p.
  p x Ln(p): Producto entre p y su logaritmo natural.") %>% table.fun()
  writeData(wb, 2, desc_h6, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1, colNames = F)
  desc_h7 <- c("Frecuencia_Sp Acompañante: Por último, se presenta cada una de las especies presentes en la subsubcuenca de estudio, junto su frecuencia (n) y proporción (percent).") %>% table.fun()
  writeData(wb, 2, desc_h7, startCol = 1,startRow = nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1+nrow(desc_h6)+1, colNames = F)
  
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
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1+nrow(desc_h6)+1
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
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1,
      nrow(desc_general)+2+nrow(desc_h1)+1+nrow(desc_h2)+1+nrow(desc_h3)+1+nrow(desc_h4)+1+nrow(desc_h5)+1+nrow(desc_h6)+1
    ),
    cols = 2
  )
  addStyle(
    wb,
    2,
    style = createStyle(textDecoration = 'bold', valign = 'center'),
    rows = as.numeric(list(desc_h1, desc_h2, desc_h3, desc_h4, desc_h5, desc_h6,desc_h7) %>% 
                        map(function(x){
                          x %>% add_row() %>% mutate(n = row_number())
                        }) %>% bind_rows() %>% rownames_to_column("row") %>% 
                        filter(n != 1 & !is.na(Campo)) %>% pull(row)) + 3,
    cols = 1
  )
  addStyle(wb, 2, style = createStyle(wrapText = T),stack = T, rows = 1:40,cols = 1:2,gridExpand = T)
  
  ptos_todos <- BD_flora %>% 
    group_by(Parcela, UTM_E, UTM_N) %>% 
    tally() %>% 
    ungroup() %>% na.omit() %>% 
    st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = 32719)
  
  ptos_BNP <- ptos_todos %>% 
    st_intersection(st_geometry(BNP_cuenca)) %>% 
    st_drop_geometry() %>% 
    pull(Parcela)
  
  BNP_int <- BNP_cuenca[Obras,]
  
  ptos_AI <- ptos_todos %>% 
    st_intersection(st_geometry(BNP_int)) %>% 
    st_drop_geometry() %>% 
    pull(Parcela)
  
  pisos_bnp <- read_sf('~/Data_SIG/PisosVegetacionalesPliscoff2017.shp') %>% 
    st_zm() %>% st_transform(32719) %>% 
    st_intersection(st_union(BNP_cuenca))
  
  pisos_ptos <- ptos_todos %>% 
    st_intersection(pisos_bnp) %>% 
    select(Parcela, piso) %>% 
    st_drop_geometry()
  
  BD_flora2 <- BD_flora %>% 
    mutate_at("Especie", str_trim) %>% 
    filter(Parcela %in% ptos_BNP,
           !str_detect(Especie,"sp.$")) %>% 
    mutate(SECTOR = case_when(Parcela %in% ptos_AI ~ 'AI',
                              T ~ 'FAI')) %>% 
    inner_join(pisos_ptos,by=c('Parcela')) %>% 
    rename(PISO_ECO = piso) %>% 
    clean_names() %>% 
    mutate_at("n_ind",~as.numeric(str_replace(.,"muerto","0")))
  
  addWorksheet(wb, "BD_Flora", zoom = 60)
  setColWidths(wb, 3, cols = 1:ncol(BD_flora2),widths = "auto")
  writeData(wb, 3, BD_flora2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  # Localización UTM
  Localizacion <- BD_flora2 %>% 
    group_by(parcela,utm_e,utm_n) %>% 
    tally() %>% select(-n)
  addWorksheet(wb, "Localización UTM")
  setColWidths(wb, 4, cols = 1:ncol(Localizacion),widths = 17)
  writeData(wb, 4, "Ubicación de las parcelas de inventario florístico",startCol = 1, startRow = 1)
  addStyle(wb, 4, style = titlestyle, rows = 1, cols = 1)
  mergeCells(wb, 4, cols = 1:ncol(Localizacion),rows = 1)
  writeData(wb, 4, Localizacion, startCol = 1, startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  # Composición BNP cuenca
  Compo_x_cuenca <- BD_flora2 %>% 
    group_by(especie, habito, origen, rce, ds_68) %>% 
    tally() %>% select(-n)
  addWorksheet(wb, "Composición BNP Cuenca")
  setColWidths(wb, 5, cols = 1:ncol(Compo_x_cuenca),widths = "auto",ignoreMergedCells = T)
  writeData(wb, 5, "Caracterización de las especies presentes en la base de datos")
  addStyle(wb, 5, style = titlestyle, rows = 1, cols = 1)
  mergeCells(wb, 5, cols = 1:ncol(Compo_x_cuenca), rows = 1)
  writeData(wb, 5, Compo_x_cuenca, startCol = 1, startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  # Composición BNP por piso
  addWorksheet(wb, "Composición BNP x Piso", zoom = 80)
  setColWidths(wb, 6, cols = 1:100, widths = 15)
  Compo_x_piso_uni <- BD_flora2 %>% 
    group_by(habito, especie, origen) %>%
    tally() %>% 
    mutate(n=1) %>% ungroup() %>% 
    pivot_wider(names_from = origen, values_from = n)
  writeData(wb, 6, "Composición por especie y origen",startCol = 1,startRow = 1)
  addStyle(wb, 6, style = titlestyle,rows = 1,cols = 1,stack = T)
  mergeCells(wb, 6, cols = 1:ncol(Compo_x_piso_uni),rows = 1)
  writeData(wb, 6, Compo_x_piso_uni,startCol = 1,startRow = 2,headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  pisos <- BD_flora2$piso_eco %>% unique()
  startcol <- ncol(Compo_x_piso_uni)+2
  for (i in seq_along(pisos)) {
    compo_piso <- BD_flora2 %>% 
      filter(piso_eco == pisos[i]) %>% 
      group_by(habito, especie, origen) %>%
      tally() %>% 
      mutate(n=1) %>% ungroup() %>% 
      pivot_wider(names_from = origen, values_from = n)
    writeData(wb, 6, pisos[i],startCol = startcol, startRow = 1)
    addStyle(wb, 6, style = titlestyle, rows = 1,cols = startcol)
    mergeCells(wb, 6, cols = startcol:(startcol+ncol(compo_piso)-1),rows = 1)
    writeData(wb, 6, compo_piso, startCol = startcol,startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
    startcol <- startcol + ncol(compo_piso) + 1
  }
  startcol_tabla <- startcol
  
  prop_x_piso_uni <- BD_flora2 %>% 
    filter(!str_detect(habito, 'Hierba')) %>% 
    group_by(especie) %>% 
    summarise(n = sum(n_ind,na.rm = T)) %>% 
    mutate(n = if_else(n == 0, 1, n)) %>% 
    mutate(p = n/sum(n),
           `Ln(p)`=log(p),
           `p x Ln(p)` = p * `Ln(p)`)
  startrow <- nrow(Compo_x_piso_uni)+5 # separación de 2 filas
  writeData(wb, 6, "Proporción de especies",startCol = 1,startRow = startrow)
  addStyle(wb, 6, style = titlestyle,rows = startrow,cols = 1,stack = T)
  mergeCells(wb, 6, cols = 1:ncol(prop_x_piso_uni),rows = startrow)
  writeData(wb, 6, prop_x_piso_uni,startCol = 1,startRow = startrow + 1,headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  startcol <- ncol(prop_x_piso_uni)+2
  sc <- function(list){list %>% map_dbl(~.x^2) %>% sum()} # función suma cuadrados
  indice_simpson <- tibble(`Índice de Simpson (a)` = prop_x_piso_uni$p %>% sc() %>% round(2), # Calculo del Indice de Simpson total
                           Shannon = (-1*sum(prop_x_piso_uni$`p x Ln(p)`) %>% round(2)), # Cálculo índice de shannon total
                           piso = 'Total')
  for (i in seq_along(pisos)) {
    prop_piso <- BD_flora2 %>% 
      filter(!str_detect(habito, 'Hierba') &
               piso_eco == pisos[i]) %>% 
      group_by(especie) %>% 
      summarise(n = sum(n_ind, na.rm = T)) %>% 
      mutate(n = if_else(n == 0, 1, n)) %>% 
      mutate(p = n/sum(n),
             `Ln(p)`=log(p),
             `p x Ln(p)` = p * `Ln(p)`)
    df <- tibble(`Índice de Simpson (a)` = prop_piso$p %>% sc() %>% round(2), # Calculo del Indice de Simpson x piso
                 Shannon = (-1*sum(prop_piso$`p x Ln(p)`) %>% round(2)), # Cálculo índice de shannon x piso
                 piso = pisos[i]) 
    indice_simpson <- rbind(indice_simpson,df)
    writeData(wb, 6, pisos[i],startCol = startcol, startRow = startrow)
    addStyle(wb, 6, style = titlestyle, rows = startrow,cols = startcol)
    mergeCells(wb, 6, cols = startcol:(startcol+ncol(prop_piso)-1),rows = startrow)
    writeData(wb, 6, prop_piso, startCol = startcol,startRow = startrow+2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
    startcol <- startcol + ncol(prop_piso) + 1
  }
  
  riqueza <- BD_flora2 %>% 
    mutate(habito = if_else(str_detect(habito, 'Hierba'), 'Hierba',habito)) %>% 
    group_by(habito, piso_eco, especie) %>% 
    tally() %>% 
    group_by(habito, piso_eco) %>% 
    tally() %>% 
    pivot_wider(names_from = piso_eco, values_from = n) %>% 
    cbind(BD_flora2 %>% 
            mutate(habito = if_else(str_detect(habito, 'Hierba'), 'Hierba',habito)) %>% 
            group_by(habito, especie) %>% 
            tally() %>% 
            group_by(habito) %>% 
            tally(name = 'Total') %>% 
            select(Total)) %>% 
    adorn_totals(name = 'Riqueza específica (S)') %>% 
    rename('Hábito/Origen' = habito) %>% 
    as_tibble()
  riqueza_x_sp <- riqueza %>% 
    head(n = nrow(riqueza) - 1)
  riqueza_esp <- riqueza %>% 
    slice_tail()
  
  origen <- BD_flora2 %>% 
    group_by(origen, piso_eco,especie) %>% 
    tally() %>% 
    group_by(origen, piso_eco) %>% 
    tally() %>% 
    pivot_wider(names_from = piso_eco, values_from = n) %>% 
    cbind(
      BD_flora2 %>% 
        group_by(origen,especie) %>% 
        tally() %>% 
        group_by(origen) %>% 
        tally(name = 'Total') %>% 
        select(Total)
    ) %>% 
    rename('Hábito/Origen' = origen) %>% 
    as_tibble()
  
  Simpson <- indice_simpson %>% 
    mutate(`Diversidad (1-a)` = 1 - `Índice de Simpson (a)`) %>% 
    pivot_longer(names_to = 'Hábito/Origen',values_to = 'val',-piso) %>% 
    pivot_wider(names_from = piso, values_from = val) %>% 
    relocate(Total, .after = last_col()) %>% 
    arrange(factor(`Hábito/Origen`, levels = c('Índice de Simpson (a)','Diversidad (1-a)','Shannon')))
  
  BNP_sup <- pisos_bnp %>% 
    mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
    drop_units() %>% st_drop_geometry() %>% 
    select(piso,Sup_ha) %>% 
    adorn_totals() %>% 
    pivot_wider(names_from = piso, values_from = Sup_ha) %>% 
    mutate(`Hábito/Origen` = 'Superficie BNP (ha)') %>% 
    relocate(`Hábito/Origen`) %>% 
    select(colnames(Simpson))
  
  Tabla_resumen <- bind_rows(
    riqueza_x_sp, riqueza_esp, origen, Simpson, BNP_sup
  )
  setColWidths(wb, 6, cols = (startcol_tabla+1):(startcol_tabla+ncol(Tabla_resumen)-2),widths = 22)
  writeData(wb, 6, Tabla_resumen, startCol = startcol_tabla, startRow = 1,headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight', fgFill = "#BBBBBB"),borders = "all")
  
  addStyle(wb, 6, style = createStyle(wrapText = T),stack = T, rows = 1, cols = 1:100,gridExpand = T)
  
  # Composición BNP por sector
  addWorksheet(wb, "Composición BNP x Sector", zoom = 70)
  setColWidths(wb, 7, cols = 1:14, widths = "auto",ignoreMergedCells = T)
  compo_x_sector_uni <- BD_flora2 %>% 
    mutate(Count = 1) %>% 
    as_tibble() %>% 
    group_by(especie, sector) %>% 
    summarise(n = mean(Count)) %>% 
    pivot_wider(names_from = sector, values_from = n) %>% 
    mutate_at(vars(contains("AI")),~ifelse(is.na(.),0,.))
  if (!"AI" %in% names(compo_x_sector_uni)){
    compo_x_sector_uni$AI <- rep(0,nrow(compo_x_sector_uni)) 
  } else if (!"FAI" %in% names(compo_x_sector_uni)){
    compo_x_sector_uni$FAI <- rep(0,nrow(compo_x_sector_uni)) 
  } 
  compo_x_sector_uni <- compo_x_sector_uni %>% mutate(`AI-FAI` = AI - FAI)
  
  writeData(wb, 7, "Cuenca de especies por sector sector", startCol = 1, startRow = 1)
  addStyle(wb, 7, style = titlestyle, rows = 1, cols = 1)
  mergeCells(wb, 7, cols = 1:ncol(compo_x_sector_uni), rows = 1)
  writeData(wb, 7, compo_x_sector_uni, startCol = 1,startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  compo_x_sector_AI <- compo_x_sector_uni[compo_x_sector_uni$AI==1,] %>% 
    select(especie, AI)
  writeData(wb, 7, "Especies dentro del área de influencia (AI) del proyecto",startCol = ncol(compo_x_sector_uni)+2, startRow = 1)
  addStyle(wb, 7, style = titlestyle, rows = 1, cols = ncol(compo_x_sector_uni)+2)
  mergeCells(wb, 7, cols = (ncol(compo_x_sector_uni)+2):(ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+1), rows = 1)
  writeData(wb, 7, compo_x_sector_AI,startCol = ncol(compo_x_sector_uni)+2,startRow = 2,headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  compo_x_sector_FAI <- compo_x_sector_uni[compo_x_sector_uni$FAI==1,] %>% 
    select(especie, FAI)
  writeData(wb, 7, "Especies fuera del área de influencia (FAI) del proyecto",startCol = ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+3, startRow = 1)
  addStyle(wb, 7, style = titlestyle, rows = 1, cols = ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+3)
  mergeCells(wb, 7, cols = (ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+3):(ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+ncol(compo_x_sector_FAI)+2), rows = 1)
  writeData(wb, 7, compo_x_sector_FAI, startCol = ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+3, startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  compo_x_sector_resumen <- compo_x_sector_uni %>% 
    group_by(`AI-FAI`) %>% count(name = 'N') %>% 
    mutate(SECTOR = if_else(`AI-FAI` == -1, 'FAI', if_else(`AI-FAI` == 0, 'AI + FAI','AI')))
  writeData(wb, 7, "Recuento de especies por sector",startCol = ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+ncol(compo_x_sector_FAI)+4, startRow = 1)
  addStyle(wb, 7, style = titlestyle, rows = 1, cols = ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+ncol(compo_x_sector_FAI)+4)
  mergeCells(wb, 7, cols = (ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+ncol(compo_x_sector_FAI)+4):(ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+ncol(compo_x_sector_FAI)+ncol(compo_x_sector_resumen)+3), rows = 1)
  writeData(wb, 7, compo_x_sector_resumen, startCol = ncol(compo_x_sector_uni)+ncol(compo_x_sector_AI)+ncol(compo_x_sector_FAI)+4, startRow = 2,headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  addStyle(wb, 7, style = createStyle(wrapText = T), stack = T, rows = 1, cols = 1:12, gridExpand = T)
  
  # Figure Riqueza
  riq <- compo_x_sector_resumen$N
  df <- tibble(
    x = c(1, 3),
    y = rep(3, 2),
    sizes = c(1.5, 1.5),
    sector = c('FAI', 'AI')
  ) 
  df_riq <- tibble(x = c(.5, 2, 3.5),
                   y = rep(3, 3),
                   riqueza = riq)
  df_sec <- tibble(
    x = c(1, 3),
    y = rep(1.2, 2),
    sector = c(
      'Especies fuera del área \nde proyecto',
      'Especies dentro del área \nde proyecto'
    )
  )
  title <- paste0('Cuenca de estudio (', sum(riq), ' spp)')
  
  plot <- ggplot() +
    ggforce::geom_circle(
      aes(
        x0 = x,
        y0 = y,
        r = sizes,
        fill = sector
      ),
      data = df,
      lwd = 1,
      alpha = .3,
      color = 'grey70',
      show.legend = F
    ) +
    scale_fill_manual(values = c("#79AF97FF", "#DF8F44FF")) +
    coord_equal() +
    scale_y_continuous(expand = c(.1, -.1)) +
    annotate(
      "text",
      x = df_riq$x,
      y = df_riq$y,
      label = df_riq$riqueza,
      size = 10,
      color = 'grey25'
    ) +
    annotate(
      'text',
      x = df_sec$x,
      y = df_sec$y,
      label = df_sec$sector,
      size = 6,
      color = c("#DF8F44FF", "#79AF97FF")
    ) +
    labs(title = title) +
    theme_void() +
    theme(
      plot.background = element_rect(colour = 'black'),
      plot.title.position = 'plot',
      plot.title = element_text(
        hjust = .5,
        size = 20,
        colour = '#A0968C',
        family = "Franklin Gothic Demi"
      ),
      text = element_text(family = "Franklin Gothic Book")
    )
  
  # Composición BNP x Sector_D.alfa
  addWorksheet(wb, "Composición BNP x Sector_D.alfa", zoom = 70)
  setColWidths(wb, 8, cols = 1:25,widths = "auto",ignoreMergedCells = T)
  D.alfa_uni <- BD_flora2 %>% 
    filter(!str_detect(habito, 'Hierba'),
           # str_to_lower(cob_bb) != 'fp'
    ) %>% 
    group_by(especie) %>% 
    summarise(n = sum(n_ind, na.rm = T)) %>% 
    mutate(n = if_else(n == 0, 1, n)) %>% 
    mutate(p = n/sum(n),
           `Ln(p)`=log(p),
           `p x Ln(p)` = p * `Ln(p)`) 
  writeData(wb, 8, "Proceso de cálculos para los índices de biodiversidad en la subsubcuenca",startCol = 1, startRow = 1)
  addStyle(wb, 8, style = titlestyle, rows = 1, cols = 1)
  mergeCells(wb, 8, cols = 1:ncol(D.alfa_uni),rows = 1)
  writeData(wb, 8, D.alfa_uni, startCol = 1,startRow = 2,headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  D.alfa_uni_index <- tibble(SECTOR = 'AI+FAI', Simpson = sc(D.alfa_uni$p), Shannon = -1*sum(D.alfa_uni$`p x Ln(p)`)) %>% mutate(Div.Simpson = 1 - Simpson)
  
  D.alfa_AI <- BD_flora2 %>% 
    filter(!str_detect(habito, 'Hierba'),
           # str_to_lower(cob_bb) != 'fp'
           sector == 'AI') %>% 
    group_by(especie) %>% 
    summarise(n = sum(n_ind, na.rm = T)) %>% 
    mutate(n = if_else(n == 0, 1, n)) %>%
    mutate(p = n/sum(n),
           `Ln(p)`=log(p),
           `p x Ln(p)` = p * `Ln(p)`) 
  writeData(wb, 8, "Proceso de cálculos para los índices de biodiversidad en el área de influencia",startCol = ncol(D.alfa_uni)+2, startRow = 1)
  addStyle(wb, 8, style = titlestyle, rows = 1, cols = ncol(D.alfa_uni)+2)
  mergeCells(wb, 8, cols = (ncol(D.alfa_uni)+2):(ncol(D.alfa_uni)+ncol(D.alfa_AI)+1),rows = 1)
  writeData(wb, 8, D.alfa_AI, startCol = ncol(D.alfa_uni)+2, startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  D.alfa_AI_index <- tibble(SECTOR = 'AI', Simpson = sc(D.alfa_AI$p), Shannon = -1*sum(D.alfa_AI$`p x Ln(p)`)) %>% mutate(Div.Simpson = 1 - Simpson)
  
  D.alfa_FAI <- BD_flora2 %>% 
    filter(!str_detect(habito, 'Hierba') &
             # str_to_lower(cob_bb) != 'fp'
             sector == 'FAI') %>% 
    group_by(especie) %>% 
    summarise(n = sum(n_ind, na.rm = T)) %>% 
    mutate(n = if_else(n == 0, 1, n)) %>% 
    mutate(p = n/sum(n),
           `Ln(p)`=log(p),
           `p x Ln(p)` = p * `Ln(p)`)
  writeData(wb, 8, "Proceso de cálculos para los índices de biodiversidad fuera del área de influencia",startCol = ncol(D.alfa_uni)+ncol(D.alfa_AI)+3, startRow = 1)
  addStyle(wb, 8, style = titlestyle, rows = 1, cols = ncol(D.alfa_uni)+ncol(D.alfa_AI)+3)
  mergeCells(wb, 8, cols = (ncol(D.alfa_uni)+ncol(D.alfa_AI)+3):(ncol(D.alfa_uni)+ncol(D.alfa_AI)+ncol(D.alfa_FAI)+2),rows = 1)
  writeData(wb, 8, D.alfa_FAI, startCol = ncol(D.alfa_uni)+ncol(D.alfa_AI)+3, startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  D.alfa_FAI_index <- tibble(SECTOR = 'FAI', Simpson = sc(D.alfa_FAI$p), Shannon = -1*sum(D.alfa_FAI$`p x Ln(p)`)) %>% mutate(Div.Simpson = 1 - Simpson)
  
  D.alfa_index <- rbind(D.alfa_AI_index,D.alfa_FAI_index,D.alfa_uni_index) %>% select(SECTOR, Simpson, Div.Simpson, Shannon)
  writeData(wb, 8, "Índices de biodiversidad por sector",startCol = ncol(D.alfa_uni)+ncol(D.alfa_AI)+ncol(D.alfa_FAI)+4, startRow = 1)
  addStyle(wb, 8, style = titlestyle, rows = 1, cols = ncol(D.alfa_uni)+ncol(D.alfa_AI)+ncol(D.alfa_FAI)+4)
  mergeCells(wb, 8, cols = (ncol(D.alfa_uni)+ncol(D.alfa_AI)+ncol(D.alfa_FAI)+4):(ncol(D.alfa_uni)+ncol(D.alfa_AI)+ncol(D.alfa_FAI)+ncol(D.alfa_index)+3),rows = 1)
  writeData(wb, 8, D.alfa_index, startCol = ncol(D.alfa_uni)+ncol(D.alfa_AI)+ncol(D.alfa_FAI)+4, startRow = 2, headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight', fgFill = "darkolivegreen1"),borders = "all")
  
  addStyle(wb, 8, style = createStyle(wrapText = T), stack = T, rows = 1, cols = 1:25, gridExpand = T)
  
  # Frecuencia Especies Acompañantes
  addWorksheet(wb, "Frecuencia_Sp Acompañante")
  setColWidths(wb, 9, cols = 1:3,widths = "auto",ignoreMergedCells = T)
  Frec_sp_acomp <- BD_flora2 %>% 
    group_by(parcela,especie) %>% 
    tally() %>% 
    mutate(n=1) %>% 
    group_by(especie) %>% 
    tally() %>%  
    mutate(Frec = n/nrow(Localizacion)) %>% 
    adorn_pct_formatting(,,,Frec) %>%
    arrange(desc(n)) %>% 
    as_tibble()
  writeData(wb, 9, "Frecuencia de especies acompañantes",startCol = 1, startRow = 1)
  addStyle(wb, 9, style = titlestyle, rows = 1, cols = 1)
  mergeCells(wb, 9, cols = 1:ncol(Frec_sp_acomp),rows = 1)
  writeData(wb, 9, Frec_sp_acomp, startCol = 1, startRow = 2,headerStyle = createStyle(halign = 'center',textDecoration = 'bold',border = 'TopBottomLeftRight'),borders = "all")
  
  spp_acomp <- Frec_sp_acomp %>% 
    mutate_at("Frec", ~as.double(str_remove(., "%"))/100) %>% 
    filter(Frec >= 0.25, especie != input$sp) %>% 
    left_join(
      compo_x_sector_AI %>% 
        mutate(presencia = "Si")
    ) %>% 
    mutate_at("presencia", replace_na, "No") %>% 
    select(especie, Frec, presencia)
  
  return(
    list(
      Tabla_resumen = Tabla_resumen, 
      D.alfa_index = D.alfa_index,
      spp_acomp = spp_acomp,
      compo_x_sector_resumen = compo_x_sector_resumen,
      plot = plot,
      wb = wb
    )
  )
}


