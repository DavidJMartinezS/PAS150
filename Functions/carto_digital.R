get_carto_digital <- function(uso_veg, BNP_alt = NULL, obras, censo, sp, BD_flora, BD_fore, densidad){
  
  crs_epsg <- if_else(
    uso_veg %>% st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_centroid() %>% st_coordinates() %>% .[,1] >= -72,
    32719, 
    32718
  )
  
  uso_veg <- uso_veg %>% 
    st_zm() %>% 
    st_transform(crs_epsg) %>% 
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>% 
    rename_all(~ if_else(. == "geometry", ., str_to_sentence(stri_trans_general(.,"Latin-ASCII")))) %>% 
    rename_at(vars(contains("ecc")), str_to_upper) 
  
  obras <- obras %>% 
    st_zm() %>% 
    st_transform(crs_epsg) %>% 
    st_make_valid()
  
  censo <- censo %>% 
    st_zm() %>% 
    st_transform(crs_epsg)
  
  get_cuenca <- function(uso_veg){
    uso_veg <- uso_veg %>% st_union()
    cuencas <- read_sf('N:/Dashboard PAS 150/SubsubcuencasBNA/Subsubcuencas_BNA.shp') %>% 
      st_zm() %>% 
      st_transform(crs_epsg)
    cuenca <- cuencas[uso_veg,][which.max(st_area(st_intersection(cuencas,uso_veg))),] 
    return(cuenca)
  }
  cuenca <- get_cuenca(uso_veg)
  
  get_caminos <- function(cuenca){
    read_sf('N:/Dashboard PAS 150/Red_Vial_Chile_18_07_2023.gdb') %>% 
      st_zm() %>% 
      st_transform(crs_epsg) %>%
      st_intersection(cuenca) %>% 
      mutate(Tipo_Cam = if_else(str_detect(CLASIFICACION,'Internacional|Nacional|Regional Principal'),1,
                                if_else(str_detect(CLASIFICACION,'Regional Provincial|Regional Comunal'),2,
                                        if_else(str_detect(CLASIFICACION,'Acceso'),3,4))),
             FUENTE = 'MOP 1:5.000 - 1:25.000') %>% 
      select(COD_SSUBC,NOM_SSUBC,ROL_ID,CALZADA,ORIENTACION,ROL_LABEL,CODIGO_CAMINO,NOMBRE_CAMINO,CLASIFICACION,CARPETA,ENROLADO,CONCESIONADO,REGION, Tipo_Cam, FUENTE)
  }
  caminos <- get_caminos(cuenca)
  
  get_CN <- function(cuenca){
    dem_stars <- elevatr::get_elev_raster(sf::st_buffer(cuenca,100) %>% sf::st_transform(4326), source = "ALOS_PALSAR", z = 12) %>%
      raster::projectRaster(crs=crs_epsg) %>%
      stars::st_as_stars() %>%
      `names<-`("Elevacion")
    step <- 50
    curv_niv <- st_contour(
      dem_stars,
      contour_lines = T,
      breaks = seq(
        plyr::round_any(min(dem_stars$Elevacion, na.rm = T), step, ceiling),
        plyr::round_any(max(dem_stars$Elevacion, na.rm = T), step, floor),
        step
      )
    ) %>%
      st_intersection(st_union(cuenca)) %>%
      mutate(NOM_SSUBC = cuenca$NOM_SSUBC,
             Elevacion = str_c(Elevacion,' m')) %>% 
      select(NOM_SSUBC, Elevacion)
    
  }
  curv_niv <- get_CN(cuenca)
  
  get_hidro <- function(cuenca){
    cod_ssbuc <- cuenca$COD_SSUBC
    read_sf('N:/Dashboard PAS 150/Hidrografia_V2/Hidrografia_V2 (1).shp',query = paste0("SELECT * FROM \"Hidrografia_V2 (1)\" WHERE COD_SSUBC = '",cod_ssbuc,"'")) %>% 
      st_set_geometry('geometry') %>% 
      st_transform(crs_epsg) %>% 
      st_intersection(st_union(cuenca)) %>% 
      mutate(Tipo_Dren = if_else(TIPO =='Rio',1,
                                 if_else(TIPO =='Estero',2,
                                         if_else(TIPO =='Arroyo',3,
                                                 if_else(TIPO =='Quebrada',4,5))))) %>% 
      select(COD_CUEN, COD_SUBC, COD_SSUBC, NOM_SSUBC, STRAHLER_N, NOMBRE, TIPO, DIRECCION, Tipo_Dren)
  }
  hidro <- get_hidro(cuenca)
  
  get_ubicacion <- function(obras, cuenca){
    obras %>%
      st_zm() %>%
      st_intersection(st_geometry(cuenca)) %>%
      summarise(geometry = st_union(geometry)) %>%
      mutate(
        NOM_SSUBC = cuenca$NOM_SSUBC,
        Centro_X = st_coordinates(st_centroid(geometry))[, 1] %>% as.vector(),
        Centro_Y = st_coordinates(st_centroid(geometry))[, 2] %>% as.vector(),
        Sup_ha = st_area(geometry) %>% set_units(hectare) %>% round(2),
      ) %>%
      drop_units() %>%
      select(NOM_SSUBC, Centro_X, Centro_Y, Sup_ha)
  }
  ubicacion <- get_ubicacion(obras, cuenca)
  
  get_obras <- function(obras, cuenca){
    obras %>%
      st_zm() %>% 
      st_intersection(st_geometry(cuenca)) %>% 
      st_cast('MULTIPOLYGON') %>% 
      st_cast('POLYGON') %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2),
             Sup_m2 = st_area(geometry) %>% set_units(m2) %>% round(2),
             Centro_X = st_coordinates(st_centroid(geometry))[,1],
             Centro_Y = st_coordinates(st_centroid(geometry))[,2]) %>% 
      drop_units() %>% 
      relocate(geometry, .after = last_col())
  }
  obras_cuenca <- get_obras(obras, cuenca)
  
  get_BNP_cuenca <- function(uso_veg, sp, cuenca){
    uso_veg %>% 
      filter(str_detect(F_ley20283, "preser") & str_detect(BNP_ECC,sp)) %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2),
             NOM_SSUBC = cuenca$NOM_SSUBC) %>% 
      drop_units() %>% 
      select(NOM_SSUBC, Formacion, Tipo_for, Subtipo_fo, starts_with('ECC'), F_ley20283, BNP_ECC, Sup_ha) %>% 
      relocate(Sup_ha, .before = geometry) %>% 
      arrange(Sup_ha)
  } 
  BNP_cuenca <- get_BNP_cuenca(uso_veg, sp, cuenca)
  
  get_BNP_intervencion <- function(BNP_cuenca, cuenca, obras_cuenca){
    BNP_cuenca %>%
      st_intersection(obras_cuenca) %>% 
      mutate(NOM_SSUBC = cuenca$NOM_SSUBC,
             Afectacion = "Intervencion") %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2),
             Sup_m2 = st_area(geometry) %>% set_units(m2) %>% round(2)) %>%
      drop_units() %>% 
      select(-c(Sup_ha.1, Centro_X, Centro_Y)) %>% 
      relocate(Sup_ha, .before = geometry) %>% 
      relocate(Sup_m2, .before = Sup_ha) %>% 
      arrange(Sup_m2)
  }
  BNP_inter <- get_BNP_intervencion(BNP_cuenca,cuenca, obras_cuenca)
  
  get_BNP_alterar <- function(BNP_alt, BNP_cuenca){
    BNP_alt %>% 
      rename_all(str_to_sentence) %>% 
      select(Obra) %>% 
      st_intersection(BNP_cuenca) %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
      drop_units()
  }
  if (is.null(BNP_alt)) {
    BNP_alterar <- BNP_inter %>% slice(0)
  } else {
    BNP_alterar <- get_BNP_alterar(BNP_alt, BNP_cuenca)
  }
  
  get_ECC_alt <- function(BNP_alterar, censo, sp){
    censo %>% 
      filter(Especie %>% str_detect(sp)) %>%
      st_intersection(BNP_alterar) %>% 
      mutate(Afectacion = "Alteración del hábitat") %>% 
      select(Especie, Afectacion, Obra, UTM_E, UTM_N)
  }
  ECC_alt <- get_ECC_alt(BNP_alterar, censo, sp)
  
  get_ECC_int <- function(BNP_inter, BNP_alterar, censo, cuenca, sp){
    codes <- censo %>% 
      rowid_to_column("ID") %>% 
      filter(Especie %>% str_detect(sp)) %>%
      st_intersection(st_union(BNP_alterar)) %>% pull(ID)
    dentro <- censo %>% 
      rowid_to_column("ID") %>% 
      filter(!ID %in% codes) %>% 
      select(ID, Especie) %>% 
      st_intersection(st_union(BNP_inter)) %>% 
      st_join(BNP_inter,join = st_intersects) %>% 
      mutate(Afectacion = 'Eliminación') %>% 
      select(Especie,Afectacion,Obra) 
    buffer <- BNP_inter %>% 
      st_buffer(5) %>% 
      st_difference(st_union(BNP_inter)) %>% 
      st_collection_extract('POLYGON') %>% 
      st_cast('MULTIPOLYGON') %>% 
      st_cast('POLYGON') %>% 
      st_difference()
    fuera <- censo %>% 
      rowid_to_column("ID") %>% 
      filter(!ID %in% codes) %>% 
      select(ID, Especie) %>% 
      st_intersection(st_union(buffer)) %>%
      st_join(buffer,join = st_intersects) %>% 
      mutate(Afectacion = 'Eliminación') %>% 
      select(Especie,Afectacion,Obra) 
    union <- bind_rows(dentro,fuera) %>% 
      rowid_to_column('ID') %>% 
      mutate(UTM_E = st_coordinates(geometry)[,1],
             UTM_N = st_coordinates(geometry)[,2]) %>% 
      relocate(geometry, .after = last_col())
    return(union)
  }
  ECC_int <- get_ECC_int(BNP_inter, BNP_alterar, censo, cuenca, sp)
  
  get_uso <- function(cuenca, uso_veg){
    uso_veg %>%
      mutate(NOM_SSUBC = cuenca$NOM_SSUBC,
             Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>%
      drop_units() %>% 
      select(NOM_SSUBC,Uso, Subuso, Sup_ha) %>% 
      arrange(Sup_ha)
  }
  uso_cuenca <- get_uso(cuenca,uso_veg)
  
  get_veg <- function(cuenca, uso_veg){
    uso_veg %>% 
      mutate(NOM_SSUBC = cuenca$NOM_SSUBC,
             Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>%
      drop_units() %>% 
      select(NOM_SSUBC, Formacion, Tipo_for, Subtipo_fo, starts_with("ECC"), F_ley20283, BNP_ECC, Sup_ha) %>% 
      arrange(Sup_ha)
  }
  veg_cuenca <- get_veg(cuenca, uso_veg)
  
  get_inv_flora <- function(BD_flora, BNP_cuenca){
    BD_flora %>%
      as_tibble() %>%
      group_by(Parcela,UTM_E,UTM_N) %>%
      tally() %>%
      select(-n) %>%
      st_as_sf(coords = c('UTM_E', 'UTM_N'),
               crs = crs_epsg,
               remove = F) %>%
      st_intersection(st_union(BNP_cuenca)) %>%
      arrange(Parcela)
  }
  inv_flora <- get_inv_flora(BD_flora, BNP_cuenca)
  
  get_inv_fores <- function(BD_fore, BNP_cuenca){
    BD_fore %>%
      as_tibble() %>%
      group_by(Parcela,UTM_E,UTM_N) %>%
      tally() %>%
      select(-n) %>%
      st_as_sf(coords = c('UTM_E', 'UTM_N'),
               crs = crs_epsg,
               remove = F) %>%
      st_intersection(st_union(BNP_cuenca)) %>%
      arrange(Parcela)
  }
  inv_forestal <- get_inv_fores(BD_fore, BNP_cuenca)
  
  # get_inv_trans <- function(BD_trans, BNP_cuenca){
  #   BD_trans %>% 
  #     as_tibble() %>%
  #     st_as_sf(coords = c('UTM_E', 'UTM_N'),
  #              crs = crs_epsg,
  #              remove = F) %>%
  #     st_intersection(st_union(BNP_cuenca)) %>%
  #     arrange(Parcela)
  # }
  # inv_transectos <- get_inv_trans(BD_trans, BNP_cuenca)
  
  get_prospeccion <- function(BD_flora, BD_fore, censo, sp){
    bind_rows(
      BD_flora %>% filter(Especie %>% str_detect(sp)) %>% group_by(Parcela, UTM_E, UTM_N) %>% tally() %>% st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = crs_epsg),
      BD_fore %>% filter(Especie %>% str_detect(sp)) %>% group_by(Parcela, UTM_E, UTM_N) %>% tally() %>% st_as_sf(coords = c('UTM_E', 'UTM_N'), crs = crs_epsg),
      censo %>% filter(Especie %>% str_detect(sp))
    ) %>% 
      group_by(geometry) %>%
      tally() %>% 
      mutate(Especie = sp,
             UTM_E = st_coordinates(geometry)[,1],
             UTM_N = st_coordinates(geometry)[,2]) %>% 
      select(Especie, starts_with("UTM")) 
  }
  prospeccion <- get_prospeccion(BD_flora, BD_fore, censo, sp)
  
  get_BNP_antes <- function(cuenca, BNP_cuenca){
    nom_ssubc <- cuenca$NOM_SSUBC %>% unique()
    BNP_cuenca %>% 
      mutate(NOM_SSUBC = nom_ssubc,
             ID = 1,
             TIPO = 'ANTES') %>% 
      select(NOM_SSUBC, Sup_ha, TIPO, ID)
  }
  BNP_antes <- get_BNP_antes(cuenca, BNP_cuenca)
  
  get_BNP_despues <- function(cuenca, BNP_cuenca, BNP_inter, BNP_alterar){
    nom_ssubc <- cuenca$NOM_SSUBC %>% unique()
    if (nrow(BNP_alterar) != 0){
      BNP_cuenca %>% 
        st_difference(st_union(BNP_inter)) %>%
        st_difference(st_union(BNP_alterar)) %>%
        st_collection_extract('POLYGON') %>%
        st_cast('MULTIPOLYGON') %>% 
        st_cast('POLYGON') %>% 
        mutate(NOM_SSUBC = cuenca$NOM_SSUBC,
               ID = 1,
               TIPO = 'DESPUÉS',
               Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>%
        drop_units() %>% 
        select(NOM_SSUBC, Sup_ha, TIPO, ID)
    } else {
      BNP_cuenca %>% 
        st_difference(st_union(BNP_inter)) %>%
        st_collection_extract('POLYGON') %>% 
        st_cast('MULTIPOLYGON') %>% 
        st_cast('POLYGON') %>% 
        mutate(NOM_SSUBC = nom_ssubc,
               ID = 1,
               TIPO = 'DESPUÉS',
               Sup_ha = st_area(geometry) %>% set_units(ha) %>% round(2)) %>% 
        drop_units() %>% 
        select(NOM_SSUBC, Sup_ha, TIPO, ID)
    }
  }
  BNP_despues <- get_BNP_despues(cuenca, BNP_cuenca, BNP_inter, BNP_alterar)
  
  get_BNP_alt_sin_pto <- function(BNP_alterar, densidad, sp, censo){
    code <- BNP_alterar %>% rowid_to_column("ID") %>% .[censo,] %>% pull(ID)
    BNP_alterar %>% 
      st_zm() %>% 
      rowid_to_column("ID") %>% 
      dplyr::filter_at(vars(starts_with('ECC')), any_vars(. == sp)) %>% 
      dplyr::filter(!ID %in% code) %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha)) %>% 
      drop_units() %>% 
      mutate(Densidad = densidad,
             Especie = sp, 
             Afectacion = "Alteración del hábitat",
             Ind_alterar = (Densidad * Sup_ha) %>% round()) %>% 
      filter(Ind_alterar != 0) %>% 
      select(NOM_SSUBC,F_ley20283, Obra, Afectacion, Especie, Densidad, Ind_alterar, Sup_ha) %>% 
      arrange(Sup_ha) 
  }
  BNP_alterar_sin_pto <- get_BNP_alt_sin_pto(BNP_alterar, densidad, sp, censo)
  
  get_BNP_int_sin_pto <- function(BNP_inter, densidad, sp, censo){
    code <- BNP_inter %>% rowid_to_column("ID") %>% .[censo,] %>% pull(ID)
    BNP_inter %>% 
      st_zm() %>% 
      rowid_to_column("ID") %>% 
      dplyr::filter_at(vars(starts_with('ECC')), any_vars(. == sp)) %>% 
      dplyr::filter(!ID %in% code) %>% 
      mutate(Sup_ha = st_area(geometry) %>% set_units(ha)) %>% 
      drop_units() %>%
      mutate(Afectacion = "Eliminación",
             Especie = sp, 
             Densidad = densidad,
             Ind_Interv = (Densidad * Sup_ha) %>% round()) %>% 
      filter(Ind_Interv != 0) %>% 
      select(NOM_SSUBC,F_ley20283, Obra, Afectacion, Especie, Densidad, Ind_Interv, Sup_ha) %>% 
      arrange(Sup_ha)
  }
  BNP_int_sin_pto <- get_BNP_int_sin_pto(BNP_inter, densidad, sp, censo)
  
  return(list(
    cuenca = cuenca,
    caminos = caminos,
    curv_niv = curv_niv,
    hidro = hidro,
    ubicacion = ubicacion,
    obras_cuenca = obras_cuenca,
    BNP_cuenca = BNP_cuenca,
    BNP_inter = BNP_inter,
    BNP_alterar = BNP_alterar,
    ECC_int = ECC_int,
    ECC_alt = ECC_alt,
    uso_cuenca = uso_cuenca,
    veg_cuenca = veg_cuenca,
    inv_flora = inv_flora,
    inv_forestal = inv_forestal,
    # inv_transectos = inv_transectos,
    prospeccion = prospeccion,
    BNP_antes = BNP_antes,
    BNP_despues = BNP_despues,
    BNP_alterar_sin_pto = BNP_alterar_sin_pto,
    BNP_int_sin_pto = BNP_int_sin_pto
  ))
}

down_carto_digital <- function(carto_digital, temp_dir, input){
  write_sf(carto_digital$cuenca, file.path(temp_dir,"Cuenca_de_estudio.shp"))
  write_sf(carto_digital$cuenca %>% relocate(NOM_SSUBC), file.path(temp_dir,"Cuenca_de_estudio.kml"))
  write_sf(carto_digital$caminos, file.path(temp_dir,"Caminos_cuenca.shp"))
  write_sf(carto_digital$caminos %>% relocate(Tipo_Cam), file.path(temp_dir,"Caminos_cuenca.kml"))
  write_sf(carto_digital$curv_niv, file.path(temp_dir,"Curvas_de_Nivel_cuenca.shp"))
  write_sf(carto_digital$curv_niv %>% group_by(Elevacion) %>% tally(), file.path(temp_dir,"Curvas_de_Nivel_cuenca.kml"))
  write_sf(carto_digital$hidro, file.path(temp_dir,"Hidrografia_cuenca.shp"))
  write_sf(carto_digital$hidro %>% relocate(NOMBRE), file.path(temp_dir,"Hidrografia_cuenca.kml"))
  write_sf(carto_digital$ubicacion, file.path(temp_dir,"Area_de_proyecto_Ubicación.shp"))
  write_sf(carto_digital$ubicacion, file.path(temp_dir,"Area_de_proyecto_Ubicación.kml"))
  write_sf(carto_digital$obras_cuenca, file.path(temp_dir,"Area_de_proyecto_Obras.shp"))
  write_sf(carto_digital$obras_cuenca, file.path(temp_dir,"Area_de_proyecto_Obras.kml"))
  write.xlsx(carto_digital$obras_cuenca %>% st_drop_geometry(), file.path(temp_dir,"Area_de_proyecto_Obras.xlsx"))
  write_sf(carto_digital$BNP_cuenca, file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_Cuenca.shp')))
  write_sf(carto_digital$BNP_cuenca, file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_Cuenca.kml')))
  write.xlsx(carto_digital$BNP_cuenca %>% st_drop_geometry(), file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_Cuenca.xlsx')))
  write_sf(carto_digital$BNP_inter, file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_a_Intervenir.shp')))
  write_sf(carto_digital$BNP_inter, file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_a_Intervenir.kml')))
  write.xlsx(carto_digital$BNP_inter %>% st_drop_geometry(), file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_a_Intervenir.xlsx')))
  if (nrow(carto_digital$BNP_alterar) != 0) {
    write_sf(carto_digital$BNP_alterar, file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_a_Alterar.shp')))
    write_sf(carto_digital$BNP_alterar, file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_a_Alterar.kml')))
    write.xlsx(carto_digital$BNP_alterar %>% st_drop_geometry(), file.path(temp_dir,paste0('BNP_', word(input$sp,1), '_a_Alterar.xlsx')))
  }
  write_sf(carto_digital$ECC_int, file.path(temp_dir,paste0('Censo_', word(input$sp,1), '_a_Intervenir.shp')))
  write_sf(carto_digital$ECC_int, file.path(temp_dir,paste0('Censo_', word(input$sp,1), '_a_Intervenir.kml')))
  write.xlsx(carto_digital$ECC_int %>% st_drop_geometry(), file.path(temp_dir,paste0('Censo_', word(input$sp,1), '_a_Intervenir.xlsx')))
  if (nrow(carto_digital$ECC_alt) != 0) {
    write_sf(carto_digital$ECC_alt, file.path(temp_dir,paste0('Censo_', word(input$sp,1), '_a_Alterar.shp')))
    write_sf(carto_digital$ECC_alt, file.path(temp_dir,paste0('Censo_', word(input$sp,1), '_a_Alterar.kml')))
    write.xlsx(carto_digital$ECC_alt %>% st_drop_geometry(), file.path(temp_dir,paste0('Censo_', word(input$sp,1), '_a_Alterar.xlsx')))
  }
  write_sf(carto_digital$uso_cuenca, file.path(temp_dir,"Uso_actual_de_la_tierra.shp"))
  write_sf(carto_digital$uso_cuenca %>% group_by(Subuso) %>% summarise(Sup_ha = sum(Sup_ha)), file.path(temp_dir,"Uso_actual_de_la_tierra.kml"))
  write_sf(carto_digital$veg_cuenca, file.path(temp_dir,"Vegetación_en_la_cuenca.shp"))
  write_sf(carto_digital$veg_cuenca %>% group_by(Formacion) %>% summarise(Sup_ha = sum(Sup_ha)), file.path(temp_dir,"Vegetación_en_la_cuenca.kml"))
  write_sf(carto_digital$inv_flora, file.path(temp_dir,"UTM_Inventarios_Flora.shp"))
  write_sf(carto_digital$inv_flora, file.path(temp_dir,"UTM_Inventarios_Flora.kml"))
  write_sf(carto_digital$inv_forestal, file.path(temp_dir,"UTM_Inventario_Forestal.shp"))
  write_sf(carto_digital$inv_forestal, file.path(temp_dir,"UTM_Inventario_Forestal.kml"))
  write_sf(carto_digital$prospeccion, file.path(temp_dir,"UTM_Registros_Porlieria.shp"))
  write_sf(carto_digital$prospeccion, file.path(temp_dir,"UTM_Registros_Porlieria.kml"))
  write_sf(carto_digital$BNP_antes, file.path(temp_dir,paste0('BNP_fragmentacion_', word(input$sp,1), '_Antes.shp')))
  write_sf(carto_digital$BNP_antes, file.path(temp_dir,paste0('BNP_fragmentacion_', word(input$sp,1), '_Antes.kml')))
  write_sf(carto_digital$BNP_despues, file.path(temp_dir,paste0('BNP_fragmentacion_', word(input$sp,1), '_Despues.shp')))
  write_sf(carto_digital$BNP_despues, file.path(temp_dir,paste0('BNP_fragmentacion_', word(input$sp,1), '_Despues.kml')))
  if (nrow(carto_digital$BNP_alterar_sin_pto) != 0) {
    write_sf(carto_digital$BNP_alterar_sin_pto, file.path(temp_dir,paste0('Estimacion_Alteracion_',word(input$sp,1),'.shp')))
    write_sf(carto_digital$BNP_alterar_sin_pto, file.path(temp_dir,paste0('Estimacion_Alteracion_',word(input$sp,1),'.kml')))
    write.xlsx(st_drop_geometry(carto_digital$BNP_alterar_sin_pto), file.path(temp_dir,paste0('Estimacion_Alteracion_',word(input$sp,1),'.xlsx')))
  }
  if (nrow(carto_digital$BNP_int_sin_pto) != 0) {
    write_sf(carto_digital$BNP_int_sin_pto, file.path(temp_dir,paste0('Estimacion_Intervencion_',word(input$sp,1),'.shp')))
    write_sf(carto_digital$BNP_int_sin_pto, file.path(temp_dir,paste0('Estimacion_Intervencion_',word(input$sp,1),'.kml')))
    write.xlsx(st_drop_geometry(carto_digital$BNP_int_sin_pto), file.path(temp_dir,paste0('Estimacion_Intervencion_',word(input$sp,1),'.xlsx')))
  }
}
