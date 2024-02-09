info_dashboard <- function(){
  bs_accordion(id = "accordion") %>%
    bs_set_opts(use_heading_link = T,
                panel_type = "success") %>%
    bs_append(title = "Objetivos e instrucciones del Dashboard",
              content = tags$p("Este Dashboard esta elaborado con el objetivo de apoyar la elaboración del informe de experto referido al
                               PAS150. Por favor considerar todas las intrucciones y consejos mencionados en esta página para evitar errores")) %>%
    bs_append(title = "Inputs y chequeo de datos",
              content = tags$div(
                tags$p(
                  "En esta sección debe cargar shp de cartografía del uso y vegetación de la subsubcuenca, el shp de obras, el shp de censos,
                  y las bases de datos de inventarios floristicos y forestales. Es importante que se ingresen todos los archivos solicitados 
                  y presione los botones para generar los analisis de inventarios forestales y biodiversidad. Con esto se habilitarán gran
                  parte del contenido de las otras pestañas."
                ),
                tags$p(
                  "Por otra parte, al ingresar la cartografía de uso de uselo y vegetación, debe chequar que esté bien guiándose por la
                  información que se despliega em las diferentes pestañas a la derecha."
                ),
                tags$p(
                  "Para ingresar los shapefiles, debe cargar los 4 archivos necesarios para su lectura, que son: 'dbf', 'prj' 'shp' y 'shx'.
                  De otro modo no se reconocerá el shapefile"
                ),
                tags$ul(
                  tags$li(
                    "Shapefile Usos de suelo y caracterización de la vegetación",
                    tags$ul(
                      tags$li(
                        "Debe presentar al menos los siguientes campos:",
                        tags$br(),
                        "'Uso', 'Subuso', 'Formacion', 'Tipo_For', 'Subtipo_Fo', 'ECC1', 'ECC2',..., 
                        'BNP_ECC', 'F_ley20283'"
                      ),
                      tags$li(
                        "Si bien no es necesario que coincidan las mayúsculas o tildes, si lo deben hacer los caractéres alfa-numéricos"
                      ),
                      tags$li(
                        "En los campos ECC deben ir las especies en categoría de conservación que se en el polígono. Si estas se encuentran 
                        en Bosque, debería figurar como 'Bosque nativo de preservación' en el campo de 'F_ley20283', y señalar las especies 
                        en el campo de 'BNP_ECC' (p. ej, 'Prosopis chilensis',  'Prosopis chilensis-Porlieria chilensis', etc) "
                      ),
                      tags$li(
                        "En los resultados del chequeo debe poner atención a; los nombres de los atributos de cada campo, que cada bosque
                        nativo tenga asociado un tipo y subtipo forestal y que figure como 'Bosque nativo' o 'Bosque nativo de preservación'
                        en el campo 'F_ley20283', que los usos que no son bosque no presenten nombres de especies en los campos de 'ECC' 
                        y 'BNP_ECC'"
                      )
                    )
                  ),
                  tags$li("Shapefile alteración de BNP",
                          tags$ul(
                            tags$li(
                              "En caso de haber alteración, cargar un shapefile de las superficies de BNP de la especie objetivo que se
                              alteran producto de las obras. Este debe contener al menos el campo 'Obra' que señale el nombre de la obra 
                              que estaría alterando el hábitat de la especie."
                            )
                          )),
                  tags$li("Shapefile layout de obras",
                          tags$ul(
                            tags$li(
                              "Cargar un shapefile del compilado de obras. Este debe ser de geometría tipo polígono, el cual debe contener
                              al menos el campo 'Obra' que señale el nombre de la obra"
                            )
                          )),
                  tags$li("Shapefile Censo",
                          tags$ul(
                            tags$li(
                              "El shapefile debe contener al menos el campo 'Especie' que indique la especie censada"
                            )
                          )),
                  tags$li(
                    "Base de datos de inventarios forestales",
                    tags$ul(
                      tags$li(
                        "Seleccionar archivo de inventarios forestales (.xlsx) que tenga al menos los siguientes campos:",
                        tags$br(),
                        "'Parcela', 'Sup_parcela', 'UTM_E', 'UTM_N', 'Especie', 'Estado_des', 'DAP', 'Altura', 'N_ind'"
                      ),
                      tags$li(
                        "Los campos 'UTM_E', 'UTM_N', 'DAP', 'Altura', 'N_ind' deben ser numericos. Fijarse bien en el separador de decimales,
                        a veces se utilizan puntos y comas en el mismo campo"
                      ),
                      tags$li(
                        "El campo 'Estado_des' debe estar completo al menos para la especie objetivo del infome con uno de las siguientes
                        clases: 'Adulto', 'Brinzal', 'Regeneración'"
                      ),
                      tags$li(
                        "Fijarse en la consistencia de las parcelas y sus coordenadas, cada parcela debe tener coordenadas unicas"
                      ),
                      tags$li(
                        "Antes de ingresar fijarse que solo estén las especies que sean de su interés presentar. Generalmente se dejan fuera
                        del analisislas especies exóticas como 'Olea europaea', 'Salix babylonica', 'Populus sp.', etc"
                      ),
                    )
                  ),
                  tags$li(
                    "Base de datos de inventarios floristicos",
                    tags$ul(
                      tags$li(
                        "seleccionar archivo de inventarios floristicos (.xlsx) que tenga al menos los siguientes campos:",
                        tags$br(),
                        "'Parcela', 'Sup_parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Origen', 'RCE', 'DS_68'"
                      ),
                      tags$li(
                        "El campo 'Habito' debe contener las siguientes clases: 'Árbol', 'Arbusto', 'Hierba anual', 'Hierba perenne',
                        'Suculento'"
                      ),
                      tags$li(
                        "El campo 'Origen' debe contener las siguientes clases: 'Nativo', 'Endémico', 'Introducido'"
                      ),
                      tags$li(
                        "En RCE indicar la categoría de conservación de acuerdo a la Resolución de Calificación Ambiental, mientras que en
                        'DS_68' señalar como 'Originaria' aquellas especies que se encuerntran en el listado del Decreto Supero 68"
                      ),
                      tags$li(
                        "Fijarse en la consistencia de las parcelas y sus coordenadas, cada parcela debe tener coordenadas unicas"
                      ),
                    )
                  ),
                  tags$li(
                    "Base de datos de transectos lineales",
                    tags$ul(
                      tags$li(
                        "seleccionar archivo (.xlsx) con al menos los siguientes campos:",
                        tags$br(),
                        "'Parcela', 'UTM_E', 'UTM_N', 'Cob_arb'"
                      ),
                      tags$li(
                        "Igualmente fijarse en la consistencia de las parcelas y sus coordenadas, cada parcela debe tener coordenadas unicas"
                      ),
                    )
                  )
                )
              )) %>%
    bs_append(title = "Intervención",
              content = "Para poder ver las tablas en esta pestaña debe haber generado previamente la cartografía digital en la pestaña 'Cartografía digital'") %>%
    bs_append(title = "Análisis de amenazas",
              content = "Seleccione los subusos de suelo que no correspondan a vegetación. Incluir en ese listado los terrenos agrícolas. Luego cargue el XLS que genera el equipo de Cartografía, en el cual se encuentran los resultados del análisis en FragStats con las condiciones antes y despues del proyecto en las hojas 1 y 2 del XLS, respectivamente. Al presionar el boton 'Generar análisis', podrá descargar el excel con los resultados y calculos, además de cargarle el contenido de los cuadros que aparecen en la página") 
}
