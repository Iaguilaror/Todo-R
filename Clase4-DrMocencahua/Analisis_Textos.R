#---------------------------------
# Análisis de Texto             
# Daniel Mocencahua Mora
# 31 octubre 2024
# 
#--------------------------------- 

#1. limpiamos el workspace
rm(list = ls()) # Limpiamos la consola
cat("\014") # limpiamos consola
options(encoding = "utf-8") # fijamos a UTF-8
options(digits=2) # 
graphics.off() # borramos gráficos

#--------------------------------------------
 
# 2. Instalación y carga de paquetes necesarios

source( file = "https://rebrand.ly/instalador2024" )

#-----------------------------------------------------
#   Conteo de palabras            
#------------------------------------------------------             
    
#   Extraer los textos de los cuentos

# 3. Indicamos donde debe buscar el archivo
ruta_pdf <-"https://rebrand.ly/narracionesbuap"   

# 4. Le decimos en qué páginas está cada cuento                      
rango_cuento <- list(
  "S.O.F.I." = c(10, 32),
  "CARMELO" = c(33, 49),
  "BOND" = c(50, 72),
  "EL QUINTO" = c(73, 85),
  "MENTE SOBRE EL CUERPO" = c(86, 93),
  "EL ÚLTIMO INTENTO" = c(94, 111),
  "EL ARMA MÁS GRANDE" = c(112, 126),
  "EL ONIRONAUTA" = c(127, 136),
  "LA RIAJA" = c(137, 152),
  "MI PADRE MARTE" = c(153, 165),
  "ENREDADERAS" = c(166, 181),
  "EL DIARIO DE MIDAS" = c(182, 190),
  "LOCOS CON GAFAS" = c(191, 215),
  "ÚNICO VIAJE" = c(216, 231),
  "MOSCA" = c(232, 240)
)


# Vamos a obtener los cuentos por separado

# 5. Función para extraer texto por cuento
extraer_cuentos <- function(ruta_pdf) {
  texto_completo <- pdf_text(ruta_pdf)
  cuentos <- lapply(names(rango_cuento), function(nombre) {
    rango <- rango_cuento[[nombre]]
    texto_cuento <- paste(texto_completo[rango[1]:rango[2]], collapse = " ")
    list(nombre = nombre, texto = texto_cuento)
  })
  
  return(cuentos)
}

# 6. Aplicamos la función (esperamos)
cuentos <- extraer_cuentos(ruta_pdf)

#----------------------------------------------------------------------------

# Tokenizar 

# 7. Nos creamos un lexicón de stopwords en español 

# Leemos el archivo desde Google docs
vacias<-read.csv( file = "https://rebrand.ly/lexicon2024", stringsAsFactors = FALSE ) 


lexiconSW <- data.frame(
  word = as.character(
    c(stopwords("es"), 
      "capítulo", 
      vacias$palabra)
    )
  )


 

#------------------------------------------------------------

# 8. Función para preparar y tokenizar texto
procesar_texto <- function(cuentos) {
  bind_rows(lapply(cuentos, function(cuento) {
    tibble(
      titulo = cuento$nombre,
      texto = cuento$texto
    ) %>%
      unnest_tokens(palabra, texto)%>%
      anti_join(lexiconSW, by = c("palabra" = "word")) 
    }))
}

# Aplicamos la función
datos_procesados <- procesar_texto(cuentos)

#------------------------------------------------------

# 9. Contar el número total de palabras por cuento (después de remover stop words)
conteo_por_cuento <- datos_procesados %>%
  group_by(titulo) %>%
  summarise(
    total_palabras = n(),
    palabras_unicas = n_distinct(palabra)
  ) %>%
  arrange(desc(total_palabras))


#-------------------------------------------------------------------

#10. Encontrar las palabras más frecuentes en general
palabras_frecuentes<-count(datos_procesados,palabra, sort = TRUE)

print(palabras_frecuentes,colnames = c('N' = 1)) 

# Gráfico de frecuencias
palabras_frecuentes %>%
  head(30) %>%
  mutate(word = reorder(palabra, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(title = paste0("Palabras mas utilizadas"),
       subtitle = "Narraciones de Ciencia y Ficción 2021",
       x = "Palabra",
       y = "Numero de veces usada")

#----------------------------------------------------------------

# 11.  Crear la wordcloud

# Definir la paleta de colores de ColorBrewer
colores <- brewer.pal(8, "Dark2")  # Puedes elegir otras paletas como "Set3", "Paired", etc.
#display.brewer.pal(8, "Dark2")

wordcloud(
  words = palabras_frecuentes$palabra,   
  freq = palabras_frecuentes$n,          
  min.freq = 1,                         
  max.words = 150,                      
  random.order = FALSE,                  
  rot.per = 0.35,                        
  colors = colores                      
)

#-----------------------------------------------------------------
 
# 12. Conteo de palabras por cuento (función)

analisis_comparativo <- function(datos_procesados) {
  datos_procesados %>%
    count(titulo, palabra, sort = TRUE) %>%
    group_by(titulo) %>%
    slice_max(n, n = 10) %>%
    ungroup()
}

# Aplicamos la función
comparacion <- analisis_comparativo(datos_procesados)

#--------------------------------------------------------------------

# 13. Función para comparaciones

visualizar_comparacion<- function(datos_comparativos, 
                                  n_palabras = 10) { 
  # Crear vector de colores suficiente para todos los títulos
  n_titulos <- length(unique(datos_comparativos$titulo))
  colores <- colorRampPalette(brewer.pal(8, "Set2"))(n_titulos)
  
  datos_comparativos %>%
    group_by(titulo) %>%
    top_n(n_palabras, n) %>%         
    ungroup() %>%
    ggplot(aes(x = reorder_within(palabra, n, titulo),
               y = n,
               fill = titulo)) +
    geom_col(show.legend = FALSE) +    
    facet_wrap(~titulo, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    scale_fill_manual(values = colores) +  # Usar los colores generados
    theme_minimal() +                  
    theme(
      axis.text = element_text(size = 6),
      strip.text = element_text(size = 8, face = "bold"),
      plot.title = element_text(size = 10, face = "bold")
    ) +
    labs(x = "Palabra",
         y = "Frecuencia",
         title = "Palabras más frecuentes por cuento",
         subtitle = paste("Top", n_palabras, "palabras"))
}

visualizar_comparacion(comparacion, n_palabras = 10)

#------------------------------------------------------------

# 14. Visualización comparativa por grupos
visualizar_grupo<- function(datos_comparativos, 
                                        numero_grupo = 1,    
                                        n_palabras = 10,
                                        n_por_grupo = 3) { 
  
  # Definir paletas de colores diferentes para cada grupo
  paletas_colores <- list(
    "1" = scale_fill_brewer(palette = "Set1"),      # Rojo, azul, verde
    "2" = scale_fill_brewer(palette = "Set2"),      # Pastel
    "3" = scale_fill_brewer(palette = "Set3"),      # Suaves
    "4" = scale_fill_brewer(palette = "Paired"),    # Pares de colores
    "5" = scale_fill_brewer(palette = "Dark2")      # Oscuros
  )
  
  titulos <- unique(datos_comparativos$titulo)
  n_grupos <- ceiling(length(titulos) / n_por_grupo)
  
  if(numero_grupo > n_grupos || numero_grupo < 1) {
    stop(paste("El número de grupo debe estar entre 1 y", n_grupos))
  }
  
  inicio <- (numero_grupo-1) * n_por_grupo + 1
  fin <- min(numero_grupo * n_por_grupo, length(titulos))
  titulos_grupo <- titulos[inicio:fin]
  
  datos_grupo <- datos_comparativos %>%
    filter(titulo %in% titulos_grupo) %>%
    group_by(titulo) %>%
    top_n(n_palabras, n) %>%
    ungroup()
  
  grafico <- ggplot(datos_grupo, 
                    aes(x = reorder_within(palabra, n, titulo),
                        y = n,
                        fill = titulo)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~titulo, scales = "free_y", nrow = 1) +
    coord_flip() +
    scale_x_reordered() +
    paletas_colores[[as.character(numero_grupo)]] +  # Seleccionar paleta según el grupo
    theme_minimal() +
    theme(
      axis.text = element_text(size = 10),
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    labs(x = "Palabra",
         y = "Frecuencia",
         title = paste("Palabras más frecuentes - Grupo", numero_grupo),
         subtitle = paste("Top", n_palabras, "palabras"))
  
  return(grafico)
}

## Para ver cada grupo:
visualizar_grupo(comparacion, numero_grupo = 1, n_palabras = 15)
visualizar_grupo(comparacion, numero_grupo = 2, n_palabras = 15)
visualizar_grupo(comparacion, numero_grupo = 3, n_palabras = 15)
visualizar_grupo(comparacion, numero_grupo = 4, n_palabras = 15)
visualizar_grupo(comparacion, numero_grupo = 5, n_palabras = 15)


#-----------------------------------------------------------------------------


# 15. Encontrar palabras únicas por cuento

palabras_por_cuento <- datos_procesados %>%
  group_by(titulo) %>%
  count(palabra, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ungroup()
head(palabras_por_cuento)


#-------------------------------------------------------------------------------

# 16. Calcular TF-IDF para encontrar palabras importantes por cuento
tfidf_palabras <- datos_procesados %>%
  count(titulo, palabra) %>%
  bind_tf_idf(palabra, titulo, n) %>%
  group_by(titulo) %>%
  top_n(5, tf_idf) %>%
  ungroup() %>%
  arrange(titulo, desc(tf_idf))

tfidf_palabras

#-------------------------------------------------------------------------------

# Cargar léxico de sentimientos
sentimientos <- read_tsv("https://rebrand.ly/sentimientos2",
                         col_types = "cccn",
                         locale = default_locale())


# Primero, vamos a ajustar el léxico para que tenga la estructura que necesitamos
sentimientos_ajustado <- sentimientos %>%
  rename(tipo = sentimiento) %>%  # Renombrar 'sentimiento' a 'tipo'
  select(palabra, tipo, valor)    # Seleccionar solo las columnas que necesitamos



# Función ajustada para el análisis de sentimientos
analizar_sentimientos <- function(cuentos) {
  # Procesar textos y unir con sentimientos
  sentimientos_cuentos <- bind_rows(lapply(cuentos, function(cuento) {
    tibble(
      titulo = cuento$nombre,
      texto = cuento$texto
    ) %>%
      unnest_tokens(palabra, texto) %>%
      inner_join(sentimientos_ajustado, by = "palabra")
  }))
  
  # Análisis por cuento
  resumen_sentimientos <- sentimientos_cuentos %>%
    group_by(titulo) %>%
    summarise(
      palabras_sentimiento = n(),
      tipos_sentimiento = n_distinct(tipo)
    ) %>%
    arrange(desc(palabras_sentimiento))
  
  
  # Análisis detallado por tipo de sentimiento
  detalle_sentimientos <- sentimientos_cuentos %>%
    group_by(titulo, tipo) %>%
    summarise(
      conteo = n()
    ) %>%
    ungroup()
  
  # Palabras más frecuentes por sentimiento
  palabras_por_sentimiento <- sentimientos_cuentos %>%
    group_by(tipo) %>%
    count(palabra, sort = TRUE) %>%
    top_n(10, n) %>%
    ungroup()
  
  return(list(
    resumen = resumen_sentimientos,
    detalle = detalle_sentimientos,
    palabras = palabras_por_sentimiento,
    datos_completos = sentimientos_cuentos
  ))
}

# Aplicar el análisis (esperamos)
resultados <- analizar_sentimientos(cuentos)

#------------------------------------------------------------------------------

# 18. Gráfico de conteo de palabras con sentimiento por cuento (esperar)

grafico_conteo <- ggplot(resultados$resumen, 
                         aes(x = reorder(titulo, palabras_sentimiento), 
                             y = palabras_sentimiento)) +
  geom_col(fill = "#982A18")+
  coord_flip() +
  theme_minimal() +
  labs(title = "Cantidad de palabras con sentimiento por cuento",
       x = "Título",
       y = "Número de palabras")

grafico_conteo

#------------------------------------------------------------------------------


# 19. Gráfico de tipos de sentimiento por cuento
grafico_tipos <- ggplot(resultados$detalle, 
                        aes(x = titulo, y = conteo, fill = tipo)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribución de tipos de sentimiento por cuento",
       x = "Título",
       y = "Conteo",
       fill = "Tipo de sentimiento")

grafico_tipos

#-------------------------------------------------------------------------------

# 20. Analisis progresivo

# Función para analizar la progresión de sentimientos
analizar_progresion_sentimientos <- function(cuentos, num_secciones = 10) {
  # Procesar cada cuento y analizar la progresión
  progresiones <- lapply(cuentos, function(cuento) {
    # Tokenizar el texto y añadir índices
    palabras <- tibble(
      texto = cuento$texto
    ) %>%
      unnest_tokens(palabra, texto) %>%
      mutate(
        indice = row_number(),
        seccion = ceiling(indice * num_secciones / n())
      )
    
    # Unir con el léxico de sentimientos
    sentimientos_texto <- palabras %>%
      inner_join(sentimientos_ajustado, by = "palabra")
    
    # Analizar sentimientos por sección
    progresion <- sentimientos_texto %>%
      group_by(seccion, tipo) %>%
      summarise(
        conteo = n(),
        .groups = "drop"
      ) %>%
      mutate(titulo = cuento$nombre)
    
    return(progresion)
  }) %>% bind_rows()
  
  return(progresiones)
}

# Aplicar el análisis de progresión
progresion_sentimientos <- analizar_progresion_sentimientos(cuentos)

# Visualizaciones

# Gráfico de líneas para mostrar la progresión de cada tipo de sentimiento
# Para un cuento específico
visualizar_progresion_cuento <- function(datos_progresion, titulo_cuento) {
  datos_filtrados <- datos_progresion %>%
    filter(titulo == titulo_cuento)
  
  ggplot(datos_filtrados, aes(x = seccion, y = conteo, color = tipo)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(
      title = paste("Progresión de sentimientos en:", titulo_cuento),
      x = "Sección del cuento",
      y = "Cantidad de palabras",
      color = "Tipo de sentimiento"
    ) +
    theme(legend.position = "bottom")
}



# Heatmap para mostrar la intensidad de sentimientos a lo largo del cuento
crear_heatmap_sentimientos <- function(datos_progresion, titulo_cuento) {
  datos_filtrados <- datos_progresion %>%
    filter(titulo == titulo_cuento)
  
  ggplot(datos_filtrados, aes(x = seccion, y = tipo, fill = conteo)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
    labs(
      title = paste("Mapa de calor de sentimientos en:", titulo_cuento),
      x = "Sección del cuento",
      y = "Tipo de sentimiento",
      fill = "Intensidad"
    )
}

# Función para calcular sentimiento dominante por sección
calcular_sentimiento_dominante <- function(datos_progresion, titulo_cuento) {
  datos_progresion %>%
    filter(titulo == titulo_cuento) %>%
    group_by(seccion) %>%
    slice_max(order_by = conteo, n = 1) %>%
    ungroup() %>%
    select(seccion, tipo_dominante = tipo, conteo)
}

# Aplicar visualizaciones para cada cuento
titulos_cuentos <- unique(progresion_sentimientos$titulo)


# Crear visualizaciones para un cuento de ejemplo (el primero)
titulo_ejemplo <- titulos_cuentos[1]
print(visualizar_progresion_cuento(progresion_sentimientos, titulo_ejemplo))
print(crear_heatmap_sentimientos(progresion_sentimientos, titulo_ejemplo))
 

