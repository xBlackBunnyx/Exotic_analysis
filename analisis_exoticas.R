#Analisis especies exoticas Gesplan

#========= IMPORTACION DE LIBRERIAS ===================================

library(dplyr)
library(readxl)
library(stringr)
library(lubridate)
library(ggplot2)
library(lubridate)
library(ggforce)
library(patchwork)
library(stringr)
library(writexl)
library(tidyr)

#=========== IMPORTACION DE DATOS ============================================

#Añadimos las tablas con los datos
#tabla con las especies
#datos_raw_especies = read_xlsx("C:\\Users\\lorenamd.proyecto\\Desktop\\Proyecto NOE\\Analisis_exoticas\\featuresv2.xlsx")
datos_raw_fixed = read_xlsx("C:\\Users\\lorenamd.proyecto\\Desktop\\Proyecto NOE\\Analisis_exoticas\\featuresv3.xlsx")
#tabla con las detecciones
#datos_raw_detecciones = read_xlsx("C:\\Users\\lorenamd.proyecto\\Desktop\\Proyecto NOE\\Analisis_exoticas\\Detecciones sectores.xlsx")
#tabla con los sectores
#datos_raw_sectores = read_xlsx("C:\\Users\\lorenamd.proyecto\\Desktop\\Proyecto NOE\\Analisis_exoticas\\Sectores.xlsx")
#tabla con las zonas
#datos_raw_zonas = read_xlsx("C:\\Users\\lorenamd.proyecto\\Desktop\\Proyecto NOE\\Analisis_exoticas\\Zonas.xlsx")

#================= LIMPIEZA DE DATOS ========================================

#tabla con las especies
clean_data_especies = select(datos_raw_fixed, -c("vid", "objectid_especie" ,"sector_ambito", "sector_acceso", "sector_descripcion_acceso", "fecha_fin", "vertedero_destino"))
clean_data_especies = clean_data_especies %>% filter(is.na(observaciones) | (!str_detect(observaciones, "^Dete") & !str_detect(observaciones, "^DE")))

#cambiamos el formato de las fechas
clean_data_especies = clean_data_especies %>% mutate(fecha_inicio = parse_date_time(fecha_inicio, orders = "mdy"))
#clean_data_especies = mutate(clean_data_especies, fecha_fin = as.Date(fecha_fin, format = "%b %d, %Y") |> format("%d-%m-%Y"))
rep_especies = clean_data_especies %>% subset(!(tipo_erradicacion =="Nueva" & densidad == 0))

#Quitamos las especies que salen solo una vez
#rep_especies = subset(cd_especies, duplicated(especie) | duplicated(especie, fromLast=TRUE))

#=================== ANALISIS ===============================================

# #Añadimos una fecha temporal al año final para que no este vacio
# rep_especies$fecha_fin[is.na(rep_especies$fecha_fin)] = as.character("31-12-2024")
# rep_especies = rep_especies %>% mutate(fecha_fin = dmy(fecha_fin))
# rep_especies = rep_especies %>% mutate(fecha_inicio = dmy(fecha_inicio))

#Creamos un columna con el año
rep_especies$year = format(rep_especies$fecha_inicio, "%Y")
#Quitamos aquellas entradas donde el año sea menor a 2020
rep_especies = rep_especies %>% filter(year >= 2020)
#Convertimos el año en un factor
rep_especies$year = as.factor(rep_especies$year)

rep_especies$densidad = as.numeric(rep_especies$densidad)
#Creamos un nuevo dataframe para cada municipio
  #df_municipio = split(rep_especies, rep_especies$municipio)
  #list2env(df_municipio, envir = .GlobalEnv)
lista_parcelas = unique(rep_especies$nombre_parcela)

#Creamos un nuevo dataframe largo para agrupar los kilos
long_data = rep_especies %>% pivot_longer(cols = c(residuos_vertedero_kg, residuos_en_terreno_kg), names_to = "localizacion_residuos", values_to = "kilos")

#Creamos una lista con los años para los futuros bucles
lista_year = unique(rep_especies$year)


#------------------Estado fenologico------------------------------------

#Creamos una lista con los nombres de todas las especies 
lista_especies = unique(rep_especies$especie)

#Arreglamos los datos de la columna num_ejemplares para que tenga bien los rangos
rep_especies = rep_especies %>% mutate(num_ejemplares = str_replace(num_ejemplares, "1 ejemplar", "1"))
rep_especies = rep_especies %>% mutate(num_ejemplares = str_replace(num_ejemplares, "Ninguno", "0"))
rep_especies$num_ejemplares = factor(rep_especies$num_ejemplares, levels = c("0", "1", "< 10", "10 - 50", "50 - 100", "100 - 500", "> 500"), ordered = TRUE)

# #Creamos una variable que identifique el numero de veces que se ha ido a ver una especie en concreto
rep_especies = rep_especies %>% group_by(especie, fecha_inicio) %>% mutate(num_visitas = row_number()) %>% ungroup()

# #Especificamos los limites del eje X
# min_date = min(rep_especies$fecha_inicio)
# max_date = max(rep_especies$fecha_inicio)

#Miramos cuando el estado fenologico no es adulto reproductor para cada municipio que se ha ido
#stat_fen = rep_especies %>% filter(estado_fenologico != "Adulto reproductor")
  
#fen_tardio = rep_especies %>% filter(estado_fenologico == "Adulto reproductor")

# #Eliminamos aquellas entradas donde las fechas de inicio y final son iguales
# stat_fen = stat_fen %>% filter(fecha_inicio != fecha_fin)

#Eliminamos aquellas entradas donde no hay plantas
stat_fen = rep_especies %>% filter(num_ejemplares != 0 | densidad == 0)
stat_fen = stat_fen %>% filter(estado_fenologico != "Ninguno")

# #Eliminamos aquellas entradas donde los días de diferencia son menor a 7
# stat_fen = stat_fen %>% filter(abs(fecha_fin - fecha_inicio) > 7)
# stat_fen2 = stat_fen %>% filter(fecha_fin != "2024-12-31")

#Miramos cual es el tiempo máximo que ha pasado para cada una de las especies que quedan en la lista
# fen_result = stat_fen2 %>% group_by(especie, municipio) %>% summarize(max_difference = as.numeric(max(fecha_fin) - min(fecha_inicio)))

# #Comprobamos cuál es el tiempo máximo para cada una de las especies según el sitio
# diff_max_fecha_vs_sitio = stat_fen %>% group_by(especie, nombre_parcela) %>% summarise(
#   max_dif_dia = if (sum(!is.na(fecha_inicio)) >1) { #Nos aseguramos de que haya más de una fecha para poder comparar
#     max(abs(diff(sort(as.Date(fecha_inicio, na.last = NA)))))
#   } else { NA }, #Devuelve NA si no hay suficientes fechas
#   date_min = min(fecha_inicio),
#   date_max = max(fecha_inicio),
#   .groups = "drop")

#Comprobamos el tiempo maximo para cada especie antes de que sea adulto reproductor para cada especie
max_dif_fen = rep_especies %>% group_by(especie, nombre_parcela) %>% #Agrupa por el nombre de la especie y el sitio
  mutate(
    fen_tardio = if_else(estado_fenologico == "Adulto reproductor", fecha_inicio, NA_Date_) #Guarda en una columna la fecha en la que hay un adulto reproductor
  ) %>% filter(is.na(fen_tardio) | fecha_inicio <= fen_tardio) %>% #Guarda todas las fechas antes de la primera fecha donde hay un adulto reproductor
  summarise(
    racha_max = if (sum(!is.na(fecha_inicio)) > 1){
      max(abs(diff(sort(as.Date(fecha_inicio)))))
    } else {NA}, 
    date_min = min(fecha_inicio),
    date_max = max(fecha_inicio),
    .groups = "drop"
  )

#Quitamos todas las entradas con NA values que indica que estamos llegando tarde
max_dif_fen_NA = max_dif_fen %>% filter(racha_max > 0)

#Guardamos el resultado en un csv. La ruta hay que ponerla según el equipo
write_xlsx(max_dif_fen, "C:\\Users\\lorenamd.proyecto\\Desktop\\Proyecto NOE\\Analisis_exoticas\\estado_fenologico_seguro_en_tiempo.xlsx")
write_xlsx(max_dif_fen_NA, "C:\\Users\\lorenamd.proyecto\\Desktop\\Proyecto NOE\\Analisis_exoticas\\estado_fenologico_sin_NA.xlsx")

#=================== GRAFICAS ===============================================

# #Creamos un grafico con los kilos vertedero en el tiempo para cada especie
# ggplot(rep_especies, aes(x = year, y = residuos_vertedero_kg)) + geom_col(color = "lightblue") + facet_wrap(~ especie, scales = "free")
# 
# #Creamos un grafico con los kilos terreno en el tiempo para cada especie
# ggplot(rep_especies, aes(x = year, y = residuos_en_terreno_kg)) + geom_col(color = "lightblue") + facet_wrap(~ especie, scales = "free")
# 
# #grafica para saber los kilos por cada especie
# ggplot(cd_especies, aes(x = especie, y = residuos_vertedero_kg)) + geom_bar(stat = "identity") + coord_flip()

#Creamos una gráfica para ver los kilos de terreno y vertedero para cada especie
for (year in lista_year) {
  p = ggplot(subset(long_data, year == year), aes(x = especie, y = kilos, fill = localizacion_residuos)) +
      geom_col(position = "dodge") +
      labs(title = "Kilos por Especie y Año",
         x = "Especie", 
         y = "Kilos",
         fill = "Localización de los residuos") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 12)) +
      coord_flip()
  
  ggsave(filename = paste0("Kilos_por_especie_", year, ".png"), plot = p, width = 17, height = 15)
}

#Creamos una gráfica para ver la evolución de la densidad de una especie a lo largo de los años 
for (especie in lista_especies) {
 
    p = ggplot(subset(rep_especies, especie == especie), aes(x = fecha_inicio, y = densidad)) +
      geom_col(position = "dodge") +
      facet_wrap(~ year, scales = "free_x") +
      labs(title = paste("Densidad de", especie),
           x = "Fecha", 
           y = "Densidad") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = 12))
    
    ggsave(filename = paste0("Densidad_", especie, ".png"), plot = p, width = 17, height = 15)
}

#gráfica para ver la evolucion de la densidad en el tiempo según la parcela || Genera demasidos datos
# for (especie in lista_especies) {
#   ruta_carpeta = paste0("graficas/", especie)
#   if (!dir.exists(ruta_carpeta)) {dir.create(ruta_carpeta, recursive = TRUE)}
#   
#   for (parcela in lista_parcelas) {
#     data_filtered <- subset(rep_especies, especie == especie & parcela == parcela)
#     
#     p <- ggplot(data_filtered, aes(x = fecha_inicio, y = densidad)) +
#       geom_col(position = "dodge", fill = "pink") +  
#       facet_wrap(~ year, scales = "free_x") +             
#       labs(title = paste("Evolución de Densidad de", especie, "en", parcela),
#            x = "Fecha",
#            y = "Densidad") +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1),
#             strip.text = element_text(size = 12))
#     
#     # Save the plot
#     ggsave(filename = paste0(ruta_carpeta, "/Densidad_", especie, "_", parcela, ".png"), plot = p, width = 15, height = 10)
#   }
# }
#Creamos una gráfica para ver la evolución del número de especies a lo largo de los años 
for (especie in lista_especies) {
  data_filtered = subset(rep_especies, especie == especie & !is.na(num_ejemplares) & !is.na(fecha_inicio))
  data_filtered = subset(data_filtered, format(fecha_inicio, "%Y") == as.character(data_filtered$year))
  
    p = ggplot(data_filtered, aes(x = fecha_inicio, y = as.numeric(num_ejemplares))) +
      geom_col(position = "dodge", aes(fill = num_ejemplares), show.legend = TRUE) +
      facet_wrap(~ year, scales = "free_x") +
      scale_y_continuous(breaks = 1:7,
                         labels = c("0", "1", "<10", "10-50", "50-100", "100-500", ">500")) +
      labs(title = paste("Número de ejemplares de", especie),
         x = "Fecha", 
         y = "Número ejemplares") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = 12))
    
    ggsave(filename = paste0("Numero_ejemplares_", especie, ".png"), plot = p, width = 17, height = 15)

}


# #grafica para ver la evolucion de la densidad respecto al tiempo para cada especie según el municipio
# for(municipios in lista_municipios) {
#   df_municipios = subset(rep_especies, municipios == municipios)
#   plot = ggplot(df_municipios, aes(x = year, y = densidad)) + geom_col(color = "lightpink") + 
#     facet_wrap(~ especie, scales = "free", ncol = 6) +
#     coord_fixed(ratio = 0.5) +
#     scale_y_continuous(breaks = scales::extended_breaks()) +
#     theme(plot.margin = unit(c(1,1,1,1), "cm"), 
#           panel.spacing = unit(1.5, "lines"), 
#           axis.text.y = element_text(size = 10),
#           plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
#           strip.text = element_text(size = 12)) +
#     coord_cartesian(clip = "off")
#   
#   wrap_plots(plot) + plot_layout(heights = c(2,1))
#   ggsave(filename = paste0("plot_", municipios, ".png"), plot = plot, width = 17, height = 15) }

#grafica que relaciona el numero de especies que habia cada vez que se iba segun el sitio
for (especies in lista_especies) {
  df_especies = subset(rep_especies, especies == especies)

  plot = ggplot(df_especies, aes(x = fecha_inicio, y = num_ejemplares, fill = num_visitas)) + 
    geom_point(size = 4, alpha = 0.7) +
    facet_wrap(~ nombre_parcela, scales = "free", ncol = 6) +
    coord_fixed(ratio = 0.5) +
    labs(title = "Número de especies por visita", 
         x = "Fecha",
         y = "Número de ejemplares") +
    scale_x_date(date_labels = "%Y-%m-%d") +
    theme(plot.margin = unit(c(1,1,1,1), "cm"), 
          panel.spacing = unit(1.5, "lines"), 
          axis.text.y = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          strip.text = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    coord_cartesian(clip = "off") 
  
  wrap_plots(plot) + plot_layout(heights = c(2,1))
  ggsave(filename = paste0("plot_", especies, ".png"), plot = plot, width = 17, height = 15)
}


