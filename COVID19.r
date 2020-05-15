
setwd("~/Documents/P/COVID19")

#rm(list = ls()) #Dejamos un espacio de trabajo limpio
library(readxl) #Usamos readxl para leer archivos xlsx
library(plotly)
library(dplyr)

url_datos <- "http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
temp <- tempfile()
download.file(url_datos, temp)
tabla_name <- unzip(temp, list = FALSE)
file.rename(tabla_name, file.path("inputs", paste0("datos_", Sys.Date(), ".csv" )))
datos_actuales <- read.csv( file.path("inputs", paste0("datos_", Sys.Date(), ".csv" )))

names(datos_actuales)

datos_actuales %>% filter(ENTIDAD_RES == 17, MUNICIPIO_RES == 56) 
datos_actuales %>% filter(ENTIDAD_RES == 17, MUNICIPIO_RES == 56) %>% select(RESULTADO)

head(datos_actuales$RESULTADO)

table(datos_actuales$RESULTADO)

url_diccionario <- "http://187.191.75.115/gobmx/salud/datos_abiertos/diccionario_datos_covid19.zip"
temp <- tempfile()
download.file(url_diccionario, temp)
tabla_name <- unzip(temp, list = FALSE)
diccionario_file <- tabla_name[grep("Catalogos",tabla_name)]
meta_datos_file <- tabla_name[grep("Descriptores",tabla_name)]
file.rename(diccionario_file, file.path("inputs", paste0("catalogo_", Sys.Date(), ".xlsx" )))
file.rename(meta_datos_file, file.path("inputs", paste0("metadatos", Sys.Date(), ".xlsx" )))



tabla_name

datos_metadatos <- readxl::read_excel( file.path("inputs", paste0("metadatos", Sys.Date(), ".xlsx" )))
datos_metadatos

names(datos_actuales)

columnas <- sort(datos_metadatos[[2]])

sort(names(datos_actuales))

all(datos_metadatos[2]  == names(datos_actuales))

names(datos_actuales)[datos_metadatos[2] != names(datos_actuales)]

datos_metadatos[datos_metadatos[2]  != names(datos_actuales),][[2]]

names(datos_actuales)[datos_metadatos[2]  != names(datos_actuales)] <- datos_metadatos[datos_metadatos[2]  != names(datos_actuales),][[2]]

names(datos_actuales)

all(datos_metadatos[2]  == names(datos_actuales))

grepl("CATÁLOGO:",datos_metadatos[["FORMATO O FUENTE"]]) | grepl("CATALÓGO:",datos_metadatos[["FORMATO O FUENTE"]]) 

# campos_cruzar 
campos_cruzar <- datos_metadatos[grepl("CATÁLOGO:",datos_metadatos[["FORMATO O FUENTE"]]) | grepl("CATALÓGO:",datos_metadatos[["FORMATO O FUENTE"]]) | grepl("CATALOGO:",datos_metadatos[["FORMATO O FUENTE"]]), ]

campos_cruzar

nombre_catalogos <- excel_sheets(file.path("inputs", paste0("catalogo_", Sys.Date(), ".xlsx" )))

nombre_catalogos

lista_prueba <- list()
lista_prueba["clase 1"] <- "Instalación de R"
lista_prueba["clase_2"] <- "Instalación de R"

lista_prueba[["clase 1"]]
lista_prueba$clase_2

 Sys.Date()

lista_catalogos = list()
for(nombre_catalogo in nombre_catalogos){
    lista_catalogos[[nombre_catalogo]] <- read_excel(file.path("inputs", paste0("catalogo_", Sys.Date(), ".xlsx" )), nombre_catalogo)
    
}

lista_catalogos

paste0(c(1, "b"), "-", c("c","d"))

lista_catalogos[["Catálogo MUNICIPIOS"]]["CLAVE_MUNICIPIO"] <- paste0(as.character(as.numeric(lista_catalogos[["Catálogo MUNICIPIOS"]]$CLAVE_MUNICIPIO)), "-", as.character(as.numeric(lista_catalogos[["Catálogo MUNICIPIOS"]]$CLAVE_ENTIDAD)))

datos_actuales["MUNICIPIO_RES"] <- paste0(datos_actuales$MUNICIPIO_RES, "-", datos_actuales$ENTIDAD_RES)

head(datos_actuales$MUNICIPIO_RES)

lista_catalogos

campos_cruzar

campos_cruzar[1, 2][[1]]

gsub("Clase", "Tarea", c("Clase 1", "Clase 2"))

as.numeric("f")

for(i_renglon in 1:nrow(campos_cruzar)){

    campo <- campos_cruzar[i_renglon, 2][[1]]
    nombre_catalogo <- gsub("CATÁLOGO:", "", gsub(" ", "", campos_cruzar[i_renglon, 4]))
    nombre_catalogo <- gsub("CATALÓGO:", "", gsub(" ", "", nombre_catalogo))
    nombre_catalogo <- gsub("CATALOGO:", "", gsub(" ", "", nombre_catalogo))
    df_auxiliar <- lista_catalogos[[paste0('Catálogo ', nombre_catalogo)]]
    if(is.null(df_auxiliar)){
        df_auxiliar <- lista_catalogos[[paste0('Catálogo de ', nombre_catalogo)]]
    }
    names(df_auxiliar)[2] <- paste0(campo, "_merge")
    if(all(!is.na(as.numeric(df_auxiliar[[1]]))))
        df_auxiliar[,1] <- as.numeric(df_auxiliar[[1]])
    columna <- names(df_auxiliar)[1]
    #merge(datos_actuales, df_auxiliar, by.x = campo, by.y = columna, all.x = T, sort = F)
    print(paste(campo, nombre_catalogo))
    print(paste(campo, columna))
    datos_actuales <- merge(datos_actuales, df_auxiliar, by.x = campo, by.y = columna, all.x = T, sort = F)
#     datos_actuales <- datos_actuales[, !grepl(campo,names(datos_actuales))]
}

table(datos_actuales$MUNICIPIO_RES_merge)

datos_actuales[is.na(datos_actuales$MUNICIPIO_RES_merge), "MUNICIPIO_RES"]

table(datos_actuales$RESULTADO_merge)


summary(datos_actuales)

coordenadas <- read.csv("inputs/AGEEML_20204142212409.csv")

dim(coordenadas)

head(coordenadas)

all(unique(datos_actuales$MUNICIPIO_RES) %in% unique(coordenadas$Cve_Loc))

datos_metadatos

head(coordenadas)

toupper(unique(coordenadas$Nom_Mun))

duplicated(c(1, 2, 3, 1))

coordenadas["llave_municipio"] <- paste0(coordenadas$Nom_Mun, "-", coordenadas$Nom_Ent)
coordenadas_unicas <- coordenadas[!duplicated(coordenadas$llave_municipio),]
coordenadas_unicas["llave_municipio"] <- toupper(coordenadas_unicas$llave_municipio)
coordenadas_unicas

unique(datos_actuales$MUNICIPIO_RES_merge)

unique(datos_actuales$MUNICIPIO_RES_merge)[!unique(datos_actuales$MUNICIPIO_RES_merge) %in% toupper(coordenadas_unicas$Nom_Mun)]

datos_actuales[is.na(datos_actuales$MUNICIPIO_RES_merge),c("MUNICIPIO_RES")]
datos_actuales[is.na(datos_actuales$MUNICIPIO_RES_merge),]

datos_actuales["llave_municipio"] <- paste0(datos_actuales$MUNICIPIO_RES_merge, "-", datos_actuales$ENTIDAD_RES_merge)

datos_actuales <- merge(datos_actuales, coordenadas_unicas[c("Longitud", "Lat_Decimal", "llave_municipio")], by = "llave_municipio", all.x = T, sort = T)

table(datos_actuales$ENTIDAD_RES_merge)

coordenadas_estados <- coordenadas[!duplicated(coordenadas$Nom_Ent), ]

datos_actuales[datos_actuales$MUNICIPIO_RES_merge == 'NO ESPECIFICADO' | is.na(datos_actuales$MUNICIPIO_RES_merge),"ENTIDAD_RES"]

names(datos_actuales)

names(datos_actuales)

datos_actuales_COVID19 <- datos_actuales[datos_actuales$RESULTADO_merge == 'Positivo SARS-CoV-2', ]

datos_actuales_COVID19[is.na(datos_actuales_COVID19$Lat_Decimal),]
n_sin_municipio <- nrow(datos_actuales_COVID19[is.na(datos_actuales_COVID19$Lat_Decimal),])

table(datos_actuales_COVID19$RESULTADO)

hist(datos_actuales_COVID19$Longitud)

hist(datos_actuales_COVID19$Lat_Decimal)

dim(datos_actuales_COVID19)

library(plotly)

mapboxToken <- paste(readLines(".mapbox_token"), collapse="")
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) 
# crear_mapa <- function()

aux_temp <- "Longitud"

confirmados <- datos_actuales_COVID19 %>% 
group_by(llave_municipio, Longitud, Lat_Decimal) %>% 
summarise(
 n = length(RESULTADO)
) 
confirmados <- confirmados %>%
plot_ly(
    lat = ~Longitud,
    lon = ~Lat_Decimal,
    marker = list(color = "red"),
    type = 'scattermapbox',
    text = paste(.$llave_municipio, .$n),
    mode = "markers",
    size = ~n*15,
    hovertemplate = paste('<i>%{text}</i>'),
    name = "Casos Confirmados"
  ) 
confirmados <- confirmados %>%
  layout(
    title=paste0("Mapa interactivo de COVID19 en México por municipios"),
    mapbox = list(
      style = 'open-street-map',
      zoom =3,
      center = list(lon = -102, lat = 25))) 
confirmados <- confirmados %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

confirmados

table(datos_actuales_COVID19$TIPO_PACIENTE_merge)

#CASOS CONFIRMADOS HOSPITALIZADOS

confirmados_hospitalizados <- datos_actuales_COVID19 %>% 
filter(TIPO_PACIENTE_merge == "HOSPITALIZADO") %>%
group_by(llave_municipio, Longitud, Lat_Decimal) %>% 
summarise(
 n = length(RESULTADO)
) 
confirmados_hospitalizados <- confirmados_hospitalizados %>%
plot_ly(
    lat = ~Longitud,
    lon = ~Lat_Decimal,
    marker = list(color = "orange"),
    type = 'scattermapbox',
    text = paste(.$llave_municipio, .$n),
    mode = "markers",
    size = ~n*15,
    hovertemplate = paste('<i>%{text}</i>'),
    name = "Casos Confirmados Hospitalizados"
  ) 
confirmados_hospitalizados <- confirmados_hospitalizados %>%
  layout(
    title="Mapa interactivo de COVID19 en México por municipios",
    mapbox = list(
      style = 'open-street-map',
      zoom =3,
      center = list(lon = -102, lat = 25))) 

confirmados_hospitalizados %>% add_annotations(x = -102,
                  y = 25,
                  text = "Prueba",
                  xref = "lon",
                  yref = "lat",
                  showarrow = TRUE,
                  arrowhead = 4,
                  arrowsize = .5,
                  ax = 20,
                  ay = -40)

confirmados_hospitalizados <- confirmados_hospitalizados %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))

# confirmados_hospitalizados

# confirmados_UCI



confirmados_UCI <- datos_actuales_COVID19 %>% 
  filter(UCI_merge ==  "SI") %>%
  group_by(llave_municipio, Longitud, Lat_Decimal) %>% 
  summarise(
    n = length(RESULTADO)
  ) 
confirmados_UCI <- confirmados_UCI %>%
  plot_ly(
    lat = ~Longitud,
    lon = ~Lat_Decimal,
    marker = list(color = "brown"),
    type = 'scattermapbox',
    text = paste(.$llave_municipio, .$n),
    mode = "markers",
    size = ~n*15,
    hovertemplate = paste('<i>%{text}</i>'),
    name = "Casos Confirmados en Cuidados Intensivos"
  ) 
confirmados_UCI <- confirmados_UCI %>%
  layout(
    title="Mapa interactivo de COVID19 en México por municipios",
    mapbox = list(
      style = 'open-street-map',
      zoom =3,
      center = list(lon = -102, lat = 25))) 


confirmados_UCI <- confirmados_UCI %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))


confirmados_UCI

table(datos_actuales_COVID19$FECHA_DEF)

sum(table(datos_actuales_COVID19$FECHA_DEF)) - 5811

#Fallecimientos

fallecimientos <- datos_actuales_COVID19 %>% 
filter(FECHA_DEF != "", FECHA_DEF != "9999-99-99") %>%
group_by(llave_municipio, Longitud, Lat_Decimal) %>% 
summarise(
 n = length(RESULTADO)
) 
fallecimientos <- fallecimientos %>%
plot_ly(
    lat = ~Longitud,
    lon = ~Lat_Decimal,
    marker = list(color = "black"),
    type = 'scattermapbox',
    text = paste(.$llave_municipio, .$n),
    mode = "markers",
    size = ~n*15,
    hovertemplate = paste('<i>%{text}</i>'),
    name = "Fallecimientos"
  ) 
fallecimientos <- fallecimientos %>%
  layout(
    title="Mapa interactivo de COVID19 en México por municipios",
    mapbox = list(
      style = 'open-street-map',
      zoom =3,
      center = list(lon = -102, lat = 25))) 
fallecimientos <- fallecimientos %>%
  config(mapboxAccessToken = Sys.getenv("MAPBOX_TOKEN"))


fallecimientos

datos_actuales$FECHA_ACTUALIZACION[1]

fig_TOTAL <- subplot(confirmados, confirmados_UCI, confirmados_hospitalizados, fallecimientos) %>%
layout(annotations = 
        list(x = 1, y = -0.1, text = paste0( "Hay ",n_sin_municipio, " casos confirmados no asociados a un municipio ", datos_actuales$FECHA_ACTUALIZACION[1]), 
      showarrow = F, xref='paper', yref='paper', 
      xanchor='right', yanchor='auto', xshift=0, yshift=0,
      font=list(size=15, color="red")),
      legend = list(   
                     x = 0,
                     y = .1
            )
      )

fig_TOTAL

sessionInfo()

Sys.getenv("RSTUDIO_PANDOC")

?htmlwidgets::saveWidget

widget_file_size <- function(p, file_path = "index.html") {
  d <- "./"
  withr::with_dir(d, htmlwidgets::saveWidget(p, file_path, selfcontained =F, libdir = "lib"))
  f <- file.path(d, file_path)
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}

widget_file_size(partial_bundle(fig_TOTAL), "mapa.html")


datos_edad_positivos <- datos_actuales_COVID19 %>% group_by(EDAD) %>% summarize(NCasos=length(EDAD))
datos_edad_fallecidos <- datos_actuales_COVID19 %>% filter(FECHA_DEF !=  "9999-99-99" & FECHA_DEF !=  "") %>% group_by(EDAD) %>% summarize(NFallecidos=length(EDAD)) 
datos_edad <- merge(datos_edad_positivos, datos_edad_fallecidos, all.x = T, by = "EDAD")
datos_edad["Letalidad"] <- datos_edad$NFallecidos / datos_edad$NCasos
datos_edad[abs(datos_edad["Letalidad"] - 1) < 0.00001 & !is.na(datos_edad["Letalidad"]), "Letalidad"] <- .999999 
datos_edad["logit"] <- log(datos_edad["Letalidad"]/(1 - datos_edad["Letalidad"]))
datos_edad

plot(1:nrow(datos_edad), datos_edad[["logit"]] )


regresion_logitica <- lm(logit~EDAD, data = datos_edad)
coeficientes = coef(regresion_logitica )
predecir_tasa_letalidad <- function(x){
    1/(1 + exp(-(coeficientes[1] + coeficientes[2] * x)))
}
datos_edad["letalidad_estimada"] <- predecir_tasa_letalidad(datos_edad["EDAD"])
datos_edad


ay <- list(
  tickfont = list(color = "red"),
  range=list(0, max(datos_edad["Letalidad"],na.rm = T)),
  overlaying = "y",
  side = "right",
  title = "Tasa de letalidad"
  
)




g_edad <- datos_edad %>% plot_ly( x = ~as.factor(EDAD), y = ~NCasos, type = "bar", name = "Número de casos Positivos")
g_edad <- g_edad %>% layout(
  title = "Análisis de Casos Covid19 por Edad", yaxis2 = ay,
  xaxis = list(title="Edad")
)

g_edad <- g_edad %>% add_trace(y = ~NFallecidos, name = "Número de Fallecidos" )
g_edad <- g_edad %>% add_lines(y = ~Letalidad, name = "Tasa de letalidad", yaxis = "y2")
g_edad <- g_edad %>% add_lines(y = ~letalidad_estimada, name = "Tasa de letalidad estimada", yaxis = "y2")
# g_edad
g_edad <- g_edad %>% layout(
  title = "Análisis de Casos Covid por Edad", yaxis2 = ay,
  xaxis = list(title="Edad"),
  legend = list(orientation = 'h')
)
g_edad

widget_file_size(partial_bundle(g_edad), "Letalidad.html")

fallecimiento_diarios <- datos_actuales_COVID19 %>% filter(FECHA_DEF != "9999-99-99", FECHA_DEF != "") %>% 
    group_by(FECHA_DEF) %>%
    summarize(Fallecimientos = n()) 

fallecimiento_diarios["Fallecimientos_acum"] <- cumsum(fallecimiento_diarios["Fallecimientos"])
# fallecimiento_diarios["FECHA_DEF"] <- as.Date(as.character(fallecimiento_diarios["FECHA_DEF"]), format = "%Y/%m/%d")
fallecimiento_diarios[["FECHA_DEF"]] <- as.Date(fallecimiento_diarios[["FECHA_DEF"]], format = "%Y-%m-%d")



g_fallecimientos_diarios <- fallecimiento_diarios %>%
    plot_ly(x = ~as.factor(FECHA_DEF), y = ~Fallecimientos, type = "bar", name = "Fallecidos diarios", marker = list(color = "brown")) %>% 
    layout(
        xaxis = list(title = "Fecha")
    ) %>%
    add_lines(y = ~Fallecimientos_acum, mode = "lines", name = "Fallecidos acumulados", type = 'scatter', line = list(color = "red"), marker = list(color = "red")) %>%
    layout(legend = list(x = 0, y = .7))

anotaciones <- list(
  x = fallecimiento_diarios$FECHA_DEF[nrow(fallecimiento_diarios)],
  y = c(fallecimiento_diarios$Fallecimientos[nrow(fallecimiento_diarios)], fallecimiento_diarios$Fallecimientos_acum[nrow(fallecimiento_diarios)]),
  text = c(fallecimiento_diarios$Fallecimientos[nrow(fallecimiento_diarios)], fallecimiento_diarios$Fallecimientos_acum[nrow(fallecimiento_diarios)]),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = -0,
  ay = -20
)

g_fallecimientos_diarios <- g_fallecimientos_diarios %>% layout(annotations = c(anotaciones))

widget_file_size(partial_bundle(g_fallecimientos_diarios), "fallecimientos_diarios.html")

## Análisis de casos diarios

list.dirs("inputs")






url_datos_diarios <- sprintf("https://coronavirus.gob.mx/datos/Downloads/Files/Casos_Diarios_Estado_Nacional_Confirmados_%s.csv", format(Sys.Date(), "%Y%m%d"))
temp <- tempfile()
download.file(url_datos_diarios, temp)
file.rename(temp, sprintf("./inputs/Datos_diarios%s.csv", Sys.Date()))
# datos_actuales <- read.csv( file.path("inputs", paste0("datos_", Sys.Date(), ".csv" )))



datos_diarios <- read.csv(sprintf("./inputs/Datos_diarios%s.csv", Sys.Date()))
head(datos_diarios)

datos_diarios <- datos_diarios %>% filter(nombre != "Nacional")

casos <- datos_diarios[, 4:ncol(datos_diarios) ]

casos_diarios <- apply(casos, 2, sum)
fechas <- as.Date(sub("X", "", names(casos_diarios)), format("%d.%m.%Y"))
df_casos_diarios <- data.frame(
    "dia" = fechas,
    "casos" = casos_diarios)
df_casos_diarios["casos_acumulados"] <- cumsum(df_casos_diarios["casos"])
df_casos_diarios

g_casos_diarios <- df_casos_diarios %>%
plot_ly(x = ~dia, y = ~casos, name = "Casos confirmados diarios", mode = "lines+markers", type = 'scatter') %>%
add_trace(y = ~casos_acumulados, name = "Casos confirmados acumulados") %>%
layout(
    title = "Casos diarios y acumulados",
    legend = list(orientation = "h")
)

anotaciones <- list(
  x = df_casos_diarios$dia[nrow(df_casos_diarios)],
  y = c(df_casos_diarios$casos[nrow(df_casos_diarios)], df_casos_diarios$casos_acumulados[nrow(df_casos_diarios)]),
  text = c(df_casos_diarios$casos[nrow(df_casos_diarios)], df_casos_diarios$casos_acumulados[nrow(df_casos_diarios)]),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = -0,
  ay = -20
)

g_casos_diarios <- g_casos_diarios %>% layout(annotations = c(anotaciones))

widget_file_size(partial_bundle(g_casos_diarios), "casos_diarios.html")

fallecimientos <- datos_actuales_COVID19 %>% 
filter(FECHA_DEF != "", FECHA_DEF != "9999-99-99")
table(fallecimientos$TABAQUISMO_merge)
fallecimientos$ALGUNAENFERMEDAD <- "NO"
fallecimientos$ALGUNAENFERMEDAD[apply(fallecimientos[, c("TABAQUISMO_merge",
"EMBARAZO_merge",
"DIABETES_merge",
"EPOC_merge",
"ASMA_merge",
"HIPERTENSION_merge",
"OBESIDAD_merge",
"CARDIOVASCULAR_merge",
"RENAL_CRONICA_merge",
"INMUSUPR_merge",
"OTRAS_COM_merge")], 1, function(x) any(x == "SI"))
] <- "SI"
                                      
fig <- plot_ly(width = 1000, height = 1400)
fig <- fig %>% add_pie(data = count(fallecimientos, TABAQUISMO_merge), labels = ~TABAQUISMO_merge, values = ~n,
          name = "Tabaquismo", domain = list(x = c(0, 0.3), y = c(0.75, 1)), title = "FUMADOR", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, EMBARAZO_merge), labels = ~EMBARAZO_merge, values = ~n,
          name = "Embarazo", domain = list(x = c(0, 0.3), y = c(0.5, 0.75)), title = "EMBARAZO", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, DIABETES_merge), labels = ~DIABETES_merge, values = ~n,
          name = "Diabetes", domain = list(x = c(0, 0.3), y = c(0.25, 0.5)), title = "DIABETES", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, EPOC_merge), labels = ~EPOC_merge, values = ~n,
          name = "Epoc", domain = list(x = c(0.35, 0.65), y = c(0.25, 0.5)), title = "EPOC", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, ASMA_merge), labels = ~ASMA_merge, values = ~n,
          name = "Asma", domain = list(x = c(0.35, 0.65), y = c(0.75, 1)), title = "ASMA", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, HIPERTENSION_merge), labels = ~HIPERTENSION_merge, values = ~n,
          name = "Hipertensión", domain = list(x = c(0.35, 0.65), y = c(0.5, 0.75)), title = "HIPERTENSION", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, OBESIDAD_merge), labels = ~OBESIDAD_merge, values = ~n,
          name = "Obesidad", domain = list(x = c(0.7, 1), y = c(0.75, 1)), title = "OBESIDAD", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, CARDIOVASCULAR_merge), labels = ~CARDIOVASCULAR_merge, values = ~n,
          name = "Cardiobascular", domain = list(x = c(0.7, 1), y = c(0.5, 0.75)), title = "CARDIOVASCULAR", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, RENAL_CRONICA_merge), labels = ~RENAL_CRONICA_merge, values = ~n,
          name = "Renal Cronica", domain = list(x = c(0.7, 1), y = c(0.25, 0.5)), title = "RENAL_CRONICA", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(fallecimientos, INMUSUPR_merge), labels = ~INMUSUPR_merge, values = ~n,
          name = "Inmunodepresión", domain = list(x = c(0, 0.3), y = c(0, 0.25)), title = "INMUNODEPRESIÓN", marker = list(colors = c("green", "black", "red")))                    
fig <- fig %>% add_pie(data = count(fallecimientos, OTRAS_COM_merge), labels = ~OTRAS_COM_merge, values = ~n,
          name = "Otras enfermedades", domain = list(x = c(0.35, 0.65), y = c(0, 0.25)), title = "OTRAS ENFERMEDADES", marker = list(colors = c("green", "black", "red")))                    
fig <- fig %>% add_pie(data = count(fallecimientos, ALGUNAENFERMEDAD), labels = ~ALGUNAENFERMEDAD, values = ~n,
          name = "Alguna condición anterior", domain = list(x = c(0.7, 1), y = c(0, 0.25)), title = "ALGUNA CONDICIÓN DE LAS ANTERIORES", marker = list(colors = c("green", "red")))                                                          
fig <- fig %>% layout(title = "Condiciones en personas fallecidas por COVID19", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     autosize = T
                     )


widget_file_size(partial_bundle(fig), "enfermedades.html")


no_fallecimientos <- datos_actuales_COVID19 %>% 
filter(FECHA_DEF == "" | FECHA_DEF == "9999-99-99")
table(no_fallecimientos$TABAQUISMO_merge)
no_fallecimientos$ALGUNAENFERMEDAD <- "NO"
no_fallecimientos$ALGUNAENFERMEDAD[apply(no_fallecimientos[, c("TABAQUISMO_merge",
"EMBARAZO_merge",
"DIABETES_merge",
"EPOC_merge",
"ASMA_merge",
"HIPERTENSION_merge",
"OBESIDAD_merge",
"CARDIOVASCULAR_merge",
"RENAL_CRONICA_merge",
"INMUSUPR_merge",
"OTRAS_COM_merge")], 1, function(x) any(x == "SI"))
] <- "SI"
                                      
fig <- plot_ly(width = 1000, height = 1400)
fig <- fig %>% add_pie(data = count(no_fallecimientos, TABAQUISMO_merge), labels = ~TABAQUISMO_merge, values = ~n,
          name = "Tabaquismo", domain = list(x = c(0, 0.3), y = c(0.75, 1)), title = "FUMADOR", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, EMBARAZO_merge), labels = ~EMBARAZO_merge, values = ~n,
          name = "Embarazo", domain = list(x = c(0, 0.3), y = c(0.5, 0.75)), title = "EMBARAZO", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, DIABETES_merge), labels = ~DIABETES_merge, values = ~n,
          name = "Diabetes", domain = list(x = c(0, 0.3), y = c(0.25, 0.5)), title = "DIABETES", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, EPOC_merge), labels = ~EPOC_merge, values = ~n,
          name = "Epoc", domain = list(x = c(0.35, 0.65), y = c(0.25, 0.5)), title = "EPOC", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, ASMA_merge), labels = ~ASMA_merge, values = ~n,
          name = "Asma", domain = list(x = c(0.35, 0.65), y = c(0.75, 1)), title = "ASMA", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, HIPERTENSION_merge), labels = ~HIPERTENSION_merge, values = ~n,
          name = "Hipertensión", domain = list(x = c(0.35, 0.65), y = c(0.5, 0.75)), title = "HIPERTENSION", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, OBESIDAD_merge), labels = ~OBESIDAD_merge, values = ~n,
          name = "Obesidad", domain = list(x = c(0.7, 1), y = c(0.75, 1)), title = "OBESIDAD", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, CARDIOVASCULAR_merge), labels = ~CARDIOVASCULAR_merge, values = ~n,
          name = "Cardiobascular", domain = list(x = c(0.7, 1), y = c(0.5, 0.75)), title = "CARDIOVASCULAR", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, RENAL_CRONICA_merge), labels = ~RENAL_CRONICA_merge, values = ~n,
          name = "Renal Cronica", domain = list(x = c(0.7, 1), y = c(0.25, 0.5)), title = "RENAL_CRONICA", marker = list(colors = c("green", "black", "red")))
fig <- fig %>% add_pie(data = count(no_fallecimientos, INMUSUPR_merge), labels = ~INMUSUPR_merge, values = ~n,
          name = "Inmunodepresión", domain = list(x = c(0, 0.3), y = c(0, 0.25)), title = "INMUNODEPRESIÓN", marker = list(colors = c("green", "black", "red")))                    
fig <- fig %>% add_pie(data = count(no_fallecimientos, OTRAS_COM_merge), labels = ~OTRAS_COM_merge, values = ~n,
          name = "Otras enfermedades", domain = list(x = c(0.35, 0.65), y = c(0, 0.25)), title = "OTRAS ENFERMEDADES", marker = list(colors = c("green", "black", "red")))                    
fig <- fig %>% add_pie(data = count(no_fallecimientos, ALGUNAENFERMEDAD), labels = ~ALGUNAENFERMEDAD, values = ~n,
          name = "Alguna condición anterior", domain = list(x = c(0.7, 1), y = c(0, 0.25)), title = "ALGUNA CONDICIÓN DE LAS ANTERIORES", marker = list(colors = c("green", "red")))                                                          
fig <- fig %>% layout(title = "Condiciones en personas que tienen o tuvieron COVID19", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     autosize = T
                     )

widget_file_size(partial_bundle(fig), "enfermedades_confirmados.html")


agrupar_edades <- function(edad, grupo = 5, omega = 110){
    i = 1
    while((i * grupo) < omega){
        edad[edad >= (i - 1) * grupo & edad <(i) * grupo] = paste0("[", (i - 1) * grupo, ",", (i) * grupo, ")")
        i = i + 1
    }
    if((i - 1)* grupo < omega){
        edad[edad >= (i - 1) * grupo] = paste0("[", (i - 1) * grupo, ",", (i) * grupo, ")")
    }
    return(edad)
}

fallecimientos$diff_dias_fallecimiento <- as.numeric(as.Date(fallecimientos$FECHA_DEF) - as.Date(fallecimientos$FECHA_SINTOMAS))
fallecimientos$EDAD_5 <- as.factor(agrupar_edades(fallecimientos$EDAD))
fallecimientos$EDAD_5 <- factor(fallecimientos$EDAD_5, levels=unique(fallecimientos$EDAD_5[order(fallecimientos$EDAD)]), ordered=TRUE)
table(fallecimientos$EDAD_5,fallecimientos$diff_dias_fallecimiento)
z = table(fallecimientos$EDAD_5,fallecimientos$diff_dias_fallecimiento)
library(plotly)
fig <- plot_ly(z = z, type = "heatmap", y = rownames(z))
fig <- fig %>% layout(
    title = "Mapa de calor por grupos de edad y diferencia entre la fecha de inicio de síntomas y fecha de defunción",
    xaxis = list(title = "Diferencia de días"),
    yaxis = list(title = "Edad")
    
)
fig <- fig %>% colorbar(title = "Número de personas")
fig
widget_file_size(fig, "tiempo_fallecimiento_edad.html")


source("Crear_dashboard.r")
