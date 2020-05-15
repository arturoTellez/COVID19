
library(plotly)
library(htmlwidgets)


get_div <- function(texto){
    bandera <- apply(texto, 1, function(x) grepl("div", x))
    return(texto[bandera, , drop = F])
}
get_script <- function(texto){
    bandera <- cumsum(apply(texto, 1, function(x) grepl("div", x)))
    texto_aux <- texto[bandera >0,, drop = F]
    bandera <- apply(texto_aux, 1, function(x) grepl("script", x))
    texto_aux <- texto_aux[bandera, , drop = F]
    return(texto_aux)
}

replace_texto <- function(texto, orig, new){
    return(gsub(orig, new, texto))
}

master <- read.delim("dashboard.html", as.is = TRUE, header = FALSE, quote = "{}")
nueva_grafica <- read.delim("mapa.html", as.is = TRUE, quote = "@")

agregar_graficas <- function(master, nueva_grafica, alto = 700, texto_intermedio = "", titulo = "Mapa interactivo"){
    master[62,] <- replace_texto(master[62,], "Mapa interactivo", titulo)
    names(nueva_grafica) <- names(master)
    divs <- get_div(nueva_grafica)
    scripts <- get_script(nueva_grafica)
    divs[1,] <- replace_texto(divs[1,], "htmlwidget_container", "container")
    divs[2,] <- replace_texto(divs[2,], "400", "700")
    bandera <- cumsum(apply(master, 1, function(x) grepl("Aquí van los conteiners", x)))
    bandera <- cumsum(apply(master, 1, function(x) grepl("Aquí van los conteiners", x)))
    master1 <- master[bandera == 0,, drop = FALSE]
    master2 <- master[bandera == 1,, drop = FALSE]
    espaciado <- data.frame("temp" = c("<br>", "<br>"), stringsAsFactors = F)
    if(texto_intermedio != ""){
        espaciado[3,"temp"] <- texto_intermedio
    }                            
    names(espaciado) <- names(master)
    master <- rbind(master1, divs, espaciado, master2)
    bandera <- cumsum(apply(master, 1, function(x) grepl("Aquí van los scripts", x)))
    master1 <- master[bandera == 0,, drop = FALSE]
    master2 <- master[bandera == 1,, drop = FALSE]
    master <- rbind(master1, scripts, master2)
    return(master)
}

                            
master <- agregar_graficas(master, nueva_grafica)
                            
write.table(master, "dashboard-mapa.html", row.names = F, quote = F, col.names = F)  
                            

master <- read.delim("dashboard.html", as.is = TRUE, header = FALSE, quote = "{}")

nueva_grafica <- read.delim("casos_diarios.html", as.is = TRUE, quote = "@")
master <- agregar_graficas(master, nueva_grafica, "500", titulo = "Casos Diarios", texto_intermedio = "<h3>Fallecimientos Diarios</h3>")
  
nueva_grafica <- read.delim("fallecimientos_diarios.html", as.is = TRUE, quote = "@")
master <- agregar_graficas(master, nueva_grafica, "500")


write.table(master, "dashboard-casos-diarios.html", row.names = F, quote = F, col.names = F)  

master <- read.delim("dashboard.html", as.is = TRUE, header = FALSE, quote = "{}")

nueva_grafica <- read.delim("enfermedades_confirmados.html", as.is = TRUE, quote = "@")
master <- agregar_graficas(master, nueva_grafica, titulo = "Condiciones de los confirmados por COVID19", texto_intermedio = "<h3>Condiciones de Defunciones</h3>")

nueva_grafica <- read.delim("enfermedades.html", as.is = TRUE, quote = "@")
master <- agregar_graficas(master, nueva_grafica)
                            
                            
write.table(master, "dashboard-condiciones.html", row.names = F, quote = F, col.names = F)  

master <- read.delim("dashboard.html", as.is = TRUE, header = FALSE, quote = "{}")

nueva_grafica <- read.delim("Letalidad.html", as.is = TRUE, quote = "@")
master <- agregar_graficas(master, nueva_grafica, "500", titulo = "Defunciones por Edad", texto_intermedio = "<h3>Evoluación de casos diarios</h3>")


nueva_grafica <- read.delim("tiempo_fallecimiento_edad.html", as.is = TRUE, quote = "@")
master <- agregar_graficas(master, nueva_grafica, "500")

                            
                            
write.table(master, "dashboard-fallecimientos.html", row.names = F, quote = F, col.names = F) 
