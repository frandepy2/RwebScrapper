library(rvest)
library(dplyr)
library(ggplot2)
#Recibimos los lenguajes y mutamos algunos valores para permitir que las busquedas sean correctas
convertLenguajes <- function(lenguajes){
  listaNom <- c("c#","c++","visual basic", "assembly language", "delphi/object pascal","classic visual basic")
  df <- data.frame(lenguajes)
  
  df$alias[df$lenguajes] <- df$lenguajes
  df$alias[df$lenguajes == "c#"] <- "csharp"
  df$alias[df$lenguajes == "c++"] <- "cpp"
  df$alias[df$lenguajes == "visual basic"] <- "visual-basic"
  df$alias[df$lenguajes == "assembly language"]<- "assembly"
  df$alias[df$lenguajes == "delphi/object pascal"] <- "delphi"
  df$alias[df$lenguajes == "classic visual basic"] <- "visual-basic-6"
  
  return(df)
}

# Extraemos de la pagina de TIOBE LA LISTA DEL TOP 20 LENGUAJES DE PROGRAMACION
link <- "https://www.tiobe.com/tiobe-index/"
page <- read_html(link)

name = page %>% 
  html_nodes(".td-top20+ td") %>% 
  html_text()

#Convertimos la lista de lenguajes del TIOBE en minusculas
#convertimos la lista en un data frame y creamos las columnas del links y alias
lenguajes_lower <- tolower(name)
lenguajes_convertidos <- convertLenguajes(lenguajes_lower)
lenguajes_convertidos$links <- paste("https://github.com/topics/",lenguajes_convertidos$alias, sep = "")

#print(lenguajes_convertidos)
print("Scrapping Github topics...")
for(link in lenguajes_convertidos$links)
{
  #print(link)
  page <- read_html(link)
  datos = page %>%
    html_nodes(".h3") %>%
    html_text()
  #print(datos)
  #Extraer la cantidad de repocitorios publicos en R
  dato_split <- strsplit(datos,"\n            ")
  dato_actual <-strsplit(dato_split[[1]][3]," ")
  
  lenguajes_convertidos$response[lenguajes_convertidos$links == link] <- as.integer(gsub(",","",dato_actual[[1]][1]))
  
  Sys.sleep(2) # Este sleep es para que no se bloquee por muchos intentos en poco tiempo
  
}
print("Scrap complete")

#1.2 Almacene los datos en un .csv
df_ordenado <- lenguajes_convertidos[order(lenguajes_convertidos$response,decreasing = TRUE), ]
write.csv(df_ordenado,"Apariciones.csv", row.names = FALSE)

#1.3 y 1.4 Rating de Github
MIN <- min(df_ordenado$response)
MAX <- max(df_ordenado$response)

#1.4
df_ordenado$Rating <- ((df_ordenado$response - MIN)/(MAX-MIN))*100
datos2 <- select(df_ordenado, -links, -alias)
print(datos2)

#1.5 Grafico de Barras de los 10 lenguajes con mas apariciones
datos2 <- datos2[1:10,]


p <- ggplot(data = datos2, aes(x = reorder(lenguajes, -response), y = response)) + geom_bar(stat = "identity", position = "stack")
p