library(rvest)
library(dplyr)
library(ggplot2)

url <- "https://github.com/topics/visual-basic?o=desc&s=updated&page="

i <- c(1:10) #LAS 10 primeras paginas 

links <-paste(url,i,sep="")

print("Scraping github topics with topic : Visual-basic ")
datos_completos <- numeric()
cont <- 0
for(link in links){
  cont <- cont+1
  print(paste("Scrapping page ",cont))
  page <- read_html(link)
  datos = page %>%
    html_nodes(".f6.mb-2") %>%
    html_text()
  
  dato_corregido <- substr(datos,16,100)
  dato_corregido <- substr(dato_corregido,1,nchar(dato_corregido)-1)
  
  datos_completos <- c(rbind(datos_completos,dato_corregido))[order(c(seq_along(datos_completos)*2 - 1, seq_along(dato_corregido)*2))]
}

df <- data.frame(Tags = datos_completos)
tb <- table(df$Tags)
dfFinal <- data.frame(tb)
dfFinal <- dfFinal[order(dfFinal$Freq, decreasing = TRUE),]

p <- ggplot(data = dfFinal[1:10,],aes(x=reorder(Var1, -Freq),y = Freq)) + geom_bar(stat = "identity", position = "stack")