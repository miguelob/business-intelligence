library(readxl)
data3 <- read_excel("datasetalquilereschamberi3.xlsx")
data2 <- read_excel("datasetalquilereschamberi2.xlsx")
names(data2)

prec_m <- data3$Precio/data3$Superficie
data3 <- cbind(data3,prec_m)
data3 <- short(data3,data3$precio_m)


funcion <- function(data3){
  for(i in 1:nrow(data3)){
    if(data3$Precio[i] < 1000 || data3$Distanciasol[i] < 3){
      if(i == 1){
        Megusta <- "SI"
      }else{
        Megusta <- c(Megusta,"SI")
      }
    }else{
      if(i == 1){
        Megusta <- "NO"
      }else{
        Megusta <- c(Megusta,"NO")
      }
    }
  }
  return(Megusta)
}
Megusta <- funcion(data3)
test <- cbind(data3,Megusta)

table(test$Megusta)
