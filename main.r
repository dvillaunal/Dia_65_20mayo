## ---- eval=FALSE, include=TRUE----------------------------------------
## "Protocolo:
## 
## 1. Daniel Felipe Villa Rengifo
## 
## 2. Lengauje: R
## 
## 3. Tema: Cree funciones que manejen listas en R (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios en archivos independientes tipo *.txt)
## 
## 4. Fuentes:
##    https://www.generatedata.com"


## ---------------------------------------------------------------------
# Exportamos la base baloto.csv:
baloto <- read.csv(file = "Baloto.csv", sep = ",", header = T, dec = ".")

baloto <- data.frame(baloto)

# Hacemos factor la columna panel:
baloto$Panel <- as.factor(baloto$Panel)

## Convertimos en matriz la columna tiquete (sera el tiquete del baloto):
tiquete <- matrix(baloto$Tiquete, nrow = 10)
print(tiquete)

#Ahora vamos agregar en un vector numerico y vector cadena los numeros jugados y vector de nombres

Panel <- as.vector(levels(baloto$Panel))

Jugadores <- as.vector(baloto$Nombre)
print(Jugadores)

NumMarcado <- as.vector(baloto$NUM_Jugado)
print(NumMarcado)

# Creamos una lista donde agregamos los vectores matriz:
lista_Baloto <- list(Jugadores, Panel,NumMarcado, tiquete, baloto)
names(lista_Baloto) <- c("Participante", "P","Numero", "Tiquet", "Base")

lista_Baloto[[2]] <- as.factor(lista_Baloto[[2]])

## Ahora creamos la función para saber quien es el ganador de los jugadores seleccionados:

ganador <- function(x){
  "Calculara el ganador del baloto"
  
  panel <-function(t){
    "A=1, B=2, C=3, D=4"
    t <- levels(x[[2]])
    # Primero calculemos el panel ganador:
   p <- sample(1:4,size = 1,replace = T)
   if(p == 1){
     p <- t[1]
     return(p)
   }
   if(p == 2){
     p <- t[2]
     return(p)
   }
   if(p == 3){
     p <- t[3]
     return(p)
   }
   if(p == 4){
     p <- t[4]
     return(p)
   }
   if(p == 5){
     p <- t[5]
     return(p)
   }
  }
  
  pg <- panel()
  #Filtramos el data.frame X panel ganador
  panelwin <- dplyr::filter(x[[5]], Panel == pg)
  
  NumGanador <- function(){
    "Ahora según el Panel Ganador Calcularemos el numero ganador:"
    winum <-as.vector(panelwin$NUM_Jugado)
    
    i <- sample(1:length(panelwin$Panel), size = 1, replace = T)
    return(winum[i])
  }
  numg <- NumGanador()
  
  NomGanador <- function(){
    winner <- dplyr::filter(x[[5]], NUM_Jugado == numg)
    winner <- as.vector(winner$Nombre)
    return(winner)
  }
  nomg <- NomGanador()
  
  WinnerBaloto <- paste("El Ganador del Baloto es: ", nomg, ", Con el Panel: ", pg, " y el numero ganador: ", numg)
  
  return(as.vector(WinnerBaloto))
}

## Ahora exportamos en ganador en un .txt:
Multimillonario <- ganador(lista_Baloto)

for (i in 1:10) {
  print(ganador(lista_Baloto))
}

# Podra ver que cada vez que corre el codigo cambia el Jugador en el .txt
write.table(Multimillonario, file = "GanadorBaloto.txt", row.names = F)


## ---------------------------------------------------------------------
# Exportamos la base de datos:
base2 <- read.csv(file = "Chance.csv", header = T, sep = ",", dec = ".")

## Ahora sacamos los datos y los convertimos en vectores y dataframes para añadir a la lista:

base2 <- data.frame(base2)

## Volvemos en factor las columnas a continuación:
base2$Chance <- as.factor(base2$Chance)

base2$Loteria <- as.factor(base2$Loteria)

Loterias <- as.vector(levels(base2$Loteria))
Cifras <- as.vector(levels(base2$Chance))

## Creamos la lista:
lista_Chance <- list(base2, Loterias, Cifras)

## Ahora creamos la función que nos saque el precio de los ganadores del chance:

ganancia <- function(x){
  options(scipen=999)
  "Calcula Cuanto gano o pudo a ver ganado, las perdidas (Relativas) seran en negativo"
  profit <- function(){
    one <- c()
    for (i in x[[1]][[5]]){
      if(i == TRUE){
        one <- c(one, 1)
      }
      if(i == FALSE){
        one <- c(one, -1)
      }
    }
    return(one)
  }
  cifra <- function(){
    "Calcula cuanto ganaria por peso apostado"
    g <- x[[1]][[3]] * profit()
    figure <- c()
    for (i in as.numeric(x[[1]][[4]])) {
      if(i == 1){
        figure <- c(figure, 5)
      }
      if(i == 2){
        figure <- c(figure, 50)
      }
      if(i == 3){
        figure <- c(figure, 400)
      }
      if(i == 4){
        figure <- c(figure, 4500)
      }
    }
    return(figure*g)
  }
  gain <- paste("La persona: ", x[[1]][[1]], "| Jugando en la loteria: ", x[[1]][[2]], "| Gano o Perdio (relativo): ", cifra())
  return(gain)
}

gain <- ganancia(lista_Chance)

## Exportamos:
write.table(gain, file = "ResultadosRelativos.txt", row.names = F)

