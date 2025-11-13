# test codice

bianco <- matrix(data = 1, nrow = 3, ncol = 3)
arancione <- matrix(data = 2, nrow = 3, ncol = 3)
rosso <- matrix(data = 5, nrow = 3, ncol = 3)
verde <- matrix(data = 3, nrow = 3, ncol = 3)
blu <- matrix(data = 4, nrow = 3, ncol = 3)
giallo <- matrix(data = 6, nrow = 3, ncol = 3)

colori <- c("1"="white","2"="orange","3"="green","4"="blue","5"="red","6"="yellow","7"="black")

bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)

facce <- function(){
  print(bianco) 
  print(arancione) 
  print(verde)
  print(blu)
  print(rosso) 
  print(giallo) 
}
facce()

# THE 12 POSSIBLE MOVES
{
  mossa1 <- function(bianco, blu, giallo, verde, arancione, rosso) {
    tris <- rep(NA,3)
    tris <- bianco[,3]
    bianco[,3] <- blu[,3]
    blu[,3] <- giallo[,3]
    giallo[,3] <- verde[,3]
    verde[,3] <- tris
    aran1 <- arancione[1,1]
    arancione[1,1] <- arancione[3,1]
    arancione[3,1] <- arancione[3,3]
    arancione[3,3] <- arancione[1,3]
    arancione[1,3] <- aran1
    aran2 <- arancione[1,2]
    arancione[1,2] <- arancione[2,1]
    arancione[2,1] <- arancione[3,2]
    arancione[3,2] <- arancione[2,3]
    arancione[2,3] <- aran2
    
    return(list(bianco = bianco, blu = blu, giallo = giallo, verde = verde, arancione = arancione, rosso = rosso))
  }
  
  risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
  bianco <- risultato1$bianco
  blu <- risultato1$blu
  giallo <- risultato1$giallo
  verde <- risultato1$verde
  arancione <- risultato1$arancione
  rosso <- risultato1$rosso
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
  
} #1
{
  mossa2 <- function(bianco, blu, giallo, verde, rosso, arancione) {
    tris <- rep(NA,3)
    tris <- bianco[,1]
    bianco[,1] <- blu[,1]
    blu[,1] <- giallo[,1]
    giallo[,1] <- verde[,1]
    verde[,1] <- tris
    ros1 <- rosso[1,1]
    rosso[1,1] <- rosso[1,3]
    rosso[1,3] <- rosso[3,3]
    rosso[3,3] <- rosso[3,1]
    rosso[3,1] <- ros1
    ros2 <- rosso[1,2]
    rosso[1,2] <- rosso[2,3]
    rosso[2,3] <- rosso[3,2]
    rosso[3,2] <- rosso[2,1]
    rosso[2,1] <- ros2
    
    return(list(bianco = bianco, blu = blu, giallo = giallo, verde = verde, rosso = rosso, arancione = arancione))
  }
  
  risultato2 <- mossa2(bianco, blu, giallo, verde, rosso, arancione)
  bianco <- risultato2$bianco
  blu <- risultato2$blu
  giallo <- risultato2$giallo
  verde <- risultato2$verde
  rosso <- risultato2$rosso
  arancione <- risultato2$arancione
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #2
{
  mossa3 <- function(bianco, arancione, giallo, verde, rosso, blu) {
    tris <- rep(NA,3)
    tris <- bianco[1,]
    bianco[1,] <- rosso[1,]
    rosso[1,3] <- giallo[3,1]
    rosso[1,2] <- giallo[3,2]
    rosso[1,1] <- giallo[3,3]
    giallo[3,1] <- arancione[1,3]
    giallo[3,2] <- arancione[1,2]
    giallo[3,3] <- arancione[1,1]
    arancione[1,] <- tris
    ver1 <- verde[1,1]
    verde[1,1] <- verde[1,3]
    verde[1,3] <- verde[3,3]
    verde[3,3] <- verde[3,1]
    verde[3,1] <- ver1
    ver2 <- verde[1,2]
    verde[1,2] <- verde[2,3]
    verde[2,3] <- verde[3,2]
    verde[3,2] <- verde[2,1]
    verde[2,1] <- ver2
    
    return(list(bianco = bianco, arancione = arancione, giallo = giallo, verde = verde, rosso = rosso, blu = blu))
  }
  
  risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
  bianco <- risultato3$bianco
  arancione <- risultato3$arancione
  giallo <- risultato3$giallo
  verde <- risultato3$verde
  rosso <- risultato3$rosso
  blu <- risultato3$blu
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #3
{
  mossa4 <- function(bianco, arancione, giallo, blu, rosso, verde) {
    tris <- rep(NA,3)
    tris <- bianco[3,]
    bianco[3,] <- rosso[3,]
    rosso[3,1] <- giallo[1,3]
    rosso[3,2] <- giallo[1,2]
    rosso[3,3] <- giallo[1,1]
    giallo[1,3] <- arancione[3,1]
    giallo[1,2] <- arancione[3,2]
    giallo[1,1] <- arancione[3,3]
    arancione[3,] <- tris
    blu1 <- blu[1,1]
    blu[1,1] <- blu[3,1]
    blu[3,1] <- blu[3,3]
    blu[3,3] <- blu[1,3]
    blu[1,3] <- blu1
    blu2 <- blu[1,2]
    blu[1,2] <- blu[2,1]
    blu[2,1] <- blu[3,2]
    blu[3,2] <- blu[2,3]
    blu[2,3] <- blu2
    
    return(list(bianco = bianco, arancione = arancione, giallo = giallo, blu = blu, rosso = rosso, verde = verde))
  }
  
  risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
  bianco <- risultato4$bianco
  arancione <- risultato4$arancione
  giallo <- risultato4$giallo
  blu <- risultato4$blu
  rosso <- risultato4$rosso
  verde <- risultato4$verde
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #4
{
  mossa5 <- function(bianco, arancione, verde, blu, rosso, giallo) {
    tris <- rep(NA,3)
    tris <- verde[3,]
    verde[3,3] <- rosso[1,3]
    verde[3,2] <- rosso[2,3]
    verde[3,1] <- rosso[3,3]
    rosso[,3] <- blu[1,]
    blu[1,1] <- arancione[3,1]
    blu[1,2] <- arancione[2,1]
    blu[1,3] <- arancione[1,1]
    arancione[,1] <- tris
    bia1 <- bianco[1,1]
    bianco[1,1] <- bianco[3,1]
    bianco[3,1] <- bianco[3,3]
    bianco[3,3] <- bianco[1,3]
    bianco[1,3] <- bia1
    bia2 <- bianco[1,2]
    bianco[1,2] <- bianco[2,1]
    bianco[2,1] <- bianco[3,2]
    bianco[3,2] <- bianco[2,3]
    bianco[2,3] <- bia2
    
    return(list(bianco = bianco, arancione = arancione, verde = verde, blu = blu, rosso = rosso, giallo = giallo))
  }
  
  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
  bianco <- risultato5$bianco
  arancione <- risultato5$arancione
  verde <- risultato5$verde
  blu <- risultato5$blu
  rosso <- risultato5$rosso
  giallo <- risultato5$giallo
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #5
{
  mossa6 <- function(giallo, arancione, verde, blu, rosso, bianco) {
    tris <- rep(NA,3)
    tris <- verde[1,]
    verde[1,3] <- rosso[1,1]
    verde[1,2] <- rosso[2,1]
    verde[1,1] <- rosso[3,1]
    rosso[,1] <- blu[3,]
    blu[3,1] <- arancione[3,3]
    blu[3,2] <- arancione[2,3]
    blu[3,3] <- arancione[1,3]
    arancione[,3] <- tris
    gia1 <- giallo[1,1]
    giallo[1,1] <- giallo[1,3]
    giallo[1,3] <- giallo[3,3]
    giallo[3,3] <- giallo[3,1]
    giallo[3,1] <- gia1
    gia2 <- giallo[1,2]
    giallo[1,2] <- giallo[2,3]
    giallo[2,3] <- giallo[3,2]
    giallo[3,2] <- giallo[2,1]
    giallo[2,1] <- gia2
    
    return(list(giallo = giallo, arancione = arancione, verde = verde, blu = blu, rosso = rosso, bianco = bianco))
  }
  
  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
  giallo <- risultato6$giallo
  arancione <- risultato6$arancione
  verde <- risultato6$verde
  blu <- risultato6$blu
  rosso <- risultato6$rosso
  bianco <- risultato6$bianco
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #6
{
  mossa7 <- function(giallo, arancione, verde, blu, rosso, bianco) {
    tris <- rep(NA,3)
    tris <- arancione[,3]
    arancione[1,3] <- blu[3,3]
    arancione[2,3] <- blu[3,2]
    arancione[3,3] <- blu[3,1]
    blu[3,] <- rosso[,1]
    rosso[1,1] <- verde[1,3]
    rosso[2,1] <- verde[1,2]
    rosso[3,1] <- verde[1,1]
    verde[1,] <- tris
    gia1 <- giallo[1,1]
    giallo[1,1] <- giallo[3,1]
    giallo[3,1] <- giallo[3,3]
    giallo[3,3] <- giallo[1,3]
    giallo[1,3] <- gia1
    gia2 <- giallo[1,2]
    giallo[1,2] <- giallo[2,1]
    giallo[2,1] <- giallo[3,2]
    giallo[3,2] <- giallo[2,3]
    giallo[2,3] <- gia2
    
    return(list(giallo = giallo, arancione = arancione, verde = verde, blu = blu, rosso = rosso, bianco = bianco))
  }
  
  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
  giallo <- risultato7$giallo
  arancione <- risultato7$arancione
  verde <- risultato7$verde
  blu <- risultato7$blu
  rosso <- risultato7$rosso
  bianco <- risultato7$bianco
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #7
{
  mossa8 <- function(bianco, arancione, verde, blu, rosso, giallo) {
    tris <- rep(NA,3)
    tris <- arancione[,1]
    arancione[1,1] <- blu[1,3]
    arancione[2,1] <- blu[1,2]
    arancione[3,1] <- blu[1,1]
    blu[1,] <- rosso[,3]
    rosso[1,3] <- verde[3,3]
    rosso[2,3] <- verde[3,2]
    rosso[3,3] <- verde[3,1]
    verde[3,] <- tris
    bia1 <- bianco[1,1]
    bianco[1,1] <- bianco[1,3]
    bianco[1,3] <- bianco[3,3]
    bianco[3,3] <- bianco[3,1]
    bianco[3,1] <- bia1
    bia2 <- bianco[1,2]
    bianco[1,2] <- bianco[2,3]
    bianco[2,3] <- bianco[3,2]
    bianco[3,2] <- bianco[2,1]
    bianco[2,1] <- bia2
    
    return(list(bianco = bianco, arancione = arancione, verde = verde, blu = blu, rosso = rosso, giallo = giallo))
  }
  
  risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
  bianco <- risultato8$bianco
  arancione <- risultato8$arancione
  verde <- risultato8$verde
  blu <- risultato8$blu
  rosso <- risultato8$rosso
  giallo <- risultato8$giallo
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #8
{
  mossa9 <- function(bianco, arancione, giallo, blu, rosso, verde) {
    tris <- rep(NA,3)
    tris <- bianco[3,]
    bianco[3,] <- arancione[3,]
    arancione[3,1] <- giallo[1,3]
    arancione[3,2] <- giallo[1,2]
    arancione[3,3] <- giallo[1,1]
    giallo[1,3] <- rosso[3,1]
    giallo[1,2] <- rosso[3,2]
    giallo[1,1] <- rosso[3,3]
    rosso[3,] <- tris
    blu1 <- blu[1,1]
    blu[1,1] <- blu[1,3]
    blu[1,3] <- blu[3,3]
    blu[3,3] <- blu[3,1]
    blu[3,1] <- blu1
    blu2 <- blu[1,2]
    blu[1,2] <- blu[2,3]
    blu[2,3] <- blu[3,2]
    blu[3,2] <- blu[2,1]
    blu[2,1] <- blu2
    
    return(list(bianco = bianco, arancione = arancione, giallo = giallo, blu = blu, rosso = rosso, verde = verde))
  }
  
  risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
  bianco <- risultato9$bianco
  arancione <- risultato9$arancione
  giallo <- risultato9$giallo
  blu <- risultato9$blu
  rosso <- risultato9$rosso
  verde <- risultato9$verde
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #9
{
  mossa10 <- function(bianco, arancione, giallo, verde, rosso, blu) {
    tris <- rep(NA,3)
    tris <- bianco[1,]
    bianco[1,] <- arancione[1,]
    arancione[1,1] <- giallo[3,3]
    arancione[1,2] <- giallo[3,2]
    arancione[1,3] <- giallo[3,1]
    giallo[3,3] <- rosso[1,1]
    giallo[3,2] <- rosso[1,2]
    giallo[3,1] <- rosso[1,3]
    rosso[1,] <- tris
    ver1 <- verde[1,1]
    verde[1,1] <- verde[3,1]
    verde[3,1] <- verde[3,3]
    verde[3,3] <- verde[1,3]
    verde[1,3] <- ver1
    ver2 <- verde[1,2]
    verde[1,2] <- verde[2,1]
    verde[2,1] <- verde[3,2]
    verde[3,2] <- verde[2,3]
    verde[2,3] <- ver2
    
    return(list(bianco = bianco, arancione = arancione, giallo = giallo, verde = verde, rosso = rosso, blu = blu))
  }
  
  risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
  bianco <- risultato10$bianco
  arancione <- risultato10$arancione
  giallo <- risultato10$giallo
  verde <- risultato10$verde
  rosso <- risultato10$rosso
  blu <- risultato10$blu
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #10
{
  mossa11 <- function(bianco, blu, giallo, verde, rosso, arancione) {
    tris <- rep(NA,3)
    tris <- bianco[,1]
    bianco[,1] <- verde[,1]
    verde[,1] <- giallo[,1]
    giallo[,1] <- blu[,1]
    blu[,1] <- tris
    ros1 <- rosso[1,1]
    rosso[1,1] <- rosso[3,1]
    rosso[3,1] <- rosso[3,3]
    rosso[3,3] <- rosso[1,3]
    rosso[1,3] <- ros1
    ros2 <- rosso[1,2]
    rosso[1,2] <- rosso[2,1]
    rosso[2,1] <- rosso[3,2]
    rosso[3,2] <- rosso[2,3]
    rosso[2,3] <- ros2
    
    return(list(bianco = bianco, blu = blu, giallo = giallo, verde = verde, rosso = rosso, arancione = arancione))
  }
  
  risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
  bianco <- risultato11$bianco
  blu <- risultato11$blu
  giallo <- risultato11$giallo
  verde <- risultato11$verde
  rosso <- risultato11$rosso
  arancione <- risultato11$arancione
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #11
{
  mossa12 <- function(bianco, blu, giallo, verde, arancione, rosso) {
    tris <- rep(NA,3)
    tris <- bianco[,3]
    bianco[,3] <- verde[,3]
    verde[,3] <- giallo[,3]
    giallo[,3] <- blu[,3]
    blu[,3] <- tris
    aran1 <- arancione[1,1]
    arancione[1,1] <- arancione[1,3]
    arancione[1,3] <- arancione[3,3]
    arancione[3,3] <- arancione[3,1]
    arancione[3,1] <- aran1
    aran2 <- arancione[1,2]
    arancione[1,2] <- arancione[2,3]
    arancione[2,3] <- arancione[3,2]
    arancione[3,2] <- arancione[2,1]
    arancione[2,1] <- aran2
    
    return(list(bianco = bianco, blu = blu, giallo = giallo, verde = verde, arancione = arancione, rosso = rosso))
  }
  
  risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
  bianco <- risultato12$bianco
  blu <- risultato12$blu
  giallo <- risultato12$giallo
  verde <- risultato12$verde
  arancione <- risultato12$arancione
  rosso <- risultato12$rosso
  
  bianco2 <- matrix(colori[as.character(bianco)], nrow = 3, ncol = 3)
  arancione2 <- matrix(colori[as.character(arancione)], nrow = 3, ncol = 3)
  verde2 <- matrix(colori[as.character(verde)], nrow = 3, ncol = 3)
  blu2 <- matrix(colori[as.character(blu)], nrow = 3, ncol = 3)
  rosso2 <- matrix(colori[as.character(rosso)], nrow = 3, ncol = 3)
  giallo2 <- matrix(colori[as.character(giallo)], nrow = 3, ncol = 3)
  
} #12


facce()


# obiettivo:
# fare una funzione in modo da poter far eseguire una mossa al cubo senza doverla per forza
# cliccare noi



bianco
risultato1

for (i in 1:6) {
  mossa1(bianco, blu, giallo, verde, arancione,rosso)
  risultato1 <- mossa1(bianco, blu, giallo, verde, arancione,rosso)
  bianco <- risultato1$bianco
  blu <- risultato1$blu
  giallo <- risultato1$giallo
  verde <- risultato1$verde
  arancione <- risultato1$arancione
  
}
risultato1


# ok se viene scritto in questo modo sembra funzionare



# proviamo ora a trasformarlo nel metodo di ricerca intelligente

# step 1: fare in modo che si possa dare un valore allo stato del cubo
# creiamo un cubo standard da riferimento

bianco_0 <- matrix(data = 1, nrow = 3, ncol = 3)
arancione_0 <- matrix(data = 2, nrow = 3, ncol = 3)
rosso_0 <- matrix(data = 5, nrow = 3, ncol = 3)
verde_0 <- matrix(data = 3, nrow = 3, ncol = 3)
blu_0 <- matrix(data = 4, nrow = 3, ncol = 3)
giallo_0 <- matrix(data = 6, nrow = 3, ncol = 3)

# queste saranno 6 facce che faranno da riferimento


valore_attuale_f <- function(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                             blu,blu_0,rosso,rosso_0,giallo,giallo_0){
                           sum(sum(bianco==bianco_0),
                           sum(arancione==arancione_0),
                           sum(verde==verde_0),
                           sum(blu==blu_0),
                           sum(rosso==rosso_0),
                           sum(giallo==giallo_0))-6
}

valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)


# prova
for (k in 1:2) {
  numero <- sample(1:12,1)
  if (numero == 1){
    mossa1(bianco, blu, giallo, verde, arancione, rosso)
    risultato1 <- mossa1(bianco, blu, giallo, verde, arancione,rosso)
    bianco <- risultato1$bianco
    blu <- risultato1$blu
    giallo <- risultato1$giallo
    verde <- risultato1$verde
    arancione <- risultato1$arancione
    rosso <- risultato1$rosso
  } else {
    if (numero == 2) {
      mossa2(bianco, blu, giallo, verde, rosso, arancione)
      risultato2 <- mossa2(bianco, blu, giallo, verde, rosso, arancione)
      bianco <- risultato2$bianco
      blu <- risultato2$blu
      giallo <- risultato2$giallo
      verde <- risultato2$verde
      rosso <- risultato2$rosso
      arancione <- risultato2$arancione
    } else {
      if (numero == 3) {
        mossa3(bianco, arancione, giallo, verde, rosso, blu)
        risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso,blu)
        bianco <- risultato3$bianco
        arancione <- risultato3$arancione
        giallo <- risultato3$giallo
        verde <- risultato3$verde
        rosso <- risultato3$rosso
        blu <- risultato3$blu
      } else {
        if (numero == 4){
          mossa4(bianco, arancione, giallo, blu, rosso, verde)
          risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
          bianco <- risultato4$bianco
          arancione <- risultato4$arancione
          giallo <- risultato4$giallo
          blu <- risultato4$blu
          rosso <- risultato4$rosso
          verde <- risultato4$verde
        } else {
          if (numero == 5){
            mossa5(bianco, arancione, verde, blu, rosso, giallo)
            risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
            bianco <- risultato5$bianco
            arancione <- risultato5$arancione
            verde <- risultato5$verde
            blu <- risultato5$blu
            rosso <- risultato5$rosso
            giallo <- risultato5$giallo
          } else {
            if (numero == 6){
              mossa6(giallo, arancione, verde, blu, rosso, bianco)
              risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
              giallo <- risultato6$giallo
              arancione <- risultato6$arancione
              verde <- risultato6$verde
              blu <- risultato6$blu
              rosso <- risultato6$rosso
              bianco <- risultato6$bianco
            } else {
              if (numero == 7){
                mossa7(giallo, arancione, verde, blu, rosso, bianco)
                risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato7$giallo
                arancione <- risultato7$arancione
                verde <- risultato7$verde
                blu <- risultato7$blu
                rosso <- risultato7$rosso
                bianco <- risultato7$bianco
              } else {
                if (numero == 8){
                  mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  bianco <- risultato8$bianco
                  arancione <- risultato8$arancione
                  verde <- risultato8$verde
                  blu <- risultato8$blu
                  rosso <- risultato8$rosso
                  giallo <- risultato8$giallo
                } else {
                  if (numero == 9){
                    mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    bianco <- risultato9$bianco
                    arancione <- risultato9$arancione
                    giallo <- risultato9$giallo
                    blu <- risultato9$blu
                    rosso <- risultato9$rosso
                    verde <- risultato9$verde
                  } else {
                    if (numero == 10){
                      mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      bianco <- risultato10$bianco
                      arancione <- risultato10$arancione
                      giallo <- risultato10$giallo
                      verde <- risultato10$verde
                      rosso <- risultato10$rosso
                      blu <- risultato10$blu
                    } else {
                      if (numero == 11){
                        mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        bianco <- risultato11$bianco
                        blu <- risultato11$blu
                        giallo <- risultato11$giallo
                        verde <- risultato11$verde
                        rosso <- risultato11$rosso
                        arancione <- risultato11$arancione
                      } else {
                        if (numero == 12){
                          mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          bianco <- risultato12$bianco
                          blu <- risultato12$blu
                          giallo <- risultato12$giallo
                          verde <- risultato12$verde
                          arancione <- risultato12$arancione
                          rosso <- risultato12$rosso
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
}
numero
facce()
valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)


## OTTENERE VALORI CUBI
n_sequenze <- 20
sequenza_valori_1 <- rep(NA,50000)

for (j in 1:50000) {
  bianco <- matrix(data = 1, nrow = 3, ncol = 3)
  arancione <- matrix(data = 2, nrow = 3, ncol = 3)
  rosso <- matrix(data = 5, nrow = 3, ncol = 3)
  verde <- matrix(data = 3, nrow = 3, ncol = 3)
  blu <- matrix(data = 4, nrow = 3, ncol = 3)
  giallo <- matrix(data = 6, nrow = 3, ncol = 3)
    for (i in 1:n_sequenze) {
      numero <- sample(1:12,1)
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione,rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso, arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  valore_attuale <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                     blu,blu_0,rosso,rosso_0,giallo,giallo_0)
  
  sequenza_valori_1[j] <- valore_attuale
}

sequenza_valori_1
hist(sequenza_valori_1)
facce()
mean(sequenza_valori_1)
max(sequenza_valori_1)


plot(sequenza_valori_1, type = "l", col = "blue", lwd = 2,
     main = "1000 random moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)
qqnorm(sequenza_valori_1)
qqline(sequenza_valori_1, col = "red") 


hist(sequenza_valori_1, probability = TRUE, col = "lightblue", breaks = 30, 
     main = "Istogramma orizzontale con distribuzione Normale", 
     xlab = "Frequenze", ylab = "Valori", horiz = TRUE)
mean(sequenza_valori_1)
sd(sequenza_valori_1)

curve(dnorm(x, mean = mean(sequenza_valori_1), sd = sd(sequenza_valori_1)), 
      col = "red", lwd = 2, add = TRUE)
probab <- 1-pnorm(35,mean = mean(sequenza_valori_1), sd=sd(sequenza_valori_1))
probab

# METODO 1
# the first method will just consist in a random starting position
# I'll make the code to do random positions and take count of the different values 
# that the cube gets
sequenza_valori_1 <- rep(NA,100000)
sequenza_valori_1
valore_max_tot <- 0
valore_attuale <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                   blu,blu_0,rosso,rosso_0,giallo,giallo_0)
valore_attuale
n_sequenze <- 100000
for (i in 1:n_sequenze) {
  numero <- sample(1:12,1)
  if (numero == 1){
    mossa1(bianco, blu, giallo, verde, arancione, rosso)
    risultato1 <- mossa1(bianco, blu, giallo, verde, arancione,rosso)
    bianco <- risultato1$bianco
    blu <- risultato1$blu
    giallo <- risultato1$giallo
    verde <- risultato1$verde
    arancione <- risultato1$arancione
    rosso <- risultato1$rosso
  } else {
    if (numero == 2) {
      mossa2(bianco, blu, giallo, verde, rosso, arancione)
      risultato2 <- mossa2(bianco, blu, giallo, verde, rosso, arancione)
      bianco <- risultato2$bianco
      blu <- risultato2$blu
      giallo <- risultato2$giallo
      verde <- risultato2$verde
      rosso <- risultato2$rosso
      arancione <- risultato2$arancione
    } else {
      if (numero == 3) {
        mossa3(bianco, arancione, giallo, verde, rosso, blu)
        risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
        bianco <- risultato3$bianco
        arancione <- risultato3$arancione
        giallo <- risultato3$giallo
        verde <- risultato3$verde
        rosso <- risultato3$rosso
        blu <- risultato3$blu
      } else {
        if (numero == 4){
          mossa4(bianco, arancione, giallo, blu, rosso, verde)
          risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
          bianco <- risultato4$bianco
          arancione <- risultato4$arancione
          giallo <- risultato4$giallo
          blu <- risultato4$blu
          rosso <- risultato4$rosso
          verde <- risultato4$verde
        } else {
          if (numero == 5){
            mossa5(bianco, arancione, verde, blu, rosso, giallo)
            risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
            bianco <- risultato5$bianco
            arancione <- risultato5$arancione
            verde <- risultato5$verde
            blu <- risultato5$blu
            rosso <- risultato5$rosso
            giallo <- risultato5$giallo
          } else {
            if (numero == 6){
              mossa6(giallo, arancione, verde, blu, rosso, bianco)
              risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
              giallo <- risultato6$giallo
              arancione <- risultato6$arancione
              verde <- risultato6$verde
              blu <- risultato6$blu
              rosso <- risultato6$rosso
              bianco <- risultato6$bianco
            } else {
              if (numero == 7){
                mossa7(giallo, arancione, verde, blu, rosso, bianco)
                risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato7$giallo
                arancione <- risultato7$arancione
                verde <- risultato7$verde
                blu <- risultato7$blu
                rosso <- risultato7$rosso
                bianco <- risultato7$bianco
              } else {
                if (numero == 8){
                  mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  bianco <- risultato8$bianco
                  arancione <- risultato8$arancione
                  verde <- risultato8$verde
                  blu <- risultato8$blu
                  rosso <- risultato8$rosso
                  giallo <- risultato8$giallo
                } else {
                  if (numero == 9){
                    mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    bianco <- risultato9$bianco
                    arancione <- risultato9$arancione
                    giallo <- risultato9$giallo
                    blu <- risultato9$blu
                    rosso <- risultato9$rosso
                    verde <- risultato9$verde
                  } else {
                    if (numero == 10){
                      mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      bianco <- risultato10$bianco
                      arancione <- risultato10$arancione
                      giallo <- risultato10$giallo
                      verde <- risultato10$verde
                      rosso <- risultato10$rosso
                      blu <- risultato10$blu
                    } else {
                      if (numero == 11){
                        mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        bianco <- risultato11$bianco
                        blu <- risultato11$blu
                        giallo <- risultato11$giallo
                        verde <- risultato11$verde
                        rosso <- risultato11$rosso
                        arancione <- risultato11$arancione
                      } else {
                        if (numero == 12){
                          mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          bianco <- risultato12$bianco
                          blu <- risultato12$blu
                          giallo <- risultato12$giallo
                          verde <- risultato12$verde
                          arancione <- risultato12$arancione
                          rosso <- risultato12$rosso
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  valore_attuale <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                    blu,blu_0,rosso,rosso_0,giallo,giallo_0)
  
  sequenza_valori_1[i] <- valore_attuale
}
sequenza_valori_1
hist(sequenza_valori_1)
facce()
mean(sequenza_valori_1)
max(sequenza_valori_1)


plot(sequenza_valori_1, type = "l", col = "blue", lwd = 2,
     main = "1000 random moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

hist(sequenza_valori_1, main = "1000 random moves", col = "blue", xlab = "Values")


qqnorm(sequenza_valori_1)
qqline(sequenza_valori_1, col = "red") 


hist(sequenza_valori_1, probability = TRUE, col = "lightblue", breaks = 30, 
     main = "Istogramma orizzontale con distribuzione Normale", 
     xlab = "Frequenze", ylab = "Valori", horiz = TRUE)
mean(sequenza_valori_1)
sd(sequenza_valori_1)
curve(dnorm(x, mean = mean(sequenza_valori_1), sd = sd(sequenza_valori_1)), 
      col = "red", lwd = 2, add = TRUE)
probab <- 1-pnorm(26,mean = mean(sequenza_valori_1), sd=sd(sequenza_valori_1))
probab
# the first method is done, how we can see even after a large number of moves, not even a
# single move is close to the resolution, in fact we can see from the different plots that
# the highest value (in the first trial, with 1000 moves) the maximum value was 18, and the 
# average value was 8.347


# let's try now the second method
# this time I'm going to continue with random moves but the move will be accepted only if the 
# current value is higher than the max value
facce()
risultato1
risultato1 <- mossa1(bianco, blu, giallo, verde, arancione,rosso)
valore_attuale
mossa1(bianco, blu, giallo, verde, arancione,rosso)
n_sequenze <- 1000
facce_max <- 0 
facce_max <- risultato1
facce_max
stato_attuale <- 0
sequenza_valori_1 <- rep(NA,1000)
sequenza_valori_1
sequenza_valori_max_1 <- rep(NA,1000)
valore_max_tot <- 0
for (i in 1:n_sequenze) {
  numero <- sample(1:12,1)
  if (numero == 1){
    mossa1(bianco, blu, giallo, verde, arancione, rosso)
    risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
    stato_attuale<-risultato1
  } else {
    if (numero == 2) {
      mossa2(bianco, blu, giallo, verde, rosso, arancione)
      risultato2 <- mossa2(bianco, blu, giallo, verde, rosso, arancione)
      stato_attuale<-risultato2
    } else {
      if (numero == 3) {
        mossa3(bianco, arancione, giallo, verde, rosso, blu)
        risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
        stato_attuale<-risultato3
      } else {
        if (numero == 4){
          mossa4(bianco, arancione, giallo, blu, rosso, verde)
          risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
          stato_attuale<-risultato4
        } else {
          if (numero == 5){
            mossa5(bianco, arancione, verde, blu, rosso, giallo)
            risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
            stato_attuale<-risultato5
          } else {
            if (numero == 6){
              mossa6(giallo, arancione, verde, blu, rosso, bianco)
              risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
              stato_attuale<-risultato6
            } else {
              if (numero == 7){
                mossa7(giallo, arancione, verde, blu, rosso, bianco)
                risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                stato_attuale<-risultato7
              } else {
                if (numero == 8){
                  mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  stato_attuale<-risultato8
                } else {
                  if (numero == 9){
                    mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    stato_attuale<-risultato9
                  } else {
                    if (numero == 10){
                      mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      stato_attuale<-risultato10
                    } else {
                      if (numero == 11){
                        mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        stato_attuale<-risultato11
                      } else {
                        if (numero == 12){
                          mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          stato_attuale<-risultato12
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                     stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
  
  sequenza_valori_1[i] <- valore_attuale
  if (valore_attuale >= valore_max_tot) {
    valore_max_tot <- valore_attuale
    bianco <- stato_attuale$bianco
    arancione <- stato_attuale$arancione
    verde <- stato_attuale$verde
    blu <- stato_attuale$blu
    rosso <- stato_attuale$rosso
    giallo <- stato_attuale$giallo
  }
  sequenza_valori_max_1[i] <- valore_max_tot
}
  

numero
valore_attuale
stato_attuale


valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)
facce()
valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                 stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)

sequenza_valori_1
sequenza_valori_max_1
valore_max_tot

plot(sequenza_valori_1, type = "l", col = "blue", lwd = 2,
     main = "1000 random moves taking only higher values",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

plot(sequenza_valori_max_1, type = "l", col = "blue", lwd = 2,
     main = "Highest value in 1000 random moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

hist(sequenza_valori_1)
hist(sequenza_valori_max_1)
mean(sequenza_valori_1)
mean(sequenza_valori_max_1)

# from this method we can clearly see that the state of the cube is much faster at improve its value
# but at the same time just after a little amount of moves the state converges: In fact, it reaches a
# point where each of the 12 possible moves would decrease its value, so it stops there.
# Moreover, notice that with this method the mean of the values is higher that the one of method 1,
# but the maximum value is much lower (12 against 18 with 1000 moves)

# METODO 3
# In order to get rid of this problem we could try to improve the number of moves before each valuation
# In other words, with method 2 we just do 1 random move and if the new value is higher then we change 
# the state of the cube. Now we are letting the cube do a sequence of moves before each valuation.
# Let's start with just 2 moves
i<-0
k<-0
n_sequenze <- 1000
valore_attuale <- 0
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
valore_max_tot <- 0
sequenza_valori_2 <- rep(NA,1000)
sequenza_valori_max_2 <- rep(NA,1000)
for (i in 1:n_sequenze) {
  for (k in 1:2) {
    numero <- sample(1:12,1)
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
      stato_attuale <- risultato1
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
        stato_attuale <- risultato2
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
          stato_attuale <- risultato3
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
            stato_attuale <- risultato4
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
              stato_attuale <- risultato5
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
                stato_attuale <- risultato6
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                  stato_attuale <- risultato7
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                    stato_attuale <- risultato8
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                      stato_attuale <- risultato9
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                        stato_attuale <- risultato10
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                          stato_attuale <- risultato11
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                            stato_attuale <- risultato12
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                     stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
  
  sequenza_valori_2[i] <- valore_attuale
  if (valore_attuale >= valore_max_tot) {
    valore_max_tot <- valore_attuale
    base_bianco <- stato_attuale$bianco
    base_arancione <- stato_attuale$arancione
    base_verde <-  stato_attuale$verde
    base_blu <- stato_attuale$blu
    base_rosso <- stato_attuale$rosso
    base_giallo <- stato_attuale$giallo
  } else {
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
  }
  sequenza_valori_max_2[i] <- valore_max_tot
}


valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)
facce()
valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                 stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)

sequenza_valori_2
sequenza_valori_max_2

plot(sequenza_valori_2, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 2 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

plot(sequenza_valori_max_2, type = "l", col = "blue", lwd = 2,
     main = "Highest values with a sequence of 2 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

hist(sequenza_valori_2)
hist(sequenza_valori_max_2)
mean(sequenza_valori_2)
mean(sequenza_valori_max_2)

# This method is clearly better than the second method but it still converges to a value
# pretty low (in the example 17). What happens if we increase further the number of the sequence?

# Let's try with 3 moves

i<-0
k<-0
n_sequenze <- 10000
valore_attuale <- 0
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
valore_max_tot <-0
sequenza_valori_3 <- rep(NA,10000)
sequenza_valori_max_3 <- rep(NA,10000)
for (i in 1:n_sequenze) {
  for (k in 1:3) {
    numero <- sample(1:12,1)
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
      stato_attuale <- risultato1
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
        stato_attuale <- risultato2
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
          stato_attuale <- risultato3
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
            stato_attuale <- risultato4
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
              stato_attuale <- risultato5
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
                stato_attuale <- risultato6
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                  stato_attuale <- risultato7
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                    stato_attuale <- risultato8
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                      stato_attuale <- risultato9
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                        stato_attuale <- risultato10
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                          stato_attuale <- risultato11
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                            stato_attuale <- risultato12
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                     stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
  
  sequenza_valori_3[i] <- valore_attuale
  if (valore_attuale > valore_max_tot) {
    valore_max_tot <- valore_attuale
    base_bianco <- stato_attuale$bianco
    base_arancione <- stato_attuale$arancione
    base_verde <-  stato_attuale$verde
    base_blu <- stato_attuale$blu
    base_rosso <- stato_attuale$rosso
    base_giallo <- stato_attuale$giallo
  } else {
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
  }
  sequenza_valori_max_3[i] <- valore_max_tot
}


valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)
facce()
valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                 stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)

sequenza_valori_3
sequenza_valori_max_3

plot(sequenza_valori_3, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 3 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

plot(sequenza_valori_max_3, type = "l", col = "blue", lwd = 2,
     main = "Highest values with a sequence of 3 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

facce()

hist(sequenza_valori_3)
hist(sequenza_valori_max_3)
mean(sequenza_valori_3)
mean(sequenza_valori_max_3)

# This is much better! For the first time we reached half of the maximum value (24). An interesting thing 
# is that with this method we have 12^3 (=1728) possible combinations each time: that means that even with 
# a thousand random sequence of moves we are not certain to have reached a converges because we have not
# explored all the possible paths. The question to answer now is: is that possible to solve the cube with 
# a sequence of 3 random moves? (we can see on the graph that even after hundreds of sequences where the 
# maximum value remain the same, it suddenly changes and improve)
# To try to give an answer let's try to increase the number of sequences from 1000 to 100000

valore_max_tot
# Actually the maximum value increased again but just by one unit (maximum value 25) and then it stopped
# for more than 99000 sequences. This shows that with an high probability it converges to maximum value 
# and cannot improve further.
# Let's try with sequences of 4 moves



i<-0
k<-0
n_sequenze <- 10000
valore_attuale <- 0
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
sequenza_valori_4 <- rep(NA,10000)
sequenza_valori_max_4 <- rep(NA,10000)
valore_max_tot <-0
for (i in 1:n_sequenze) {
  for (k in 1:4) {
    numero <- sample(1:12,1)
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
      stato_attuale <- risultato1
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
        stato_attuale <- risultato2
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
          stato_attuale <- risultato3
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
            stato_attuale <- risultato4
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
              stato_attuale <- risultato5
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
                stato_attuale <- risultato6
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                  stato_attuale <- risultato7
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                    stato_attuale <- risultato8
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                      stato_attuale <- risultato9
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                        stato_attuale <- risultato10
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                          stato_attuale <- risultato11
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                            stato_attuale <- risultato12
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                     stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
  
  sequenza_valori_4[i] <- valore_attuale
  if (valore_attuale > valore_max_tot) {
    valore_max_tot <- valore_attuale
    base_bianco <- stato_attuale$bianco
    base_arancione <- stato_attuale$arancione
    base_verde <-  stato_attuale$verde
    base_blu <- stato_attuale$blu
    base_rosso <- stato_attuale$rosso
    base_giallo <- stato_attuale$giallo
  } else {
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
  }
  sequenza_valori_max_4[i] <- valore_max_tot
}


valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)
facce()
valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                 stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)

sequenza_valori_4
sequenza_valori_max_4[9999]

plot(sequenza_valori_4, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 4 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

plot(sequenza_valori_max_4, type = "l", col = "blue", lwd = 2,
     main = "Highest values with a sequence of 4 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

hist(sequenza_valori_4)
hist(sequenza_valori_max_4)
mean(sequenza_valori_4)
mean(sequenza_valori_max_4)

# The result is pretty much the same, let's try if it can overcome the convergence with
# 100000 sequences
# It looks that  it still converges


# let's try 5 moves for sequence


i<-0
k<-0
n_sequenze <- 100000
valore_attuale <- 0
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
sequenza_valori_5 <- rep(NA,100000)
sequenza_valori_max_5 <- rep(NA,100000)
valore_max_tot <-0
for (i in 1:n_sequenze) {
  for (k in 1:5) {
    numero <- sample(1:12,1)
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
      stato_attuale <- risultato1
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
        stato_attuale <- risultato2
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
          stato_attuale <- risultato3
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
            stato_attuale <- risultato4
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
              stato_attuale <- risultato5
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
                stato_attuale <- risultato6
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                  stato_attuale <- risultato7
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                    stato_attuale <- risultato8
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                      stato_attuale <- risultato9
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                        stato_attuale <- risultato10
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                          stato_attuale <- risultato11
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                            stato_attuale <- risultato12
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                     stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
  
  sequenza_valori_5[i] <- valore_attuale
  if (valore_attuale > valore_max_tot) {
    valore_max_tot <- valore_attuale
    base_bianco <- stato_attuale$bianco
    base_arancione <- stato_attuale$arancione
    base_verde <-  stato_attuale$verde
    base_blu <- stato_attuale$blu
    base_rosso <- stato_attuale$rosso
    base_giallo <- stato_attuale$giallo
  } else {
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
  }
  sequenza_valori_max_5[i] <- valore_max_tot
}


valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)
facce()
valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                 stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)

sequenza_valori_5
sequenza_valori_max_5[9999]

plot(sequenza_valori_5, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 5 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

plot(sequenza_valori_max_5, type = "l", col = "blue", lwd = 2,
     main = "Highest values with a sequence of 5 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)

hist(sequenza_valori_5)
hist(sequenza_valori_max_5)
mean(sequenza_valori_5)
mean(sequenza_valori_max_5)


# Ok so with the same method but using sequences of 5 number we discover something really interesting:
# apparently in the first 1000 sequences the max value is lower than the one with obtain when using 
# sequences with less moves (sequences with 4 was slower than the one with 3 and so on, the fastest was
# the sequences made by only 1 move). However, once the maximum value increase all the methods tends to 
# converge but the higher the number of moves of each sequence the higher the maximum value
# In the first trial we have:
# sequence of 1 <- maximum 12
# sequence of 2 <- maximum 18
# sequence of 3 <- maximum 24
# sequence of 4 <- maximum 25
# sequence of 5 <- maximum 31
# I did some more trials with sequences of 6 o more moves (until some trials with 10 moves per sequence) 
# and after over 2.000.000 sequences the maximum value is 32. This method is useful to go from 0
# to a discrete high value fast (if used correctly). A method that would increase the speed at  
# which the max value increases would be a mixture of sequences with different numbers of moves each:
# at the beginning is better to start with sequences made of 1 or 2 moves in order to increase faster.
# Why is that faster? Because with this method the moves are random so if we just do 1 or 2 moves the 
# the state of the cube won't change much and for this reason will be easier to obtain (slightly) higher
# values. At the same time the cube will converge very quickly and this will be the moment we increase
# the number of moves per sequence, starting from 3 until 5 or 6. I wouldn't suggest to use sequences made
# by more than 6 moves: the reason is that by doing this 7 or more random moves are more than enough to 
# completely change the state of the cube. This will be the same of returning to a start position (and how we said before 
# it's  almost impossible to solve it random)

# So is there a better method that can help us to obtain an higher value of the cube?













###############################################

# ACO METHOD #

###############################################

# to begin with, let's try to do ants that explore sequences of 2 moves and see what happens

n_formiche <- 50
mosse_1 <- c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),
             rep(7,12),rep(8,12),rep(9,12),rep(10,12),rep(11,12),rep(12,12))
mosse_1
mosse_2 <- rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12)
mosse_2
mosse_tot <- c(mosse_1,mosse_2)
mosse_tot
sequenza_mosse <- matrix(data = mosse_tot, ncol = 2)
feromoni <- rep(100,144)
feromoni
sequenza_mosse <- cbind(sequenza_mosse, feromoni)
sequenza_mosse
# now the sequence of 2 moves has the third column that represents the number of pheromones that will influence
# the choice of the 2 moves for the ant
# after each interaction I'll change the number of pheromones based on the results

feromoni_cumul <- cumsum(feromoni)
feromoni_cumul
sequenza_mosse <- cbind(sequenza_mosse, feromoni_cumul)
sequenza_mosse[,3] <- 100
sequenza_mosse


valore <- sample(1:sum(sequenza_mosse[,3]),1)

scelta <- which(feromoni_cumul>=valore)[1]
scelta
# with this code we should be able to find a sequence of 2 moves with randomness but based on the number of
# pheromones




i<-0
k<-0
n_sequenze <- 10000
valore_attuale <- 0
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
sequenza_valori_5 <- rep(NA,10000)
sequenza_valori_max_5 <- rep(NA,10000)
valore_max_tot <-0

scelta
#11 7
n_formiche <- 10
diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
diff_valori



sequenza_mosse <- cbind(c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),
                          rep(7,12),rep(8,12),rep(9,12),rep(10,12),rep(11,12),rep(12,12)),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12),
                        rep(100,144))
valore_base <- 14
n_iterazioni <- 10000
valori_aco1 <- rep(NA,n_iterazioni)
for (p in 1:n_iterazioni) {
  feromoni_cumul <- cumsum(sequenza_mosse[,3])
  valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                  base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
  valori_aco1[p] <- valore_base
  for (i in 1:n_formiche) {
    valore <- sample(1:sum(sequenza_mosse[,3]),1)
    scelta <- which(feromoni_cumul>=valore)[1]
    for (k in 1:2) {
      numero <- sequenza_mosse[scelta,k]
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_nuovo <-  valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      diff_valori[i,1] <- scelta
      diff_valori[i,2] <- valore_nuovo - valore_base
      # fine per ogni formica
      # calcolare la differenza in valore 
    }
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
    # fine per tutte le formiche
    # qua mettere i cambi di feromoni
  }
  for (j in 1:n_formiche) {
    sequenza_mosse[diff_valori[j,1],3] <- max(sequenza_mosse[diff_valori[j,1],3] + diff_valori[j,2],20)
  } # adesso bisogna effettuare la mossa con il maggior guadagno
  for (k in 1:2) {
    numero <- sequenza_mosse[diff_valori[which(diff_valori[,2]==max(diff_valori[,2]))[1],1],k]
    numero
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  base_bianco <- bianco
  base_arancione <- arancione
  base_verde <- verde
  base_blu <- blu
  base_rosso <- rosso
  base_giallo <- giallo
}
valori_aco_2mosse <- valori_aco1 # creato solo per salvare i risultati nel caso servissero in futuro
valori_aco1


# if more than 1 sequence has the same maximum value in difference of values, I'll take the first one 
# because the sequences are chosen with randomness so their order is also random and that means that 
# the sequence chosen is not chose with a deterministic decision


# cosa da tenere conto: se si prende due volte lo stesso valore (per quanto improbabile) il precorso verrà fatto
# 2 volte e i feromoni verranno cambiati doppiamente, andando così a creare errori di probabilità (per quanto piccoli)
# da risolvere

plot(valori_aco1, type = "l", col = "blue", lwd = 2,
     main = "ACO Algorithm with sequences of 2 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))
abline(h = 48, col = "red", lwd = 2, lty = 2)
sequenza_mosse

feromoni_cumul

mean(valori_aco1)
max(valori_aco1)

# prime impressione
# la prima versione del codice per l'ACO sembra funzionare bene, tuttavia ci sono alcune cose da dire. Prima tutto
# i risultati non sono così soprendenti, anche se sicuramente migliori del metodo random non sono ancora in grado 
# battere il secondo metodo. Con sequenze di due mosse il massimo raggiunto in 1000 mosse è stato 21, con una media
# di 9.85. E' sicuramente un punto di partenza però bisogna fare degli accorgimenti: sarebbe opportuno aggiungere 
# un'evaporazione dei feromoni nel tempo in modo da non sovraccaricare le mosse più usate (credo che un'evaporazione
# in percentuale sia l'idea migliore), per poterlo fare bisogna ricalibrare i feromoni di partenza e soprattutto il 
# modo con cui essi vengano aumentati o diminuiti dopo un certo percorso da parte di una formica. Anche il numero 
# di formiche da cui partire dovrebbe essere cambiato (sicuramente + di 10 perchè altrimenti non c'è abbastanza esplorazione
# ma allo stesso tempo non troppo per non sovraccaricare il calcolo computazionale). Bisogna quindi giocare con tutti
# questi parametri per cercare di trovare una soluzione ottimale. Infine, bisogna assolutamente aumenare il numero di 
# mosse per sequenza perchè secondo me 2 mosse non danno molta informazione nel cambio di feromoni. Questo aumento,
# però, richiederebbe ampliare esponenzialmente la matrice con le varie combinazione di mosse.
# In ogni caso lascerò questo codice sopra come base e lo modificherò sotto con diversi esperimenti.



# prima di tutto proviamo ad aumentare il numero di formiche (da 10 a 100)
# non va bene, con così tante formiche un sacco di feromoni sono arrivati a 0 (o in negativo)
# provo quindi a mettere un limite basso di 20 feromoni
# questa volta il valore ottenuto non è andato a convergere, il valore massimo raggiunto è 25 con una media di 12.365
# decisamente un grande miglioramento rispetto a prima, riuscendo persino a superare il metodo 2 (con sequenze
# di 2 mosse, dove il massimo era 23). Tuttavia il numero di calcoli sta già diventando troppo alto, se si vuole 
# aumentare il numero di mosse per sequenza bisogna ridurre il numero di formiche (io starei tra 20 e 50). Inoltre
# è vero che la media e il massimo sono aumentati, però il grafico mostra come i valori continuino ad oscillare senza
# che ci sia un aumento costante
# aggiornamento: tenere conto del fatto che prima c'era un errore (corretto solo dopo) nel codice con le sequenze
# a due mosse: in verità, ancora prima degli accorgimenti sul numero di feromoni minimo,con l'ACO si poteva 
# arrivare ad un valore di 25, per poi convergere dati i valori dei feromoni negativi non cambiati
# proverei ad aumentare a sequenze di 3 mosse


# SE SI TENGONO 10000 ITERAZIONI L'ALGORITMO POTREBBE RICHIEDERE PIU' TEMPO (25-30 secs), NEL CASO CONSIGLIO DI ABBASSARE LE ITERAZIONI (ES 5000 O 1000)
n_formiche <- 50
diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
diff_valori


mosse_per_sequenza <- 3
sequenza_mosse <- cbind(c(rep(1,12^(mosse_per_sequenza-1)),rep(2,12^(mosse_per_sequenza-1)),rep(3,12^(mosse_per_sequenza-1)),
                          rep(4,12^(mosse_per_sequenza-1)),rep(5,12^(mosse_per_sequenza-1)),rep(6,12^(mosse_per_sequenza-1)),
                          rep(7,12^(mosse_per_sequenza-1)),rep(8,12^(mosse_per_sequenza-1)),rep(9,12^(mosse_per_sequenza-1)),
                          rep(10,12^(mosse_per_sequenza-1)),rep(11,12^(mosse_per_sequenza-1)),rep(12,12^(mosse_per_sequenza-1))),
                          rep(c(rep(1,12^(mosse_per_sequenza-2)),rep(2,12^(mosse_per_sequenza-2)),rep(3,12^(mosse_per_sequenza-2)),
                              rep(4,12^(mosse_per_sequenza-2)),rep(5,12^(mosse_per_sequenza-2)),rep(6,12^(mosse_per_sequenza-2)),
                              rep(7,12^(mosse_per_sequenza-2)),rep(8,12^(mosse_per_sequenza-2)),rep(9,12^(mosse_per_sequenza-2)),
                              rep(10,12^(mosse_per_sequenza-2)),rep(11,12^(mosse_per_sequenza-2)),rep(12,12^(mosse_per_sequenza-2))),12),
                          rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12^(mosse_per_sequenza-1)))
sequenza_mosse <- cbind(sequenza_mosse,rep(100,12^(mosse_per_sequenza)))
sequenza_mosse

n_iterazioni <- 10000
valori_aco1 <- rep(NA,n_iterazioni)
for (p in 1:n_iterazioni) {
  feromoni_cumul <- cumsum(sequenza_mosse[,3])
  valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                  base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
  valori_aco1[p] <- valore_base
  for (i in 1:n_formiche) {
    valore <- sample(1:sum(sequenza_mosse[,3]),1)
    scelta <- which(feromoni_cumul>=valore)[1]
    for (k in 1:mosse_per_sequenza) {
      numero <- sequenza_mosse[scelta,k]
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_nuovo <-  valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      diff_valori[i,1] <- scelta
      diff_valori[i,2] <- valore_nuovo - valore_base
      # fine per ogni formica
      # calcolare la differenza in valore 
      
    }
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
    # fine per tutte le formiche
    # qua mettere i cambi di feromoni
  }
  for (j in 1:n_formiche) {
    sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] <- max(sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] + diff_valori[j,2],20)
  } # adesso bisogna effettuare la mossa con il maggior guadagno
  for (k in 1:mosse_per_sequenza) {
    numero <- sequenza_mosse[diff_valori[which(diff_valori[,2]==max(diff_valori[,2]))[1],1],k]
    numero
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  base_bianco <- bianco
  base_arancione <- arancione
  base_verde <- verde
  base_blu <- blu
  base_rosso <- rosso
  base_giallo <- giallo
}


valori_aco1
sequenza_mosse

plot(valori_aco1, type = "l", col = "blue", lwd = 2,
     main = "ACO Algorithm with sequences of 3 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))

abline(h = 48, col = "red", lwd = 2, lty = 2)
max(valori_aco1)
mean(valori_aco1)
valori_aco_3mosse <- valori_aco1
valori_aco_3mosse
# per la prima volta, con sequenze di appena 3 mosse, abbiamo superato il record raggiundendo un valore di 34!
# anche la media estremamente alta, 24.52

# proviamo a 4 mosse, forse ci avviciniamo ancora di più alla soluzione
# ANCHE QUESTO ALGORITMO E MOLTI DEI PROSSIMI CON 10000+ ITERAZIONI, RICHIEDERANNO TEMPO


n_formiche <- 30
diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
diff_valori


mosse_per_sequenza <- 4
sequenza_mosse <- cbind(rep(c(rep(1,12^(mosse_per_sequenza-1)),rep(2,12^(mosse_per_sequenza-1)),rep(3,12^(mosse_per_sequenza-1)),
                              rep(4,12^(mosse_per_sequenza-1)),rep(5,12^(mosse_per_sequenza-1)),rep(6,12^(mosse_per_sequenza-1)),
                              rep(7,12^(mosse_per_sequenza-1)),rep(8,12^(mosse_per_sequenza-1)),rep(9,12^(mosse_per_sequenza-1)),
                              rep(10,12^(mosse_per_sequenza-1)),rep(11,12^(mosse_per_sequenza-1)),rep(12,12^(mosse_per_sequenza-1))),12^(mosse_per_sequenza-4)),
                        rep(c(rep(1,12^(mosse_per_sequenza-2)),rep(2,12^(mosse_per_sequenza-2)),rep(3,12^(mosse_per_sequenza-2)),
                          rep(4,12^(mosse_per_sequenza-2)),rep(5,12^(mosse_per_sequenza-2)),rep(6,12^(mosse_per_sequenza-2)),
                          rep(7,12^(mosse_per_sequenza-2)),rep(8,12^(mosse_per_sequenza-2)),rep(9,12^(mosse_per_sequenza-2)),
                          rep(10,12^(mosse_per_sequenza-2)),rep(11,12^(mosse_per_sequenza-2)),rep(12,12^(mosse_per_sequenza-2))),12^(mosse_per_sequenza-3)),
                        rep(c(rep(1,12^(mosse_per_sequenza-3)),rep(2,12^(mosse_per_sequenza-3)),rep(3,12^(mosse_per_sequenza-3)),
                              rep(4,12^(mosse_per_sequenza-3)),rep(5,12^(mosse_per_sequenza-3)),rep(6,12^(mosse_per_sequenza-3)),
                              rep(7,12^(mosse_per_sequenza-3)),rep(8,12^(mosse_per_sequenza-3)),rep(9,12^(mosse_per_sequenza-3)),
                              rep(10,12^(mosse_per_sequenza-3)),rep(11,12^(mosse_per_sequenza-3)),rep(12,12^(mosse_per_sequenza-3))),12^(mosse_per_sequenza-2)),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12^(mosse_per_sequenza-1)))
sequenza_mosse <- cbind(sequenza_mosse,rep(100,12^(mosse_per_sequenza)))
sequenza_mosse
p<-1
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
n_iterazioni <- 10000
valori_aco1 <- rep(NA,n_iterazioni)
for (p in 1:n_iterazioni) {
  feromoni_cumul <- cumsum(sequenza_mosse[,3])
  valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                  base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
  valori_aco1[p] <- valore_base
  for (i in 1:n_formiche) {
    valore <- sample(1:sum(sequenza_mosse[,3]),1)
    scelta <- which(feromoni_cumul>=valore)[1]
    for (k in 1:mosse_per_sequenza) {
      numero <- sequenza_mosse[scelta,k]
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_nuovo <-  valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      diff_valori[i,1] <- scelta
      diff_valori[i,2] <- valore_nuovo - valore_base
      # fine per ogni formica
      # calcolare la differenza in valore 
      
    }
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
    # fine per tutte le formiche
    # qua mettere i cambi di feromoni
  }
  for (j in 1:n_formiche) {
    sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] <- max(sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] + diff_valori[j,2],20)
  } # adesso bisogna effettuare la mossa con il maggior guadagno
  for (k in 1:mosse_per_sequenza) {
    numero <- sequenza_mosse[diff_valori[which(diff_valori[,2]==max(diff_valori[,2]))[1],1],k]
    numero
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  base_bianco <- bianco
  base_arancione <- arancione
  base_verde <- verde
  base_blu <- blu
  base_rosso <- rosso
  base_giallo <- giallo
}



valori_aco1
sequenza_mosse

plot(valori_aco1, type = "l", col = "blue", lwd = 2,
     main = "ACO Algorithm with sequences of 4 moves, 30 ants",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))

abline(h = 48, col = "red", lwd = 2, lty = 2)
max(valori_aco1)
mean(valori_aco1)
hist(valori_aco1)
valori_aco_4mosse<-valori_aco1
max(valori_aco_4mosse)
max(sequenza_mosse[,5])
sd(valori_aco_2mosse)
sd(valori_aco_3mosse)
sd(valori_aco1)
# purtroppo con una sequenza di mosse non ho avuto miglioramenti sulla media dei valori e sul valore massimo
# si potrebbe provare a fare un misto delle 3 mosse dove: se il valore sta sotto una certa soglia allora si 
# usano sequenze di 2 mosse, se supera la soglia iniziale si usano sequenze a 3 mosse e se si supera un'ulteriore 
# soglia allora si usano sequenze a 4 mosse

valori_aco1 <- rep(NA,1000)
n_formiche <- 100

sequenza_mosse2 <- cbind(c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),
                          rep(7,12),rep(8,12),rep(9,12),rep(10,12),rep(11,12),rep(12,12)),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12),
                        rep(100,144))
mosse_per_sequenza <- 3
sequenza_mosse3 <- cbind(c(rep(1,12^(mosse_per_sequenza-1)),rep(2,12^(mosse_per_sequenza-1)),rep(3,12^(mosse_per_sequenza-1)),
                          rep(4,12^(mosse_per_sequenza-1)),rep(5,12^(mosse_per_sequenza-1)),rep(6,12^(mosse_per_sequenza-1)),
                          rep(7,12^(mosse_per_sequenza-1)),rep(8,12^(mosse_per_sequenza-1)),rep(9,12^(mosse_per_sequenza-1)),
                          rep(10,12^(mosse_per_sequenza-1)),rep(11,12^(mosse_per_sequenza-1)),rep(12,12^(mosse_per_sequenza-1))),
                        rep(c(rep(1,12^(mosse_per_sequenza-2)),rep(2,12^(mosse_per_sequenza-2)),rep(3,12^(mosse_per_sequenza-2)),
                              rep(4,12^(mosse_per_sequenza-2)),rep(5,12^(mosse_per_sequenza-2)),rep(6,12^(mosse_per_sequenza-2)),
                              rep(7,12^(mosse_per_sequenza-2)),rep(8,12^(mosse_per_sequenza-2)),rep(9,12^(mosse_per_sequenza-2)),
                              rep(10,12^(mosse_per_sequenza-2)),rep(11,12^(mosse_per_sequenza-2)),rep(12,12^(mosse_per_sequenza-2))),12),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12^(mosse_per_sequenza-1)),
                        rep(100,12^(mosse_per_sequenza)))
mosse_per_sequenza <- 4
sequenza_mosse4 <- cbind(rep(c(rep(1,12^(mosse_per_sequenza-1)),rep(2,12^(mosse_per_sequenza-1)),rep(3,12^(mosse_per_sequenza-1)),
                              rep(4,12^(mosse_per_sequenza-1)),rep(5,12^(mosse_per_sequenza-1)),rep(6,12^(mosse_per_sequenza-1)),
                              rep(7,12^(mosse_per_sequenza-1)),rep(8,12^(mosse_per_sequenza-1)),rep(9,12^(mosse_per_sequenza-1)),
                              rep(10,12^(mosse_per_sequenza-1)),rep(11,12^(mosse_per_sequenza-1)),rep(12,12^(mosse_per_sequenza-1))),12^(mosse_per_sequenza-4)),
                        rep(c(rep(1,12^(mosse_per_sequenza-2)),rep(2,12^(mosse_per_sequenza-2)),rep(3,12^(mosse_per_sequenza-2)),
                              rep(4,12^(mosse_per_sequenza-2)),rep(5,12^(mosse_per_sequenza-2)),rep(6,12^(mosse_per_sequenza-2)),
                              rep(7,12^(mosse_per_sequenza-2)),rep(8,12^(mosse_per_sequenza-2)),rep(9,12^(mosse_per_sequenza-2)),
                              rep(10,12^(mosse_per_sequenza-2)),rep(11,12^(mosse_per_sequenza-2)),rep(12,12^(mosse_per_sequenza-2))),12^(mosse_per_sequenza-3)),
                        rep(c(rep(1,12^(mosse_per_sequenza-3)),rep(2,12^(mosse_per_sequenza-3)),rep(3,12^(mosse_per_sequenza-3)),
                              rep(4,12^(mosse_per_sequenza-3)),rep(5,12^(mosse_per_sequenza-3)),rep(6,12^(mosse_per_sequenza-3)),
                              rep(7,12^(mosse_per_sequenza-3)),rep(8,12^(mosse_per_sequenza-3)),rep(9,12^(mosse_per_sequenza-3)),
                              rep(10,12^(mosse_per_sequenza-3)),rep(11,12^(mosse_per_sequenza-3)),rep(12,12^(mosse_per_sequenza-3))),12^(mosse_per_sequenza-2)),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12^(mosse_per_sequenza-1)),
                        rep(100,12^(mosse_per_sequenza)))
sequenza_mosse4
sequenza_mosse2
p<-1
valore_base <- 0
valore_base
base_bianco
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
diff_valori
p
n_iterazioni <-10000
for (p in 1:n_iterazioni) {
  if (valore_base < 18){
    n_formiche <- 10
    diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
    feromoni_cumul <- cumsum(sequenza_mosse2[,3])
    valori_aco1[p] <- valore_base
    for (i in 1:n_formiche) {
      valore <- sample(1:sum(sequenza_mosse2[,3]),1)
      scelta <- which(feromoni_cumul>=valore)[1]
      for (k in 1:2) {
        numero <- sequenza_mosse2[scelta,k]
        if (numero == 1){
          mossa1(bianco, blu, giallo, verde, arancione, rosso)
          risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
          bianco <- risultato1$bianco
          blu <- risultato1$blu
          giallo <- risultato1$giallo
          verde <- risultato1$verde
          arancione <- risultato1$arancione
          rosso <- risultato1$rosso
        } else {
          if (numero == 2) {
            mossa2(bianco, blu, giallo, verde, rosso, arancione)
            risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
            bianco <- risultato2$bianco
            blu <- risultato2$blu
            giallo <- risultato2$giallo
            verde <- risultato2$verde
            rosso <- risultato2$rosso
            arancione <- risultato2$arancione
          } else {
            if (numero == 3) {
              mossa3(bianco, arancione, giallo, verde, rosso, blu)
              risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
              bianco <- risultato3$bianco
              arancione <- risultato3$arancione
              giallo <- risultato3$giallo
              verde <- risultato3$verde
              rosso <- risultato3$rosso
              blu <- risultato3$blu
            } else {
              if (numero == 4){
                mossa4(bianco, arancione, giallo, blu, rosso, verde)
                risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                bianco <- risultato4$bianco
                arancione <- risultato4$arancione
                giallo <- risultato4$giallo
                blu <- risultato4$blu
                rosso <- risultato4$rosso
                verde <- risultato4$verde
              } else {
                if (numero == 5){
                  mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  bianco <- risultato5$bianco
                  arancione <- risultato5$arancione
                  verde <- risultato5$verde
                  blu <- risultato5$blu
                  rosso <- risultato5$rosso
                  giallo <- risultato5$giallo
                } else {
                  if (numero == 6){
                    mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato6$giallo
                    arancione <- risultato6$arancione
                    verde <- risultato6$verde
                    blu <- risultato6$blu
                    rosso <- risultato6$rosso
                    bianco <- risultato6$bianco
                  } else {
                    if (numero == 7){
                      mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      giallo <- risultato7$giallo
                      arancione <- risultato7$arancione
                      verde <- risultato7$verde
                      blu <- risultato7$blu
                      rosso <- risultato7$rosso
                      bianco <- risultato7$bianco
                    } else {
                      if (numero == 8){
                        mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        bianco <- risultato8$bianco
                        arancione <- risultato8$arancione
                        verde <- risultato8$verde
                        blu <- risultato8$blu
                        rosso <- risultato8$rosso
                        giallo <- risultato8$giallo
                      } else {
                        if (numero == 9){
                          mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          bianco <- risultato9$bianco
                          arancione <- risultato9$arancione
                          giallo <- risultato9$giallo
                          blu <- risultato9$blu
                          rosso <- risultato9$rosso
                          verde <- risultato9$verde
                        } else {
                          if (numero == 10){
                            mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            bianco <- risultato10$bianco
                            arancione <- risultato10$arancione
                            giallo <- risultato10$giallo
                            verde <- risultato10$verde
                            rosso <- risultato10$rosso
                            blu <- risultato10$blu
                          } else {
                            if (numero == 11){
                              mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              bianco <- risultato11$bianco
                              blu <- risultato11$blu
                              giallo <- risultato11$giallo
                              verde <- risultato11$verde
                              rosso <- risultato11$rosso
                              arancione <- risultato11$arancione
                            } else {
                              if (numero == 12){
                                mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                bianco <- risultato12$bianco
                                blu <- risultato12$blu
                                giallo <- risultato12$giallo
                                verde <- risultato12$verde
                                arancione <- risultato12$arancione
                                rosso <- risultato12$rosso
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
        
      }
      valore_nuovo <-  valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      diff_valori[i,1] <- scelta
      diff_valori[i,2] <- valore_nuovo - valore_base
      bianco <- base_bianco
      arancione <- base_arancione
      verde <- base_verde
      blu <- base_blu
      rosso <- base_rosso
      giallo <- base_giallo
      # fine per tutte le formiche
      # qua mettere i cambi di feromoni
    }
    for (j in 1:n_formiche) {
      sequenza_mosse2[diff_valori[j,1],3] <- max(sequenza_mosse2[diff_valori[j,1],3] + diff_valori[j,2],20)
    } # adesso bisogna effettuare la mossa con il maggior guadagno
    for (k in 1:2) {
      numero <- sequenza_mosse2[diff_valori[which(diff_valori[,2]==max(diff_valori[,2]))[1],1],k]
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    base_bianco <- bianco
    base_arancione <- arancione
    base_verde <- verde
    base_blu <- blu
    base_rosso <- rosso
    base_giallo <- giallo
    valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                    base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
    
    p <- p+1
  }
  else {
    if (valore_base < 33){
      n_formiche <- 100
      mosse_per_sequenza <- 3
      diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
      feromoni_cumul <- cumsum(sequenza_mosse3[,3])
      valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                      base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
      valori_aco1[p] <- valore_base
      for (i in 1:n_formiche) {
        valore <- sample(1:sum(sequenza_mosse3[,3]),1)
        scelta <- which(feromoni_cumul>=valore)[1]
        for (k in 1:mosse_per_sequenza) {
          numero <- sequenza_mosse3[scelta,k]
          if (numero == 1){
            mossa1(bianco, blu, giallo, verde, arancione, rosso)
            risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
            bianco <- risultato1$bianco
            blu <- risultato1$blu
            giallo <- risultato1$giallo
            verde <- risultato1$verde
            arancione <- risultato1$arancione
            rosso <- risultato1$rosso
          } else {
            if (numero == 2) {
              mossa2(bianco, blu, giallo, verde, rosso, arancione)
              risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
              bianco <- risultato2$bianco
              blu <- risultato2$blu
              giallo <- risultato2$giallo
              verde <- risultato2$verde
              rosso <- risultato2$rosso
              arancione <- risultato2$arancione
            } else {
              if (numero == 3) {
                mossa3(bianco, arancione, giallo, verde, rosso, blu)
                risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
                bianco <- risultato3$bianco
                arancione <- risultato3$arancione
                giallo <- risultato3$giallo
                verde <- risultato3$verde
                rosso <- risultato3$rosso
                blu <- risultato3$blu
              } else {
                if (numero == 4){
                  mossa4(bianco, arancione, giallo, blu, rosso, verde)
                  risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                  bianco <- risultato4$bianco
                  arancione <- risultato4$arancione
                  giallo <- risultato4$giallo
                  blu <- risultato4$blu
                  rosso <- risultato4$rosso
                  verde <- risultato4$verde
                } else {
                  if (numero == 5){
                    mossa5(bianco, arancione, verde, blu, rosso, giallo)
                    risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato5$bianco
                    arancione <- risultato5$arancione
                    verde <- risultato5$verde
                    blu <- risultato5$blu
                    rosso <- risultato5$rosso
                    giallo <- risultato5$giallo
                  } else {
                    if (numero == 6){
                      mossa6(giallo, arancione, verde, blu, rosso, bianco)
                      risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                      giallo <- risultato6$giallo
                      arancione <- risultato6$arancione
                      verde <- risultato6$verde
                      blu <- risultato6$blu
                      rosso <- risultato6$rosso
                      bianco <- risultato6$bianco
                    } else {
                      if (numero == 7){
                        mossa7(giallo, arancione, verde, blu, rosso, bianco)
                        risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                        giallo <- risultato7$giallo
                        arancione <- risultato7$arancione
                        verde <- risultato7$verde
                        blu <- risultato7$blu
                        rosso <- risultato7$rosso
                        bianco <- risultato7$bianco
                      } else {
                        if (numero == 8){
                          mossa8(bianco, arancione, verde, blu, rosso, giallo)
                          risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                          bianco <- risultato8$bianco
                          arancione <- risultato8$arancione
                          verde <- risultato8$verde
                          blu <- risultato8$blu
                          rosso <- risultato8$rosso
                          giallo <- risultato8$giallo
                        } else {
                          if (numero == 9){
                            mossa9(bianco, arancione, giallo, blu, rosso, verde)
                            risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                            bianco <- risultato9$bianco
                            arancione <- risultato9$arancione
                            giallo <- risultato9$giallo
                            blu <- risultato9$blu
                            rosso <- risultato9$rosso
                            verde <- risultato9$verde
                          } else {
                            if (numero == 10){
                              mossa10(bianco, arancione, giallo, verde, rosso, blu)
                              risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                              bianco <- risultato10$bianco
                              arancione <- risultato10$arancione
                              giallo <- risultato10$giallo
                              verde <- risultato10$verde
                              rosso <- risultato10$rosso
                              blu <- risultato10$blu
                            } else {
                              if (numero == 11){
                                mossa11(bianco, blu, giallo, verde, rosso, arancione)
                                risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                                bianco <- risultato11$bianco
                                blu <- risultato11$blu
                                giallo <- risultato11$giallo
                                verde <- risultato11$verde
                                rosso <- risultato11$rosso
                                arancione <- risultato11$arancione
                              } else {
                                if (numero == 12){
                                  mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                  risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                  bianco <- risultato12$bianco
                                  blu <- risultato12$blu
                                  giallo <- risultato12$giallo
                                  verde <- risultato12$verde
                                  arancione <- risultato12$arancione
                                  rosso <- risultato12$rosso
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          valore_nuovo <-  valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                            blu,blu_0,rosso,rosso_0,giallo,giallo_0)
          diff_valori[i,1] <- scelta
          diff_valori[i,2] <- valore_nuovo - valore_base
          # fine per ogni formica
          # calcolare la differenza in valore 
          
        }
        bianco <- base_bianco
        arancione <- base_arancione
        verde <- base_verde
        blu <- base_blu
        rosso <- base_rosso
        giallo <- base_giallo
        # fine per tutte le formiche
        # qua mettere i cambi di feromoni
      }
      for (j in 1:n_formiche) {
        sequenza_mosse3[diff_valori[j,1],mosse_per_sequenza+1] <- max(sequenza_mosse3[diff_valori[j,1],mosse_per_sequenza+1] + diff_valori[j,2],20)
      } # adesso bisogna effettuare la mossa con il maggior guadagno
      for (k in 1:mosse_per_sequenza) {
        numero <- sequenza_mosse3[diff_valori[which(diff_valori[,2]==max(diff_valori[,2]))[1],1],k]
        numero
        if (numero == 1){
          mossa1(bianco, blu, giallo, verde, arancione, rosso)
          risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
          bianco <- risultato1$bianco
          blu <- risultato1$blu
          giallo <- risultato1$giallo
          verde <- risultato1$verde
          arancione <- risultato1$arancione
          rosso <- risultato1$rosso
        } else {
          if (numero == 2) {
            mossa2(bianco, blu, giallo, verde, rosso, arancione)
            risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
            bianco <- risultato2$bianco
            blu <- risultato2$blu
            giallo <- risultato2$giallo
            verde <- risultato2$verde
            rosso <- risultato2$rosso
            arancione <- risultato2$arancione
          } else {
            if (numero == 3) {
              mossa3(bianco, arancione, giallo, verde, rosso, blu)
              risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
              bianco <- risultato3$bianco
              arancione <- risultato3$arancione
              giallo <- risultato3$giallo
              verde <- risultato3$verde
              rosso <- risultato3$rosso
              blu <- risultato3$blu
            } else {
              if (numero == 4){
                mossa4(bianco, arancione, giallo, blu, rosso, verde)
                risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                bianco <- risultato4$bianco
                arancione <- risultato4$arancione
                giallo <- risultato4$giallo
                blu <- risultato4$blu
                rosso <- risultato4$rosso
                verde <- risultato4$verde
              } else {
                if (numero == 5){
                  mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  bianco <- risultato5$bianco
                  arancione <- risultato5$arancione
                  verde <- risultato5$verde
                  blu <- risultato5$blu
                  rosso <- risultato5$rosso
                  giallo <- risultato5$giallo
                } else {
                  if (numero == 6){
                    mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato6$giallo
                    arancione <- risultato6$arancione
                    verde <- risultato6$verde
                    blu <- risultato6$blu
                    rosso <- risultato6$rosso
                    bianco <- risultato6$bianco
                  } else {
                    if (numero == 7){
                      mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      giallo <- risultato7$giallo
                      arancione <- risultato7$arancione
                      verde <- risultato7$verde
                      blu <- risultato7$blu
                      rosso <- risultato7$rosso
                      bianco <- risultato7$bianco
                    } else {
                      if (numero == 8){
                        mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        bianco <- risultato8$bianco
                        arancione <- risultato8$arancione
                        verde <- risultato8$verde
                        blu <- risultato8$blu
                        rosso <- risultato8$rosso
                        giallo <- risultato8$giallo
                      } else {
                        if (numero == 9){
                          mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          bianco <- risultato9$bianco
                          arancione <- risultato9$arancione
                          giallo <- risultato9$giallo
                          blu <- risultato9$blu
                          rosso <- risultato9$rosso
                          verde <- risultato9$verde
                        } else {
                          if (numero == 10){
                            mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            bianco <- risultato10$bianco
                            arancione <- risultato10$arancione
                            giallo <- risultato10$giallo
                            verde <- risultato10$verde
                            rosso <- risultato10$rosso
                            blu <- risultato10$blu
                          } else {
                            if (numero == 11){
                              mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              bianco <- risultato11$bianco
                              blu <- risultato11$blu
                              giallo <- risultato11$giallo
                              verde <- risultato11$verde
                              rosso <- risultato11$rosso
                              arancione <- risultato11$arancione
                            } else {
                              if (numero == 12){
                                mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                bianco <- risultato12$bianco
                                blu <- risultato12$blu
                                giallo <- risultato12$giallo
                                verde <- risultato12$verde
                                arancione <- risultato12$arancione
                                rosso <- risultato12$rosso
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      base_bianco <- bianco
      base_arancione <- arancione
      base_verde <- verde
      base_blu <- blu
      base_rosso <- rosso
      base_giallo <- giallo
      p <- p+1
    }
    else {
      mosse_per_sequenza <- 4
      diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
      feromoni_cumul <- cumsum(sequenza_mosse4[,3])
      valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                      base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
      valori_aco1[p] <- valore_base
      for (i in 1:n_formiche) {
        valore <- sample(1:sum(sequenza_mosse4[,3]),1)
        scelta <- which(feromoni_cumul>=valore)[1]
        for (k in 1:mosse_per_sequenza) {
          numero <- sequenza_mosse4[scelta,k]
          if (numero == 1){
            mossa1(bianco, blu, giallo, verde, arancione, rosso)
            risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
            bianco <- risultato1$bianco
            blu <- risultato1$blu
            giallo <- risultato1$giallo
            verde <- risultato1$verde
            arancione <- risultato1$arancione
            rosso <- risultato1$rosso
          } else {
            if (numero == 2) {
              mossa2(bianco, blu, giallo, verde, rosso, arancione)
              risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
              bianco <- risultato2$bianco
              blu <- risultato2$blu
              giallo <- risultato2$giallo
              verde <- risultato2$verde
              rosso <- risultato2$rosso
              arancione <- risultato2$arancione
            } else {
              if (numero == 3) {
                mossa3(bianco, arancione, giallo, verde, rosso, blu)
                risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
                bianco <- risultato3$bianco
                arancione <- risultato3$arancione
                giallo <- risultato3$giallo
                verde <- risultato3$verde
                rosso <- risultato3$rosso
                blu <- risultato3$blu
              } else {
                if (numero == 4){
                  mossa4(bianco, arancione, giallo, blu, rosso, verde)
                  risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                  bianco <- risultato4$bianco
                  arancione <- risultato4$arancione
                  giallo <- risultato4$giallo
                  blu <- risultato4$blu
                  rosso <- risultato4$rosso
                  verde <- risultato4$verde
                } else {
                  if (numero == 5){
                    mossa5(bianco, arancione, verde, blu, rosso, giallo)
                    risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato5$bianco
                    arancione <- risultato5$arancione
                    verde <- risultato5$verde
                    blu <- risultato5$blu
                    rosso <- risultato5$rosso
                    giallo <- risultato5$giallo
                  } else {
                    if (numero == 6){
                      mossa6(giallo, arancione, verde, blu, rosso, bianco)
                      risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                      giallo <- risultato6$giallo
                      arancione <- risultato6$arancione
                      verde <- risultato6$verde
                      blu <- risultato6$blu
                      rosso <- risultato6$rosso
                      bianco <- risultato6$bianco
                    } else {
                      if (numero == 7){
                        mossa7(giallo, arancione, verde, blu, rosso, bianco)
                        risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                        giallo <- risultato7$giallo
                        arancione <- risultato7$arancione
                        verde <- risultato7$verde
                        blu <- risultato7$blu
                        rosso <- risultato7$rosso
                        bianco <- risultato7$bianco
                      } else {
                        if (numero == 8){
                          mossa8(bianco, arancione, verde, blu, rosso, giallo)
                          risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                          bianco <- risultato8$bianco
                          arancione <- risultato8$arancione
                          verde <- risultato8$verde
                          blu <- risultato8$blu
                          rosso <- risultato8$rosso
                          giallo <- risultato8$giallo
                        } else {
                          if (numero == 9){
                            mossa9(bianco, arancione, giallo, blu, rosso, verde)
                            risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                            bianco <- risultato9$bianco
                            arancione <- risultato9$arancione
                            giallo <- risultato9$giallo
                            blu <- risultato9$blu
                            rosso <- risultato9$rosso
                            verde <- risultato9$verde
                          } else {
                            if (numero == 10){
                              mossa10(bianco, arancione, giallo, verde, rosso, blu)
                              risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                              bianco <- risultato10$bianco
                              arancione <- risultato10$arancione
                              giallo <- risultato10$giallo
                              verde <- risultato10$verde
                              rosso <- risultato10$rosso
                              blu <- risultato10$blu
                            } else {
                              if (numero == 11){
                                mossa11(bianco, blu, giallo, verde, rosso, arancione)
                                risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                                bianco <- risultato11$bianco
                                blu <- risultato11$blu
                                giallo <- risultato11$giallo
                                verde <- risultato11$verde
                                rosso <- risultato11$rosso
                                arancione <- risultato11$arancione
                              } else {
                                if (numero == 12){
                                  mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                  risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                  bianco <- risultato12$bianco
                                  blu <- risultato12$blu
                                  giallo <- risultato12$giallo
                                  verde <- risultato12$verde
                                  arancione <- risultato12$arancione
                                  rosso <- risultato12$rosso
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
          valore_nuovo <-  valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                            blu,blu_0,rosso,rosso_0,giallo,giallo_0)
          diff_valori[i,1] <- scelta
          diff_valori[i,2] <- valore_nuovo - valore_base
          # fine per ogni formica
          # calcolare la differenza in valore 
          
        }
        bianco <- base_bianco
        arancione <- base_arancione
        verde <- base_verde
        blu <- base_blu
        rosso <- base_rosso
        giallo <- base_giallo
        # fine per tutte le formiche
        # qua mettere i cambi di feromoni
      }
      for (j in 1:n_formiche) {
        sequenza_mosse4[diff_valori[j,1],mosse_per_sequenza+1] <- max(sequenza_mosse4[diff_valori[j,1],mosse_per_sequenza+1] + diff_valori[j,2],20)
      } # adesso bisogna effettuare la mossa con il maggior guadagno
      for (k in 1:mosse_per_sequenza) {
        numero <- sequenza_mosse4[diff_valori[which(diff_valori[,2]==max(diff_valori[,2]))[1],1],k]
        numero
        if (numero == 1){
          mossa1(bianco, blu, giallo, verde, arancione, rosso)
          risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
          bianco <- risultato1$bianco
          blu <- risultato1$blu
          giallo <- risultato1$giallo
          verde <- risultato1$verde
          arancione <- risultato1$arancione
          rosso <- risultato1$rosso
        } else {
          if (numero == 2) {
            mossa2(bianco, blu, giallo, verde, rosso, arancione)
            risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
            bianco <- risultato2$bianco
            blu <- risultato2$blu
            giallo <- risultato2$giallo
            verde <- risultato2$verde
            rosso <- risultato2$rosso
            arancione <- risultato2$arancione
          } else {
            if (numero == 3) {
              mossa3(bianco, arancione, giallo, verde, rosso, blu)
              risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
              bianco <- risultato3$bianco
              arancione <- risultato3$arancione
              giallo <- risultato3$giallo
              verde <- risultato3$verde
              rosso <- risultato3$rosso
              blu <- risultato3$blu
            } else {
              if (numero == 4){
                mossa4(bianco, arancione, giallo, blu, rosso, verde)
                risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                bianco <- risultato4$bianco
                arancione <- risultato4$arancione
                giallo <- risultato4$giallo
                blu <- risultato4$blu
                rosso <- risultato4$rosso
                verde <- risultato4$verde
              } else {
                if (numero == 5){
                  mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  bianco <- risultato5$bianco
                  arancione <- risultato5$arancione
                  verde <- risultato5$verde
                  blu <- risultato5$blu
                  rosso <- risultato5$rosso
                  giallo <- risultato5$giallo
                } else {
                  if (numero == 6){
                    mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato6$giallo
                    arancione <- risultato6$arancione
                    verde <- risultato6$verde
                    blu <- risultato6$blu
                    rosso <- risultato6$rosso
                    bianco <- risultato6$bianco
                  } else {
                    if (numero == 7){
                      mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      giallo <- risultato7$giallo
                      arancione <- risultato7$arancione
                      verde <- risultato7$verde
                      blu <- risultato7$blu
                      rosso <- risultato7$rosso
                      bianco <- risultato7$bianco
                    } else {
                      if (numero == 8){
                        mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        bianco <- risultato8$bianco
                        arancione <- risultato8$arancione
                        verde <- risultato8$verde
                        blu <- risultato8$blu
                        rosso <- risultato8$rosso
                        giallo <- risultato8$giallo
                      } else {
                        if (numero == 9){
                          mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          bianco <- risultato9$bianco
                          arancione <- risultato9$arancione
                          giallo <- risultato9$giallo
                          blu <- risultato9$blu
                          rosso <- risultato9$rosso
                          verde <- risultato9$verde
                        } else {
                          if (numero == 10){
                            mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            bianco <- risultato10$bianco
                            arancione <- risultato10$arancione
                            giallo <- risultato10$giallo
                            verde <- risultato10$verde
                            rosso <- risultato10$rosso
                            blu <- risultato10$blu
                          } else {
                            if (numero == 11){
                              mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              bianco <- risultato11$bianco
                              blu <- risultato11$blu
                              giallo <- risultato11$giallo
                              verde <- risultato11$verde
                              rosso <- risultato11$rosso
                              arancione <- risultato11$arancione
                            } else {
                              if (numero == 12){
                                mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                bianco <- risultato12$bianco
                                blu <- risultato12$blu
                                giallo <- risultato12$giallo
                                verde <- risultato12$verde
                                arancione <- risultato12$arancione
                                rosso <- risultato12$rosso
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      base_bianco <- bianco
      base_arancione <- arancione
      base_verde <- verde
      base_blu <- blu
      base_rosso <- rosso
      base_giallo <- giallo
      p <- p+1
    }
  } 
}
valori_aco1
hist(valori_aco1)

plot(valori_aco1, type = "l", col = "blue", lwd = 2,
     main = "ACO Algorithm with different sequences",
     xlab = "Moves", ylab = "Values", ylim = c(0,50), xaxt="n")
axis(1, at = seq(0, 100000, by = 20000), labels = c("0", "20000", "40000", "60000", "80000", "100000"))
abline(h = 48, col = "red", lwd = 2, lty = 2)
max(valori_aco1)
na.omit(valori_aco1)
valori_aco1 <- na.omit(valori_aco1)

print(valori_aco1)
valori_aco1[3000]
valori_aco1[49]
mean(valori_aco1)
which(valori_aco1==max(valori_aco1))




valori_aco_diff_mosse <- valori_aco1
valori_aco_diff_mosse
qqnorm(valori_aco_diff_mosse)
qqline(valori_aco_diff_mosse, col = "red") 


hist(valori_aco_diff_mosse, probability = TRUE, col = "lightblue", breaks = 15, 
     main = "Istogramma orizzontale con distribuzione Normale", 
     xlab = "Frequenze", ylab = "Valori", horiz = TRUE)
mean(valori_aco_diff_mosse)
sd(valori_aco_diff_mosse)
curve(dnorm(x, mean = mean(valori_aco_diff_mosse), sd = sd(valori_aco_diff_mosse)), 
      col = "red", lwd = 2, add = TRUE)
probab <- 1-pnorm(48,mean = mean(valori_aco_diff_mosse), sd=sd(valori_aco_diff_mosse))
probab
# Con questo metodo siamo riusciti a raggiungere il valore di 36 ben 15 volte dopo circa 7000 mosse effettive
# Magari andando avanti si può migliorare. Aggiornamento: dopo 37000 mosse siamo arrivati al valore di 39!
# non credo che per migliorare ulteriormente basti questo. Bisogna cambiare qualcosa, a partire dai vari
# parametri: se per esempio aumento il numero minimo dei feromoni che possono avere le varie sequenze si può ampliare
# la capacità di esplorazione che ha l'algoritmo, magari per uscire più facilmente da massimi locali. Aumentare il numero 
# di formiche invece potrebbe portare ad un aumento di velocità nel raggiungere un valore alto, ma allo stesso tempo 
# aumenterebbe di molto i calcoli che il computer dovrebbe fare per eseguire lo stesso numero di mosse rispetto a prima,
# rendendolo più lento nelle mosse effettive. Se aumentassi il numero di mosse per frequenza invece aumenterei le possibilità
# di trovare i valori più alti in assoluto, ma allo stesso tempo aumenterei sia il calcolo computazionale (esponenzialmente) 
# si la volatilità (la varianza) dei valori, che oscillerebbero molto di più. Il trucco sta nel trovare una buona
# sintonia tra questi parametri che possa raggiungere alti valori minimizzando tempo e calcoli (l'obiettivo principale
# rimane quello di dimostrare il miglioramento che si ha utilizzando questi metodi avanzati di ricerca, sia per quanto
# riguarda il valore massimo sia per qunato riguarda la velocità di raggiungere certi valori con il minor numero di 
# di mosse possibili)



## Mi è venuta un'idea che potrebbe aumentare le possibilità di successo in modo esponenziale: se noi partiamo da un 
# cubo e facciamo una qualunque delle 12 mosse, il valore si abbasserà da 48 a 36. Questo vuol dire che a prescindere dal 
# modo che utilizziamo per risolverlo, la penultima mossa deve portare ad un valore di 36. Di conseguenza si può impostare 
# il nostro algoritmo per cercare di arrivare al valore di 36 esatto e, una volta raggiunto, provare a fare tutte e 12 
# le mosse (singolarmente) per vedere se una di queste lo risolve. Allo stesso modo se partiamo dalla penultima mossa con 
# un valore di 36 e facciamo una mossa delle 12 a caso possiamo avere solo 2 valori: 24 con una delle 12 mosse (se essa consiste
# nel muovere lo strato non ancora mosso) e 26 con le altre 11 mosse. Questo significa che a prescindere da come si vuole 
# risolvere il cubo, la penultima mossa dovrà avere un valore di 36 e la terzultima un valore di 26 (o 24, con probabilità 
# molto più basse). Di conseguenza l'obiettivo per risolvere il cubo con metodi di ricerca intelligenza potrebbe essere 
# non più quello di puntare al valore più alto possibile, ma di cercare di arrivare il più possibile al valore di 26 (e 24)
# e di cercare di trovare combinazioni dal valore di 26 che con una mossa portano al valore di 36. 
# In teoria si potrebbe continuare a ritroso per 3 o più mosse, ma si tratterebbe di un aumento esponenziale di possibili valori
# che portano alla soluzione in poche mosse al punto tale in cui quasi ogni valore è da studiare più in particolare. Per 
# ora proverei ad applicare una ricerca potenziata per questi 3 valori e vedere se abbiamo un qualche miglioramento.

# provo ad impostare l'algoritmo in modo che cerchi in ogni modo possibile di raggiungere il valore 26 e subito dopo 36
# in teoria ogni volta che raggiunge 24 e può raggiungere 36 con una mossa, quella sarebbe l'ottimale, di conseguenza mi 
# basterebbe raggiungere 26 (o 24) più volte possibili con l'ACO e poi usare il metodo 2 con sequenze di 1 mossa e vedere
# se riesce a raggiungere 36, se così non fosse faccio la mossa con più valore e poi riparto con l'ACO per tornare il 
# prima possibile sul 26. Per essere chiari, essendo 11 mosse su 12 ad un valore di 26 a due mosse dalla fine, per ora
# mi concentro sull'arrivare a quel valore. Se non dovesse funzionare, potrei provare ad ampliare il metodo di ricerca
# concentrandomi anche sul valore 24.
numero_26 <- 26
sum(valori_aco1==26)
# nella scorsa sequenza ben 9957 volte su 70000 mosse, e consideriamo che non cercavo di arrivare la valore di 26 il più
# possibile

# Proverei con sequenze di 3 mosse (per ora le più efficienti usando l'ACO)
# Inoltre, visto che ora l'obiettivo è quello di arrivare a 26, molto più facile di 48, posso provare a diminuire il numero 
# di formiche per aumentare la velocità delle mosse effettive.





n_formiche <- 100
diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
diff_valori
valori_aco1 <- rep(NA,10000)

mosse_per_sequenza <- 3
sequenza_mosse <- cbind(c(rep(1,12^(mosse_per_sequenza-1)),rep(2,12^(mosse_per_sequenza-1)),rep(3,12^(mosse_per_sequenza-1)),
                          rep(4,12^(mosse_per_sequenza-1)),rep(5,12^(mosse_per_sequenza-1)),rep(6,12^(mosse_per_sequenza-1)),
                          rep(7,12^(mosse_per_sequenza-1)),rep(8,12^(mosse_per_sequenza-1)),rep(9,12^(mosse_per_sequenza-1)),
                          rep(10,12^(mosse_per_sequenza-1)),rep(11,12^(mosse_per_sequenza-1)),rep(12,12^(mosse_per_sequenza-1))),
                        rep(c(rep(1,12^(mosse_per_sequenza-2)),rep(2,12^(mosse_per_sequenza-2)),rep(3,12^(mosse_per_sequenza-2)),
                              rep(4,12^(mosse_per_sequenza-2)),rep(5,12^(mosse_per_sequenza-2)),rep(6,12^(mosse_per_sequenza-2)),
                              rep(7,12^(mosse_per_sequenza-2)),rep(8,12^(mosse_per_sequenza-2)),rep(9,12^(mosse_per_sequenza-2)),
                              rep(10,12^(mosse_per_sequenza-2)),rep(11,12^(mosse_per_sequenza-2)),rep(12,12^(mosse_per_sequenza-2))),12),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12^(mosse_per_sequenza-1)))
sequenza_mosse <- cbind(sequenza_mosse,rep(100,12^(mosse_per_sequenza)))
sequenza_mosse
p<-1 



while (p<10001) {
  if (valore_base==48){
    break
  } 
  #ACO
  feromoni_cumul <- cumsum(sequenza_mosse[,3])
  for (i in 1:n_formiche) {
    valore <- sample(1:sum(sequenza_mosse[,3]),1)
    scelta <- which(feromoni_cumul>=valore)[1]
    for (k in 1:mosse_per_sequenza) {
      numero <- sequenza_mosse[scelta,k]
      numero
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_nuovo <-  valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      diff_valori[i,1] <- scelta
      diff_valori[i,2] <- abs(valore_nuovo-26)
      # non bisogna più guardare alla differenza tra valori ma solo se raggiunge o no il 26
      
    }
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
    # fine per tutte le formiche
    # qua mettere i cambi di feromoni
  }
  for (j in 1:n_formiche) {
    if (diff_valori[j,2]==0){
      incremento <- 30
    } else {
      incremento <- -diff_valori[j,2]
    }
    sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] <- max(sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] + incremento,20)
  } 
  for (k in 1:mosse_per_sequenza) {
    numero <- sequenza_mosse[diff_valori[which(diff_valori[,2]==min(diff_valori[,2]))[1],1],k]
    numero
    diff_valori
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  base_bianco <- bianco
  base_arancione <- arancione
  base_verde <- verde
  base_blu <- blu
  base_rosso <- rosso
  base_giallo <- giallo
  valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                  base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
  
  
  
  
  
  if (valore_base==26){
    
    for (i in 1:12) {
      if (valore_base != 36 ){
        if (i == 1){
          mossa1(bianco, blu, giallo, verde, arancione, rosso)
          risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
          stato_attuale <- risultato1
        } else {
          if (i == 2) {
            mossa2(bianco, blu, giallo, verde, rosso, arancione)
            risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
            stato_attuale <- risultato2
          } else {
            if (i == 3) {
              mossa3(bianco, arancione, giallo, verde, rosso, blu)
              risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
              stato_attuale <- risultato3
            } else {
              if (i == 4){
                mossa4(bianco, arancione, giallo, blu, rosso, verde)
                risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                stato_attuale <- risultato4
              } else {
                if (i == 5){
                  mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  stato_attuale <- risultato5
                } else {
                  if (i == 6){
                    mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    stato_attuale <- risultato6
                  } else {
                    if (i == 7){
                      mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      stato_attuale <- risultato7
                    } else {
                      if (i == 8){
                        mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        stato_attuale <- risultato8
                      } else {
                        if (i == 9){
                          mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          stato_attuale <- risultato9
                        } else {
                          if (i == 10){
                            mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            stato_attuale <- risultato10
                          } else {
                            if (i == 11){
                              mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              stato_attuale <- risultato11
                            } else {
                              if (i == 12){
                                mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                stato_attuale <- risultato12
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                         stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
      if (valore_attuale == 36){
        bianco <- stato_attuale$bianco
        arancione <- stato_attuale$arancione
        verde <- stato_attuale$verde
        blu <- stato_attuale$blu
        rosso <- stato_attuale$rosso
        giallo <- stato_attuale$giallo
        valore_base <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      } 
      
    } 
    
  }
  if (valore_base==36){
    for (i in 1:12) {
      if (valore_base != 48 ){
        if (i == 1){
          mossa1(bianco, blu, giallo, verde, arancione, rosso)
          risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
          stato_attuale <- risultato1
        } else {
          if (i == 2) {
            mossa2(bianco, blu, giallo, verde, rosso, arancione)
            risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
            stato_attuale <- risultato2
          } else {
            if (i == 3) {
              mossa3(bianco, arancione, giallo, verde, rosso, blu)
              risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
              stato_attuale <- risultato3
            } else {
              if (i == 4){
                mossa4(bianco, arancione, giallo, blu, rosso, verde)
                risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                stato_attuale <- risultato4
              } else {
                if (i == 5){
                  mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  stato_attuale <- risultato5
                } else {
                  if (i == 6){
                    mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    stato_attuale <- risultato6
                  } else {
                    if (i == 7){
                      mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      stato_attuale <- risultato7
                    } else {
                      if (i == 8){
                        mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        stato_attuale <- risultato8
                      } else {
                        if (i == 9){
                          mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          stato_attuale <- risultato9
                        } else {
                          if (i == 10){
                            mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            stato_attuale <- risultato10
                          } else {
                            if (i == 11){
                              mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              stato_attuale <- risultato11
                            } else {
                              if (i == 12){
                                mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                stato_attuale <- risultato12
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                         stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
      if (valore_attuale == 48){
        bianco <- stato_attuale$bianco
        arancione <- stato_attuale$arancione
        verde <- stato_attuale$verde
        blu <- stato_attuale$blu
        rosso <- stato_attuale$rosso
        giallo <- stato_attuale$giallo
        valore_base <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      } 
    }
  }
  valori_aco1[p] <- valore_base
  p <- p+1
    
}



valori_aco1
valori_aco_26_2 <- valori_aco1
valori_aco_26_1 <- valori_aco1
valori_aco_26_3 <- valori_aco1
hist(valori_aco1)
plot(valori_aco_26_1, type = "l", col = "blue", lwd = 2,
     main = "ACO Algortihm for value 26",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))

abline(h = 48, col = "red", lwd = 2, lty = 2)
max(valori_aco1)
mean(valori_aco1)

sum(valori_aco1==26)
sum(valori_aco_26_2==26)

valori_aco1==26


#############


#############################
#    GRUPPI               ###
#############################

facce()

## serve una funzione per capire quante adiacenze ci siano in una faccia

bianco

n_adiacenze <- 0

{
  adiacenze_bianco <- function(){
  n_adiacenze_bianco <- c()
  if (bianco[1,2]==bianco[2,2]){
    n_adiacenze_bianco <- c(n_adiacenze_bianco,1)
  }
  if (bianco[2,3]==bianco[2,2]){
    n_adiacenze_bianco <- c(n_adiacenze_bianco,2)
  }
  if (bianco[3,2]==bianco[2,2]){
    n_adiacenze_bianco <- c(n_adiacenze_bianco,3)
  }
  if (bianco[2,1]==bianco[2,2]){
    n_adiacenze_bianco <- c(n_adiacenze_bianco,4)
  }
  return(n_adiacenze_bianco)
}
  n_adiacenze_bianco <- adiacenze_bianco()
  } # adiacenze bianco
{
  adiacenze_arancione <- function(){
    n_adiacenze_arancione <- c()
    if (arancione[1,2]==arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,1)
    }
    if (arancione[2,3]==arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,2)
    }
    if (arancione[3,2]==arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,3)
    }
    if (arancione[2,1]==arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,4)
    }
    return(n_adiacenze_arancione)
  }
  n_adiacenze_arancione <- adiacenze_arancione()
} # adiacenze arancione
{
  adiacenze_verde <- function(){
    n_adiacenze_verde <- c()
    if (verde[1,2]==verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,1)
    }
    if (verde[2,3]==verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,2)
    }
    if (verde[3,2]==verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,3)
    }
    if (verde[2,1]==verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,4)
    }
    return(n_adiacenze_verde)
  }
  n_adiacenze_verde <- adiacenze_verde()
} # adiacenze verde
{
  adiacenze_blu <- function(){
    n_adiacenze_blu <- c()
    if (blu[1,2]==blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,1)
    }
    if (blu[2,3]==blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,2)
    }
    if (blu[3,2]==blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,3)
    }
    if (blu[2,1]==blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,4)
    }
    return(n_adiacenze_blu)
  }
  n_adiacenze_blu <- adiacenze_blu()
} # adiacenze blu
{
  adiacenze_rosso <- function(){
    n_adiacenze_rosso <- c()
    if (rosso[1,2]==rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,1)
    }
    if (rosso[2,3]==rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,2)
    }
    if (rosso[3,2]==rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,3)
    }
    if (rosso[2,1]==rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,4)
    }
    return(n_adiacenze_rosso)
  }
  n_adiacenze_rosso <- adiacenze_rosso()
} # adiacenze rosso
{
  adiacenze_giallo <- function(){
    n_adiacenze_giallo <- c()
    if (giallo[1,2]==giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,1)
    }
    if (giallo[2,3]==giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,2)
    }
    if (giallo[3,2]==giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,3)
    }
    if (giallo[2,1]==giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,4)
    }
    return(n_adiacenze_giallo)
  }
  n_adiacenze_giallo <- adiacenze_giallo()
} # adiacenze giallo

# numero angoli adiacenti
possibili_ang <- rep(0,6)
{
  angoli_ad_bianco <- function(){
    n_angoli_bianco <- c()
    if (bianco[1,3]==bianco[1,2] && bianco[1,3]==bianco[2,3] && bianco[1,3]==bianco[2,2] 
        && verde[3,2]==verde[3,3] && arancione[1,1]==arancione[2,1]){
      n_angoli_bianco <- c(n_angoli_bianco,1)
    }
    if (bianco[3,3]==bianco[3,2] && bianco[3,3]==bianco[2,3] && bianco[3,3]==bianco[2,2]
        && arancione[2,1]==arancione[3,1] && blu[1,2]==blu[1,3]){
      n_angoli_bianco <- c(n_angoli_bianco,2)
    }
    if (bianco[3,1]==bianco[2,1] && bianco[3,1]==bianco[3,2] && bianco[3,1]==bianco[2,2]
        && blu[1,2]==blu[1,1] && rosso[2,3]==rosso[3,3]){
      n_angoli_bianco <- c(n_angoli_bianco,3)
    }
    if (bianco[1,1]==bianco[1,2] && bianco[1,1]==bianco[2,1] && bianco[1,1]==bianco[2,2]
        && rosso[2,3]==rosso[1,3] && verde[3,2]==verde[3,1]){
      n_angoli_bianco <- c(n_angoli_bianco,4)
    }
    return(n_angoli_bianco)
    return()
  }
  n_angoli_bianco <- angoli_ad_bianco()
  possibili_ang[1]<- length(n_angoli_bianco)
} # angoli ad. bianco
{
  angoli_ad_arancione <- function(){
    n_angoli_arancione <- c()
    if (arancione[1,3]==arancione[1,2] && arancione[1,3]==arancione[2,3] && arancione[1,3]==arancione[2,2]
        && verde[2,3]==verde[1,3] && giallo[2,3]==giallo[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,1)
    }
    if (arancione[3,3]==arancione[3,2] && arancione[3,3]==arancione[2,3] && arancione[3,3]==arancione[2,2]
        && giallo[1,3]==giallo[2,3] && blu[2,3]==blu[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,2)
    }
    if (arancione[3,1]==arancione[2,1] && arancione[3,1]==arancione[3,2] && arancione[3,1]==arancione[2,2]
        && blu[1,3]==blu[2,3] && bianco[2,3]==bianco[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,3)
    }
    if (arancione[1,1]==arancione[1,2] && arancione[1,1]==arancione[2,1] && arancione[1,1]==arancione[2,2]
        && bianco[1,3]==bianco[2,3] && verde[2,3]==verde[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,4)
    }
    return(n_angoli_arancione)
  }
  n_angoli_arancione <- angoli_ad_arancione()
  possibili_ang[2]<- length(n_angoli_arancione)
} # angoli ad. arancione
{
  angoli_ad_verde <- function(){
    n_angoli_verde <- c()
    if (verde[1,3]==verde[1,2] && verde[1,3]==verde[2,3] && verde[1,3]==verde[2,2]
        && giallo[1,3]==giallo[2,3] && arancione[1,2]==arancione[1,3]){
      n_angoli_verde <- c(n_angoli_verde,1)
    }
    if (verde[3,3]==verde[3,2] && verde[3,3]==verde[2,3] && verde[3,3]==verde[2,2]
        && arancione[1,1]==arancione[1,2] && bianco[1,2]==bianco[1,3]){
      n_angoli_verde <- c(n_angoli_verde,2)
    }
    if (verde[3,1]==verde[2,1] && verde[3,1]==verde[3,2] && verde[3,1]==verde[2,2]
        && bianco[1,1]==bianco[1,2] && rosso[1,2]==rosso[1,3]){
      n_angoli_verde <- c(n_angoli_verde,3)
    }
    if (verde[1,1]==verde[1,2] && verde[1,1]==verde[2,1] && verde[1,1]==verde[2,2]
        && rosso[1,1]==rosso[1,2] && giallo[3,1]==giallo[3,2]){
      n_angoli_verde <- c(n_angoli_verde,4)
    }
    return(n_angoli_verde)
  }
  n_angoli_verde <- angoli_ad_verde()
  possibili_ang[3]<- length(n_angoli_verde)
} # angoli ad. verde
{
  angoli_ad_blu <- function(){
    n_angoli_blu <- c()
    if (blu[1,3]==blu[1,2] && blu[1,3]==blu[2,3] && blu[1,3]==blu[2,2]
        && bianco[3,2]==bianco[3,3] && arancione[3,1]==arancione[3,2]){
      n_angoli_blu <- c(n_angoli_blu,1)
    }
    if (blu[3,3]==blu[3,2] && blu[3,3]==blu[2,3] && blu[3,3]==blu[2,2]
        && arancione[3,2]==arancione[3,3] && giallo[1,2]==giallo[1,3]){
      n_angoli_blu <- c(n_angoli_blu,2)
    }
    if (blu[3,1]==blu[2,1] && blu[3,1]==blu[3,2] && blu[3,1]==blu[2,2]
        && giallo[1,1]==giallo[1,2] && rosso[3,1]==rosso[3,2]){
      n_angoli_blu <- c(n_angoli_blu,3)
    }
    if (blu[1,1]==blu[1,2] && blu[1,1]==blu[2,1] && blu[1,1]==blu[2,2]
        && rosso[3,2]==rosso[3,3] && bianco[3,1]==bianco[3,2]){
      n_angoli_blu <- c(n_angoli_blu,4)
    }
    return(n_angoli_blu)
  }
  n_angoli_blu <- angoli_ad_blu()
  possibili_ang[4]<- length(n_angoli_blu)
} # angoli ad. blu
{
  angoli_ad_rosso <- function(){
    n_angoli_rosso <- c()
    if (rosso[1,3]==rosso[1,2] && rosso[1,3]==rosso[2,3] && rosso[1,3]==rosso[2,2]
        && verde[2,1]==verde[3,1] && bianco[1,1]==bianco[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,1)
    }
    if (rosso[3,3]==rosso[3,2] && rosso[3,3]==rosso[2,3] && rosso[3,3]==rosso[2,2]
        && bianco[2,1]==bianco[3,1] && blu[1,1]==blu[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,2)
    }
    if (rosso[3,1]==rosso[2,1] && rosso[3,1]==rosso[3,2] && rosso[3,1]==rosso[2,2]
        && blu[2,1]==blu[3,1] && giallo[1,1]==giallo[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,3)
    }
    if (rosso[1,1]==rosso[1,2] && rosso[1,1]==rosso[2,1] && rosso[1,1]==rosso[2,2]
        && giallo[2,1]==giallo[3,1] && verde[1,1]==verde[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,4)
    }
    return(n_angoli_rosso)
  }
  n_angoli_rosso <- angoli_ad_rosso()
  possibili_ang[5]<- length(n_angoli_rosso)
} # angoli ad. rosso
{
  angoli_ad_giallo <- function(){
    n_angoli_giallo <- c()
    if (giallo[1,3]==giallo[1,2] && giallo[1,3]==giallo[2,3] && giallo[1,3]==giallo[2,2]
        && blu[3,2]==blu[3,3] && arancione[2,3]==arancione[3,3]){
      n_angoli_giallo <- c(n_angoli_giallo,1)
    }
    if (giallo[3,3]==giallo[3,2] && giallo[3,3]==giallo[2,3] && giallo[3,3]==giallo[2,2]
        && arancione[1,3]==arancione[2,3] && verde[1,2]==verde[1,3]){
      n_angoli_giallo <- c(n_angoli_giallo,2)
    }
    if (giallo[3,1]==giallo[2,1] && giallo[3,1]==giallo[3,2] && giallo[3,1]==giallo[2,2]
        && verde[1,1]==verde[1,2] && rosso[1,1]==rosso[2,1]){
      n_angoli_giallo <- c(n_angoli_giallo,3)
    }
    if (giallo[1,1]==giallo[1,2] && giallo[1,1]==giallo[2,1] && giallo[1,1]==giallo[2,2]
        && rosso[2,1]==rosso[3,1] && blu[3,1]==blu[3,2]){
      n_angoli_giallo <- c(n_angoli_giallo,4)
    }
    return(n_angoli_giallo)
  }
  n_angoli_giallo <- angoli_ad_giallo()
  possibili_ang[6]<- length(n_angoli_giallo)
} # angoli ad. giallo
n_adiacenze_bianco
n_adiacenze_arancione
n_adiacenze_verde
n_adiacenze_blu
n_adiacenze_rosso
n_adiacenze_giallo

n_angoli_bianco
n_angoli_arancione
n_angoli_verde
n_angoli_blu
n_angoli_rosso
n_angoli_giallo




# Ok sono state fatte delle funzioni che ci possono far capire se all'interno di una faccia ci sono gruppi omogenei
# Per essere più precisi ci si può basare anche solo sugli angoli e a seconda del numero di angoli si avrà un'area 
# omogenea maggiore, in particolare:
# 1 angolo <- gruppo da 4
# 2 angoli <- gruppo da 6
# 3 angoli <- faccia completa tranne che per un numero
# 4 angoli <- faccia completa
# Notare che nelle condizioni per fare i gruppi ho aggiunto la necessità che anche le faccette dei colori adiacenti 
# corrispondano, altrimenti quei gruppi non funzionerebbero

# ora sono in grado di verificare se sul cubo sono presenti dei blocchi, in particolare nella condizione in cui siamo a
# due mosse dalla soluzione dal cubo abbiamo 2 blocchi da 1 angolo e 4 blocchi da 2 angoli
# 2 blocchi da un angolo e 2 blocchi da due angoli possono formare un blocco grande (3x2x2) mentre un blocco da 2 angoli
# corrisponde ad un blocco medio (3x2x1). Il blocco piccolo sarebbe una sola riga (3x1x1) ma in questo momento non la
# prendiamo in considerazione. Posta in questi termini, per arrivare a 2 mosse dalla soluzione abbiamo bisogno di 
# 1 blocco grande e due blocchi medi contemporaneamente.

# partiamo con il dare un valore ai vari blocchi


# calcolo di quanti blocchi (e quali) abbiamo:
punteggio<-0
blocchi_grandi <- 0
blocchi_medi <- 0
blocchi_piccoli <- 0
facce_giuste <- 0
# possibili_ang ci servirà per capire se ci sono blocchi grandi nel cubo. Ci dovrebbe essere al massimo un blocco grande
# in una combinazione.
possibili_ang
{
  condiz <- 0
  for (i in 1:6) {
    if (possibili_ang[i]==2){
      for (j in 1:6) {
        if (possibili_ang[j]==2 && j!=i && (j+i)!=7 ){
          for (k in 1:6) {
            if (possibili_ang[k]==1 && k!=i && k!=j && (k+i)!=7 && (k+j)!=7){
              condiz <- condiz+1
            }
            if (condiz==2){
              blocchi_grandi <- 1
              
            }
          }
        }
      }
    }
  }
} # calcolo blocco grande
blocchi_grandi

punteggio_bianco <- function(){
  angoli <- length(angoli_ad_bianco())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_bianco())>2*length(angoli_ad_bianco())){
    punteggio <- punteggio + (length(adiacenze_bianco())-2*length(angoli_ad_bianco()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_bianco() # punteggio bianco
punteggio

punteggio_arancione <- function(){
    angoli <- length(angoli_ad_arancione())
    if (angoli==4){
      punteggio <- punteggio+100
      facce_giuste <- facce_giuste+1
    } else {
      if (angoli==3){
        punteggio <- punteggio+50
      } else {
        if (angoli==2){
          punteggio <- punteggio+30
          blocchi_medi <- blocchi_medi+1
        } else {
          if (angoli==1){
            punteggio <- punteggio+15
            blocchi_piccoli <- blocchi_piccoli+1
          }
        }
      }
    }
    if (length(adiacenze_arancione())>2*length(angoli_ad_arancione())){
      punteggio <- punteggio + (length(adiacenze_arancione())-2*length(angoli_ad_arancione()))*3
    }
    return(punteggio)
  } 
punteggio <- punteggio_arancione() # punteggio arancione

punteggio_verde <- function(){
  angoli <- length(angoli_ad_verde())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_verde())>2*length(angoli_ad_verde())){
    punteggio <- punteggio + (length(adiacenze_verde())-2*length(angoli_ad_verde()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_verde()# punteggio verde

punteggio_blu <- function(){
  angoli <- length(angoli_ad_blu())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_blu())>2*length(angoli_ad_blu())){
    punteggio <- punteggio + (length(adiacenze_blu())-2*length(angoli_ad_blu()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_blu()# punteggio blu

punteggio_rosso <- function(){
  angoli <- length(angoli_ad_rosso())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_rosso())>2*length(angoli_ad_rosso())){
    punteggio <- punteggio + (length(adiacenze_rosso())-2*length(angoli_ad_rosso()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_rosso()# punteggio rosso

punteggio_giallo <- function(){
  angoli <- length(angoli_ad_giallo())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_giallo())>2*length(angoli_ad_giallo())){
    punteggio <- punteggio + (length(adiacenze_giallo())-2*length(angoli_ad_giallo()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_giallo() # punteggio giallo
punteggio
facce_giuste
blocchi_medi
blocchi_piccoli
blocchi_grandi


# bene ora siamo in grado di capire quanti e quali blocchi sono presenti in una possibile combinazione
# proviamo a fare il metodo 1 (mosse random) e vediamo quali valori otteniamo

sequenza_valori_1 <- rep(NA,10000)
sequenza_valori_1
valore_max_tot <- 0
valore_attuale <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                   blu,blu_0,rosso,rosso_0,giallo,giallo_0)
valore_attuale
n_sequenze <- 10000
punteggio <- 0
for (i in 1:n_sequenze) {
  numero <- sample(1:12,1)
  if (numero == 1){
    mossa1(bianco, blu, giallo, verde, arancione, rosso)
    risultato1 <- mossa1(bianco, blu, giallo, verde, arancione,rosso)
    bianco <- risultato1$bianco
    blu <- risultato1$blu
    giallo <- risultato1$giallo
    verde <- risultato1$verde
    arancione <- risultato1$arancione
    rosso <- risultato1$rosso
  } else {
    if (numero == 2) {
      mossa2(bianco, blu, giallo, verde, rosso, arancione)
      risultato2 <- mossa2(bianco, blu, giallo, verde, rosso, arancione)
      bianco <- risultato2$bianco
      blu <- risultato2$blu
      giallo <- risultato2$giallo
      verde <- risultato2$verde
      rosso <- risultato2$rosso
      arancione <- risultato2$arancione
    } else {
      if (numero == 3) {
        mossa3(bianco, arancione, giallo, verde, rosso, blu)
        risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
        bianco <- risultato3$bianco
        arancione <- risultato3$arancione
        giallo <- risultato3$giallo
        verde <- risultato3$verde
        rosso <- risultato3$rosso
        blu <- risultato3$blu
      } else {
        if (numero == 4){
          mossa4(bianco, arancione, giallo, blu, rosso, verde)
          risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
          bianco <- risultato4$bianco
          arancione <- risultato4$arancione
          giallo <- risultato4$giallo
          blu <- risultato4$blu
          rosso <- risultato4$rosso
          verde <- risultato4$verde
        } else {
          if (numero == 5){
            mossa5(bianco, arancione, verde, blu, rosso, giallo)
            risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
            bianco <- risultato5$bianco
            arancione <- risultato5$arancione
            verde <- risultato5$verde
            blu <- risultato5$blu
            rosso <- risultato5$rosso
            giallo <- risultato5$giallo
          } else {
            if (numero == 6){
              mossa6(giallo, arancione, verde, blu, rosso, bianco)
              risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
              giallo <- risultato6$giallo
              arancione <- risultato6$arancione
              verde <- risultato6$verde
              blu <- risultato6$blu
              rosso <- risultato6$rosso
              bianco <- risultato6$bianco
            } else {
              if (numero == 7){
                mossa7(giallo, arancione, verde, blu, rosso, bianco)
                risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato7$giallo
                arancione <- risultato7$arancione
                verde <- risultato7$verde
                blu <- risultato7$blu
                rosso <- risultato7$rosso
                bianco <- risultato7$bianco
              } else {
                if (numero == 8){
                  mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  bianco <- risultato8$bianco
                  arancione <- risultato8$arancione
                  verde <- risultato8$verde
                  blu <- risultato8$blu
                  rosso <- risultato8$rosso
                  giallo <- risultato8$giallo
                } else {
                  if (numero == 9){
                    mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    bianco <- risultato9$bianco
                    arancione <- risultato9$arancione
                    giallo <- risultato9$giallo
                    blu <- risultato9$blu
                    rosso <- risultato9$rosso
                    verde <- risultato9$verde
                  } else {
                    if (numero == 10){
                      mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      bianco <- risultato10$bianco
                      arancione <- risultato10$arancione
                      giallo <- risultato10$giallo
                      verde <- risultato10$verde
                      rosso <- risultato10$rosso
                      blu <- risultato10$blu
                    } else {
                      if (numero == 11){
                        mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        bianco <- risultato11$bianco
                        blu <- risultato11$blu
                        giallo <- risultato11$giallo
                        verde <- risultato11$verde
                        rosso <- risultato11$rosso
                        arancione <- risultato11$arancione
                      } else {
                        if (numero == 12){
                          mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          bianco <- risultato12$bianco
                          blu <- risultato12$blu
                          giallo <- risultato12$giallo
                          verde <- risultato12$verde
                          arancione <- risultato12$arancione
                          rosso <- risultato12$rosso
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  punteggio <- punteggio_bianco()
  punteggio <- punteggio_arancione()
  punteggio <- punteggio_verde()
  punteggio <- punteggio_blu()
  punteggio <- punteggio_rosso()
  punteggio <- punteggio_giallo() 
  
  sequenza_valori_1[i] <- punteggio
  punteggio<-0
}
sequenza_valori_1
hist(sequenza_valori_1)
facce()
mean(sequenza_valori_1)
max(sequenza_valori_1)
valore_max_tot

plot(sequenza_valori_1, type = "l", col = "blue", lwd = 2,
     main = "10,000 random moves with adjacencies",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))
abline(h = 600, col = "red", lwd = 2, lty = 2)
abline(h = max(sequenza_valori_1), col = "green", lwd = 2, lty = 2)


hist(sequenza_valori_1, main = "100000 random moves", col = "blue", xlab = "Values")
length(which(sequenza_valori_1>=30))
# dopo 100000 mosse random il massimo valore ottenuto is 54 su 600. 600 è il valore massimo ottenuto nel caso tutte 
# tutte le facce siano complete (100 punti per faccia). Da ciò si può dedurre che è estremamente difficile formare dei
# blocchi attraverso mosse random, appena 0.79% delle mosse hanno raggiunto il valore di 30 (e che quindi FORSE sono 
# riuscite a creare blocchi piccoli)
# Se vogliamo controllare solo i punteggi basati su blocchi (e quindii senza tenere in considerazione i punteggi extra
# dovuti dalle adiacenze extra) saremo molto più abili a capire quanto è effettivamente probabile ottenere i vari blocchi.

# prima di tutto modifichiamo come vengono calcolati i punteggi
punteggio_bianco <- function(){
  angoli <- length(angoli_ad_bianco())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_bianco() # punteggio bianco
punteggio

punteggio_arancione <- function(){
  angoli <- length(angoli_ad_arancione())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
} 
punteggio <- punteggio_arancione() # punteggio arancione

punteggio_verde <- function(){
  angoli <- length(angoli_ad_verde())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_verde()# punteggio verde

punteggio_blu <- function(){
  angoli <- length(angoli_ad_blu())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_blu()# punteggio blu

punteggio_rosso <- function(){
  angoli <- length(angoli_ad_rosso())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_rosso()# punteggio rosso

punteggio_giallo <- function(){
  angoli <- length(angoli_ad_giallo())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_giallo() # punteggio giallo
punteggio

# ora ricalcoliamo i punteggi su 100000 mosse random

sequenza_valori_1 <- rep(NA,10000)
sequenza_valori_1
valore_max_tot <- 0
valore_attuale <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                   blu,blu_0,rosso,rosso_0,giallo,giallo_0)
valore_attuale
n_sequenze <- 10000
punteggio <- 0
for (i in 1:n_sequenze) {
  numero <- sample(1:12,1)
  if (numero == 1){
    mossa1(bianco, blu, giallo, verde, arancione, rosso)
    risultato1 <- mossa1(bianco, blu, giallo, verde, arancione,rosso)
    bianco <- risultato1$bianco
    blu <- risultato1$blu
    giallo <- risultato1$giallo
    verde <- risultato1$verde
    arancione <- risultato1$arancione
    rosso <- risultato1$rosso
  } else {
    if (numero == 2) {
      mossa2(bianco, blu, giallo, verde, rosso, arancione)
      risultato2 <- mossa2(bianco, blu, giallo, verde, rosso, arancione)
      bianco <- risultato2$bianco
      blu <- risultato2$blu
      giallo <- risultato2$giallo
      verde <- risultato2$verde
      rosso <- risultato2$rosso
      arancione <- risultato2$arancione
    } else {
      if (numero == 3) {
        mossa3(bianco, arancione, giallo, verde, rosso, blu)
        risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
        bianco <- risultato3$bianco
        arancione <- risultato3$arancione
        giallo <- risultato3$giallo
        verde <- risultato3$verde
        rosso <- risultato3$rosso
        blu <- risultato3$blu
      } else {
        if (numero == 4){
          mossa4(bianco, arancione, giallo, blu, rosso, verde)
          risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
          bianco <- risultato4$bianco
          arancione <- risultato4$arancione
          giallo <- risultato4$giallo
          blu <- risultato4$blu
          rosso <- risultato4$rosso
          verde <- risultato4$verde
        } else {
          if (numero == 5){
            mossa5(bianco, arancione, verde, blu, rosso, giallo)
            risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
            bianco <- risultato5$bianco
            arancione <- risultato5$arancione
            verde <- risultato5$verde
            blu <- risultato5$blu
            rosso <- risultato5$rosso
            giallo <- risultato5$giallo
          } else {
            if (numero == 6){
              mossa6(giallo, arancione, verde, blu, rosso, bianco)
              risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
              giallo <- risultato6$giallo
              arancione <- risultato6$arancione
              verde <- risultato6$verde
              blu <- risultato6$blu
              rosso <- risultato6$rosso
              bianco <- risultato6$bianco
            } else {
              if (numero == 7){
                mossa7(giallo, arancione, verde, blu, rosso, bianco)
                risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato7$giallo
                arancione <- risultato7$arancione
                verde <- risultato7$verde
                blu <- risultato7$blu
                rosso <- risultato7$rosso
                bianco <- risultato7$bianco
              } else {
                if (numero == 8){
                  mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                  bianco <- risultato8$bianco
                  arancione <- risultato8$arancione
                  verde <- risultato8$verde
                  blu <- risultato8$blu
                  rosso <- risultato8$rosso
                  giallo <- risultato8$giallo
                } else {
                  if (numero == 9){
                    mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                    bianco <- risultato9$bianco
                    arancione <- risultato9$arancione
                    giallo <- risultato9$giallo
                    blu <- risultato9$blu
                    rosso <- risultato9$rosso
                    verde <- risultato9$verde
                  } else {
                    if (numero == 10){
                      mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                      bianco <- risultato10$bianco
                      arancione <- risultato10$arancione
                      giallo <- risultato10$giallo
                      verde <- risultato10$verde
                      rosso <- risultato10$rosso
                      blu <- risultato10$blu
                    } else {
                      if (numero == 11){
                        mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                        bianco <- risultato11$bianco
                        blu <- risultato11$blu
                        giallo <- risultato11$giallo
                        verde <- risultato11$verde
                        rosso <- risultato11$rosso
                        arancione <- risultato11$arancione
                      } else {
                        if (numero == 12){
                          mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                          bianco <- risultato12$bianco
                          blu <- risultato12$blu
                          giallo <- risultato12$giallo
                          verde <- risultato12$verde
                          arancione <- risultato12$arancione
                          rosso <- risultato12$rosso
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  punteggio <- punteggio_bianco()
  punteggio <- punteggio_arancione()
  punteggio <- punteggio_verde()
  punteggio <- punteggio_blu()
  punteggio <- punteggio_rosso()
  punteggio <- punteggio_giallo() 
  
  sequenza_valori_1[i] <- punteggio
  punteggio<-0
}
sequenza_valori_1
hist(sequenza_valori_1)
facce()
mean(sequenza_valori_1)
max(sequenza_valori_1)
valore_max_tot

plot(sequenza_valori_1, type = "l", col = "blue", lwd = 2,
     main = "10,000 random moves without adjacencies",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))
abline(h = 600, col = "red", lwd = 2, lty = 2)
abline(h = max(sequenza_valori_1), col = "green", lwd = 2, lty = 2)

hist(sequenza_valori_1, main = "100000 random moves", col = "blue", xlab = "Values")
length(which(sequenza_valori_1>=30))
length(which(sequenza_valori_1>=15))
# Appena 16 volte su 100000 abbiamo raggiunto un punteggio di almeno 30 punti: solo 0.016%. 
# Abbiamo trovato almeno un blocco (punteggio di almeno 15) solo 738 volte: lo 0.738% delle mosse.
# Neanche una volta siamo riusciti ad ottenere un blocco grande o una faccia risolta. 

# Adesso proverò il metodo 2: prima solo sui blocchi e poi con le adiacenze extra (punti extra per ogni adiacenze che non 
# fa parte di un blocco) così vediamo se otteniamo qualche miglioramento di valori. Userò direttamente sequenze di 3 mosse.

# modifichiamo i punteggi per farli sulle basi 
stato_attuale$bianco
{
  adiacenze_bianco <- function(){
    n_adiacenze_bianco <- c()
    if (stato_attuale$bianco[1,2]==stato_attuale$bianco[2,2]){
      n_adiacenze_bianco <- c(n_adiacenze_bianco,1)
    }
    if (stato_attuale$bianco[2,3]==stato_attuale$bianco[2,2]){
      n_adiacenze_bianco <- c(n_adiacenze_bianco,2)
    }
    if (stato_attuale$bianco[3,2]==stato_attuale$bianco[2,2]){
      n_adiacenze_bianco <- c(n_adiacenze_bianco,3)
    }
    if (stato_attuale$bianco[2,1]==stato_attuale$bianco[2,2]){
      n_adiacenze_bianco <- c(n_adiacenze_bianco,4)
    }
    return(n_adiacenze_bianco)
  }
  n_adiacenze_bianco <- adiacenze_bianco()
} # adiacenze bianco
{
  adiacenze_arancione <- function(){
    n_adiacenze_arancione <- c()
    if (stato_attuale$arancione[1,2]==stato_attuale$arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,1)
    }
    if (stato_attuale$arancione[2,3]==stato_attuale$arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,2)
    }
    if (stato_attuale$arancione[3,2]==stato_attuale$arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,3)
    }
    if (stato_attuale$arancione[2,1]==stato_attuale$arancione[2,2]){
      n_adiacenze_arancione <- c(n_adiacenze_arancione,4)
    }
    return(n_adiacenze_arancione)
  }
  n_adiacenze_arancione <- adiacenze_arancione()
} # adiacenze arancione
{
  adiacenze_verde <- function(){
    n_adiacenze_verde <- c()
    if (stato_attuale$verde[1,2]==stato_attuale$verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,1)
    }
    if (stato_attuale$verde[2,3]==stato_attuale$verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,2)
    }
    if (stato_attuale$verde[3,2]==stato_attuale$verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,3)
    }
    if (stato_attuale$verde[2,1]==stato_attuale$verde[2,2]){
      n_adiacenze_verde <- c(n_adiacenze_verde,4)
    }
    return(n_adiacenze_verde)
  }
  n_adiacenze_verde <- adiacenze_verde()
} # adiacenze verde
{
  adiacenze_blu <- function(){
    n_adiacenze_blu <- c()
    if (stato_attuale$blu[1,2]==stato_attuale$blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,1)
    }
    if (stato_attuale$blu[2,3]==stato_attuale$blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,2)
    }
    if (stato_attuale$blu[3,2]==stato_attuale$blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,3)
    }
    if (stato_attuale$blu[2,1]==stato_attuale$blu[2,2]){
      n_adiacenze_blu <- c(n_adiacenze_blu,4)
    }
    return(n_adiacenze_blu)
  }
  n_adiacenze_blu <- adiacenze_blu()
} # adiacenze blu
{
  adiacenze_rosso <- function(){
    n_adiacenze_rosso <- c()
    if (stato_attuale$rosso[1,2]==stato_attuale$rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,1)
    }
    if (stato_attuale$rosso[2,3]==stato_attuale$rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,2)
    }
    if (stato_attuale$rosso[3,2]==stato_attuale$rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,3)
    }
    if (stato_attuale$rosso[2,1]==stato_attuale$rosso[2,2]){
      n_adiacenze_rosso <- c(n_adiacenze_rosso,4)
    }
    return(n_adiacenze_rosso)
  }
  n_adiacenze_rosso <- adiacenze_rosso()
} # adiacenze rosso
{
  adiacenze_giallo <- function(){
    n_adiacenze_giallo <- c()
    if (stato_attuale$giallo[1,2]==stato_attuale$giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,1)
    }
    if (stato_attuale$giallo[2,3]==stato_attuale$giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,2)
    }
    if (stato_attuale$giallo[3,2]==stato_attuale$giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,3)
    }
    if (stato_attuale$giallo[2,1]==stato_attuale$giallo[2,2]){
      n_adiacenze_giallo <- c(n_adiacenze_giallo,4)
    }
    return(n_adiacenze_giallo)
  }
  n_adiacenze_giallo <- adiacenze_giallo()
} # adiacenze giallo

# numero angoli adiacenti
possibili_ang <- rep(0,6)
{
  angoli_ad_bianco <- function(){
    n_angoli_bianco <- c()
    if (stato_attuale$bianco[1,3]==stato_attuale$bianco[1,2] && stato_attuale$bianco[1,3]==stato_attuale$bianco[2,3] && stato_attuale$bianco[1,3]==stato_attuale$bianco[2,2] 
        && stato_attuale$verde[3,2]==stato_attuale$verde[3,3] && stato_attuale$arancione[1,1]==stato_attuale$arancione[2,1]){
      n_angoli_bianco <- c(n_angoli_bianco,1)
    }
    if (stato_attuale$bianco[3,3]==stato_attuale$bianco[3,2] && stato_attuale$bianco[3,3]==stato_attuale$bianco[2,3] && stato_attuale$bianco[3,3]==stato_attuale$bianco[2,2]
        && stato_attuale$arancione[2,1]==stato_attuale$arancione[3,1] && stato_attuale$blu[1,2]==stato_attuale$blu[1,3]){
      n_angoli_bianco <- c(n_angoli_bianco,2)
    }
    if (stato_attuale$bianco[3,1]==stato_attuale$bianco[2,1] && stato_attuale$bianco[3,1]==stato_attuale$bianco[3,2] && stato_attuale$bianco[3,1]==stato_attuale$bianco[2,2]
        && stato_attuale$blu[1,2]==stato_attuale$blu[1,1] && stato_attuale$rosso[2,3]==stato_attuale$rosso[3,3]){
      n_angoli_bianco <- c(n_angoli_bianco,3)
    }
    if (stato_attuale$bianco[1,1]==stato_attuale$bianco[1,2] && stato_attuale$bianco[1,1]==stato_attuale$bianco[2,1] && stato_attuale$bianco[1,1]==stato_attuale$bianco[2,2]
        && stato_attuale$rosso[2,3]==stato_attuale$rosso[1,3] && stato_attuale$verde[3,2]==stato_attuale$verde[3,1]){
      n_angoli_bianco <- c(n_angoli_bianco,4)
    }
    return(n_angoli_bianco)
    return()
  }
  n_angoli_bianco <- angoli_ad_bianco()
  possibili_ang[1]<- length(n_angoli_bianco)
} # angoli ad. bianco
{
  angoli_ad_arancione <- function(){
    n_angoli_arancione <- c()
    if (stato_attuale$arancione[1,3]==stato_attuale$arancione[1,2] && stato_attuale$arancione[1,3]==stato_attuale$arancione[2,3] && stato_attuale$arancione[1,3]==stato_attuale$arancione[2,2]
        && stato_attuale$verde[2,3]==stato_attuale$verde[1,3] && stato_attuale$giallo[2,3]==stato_attuale$giallo[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,1)
    }
    if (stato_attuale$arancione[3,3]==stato_attuale$arancione[3,2] && stato_attuale$arancione[3,3]==stato_attuale$arancione[2,3] && stato_attuale$arancione[3,3]==stato_attuale$arancione[2,2]
        && stato_attuale$giallo[1,3]==stato_attuale$giallo[2,3] && stato_attuale$blu[2,3]==stato_attuale$blu[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,2)
    }
    if (stato_attuale$arancione[3,1]==stato_attuale$arancione[2,1] && stato_attuale$arancione[3,1]==stato_attuale$arancione[3,2] && stato_attuale$arancione[3,1]==stato_attuale$arancione[2,2]
        && stato_attuale$blu[1,3]==stato_attuale$blu[2,3] && stato_attuale$bianco[2,3]==stato_attuale$bianco[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,3)
    }
    if (stato_attuale$arancione[1,1]==stato_attuale$arancione[1,2] && stato_attuale$arancione[1,1]==stato_attuale$arancione[2,1] && stato_attuale$arancione[1,1]==stato_attuale$arancione[2,2]
        && stato_attuale$bianco[1,3]==stato_attuale$bianco[2,3] && stato_attuale$verde[2,3]==stato_attuale$verde[3,3]){
      n_angoli_arancione <- c(n_angoli_arancione,4)
    }
    return(n_angoli_arancione)
  }
  n_angoli_arancione <- angoli_ad_arancione()
  possibili_ang[2]<- length(n_angoli_arancione)
} # angoli ad. arancione
{
  angoli_ad_verde <- function(){
    n_angoli_verde <- c()
    if (stato_attuale$verde[1,3]==stato_attuale$verde[1,2] && stato_attuale$verde[1,3]==stato_attuale$verde[2,3] && stato_attuale$verde[1,3]==stato_attuale$verde[2,2]
        && stato_attuale$giallo[1,3]==stato_attuale$giallo[2,3] && stato_attuale$arancione[1,2]==stato_attuale$arancione[1,3]){
      n_angoli_verde <- c(n_angoli_verde,1)
    }
    if (stato_attuale$verde[3,3]==stato_attuale$verde[3,2] && stato_attuale$verde[3,3]==stato_attuale$verde[2,3] && stato_attuale$verde[3,3]==stato_attuale$verde[2,2]
        && stato_attuale$arancione[1,1]==stato_attuale$arancione[1,2] && stato_attuale$bianco[1,2]==stato_attuale$bianco[1,3]){
      n_angoli_verde <- c(n_angoli_verde,2)
    }
    if (stato_attuale$verde[3,1]==stato_attuale$verde[2,1] && stato_attuale$verde[3,1]==stato_attuale$verde[3,2] && stato_attuale$verde[3,1]==stato_attuale$verde[2,2]
        && stato_attuale$bianco[1,1]==stato_attuale$bianco[1,2] && stato_attuale$rosso[1,2]==stato_attuale$rosso[1,3]){
      n_angoli_verde <- c(n_angoli_verde,3)
    }
    if (stato_attuale$verde[1,1]==stato_attuale$verde[1,2] && stato_attuale$verde[1,1]==stato_attuale$verde[2,1] && stato_attuale$verde[1,1]==stato_attuale$verde[2,2]
        && stato_attuale$rosso[1,1]==stato_attuale$rosso[1,2] && stato_attuale$giallo[3,1]==stato_attuale$giallo[3,2]){
      n_angoli_verde <- c(n_angoli_verde,4)
    }
    return(n_angoli_verde)
  }
  n_angoli_verde <- angoli_ad_verde()
  possibili_ang[3]<- length(n_angoli_verde)
} # angoli ad. verde
{
  angoli_ad_blu <- function(){
    n_angoli_blu <- c()
    if (stato_attuale$blu[1,3]==stato_attuale$blu[1,2] && stato_attuale$blu[1,3]==stato_attuale$blu[2,3] && stato_attuale$blu[1,3]==stato_attuale$blu[2,2]
        && stato_attuale$bianco[3,2]==stato_attuale$bianco[3,3] && stato_attuale$arancione[3,1]==stato_attuale$arancione[3,2]){
      n_angoli_blu <- c(n_angoli_blu,1)
    }
    if (stato_attuale$blu[3,3]==stato_attuale$blu[3,2] && stato_attuale$blu[3,3]==stato_attuale$blu[2,3] && stato_attuale$blu[3,3]==stato_attuale$blu[2,2]
        && stato_attuale$arancione[3,2]==stato_attuale$arancione[3,3] && stato_attuale$giallo[1,2]==stato_attuale$giallo[1,3]){
      n_angoli_blu <- c(n_angoli_blu,2)
    }
    if (stato_attuale$blu[3,1]==stato_attuale$blu[2,1] && stato_attuale$blu[3,1]==stato_attuale$blu[3,2] && stato_attuale$blu[3,1]==stato_attuale$blu[2,2]
        && stato_attuale$giallo[1,1]==stato_attuale$giallo[1,2] && stato_attuale$rosso[3,1]==stato_attuale$rosso[3,2]){
      n_angoli_blu <- c(n_angoli_blu,3)
    }
    if (stato_attuale$blu[1,1]==stato_attuale$blu[1,2] && stato_attuale$blu[1,1]==stato_attuale$blu[2,1] && stato_attuale$blu[1,1]==stato_attuale$blu[2,2]
        && stato_attuale$rosso[3,2]==stato_attuale$rosso[3,3] && stato_attuale$bianco[3,1]==stato_attuale$bianco[3,2]){
      n_angoli_blu <- c(n_angoli_blu,4)
    }
    return(n_angoli_blu)
  }
  n_angoli_blu <- angoli_ad_blu()
  possibili_ang[4]<- length(n_angoli_blu)
} # angoli ad. blu
{
  angoli_ad_rosso <- function(){
    n_angoli_rosso <- c()
    if (stato_attuale$rosso[1,3]==stato_attuale$rosso[1,2] && stato_attuale$rosso[1,3]==stato_attuale$rosso[2,3] && stato_attuale$rosso[1,3]==stato_attuale$rosso[2,2]
        && stato_attuale$verde[2,1]==stato_attuale$verde[3,1] && stato_attuale$bianco[1,1]==stato_attuale$bianco[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,1)
    }
    if (stato_attuale$rosso[3,3]==stato_attuale$rosso[3,2] && stato_attuale$rosso[3,3]==stato_attuale$rosso[2,3] && stato_attuale$rosso[3,3]==stato_attuale$rosso[2,2]
        && stato_attuale$bianco[2,1]==stato_attuale$bianco[3,1] && stato_attuale$blu[1,1]==stato_attuale$blu[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,2)
    }
    if (stato_attuale$rosso[3,1]==stato_attuale$rosso[2,1] && stato_attuale$rosso[3,1]==stato_attuale$rosso[3,2] && stato_attuale$rosso[3,1]==stato_attuale$rosso[2,2]
        && stato_attuale$blu[2,1]==stato_attuale$blu[3,1] && stato_attuale$giallo[1,1]==stato_attuale$giallo[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,3)
    }
    if (stato_attuale$rosso[1,1]==stato_attuale$rosso[1,2] && stato_attuale$rosso[1,1]==stato_attuale$rosso[2,1] && stato_attuale$rosso[1,1]==stato_attuale$rosso[2,2]
        && stato_attuale$giallo[2,1]==stato_attuale$giallo[3,1] && stato_attuale$verde[1,1]==stato_attuale$verde[2,1]){
      n_angoli_rosso <- c(n_angoli_rosso,4)
    }
    return(n_angoli_rosso)
  }
  n_angoli_rosso <- angoli_ad_rosso()
  possibili_ang[5]<- length(n_angoli_rosso)
} # angoli ad. rosso
{
  angoli_ad_giallo <- function(){
    n_angoli_giallo <- c()
    if (stato_attuale$giallo[1,3]==stato_attuale$giallo[1,2] && stato_attuale$giallo[1,3]==stato_attuale$giallo[2,3] && stato_attuale$giallo[1,3]==stato_attuale$giallo[2,2]
        && stato_attuale$blu[3,2]==stato_attuale$blu[3,3] && stato_attuale$arancione[2,3]==stato_attuale$arancione[3,3]){
      n_angoli_giallo <- c(n_angoli_giallo,1)
    }
    if (stato_attuale$giallo[3,3]==stato_attuale$giallo[3,2] && stato_attuale$giallo[3,3]==stato_attuale$giallo[2,3] && stato_attuale$giallo[3,3]==stato_attuale$giallo[2,2]
        && stato_attuale$arancione[1,3]==stato_attuale$arancione[2,3] && stato_attuale$verde[1,2]==stato_attuale$verde[1,3]){
      n_angoli_giallo <- c(n_angoli_giallo,2)
    }
    if (stato_attuale$giallo[3,1]==stato_attuale$giallo[2,1] && stato_attuale$giallo[3,1]==stato_attuale$giallo[3,2] && stato_attuale$giallo[3,1]==stato_attuale$giallo[2,2]
        && stato_attuale$verde[1,1]==stato_attuale$verde[1,2] && stato_attuale$rosso[1,1]==stato_attuale$rosso[2,1]){
      n_angoli_giallo <- c(n_angoli_giallo,3)
    }
    if (stato_attuale$giallo[1,1]==stato_attuale$giallo[1,2] && stato_attuale$giallo[1,1]==stato_attuale$giallo[2,1] && stato_attuale$giallo[1,1]==stato_attuale$giallo[2,2]
        && stato_attuale$rosso[2,1]==stato_attuale$rosso[3,1] && stato_attuale$blu[3,1]==stato_attuale$blu[3,2]){
      n_angoli_giallo <- c(n_angoli_giallo,4)
    }
    return(n_angoli_giallo)
  }
  n_angoli_giallo <- angoli_ad_giallo()
  possibili_ang[6]<- length(n_angoli_giallo)
} # angoli ad. giallo

# runnare di nuovo le funzioni di calcolo dei vari punteggi delle facce (punteggio_bianco ecc) in modo da modificarli in 
# funzione del cambiamento dei punteggi sulle basi

i<-0
k<-0
n_sequenze <- 100000
valore_attuale <- 0
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
valore_max_tot <-0
sequenza_valori_3 <- rep(0,100000)
sequenza_valori_max_3 <- rep(0,100000)
punteggio<-0
for (i in 1:n_sequenze) {
  for (k in 1:4) {
    numero <- sample(1:12,1)
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
      stato_attuale <- risultato1
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
        stato_attuale <- risultato2
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
          stato_attuale <- risultato3
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
            stato_attuale <- risultato4
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
              stato_attuale <- risultato5
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
                stato_attuale <- risultato6
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                  stato_attuale <- risultato7
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                    stato_attuale <- risultato8
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                      stato_attuale <- risultato9
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                        stato_attuale <- risultato10
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                          stato_attuale <- risultato11
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                            stato_attuale <- risultato12
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  punteggio <- punteggio_bianco()
  punteggio <- punteggio_arancione()
  punteggio <- punteggio_verde()
  punteggio <- punteggio_blu()
  punteggio <- punteggio_rosso()
  punteggio <- punteggio_giallo() 
  
  sequenza_valori_1[i] <- punteggio
  if (punteggio >= valore_max_tot) {
    valore_max_tot <- punteggio
    base_bianco <- stato_attuale$bianco
    base_arancione <- stato_attuale$arancione
    base_verde <-  stato_attuale$verde
    base_blu <- stato_attuale$blu
    base_rosso <- stato_attuale$rosso
    base_giallo <- stato_attuale$giallo
  } else {
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
  }
  sequenza_valori_max_3[i] <- valore_max_tot
  punteggio<-0
}




sequenza_valori_3
sequenza_valori_max_3

plot(sequenza_valori_3, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 3 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))
abline(h = 600, col = "red", lwd = 2, lty = 2)
abline(h = max(sequenza_valori_1), col = "green", lwd = 2, lty = 2)

plot(sequenza_valori_max_3, type = "l", col = "blue", lwd = 2,
     main = "Highest values with adjacencies",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))
abline(h = 600, col = "red", lwd = 2, lty = 2)

facce()
max(sequenza_valori_max_3)
hist(sequenza_valori_3)
hist(sequenza_valori_max_3)
mean(sequenza_valori_3)
mean(sequenza_valori_max_3)
# Se prendiamo i valori senza contare le adiacenze extra si raggiunge un valore convergente di valore 100. Questo significa che 
# siamo riusciti ad ottenere una faccia intera risolta. In pratica uno strato su 3 è stato perfettamente risolto, ma in questo caso
# si raggiunge una convergenza locale da cui è impossible uscire in 3 mosse (e che richiederebbe troppi calcoli per migliorare con 4 
# o più mosse).
# Cosa succede se ripetiamo agginugendo le adiacenze?


punteggio_bianco <- function(){
  angoli <- length(angoli_ad_bianco())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_bianco())>2*length(angoli_ad_bianco())){
    punteggio <- punteggio + (length(adiacenze_bianco())-2*length(angoli_ad_bianco()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_bianco() # punteggio bianco
punteggio

punteggio_arancione <- function(){
  angoli <- length(angoli_ad_arancione())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_arancione())>2*length(angoli_ad_arancione())){
    punteggio <- punteggio + (length(adiacenze_arancione())-2*length(angoli_ad_arancione()))*3
  }
  return(punteggio)
} 
punteggio <- punteggio_arancione() # punteggio arancione

punteggio_verde <- function(){
  angoli <- length(angoli_ad_verde())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_verde())>2*length(angoli_ad_verde())){
    punteggio <- punteggio + (length(adiacenze_verde())-2*length(angoli_ad_verde()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_verde()# punteggio verde

punteggio_blu <- function(){
  angoli <- length(angoli_ad_blu())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_blu())>2*length(angoli_ad_blu())){
    punteggio <- punteggio + (length(adiacenze_blu())-2*length(angoli_ad_blu()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_blu()# punteggio blu

punteggio_rosso <- function(){
  angoli <- length(angoli_ad_rosso())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_rosso())>2*length(angoli_ad_rosso())){
    punteggio <- punteggio + (length(adiacenze_rosso())-2*length(angoli_ad_rosso()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_rosso()# punteggio rosso

punteggio_giallo <- function(){
  angoli <- length(angoli_ad_giallo())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  if (length(adiacenze_giallo())>2*length(angoli_ad_giallo())){
    punteggio <- punteggio + (length(adiacenze_giallo())-2*length(angoli_ad_giallo()))*3
  }
  return(punteggio)
}
punteggio <- punteggio_giallo() # punteggio giallo
punteggio

i<-0
k<-0
n_sequenze <- 100000
valore_attuale <- 0
base_bianco <- bianco
base_arancione <- arancione
base_verde <- verde
base_blu <- blu
base_rosso <- rosso
base_giallo <- giallo
valore_max_tot <-0
sequenza_valori_3 <- rep(0,100000)
sequenza_valori_max_3 <- rep(0,100000)
punteggio<-0
for (i in 1:n_sequenze) {
  for (k in 1:4) {
    numero <- sample(1:12,1)
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
      stato_attuale <- risultato1
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
        stato_attuale <- risultato2
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
          stato_attuale <- risultato3
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
            stato_attuale <- risultato4
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
              stato_attuale <- risultato5
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
                stato_attuale <- risultato6
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                  stato_attuale <- risultato7
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                    stato_attuale <- risultato8
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                      stato_attuale <- risultato9
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                        stato_attuale <- risultato10
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                          stato_attuale <- risultato11
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                            stato_attuale <- risultato12
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  punteggio <- punteggio_bianco()
  punteggio <- punteggio_arancione()
  punteggio <- punteggio_verde()
  punteggio <- punteggio_blu()
  punteggio <- punteggio_rosso()
  punteggio <- punteggio_giallo() 
  
  sequenza_valori_1[i] <- punteggio
  if (punteggio >= valore_max_tot) {
    valore_max_tot <- punteggio
    base_bianco <- stato_attuale$bianco
    base_arancione <- stato_attuale$arancione
    base_verde <-  stato_attuale$verde
    base_blu <- stato_attuale$blu
    base_rosso <- stato_attuale$rosso
    base_giallo <- stato_attuale$giallo
  } else {
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
  }
  sequenza_valori_max_3[i] <- valore_max_tot
  punteggio<-0
}




sequenza_valori_3
sequenza_valori_max_3

plot(sequenza_valori_3, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 3 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))
abline(h = 600, col = "red", lwd = 2, lty = 2)

plot(sequenza_valori_max_3, type = "l", col = "blue", lwd = 2,
     main = "Highest values with a sequence of 3 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))
abline(h = 600, col = "red", lwd = 2, lty = 2)
max(sequenza_valori_max_3)
facce()

hist(sequenza_valori_3)
hist(sequenza_valori_max_3)
mean(sequenza_valori_3)
mean(sequenza_valori_max_3)
valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                 blu,blu_0,rosso,rosso_0,giallo,giallo_0)

# I risultati sono ottimi. Il valore massimo di 165 su 600 sembra basso ma in verità è molto alto. Per 
# interdenci se valuto questa combinazione con il vecchio metodo delle faccette corrispondenti si ottiene
# 36 su 48, molto più alto di ciò che si era ottenuto con lo stesso metodo precedentemente. 
# Inoltre, se vogliamo raggiungere lo stato del cubo a 2 mosse dalla fine, il valore (con il nuovo metodo
# di valutazione) dovrebbe essere di 150. 

# A questo punto non ci resta che provare ad applicare l'ACO. Farò 4 prove: le prime 2 a cercare il 
# valore massimo (1 senza adiacenze e 1 con adiacenze), le ultime 2 a cercare di convergere il più
# possibile verso il valore di 150, cercando magari di aiutarmi mettendo come condizioni il numero e 
# tipo esatto di blocchi che bisogna avere per raggiungere quello stato.
# Anche per l'ACO partirò direttamente da sequenze di 3 mosse.
facce()
## PRIMA DI RUNNARE QUESTO ALGORITMO, RUNNARE DI NUOVO LE FUNZIONI DI ADIACENZA INIZIALI DEGLI SPIGOLI E DEGLI ANGOLI
# PERCHE' ALTRIMENTI NON VIENE CALCOLATO BENE IL PUNTEGGIO E SEMBRERA' CHE RIMANGA COSTANTE
n_formiche <- 50
diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
diff_valori


mosse_per_sequenza <- 3
sequenza_mosse <- cbind(c(rep(1,12^(mosse_per_sequenza-1)),rep(2,12^(mosse_per_sequenza-1)),rep(3,12^(mosse_per_sequenza-1)),
                          rep(4,12^(mosse_per_sequenza-1)),rep(5,12^(mosse_per_sequenza-1)),rep(6,12^(mosse_per_sequenza-1)),
                          rep(7,12^(mosse_per_sequenza-1)),rep(8,12^(mosse_per_sequenza-1)),rep(9,12^(mosse_per_sequenza-1)),
                          rep(10,12^(mosse_per_sequenza-1)),rep(11,12^(mosse_per_sequenza-1)),rep(12,12^(mosse_per_sequenza-1))),
                        rep(c(rep(1,12^(mosse_per_sequenza-2)),rep(2,12^(mosse_per_sequenza-2)),rep(3,12^(mosse_per_sequenza-2)),
                              rep(4,12^(mosse_per_sequenza-2)),rep(5,12^(mosse_per_sequenza-2)),rep(6,12^(mosse_per_sequenza-2)),
                              rep(7,12^(mosse_per_sequenza-2)),rep(8,12^(mosse_per_sequenza-2)),rep(9,12^(mosse_per_sequenza-2)),
                              rep(10,12^(mosse_per_sequenza-2)),rep(11,12^(mosse_per_sequenza-2)),rep(12,12^(mosse_per_sequenza-2))),12),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12^(mosse_per_sequenza-1)))
sequenza_mosse <- cbind(sequenza_mosse,rep(1000,12^(mosse_per_sequenza)))
sequenza_mosse

n_iterazioni <- 10000
valori_aco1 <- rep(NA,n_iterazioni)
valori_aco2 <- rep(NA,n_iterazioni)

punteggio <- 0
for (p in 1:n_iterazioni) {
  feromoni_cumul <- cumsum(sequenza_mosse[,4])
  valore_base <- punteggio
  punteggio <- 0
  valori_aco1[p] <- valore_base
  valore_base2 <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                  base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
  valori_aco2[p] <- valore_base2
  for (i in 1:n_formiche) {
    valore <- sample(1:sum(sequenza_mosse[,4]),1)
    scelta <- which(feromoni_cumul>=valore)[1]
    scelta
    for (k in 1:mosse_per_sequenza) {
      numero <- sequenza_mosse[scelta,k]
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      
    }
    punteggio <- punteggio_bianco()
    punteggio <- punteggio_arancione()
    punteggio <- punteggio_verde()
    punteggio <- punteggio_blu()
    punteggio <- punteggio_rosso()
    punteggio <- punteggio_giallo() 
    
    diff_valori[i,1] <- scelta
    diff_valori[i,2] <- punteggio - valore_base
    punteggio <- 0
    # fine per ogni formica
    # calcolare la differenza in valore 
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
    # fine per tutte le formiche
    # qua mettere i cambi di feromoni
  }
  for (j in 1:n_formiche) {
    sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] <- max(sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] + diff_valori[j,2],20)
  } # adesso bisogna effettuare la mossa con il maggior guadagno
  for (k in 1:mosse_per_sequenza) {
    numero <- sequenza_mosse[diff_valori[which(diff_valori[,2]==max(diff_valori[,2]))[1],1],k]
    numero
    diff_valori
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  punteggio <- punteggio_bianco()
  punteggio <- punteggio_arancione()
  punteggio <- punteggio_verde()
  punteggio <- punteggio_blu()
  punteggio <- punteggio_rosso()
  punteggio <- punteggio_giallo() 
  punteggio
  base_bianco <- bianco
  base_arancione <- arancione
  base_verde <- verde
  base_blu <- blu
  base_rosso <- rosso
  base_giallo <- giallo
}




valori_aco1
valori_aco2
sequenza_mosse
valore_base
plot(valori_aco1, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 5 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))

abline(h = 600, col = "red", lwd = 2, lty = 2)
max(valori_aco1)
mean(valori_aco1)
valori_aco2
plot(valori_aco2, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 5 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))

abline(h = 48, col = "red", lwd = 2, lty = 2)
max(valori_aco2)
which(valori_aco1== max(valori_aco1))
which(valori_aco2== max(valori_aco2))
which(valori_aco1== 320)
table(valori_aco1)
facce()
# Usando 10000 mosse effettive:
# Per quanto riguarda i risultati il valore più alto dei gruppi (298) non corisponde con il valore 
# più alto misurato sul numero di faccette sul posto giusto. Inoltre i valori riguardante i gruppi sono
# molto più alti (mediamente e come valori massimi) rispetto allo scorso metodo.
# Togliendo le adiacenze siamo riusciti a raggiungere il valore di 275 nei gruppi e 41 nelle faccette.
# Usando 100000 mosse effettive:
# con il metodo senza adiacenze siamo riusciti a raggiungere un valore di 325 per quanto riguarda la 
# valutazione a gruppi e un valore di 44 per la valutazione a faccette: il più alto mai raggiunto fin'ora


# proviamo a raggiungere la convergenza a 150. Bisogna però modificare le funzioni per calcolare i 
# punteggi togliendo le adiacenze


punteggio_bianco <- function(){
  angoli <- length(angoli_ad_bianco())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_bianco() # punteggio bianco
punteggio

punteggio_arancione <- function(){
  angoli <- length(angoli_ad_arancione())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
} 
punteggio <- punteggio_arancione() # punteggio arancione

punteggio_verde <- function(){
  angoli <- length(angoli_ad_verde())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_verde()# punteggio verde

punteggio_blu <- function(){
  angoli <- length(angoli_ad_blu())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_blu()# punteggio blu

punteggio_rosso <- function(){
  angoli <- length(angoli_ad_rosso())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_rosso()# punteggio rosso

punteggio_giallo <- function(){
  angoli <- length(angoli_ad_giallo())
  if (angoli==4){
    punteggio <- punteggio+100
    facce_giuste <- facce_giuste+1
  } else {
    if (angoli==3){
      punteggio <- punteggio+50
    } else {
      if (angoli==2){
        punteggio <- punteggio+30
        blocchi_medi <- blocchi_medi+1
      } else {
        if (angoli==1){
          punteggio <- punteggio+15
          blocchi_piccoli <- blocchi_piccoli+1
        }
      }
    }
  }
  return(punteggio)
}
punteggio <- punteggio_giallo() # punteggio giallo
punteggio

n_formiche <- 50
diff_valori <- matrix(data = NA, nrow = n_formiche, ncol = 2)
diff_valori
valori_aco1 <- rep(NA,10000)

mosse_per_sequenza <- 3
sequenza_mosse <- cbind(c(rep(1,12^(mosse_per_sequenza-1)),rep(2,12^(mosse_per_sequenza-1)),rep(3,12^(mosse_per_sequenza-1)),
                          rep(4,12^(mosse_per_sequenza-1)),rep(5,12^(mosse_per_sequenza-1)),rep(6,12^(mosse_per_sequenza-1)),
                          rep(7,12^(mosse_per_sequenza-1)),rep(8,12^(mosse_per_sequenza-1)),rep(9,12^(mosse_per_sequenza-1)),
                          rep(10,12^(mosse_per_sequenza-1)),rep(11,12^(mosse_per_sequenza-1)),rep(12,12^(mosse_per_sequenza-1))),
                        rep(c(rep(1,12^(mosse_per_sequenza-2)),rep(2,12^(mosse_per_sequenza-2)),rep(3,12^(mosse_per_sequenza-2)),
                              rep(4,12^(mosse_per_sequenza-2)),rep(5,12^(mosse_per_sequenza-2)),rep(6,12^(mosse_per_sequenza-2)),
                              rep(7,12^(mosse_per_sequenza-2)),rep(8,12^(mosse_per_sequenza-2)),rep(9,12^(mosse_per_sequenza-2)),
                              rep(10,12^(mosse_per_sequenza-2)),rep(11,12^(mosse_per_sequenza-2)),rep(12,12^(mosse_per_sequenza-2))),12),
                        rep(x=c(1,2,3,4,5,6,7,8,9,10,11,12),12^(mosse_per_sequenza-1)))
sequenza_mosse <- cbind(sequenza_mosse,rep(1000,12^(mosse_per_sequenza)))
sequenza_mosse
p<-1 



while (p<10001) {
  if (valore_base==48){
    break
  } 
  #ACO
  feromoni_cumul <- cumsum(sequenza_mosse[,3])
  for (i in 1:n_formiche) {
    valore <- sample(1:sum(sequenza_mosse[,3]),1)
    scelta <- which(feromoni_cumul>=valore)[1]
    for (k in 1:mosse_per_sequenza) {
      numero <- sequenza_mosse[scelta,k]
      numero
      if (numero == 1){
        mossa1(bianco, blu, giallo, verde, arancione, rosso)
        risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
        bianco <- risultato1$bianco
        blu <- risultato1$blu
        giallo <- risultato1$giallo
        verde <- risultato1$verde
        arancione <- risultato1$arancione
        rosso <- risultato1$rosso
      } else {
        if (numero == 2) {
          mossa2(bianco, blu, giallo, verde, rosso, arancione)
          risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
          bianco <- risultato2$bianco
          blu <- risultato2$blu
          giallo <- risultato2$giallo
          verde <- risultato2$verde
          rosso <- risultato2$rosso
          arancione <- risultato2$arancione
        } else {
          if (numero == 3) {
            mossa3(bianco, arancione, giallo, verde, rosso, blu)
            risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
            bianco <- risultato3$bianco
            arancione <- risultato3$arancione
            giallo <- risultato3$giallo
            verde <- risultato3$verde
            rosso <- risultato3$rosso
            blu <- risultato3$blu
          } else {
            if (numero == 4){
              mossa4(bianco, arancione, giallo, blu, rosso, verde)
              risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
              bianco <- risultato4$bianco
              arancione <- risultato4$arancione
              giallo <- risultato4$giallo
              blu <- risultato4$blu
              rosso <- risultato4$rosso
              verde <- risultato4$verde
            } else {
              if (numero == 5){
                mossa5(bianco, arancione, verde, blu, rosso, giallo)
                risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                bianco <- risultato5$bianco
                arancione <- risultato5$arancione
                verde <- risultato5$verde
                blu <- risultato5$blu
                rosso <- risultato5$rosso
                giallo <- risultato5$giallo
              } else {
                if (numero == 6){
                  mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato6$giallo
                  arancione <- risultato6$arancione
                  verde <- risultato6$verde
                  blu <- risultato6$blu
                  rosso <- risultato6$rosso
                  bianco <- risultato6$bianco
                } else {
                  if (numero == 7){
                    mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                    giallo <- risultato7$giallo
                    arancione <- risultato7$arancione
                    verde <- risultato7$verde
                    blu <- risultato7$blu
                    rosso <- risultato7$rosso
                    bianco <- risultato7$bianco
                  } else {
                    if (numero == 8){
                      mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                      bianco <- risultato8$bianco
                      arancione <- risultato8$arancione
                      verde <- risultato8$verde
                      blu <- risultato8$blu
                      rosso <- risultato8$rosso
                      giallo <- risultato8$giallo
                    } else {
                      if (numero == 9){
                        mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                        bianco <- risultato9$bianco
                        arancione <- risultato9$arancione
                        giallo <- risultato9$giallo
                        blu <- risultato9$blu
                        rosso <- risultato9$rosso
                        verde <- risultato9$verde
                      } else {
                        if (numero == 10){
                          mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                          bianco <- risultato10$bianco
                          arancione <- risultato10$arancione
                          giallo <- risultato10$giallo
                          verde <- risultato10$verde
                          rosso <- risultato10$rosso
                          blu <- risultato10$blu
                        } else {
                          if (numero == 11){
                            mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                            bianco <- risultato11$bianco
                            blu <- risultato11$blu
                            giallo <- risultato11$giallo
                            verde <- risultato11$verde
                            rosso <- risultato11$rosso
                            arancione <- risultato11$arancione
                          } else {
                            if (numero == 12){
                              mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                              bianco <- risultato12$bianco
                              blu <- risultato12$blu
                              giallo <- risultato12$giallo
                              verde <- risultato12$verde
                              arancione <- risultato12$arancione
                              rosso <- risultato12$rosso
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      # non bisogna più guardare alla differenza tra valori ma solo se raggiunge o no il 150
      
    }
    punteggio <- punteggio_bianco()
    punteggio <- punteggio_arancione()
    punteggio <- punteggio_verde()
    punteggio <- punteggio_blu()
    punteggio <- punteggio_rosso()
    punteggio <- punteggio_giallo() 
    
    diff_valori[i,1] <- scelta
    diff_valori[i,2] <- abs(punteggio-150)
    punteggio <- 0
    bianco <- base_bianco
    arancione <- base_arancione
    verde <- base_verde
    blu <- base_blu
    rosso <- base_rosso
    giallo <- base_giallo
    # fine per tutte le formiche
    # qua mettere i cambi di feromoni
  }
  for (j in 1:n_formiche) {
    if (diff_valori[j,2]==0){
      incremento <- 200
    } else {
      incremento <- -diff_valori[j,2]
    }
    sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] <- max(sequenza_mosse[diff_valori[j,1],mosse_per_sequenza+1] + incremento,20)
  } 
  for (k in 1:mosse_per_sequenza) {
    numero <- sequenza_mosse[diff_valori[which(diff_valori[,2]==min(diff_valori[,2]))[1],1],k]
    numero
    diff_valori
    if (numero == 1){
      mossa1(bianco, blu, giallo, verde, arancione, rosso)
      risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
      bianco <- risultato1$bianco
      blu <- risultato1$blu
      giallo <- risultato1$giallo
      verde <- risultato1$verde
      arancione <- risultato1$arancione
      rosso <- risultato1$rosso
    } else {
      if (numero == 2) {
        mossa2(bianco, blu, giallo, verde, rosso, arancione)
        risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
        bianco <- risultato2$bianco
        blu <- risultato2$blu
        giallo <- risultato2$giallo
        verde <- risultato2$verde
        rosso <- risultato2$rosso
        arancione <- risultato2$arancione
      } else {
        if (numero == 3) {
          mossa3(bianco, arancione, giallo, verde, rosso, blu)
          risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
          bianco <- risultato3$bianco
          arancione <- risultato3$arancione
          giallo <- risultato3$giallo
          verde <- risultato3$verde
          rosso <- risultato3$rosso
          blu <- risultato3$blu
        } else {
          if (numero == 4){
            mossa4(bianco, arancione, giallo, blu, rosso, verde)
            risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
            bianco <- risultato4$bianco
            arancione <- risultato4$arancione
            giallo <- risultato4$giallo
            blu <- risultato4$blu
            rosso <- risultato4$rosso
            verde <- risultato4$verde
          } else {
            if (numero == 5){
              mossa5(bianco, arancione, verde, blu, rosso, giallo)
              risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
              bianco <- risultato5$bianco
              arancione <- risultato5$arancione
              verde <- risultato5$verde
              blu <- risultato5$blu
              rosso <- risultato5$rosso
              giallo <- risultato5$giallo
            } else {
              if (numero == 6){
                mossa6(giallo, arancione, verde, blu, rosso, bianco)
                risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                giallo <- risultato6$giallo
                arancione <- risultato6$arancione
                verde <- risultato6$verde
                blu <- risultato6$blu
                rosso <- risultato6$rosso
                bianco <- risultato6$bianco
              } else {
                if (numero == 7){
                  mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                  giallo <- risultato7$giallo
                  arancione <- risultato7$arancione
                  verde <- risultato7$verde
                  blu <- risultato7$blu
                  rosso <- risultato7$rosso
                  bianco <- risultato7$bianco
                } else {
                  if (numero == 8){
                    mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                    bianco <- risultato8$bianco
                    arancione <- risultato8$arancione
                    verde <- risultato8$verde
                    blu <- risultato8$blu
                    rosso <- risultato8$rosso
                    giallo <- risultato8$giallo
                  } else {
                    if (numero == 9){
                      mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                      bianco <- risultato9$bianco
                      arancione <- risultato9$arancione
                      giallo <- risultato9$giallo
                      blu <- risultato9$blu
                      rosso <- risultato9$rosso
                      verde <- risultato9$verde
                    } else {
                      if (numero == 10){
                        mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                        bianco <- risultato10$bianco
                        arancione <- risultato10$arancione
                        giallo <- risultato10$giallo
                        verde <- risultato10$verde
                        rosso <- risultato10$rosso
                        blu <- risultato10$blu
                      } else {
                        if (numero == 11){
                          mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                          bianco <- risultato11$bianco
                          blu <- risultato11$blu
                          giallo <- risultato11$giallo
                          verde <- risultato11$verde
                          rosso <- risultato11$rosso
                          arancione <- risultato11$arancione
                        } else {
                          if (numero == 12){
                            mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                            bianco <- risultato12$bianco
                            blu <- risultato12$blu
                            giallo <- risultato12$giallo
                            verde <- risultato12$verde
                            arancione <- risultato12$arancione
                            rosso <- risultato12$rosso
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  base_bianco <- bianco
  base_arancione <- arancione
  base_verde <- verde
  base_blu <- blu
  base_rosso <- rosso
  base_giallo <- giallo
  
  
  punteggio <- punteggio_bianco()
  punteggio <- punteggio_arancione()
  punteggio <- punteggio_verde()
  punteggio <- punteggio_blu()
  punteggio <- punteggio_rosso()
  punteggio <- punteggio_giallo() 
  
  valore_base <- valore_attuale_f(base_bianco,bianco_0,base_arancione,arancione_0,base_verde,verde_0,
                                  base_blu,blu_0,base_rosso,rosso_0,base_giallo,giallo_0)
  
  
  if (punteggio==150&& valore_base==26){
    for (i in 1:12) {
      if (valore_base != 36 ){
        if (i == 1){
          mossa1(bianco, blu, giallo, verde, arancione, rosso)
          risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
          stato_attuale <- risultato1
        } else {
          if (i == 2) {
            mossa2(bianco, blu, giallo, verde, rosso, arancione)
            risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
            stato_attuale <- risultato2
          } else {
            if (i == 3) {
              mossa3(bianco, arancione, giallo, verde, rosso, blu)
              risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
              stato_attuale <- risultato3
            } else {
              if (i == 4){
                mossa4(bianco, arancione, giallo, blu, rosso, verde)
                risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                stato_attuale <- risultato4
              } else {
                if (i == 5){
                  mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  stato_attuale <- risultato5
                } else {
                  if (i == 6){
                    mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    stato_attuale <- risultato6
                  } else {
                    if (i == 7){
                      mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      stato_attuale <- risultato7
                    } else {
                      if (i == 8){
                        mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        stato_attuale <- risultato8
                      } else {
                        if (i == 9){
                          mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          stato_attuale <- risultato9
                        } else {
                          if (i == 10){
                            mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            stato_attuale <- risultato10
                          } else {
                            if (i == 11){
                              mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              stato_attuale <- risultato11
                            } else {
                              if (i == 12){
                                mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                stato_attuale <- risultato12
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                         stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
      if (valore_attuale == 36){
        bianco <- stato_attuale$bianco
        arancione <- stato_attuale$arancione
        verde <- stato_attuale$verde
        blu <- stato_attuale$blu
        rosso <- stato_attuale$rosso
        giallo <- stato_attuale$giallo
        valore_base <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      } 
      
    } 
    
  }
  if (valore_base==36){
    for (i in 1:12) {
      if (valore_base != 48 ){
        if (i == 1){
          mossa1(bianco, blu, giallo, verde, arancione, rosso)
          risultato1 <- mossa1(bianco, blu, giallo, verde, arancione, rosso)
          stato_attuale <- risultato1
        } else {
          if (i == 2) {
            mossa2(bianco, blu, giallo, verde, rosso, arancione)
            risultato2 <- mossa2(bianco, blu, giallo, verde, rosso,arancione)
            stato_attuale <- risultato2
          } else {
            if (i == 3) {
              mossa3(bianco, arancione, giallo, verde, rosso, blu)
              risultato3 <- mossa3(bianco, arancione, giallo, verde, rosso, blu)
              stato_attuale <- risultato3
            } else {
              if (i == 4){
                mossa4(bianco, arancione, giallo, blu, rosso, verde)
                risultato4 <- mossa4(bianco, arancione, giallo, blu, rosso, verde)
                stato_attuale <- risultato4
              } else {
                if (i == 5){
                  mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  risultato5 <- mossa5(bianco, arancione, verde, blu, rosso, giallo)
                  stato_attuale <- risultato5
                } else {
                  if (i == 6){
                    mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    risultato6 <- mossa6(giallo, arancione, verde, blu, rosso, bianco)
                    stato_attuale <- risultato6
                  } else {
                    if (i == 7){
                      mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      risultato7 <- mossa7(giallo, arancione, verde, blu, rosso, bianco)
                      stato_attuale <- risultato7
                    } else {
                      if (i == 8){
                        mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        risultato8 <- mossa8(bianco, arancione, verde, blu, rosso, giallo)
                        stato_attuale <- risultato8
                      } else {
                        if (i == 9){
                          mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          risultato9 <- mossa9(bianco, arancione, giallo, blu, rosso, verde)
                          stato_attuale <- risultato9
                        } else {
                          if (i == 10){
                            mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            risultato10 <- mossa10(bianco, arancione, giallo, verde, rosso, blu)
                            stato_attuale <- risultato10
                          } else {
                            if (i == 11){
                              mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              risultato11 <- mossa11(bianco, blu, giallo, verde, rosso, arancione)
                              stato_attuale <- risultato11
                            } else {
                              if (i == 12){
                                mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                risultato12 <- mossa12(bianco, blu, giallo, verde, arancione, rosso)
                                stato_attuale <- risultato12
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      valore_attuale <- valore_attuale_f(stato_attuale$bianco,bianco_0,stato_attuale$arancione,arancione_0,stato_attuale$verde,verde_0,
                                         stato_attuale$blu,blu_0,stato_attuale$rosso,rosso_0,stato_attuale$giallo,giallo_0)
      if (valore_attuale == 48){
        bianco <- stato_attuale$bianco
        arancione <- stato_attuale$arancione
        verde <- stato_attuale$verde
        blu <- stato_attuale$blu
        rosso <- stato_attuale$rosso
        giallo <- stato_attuale$giallo
        valore_base <- valore_attuale_f(bianco,bianco_0,arancione,arancione_0,verde,verde_0,
                                        blu,blu_0,rosso,rosso_0,giallo,giallo_0)
      } 
    }
  }
  valori_aco1[p] <- punteggio
  valori_aco2[p] <- valore_base
  p <- p+1
  punteggio <- 0
}



valori_aco1
valori_aco2
hist(valori_aco1)
plot(valori_aco1, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 5 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,600))

abline(h = 600, col = "red", lwd = 2, lty = 2)
max(valori_aco1)
mean(valori_aco1)
plot(valori_aco2, type = "l", col = "blue", lwd = 2,
     main = "Values with a sequence of 5 moves",
     xlab = "Moves", ylab = "Values", ylim = c(0,50))

abline(h = 48, col = "red", lwd = 2, lty = 2)
sum(valori_aco1==150)
sum(valori_aco2==26)

possibilità <- 0
for (i in 1:10000) {
  if (valori_aco1[i]==150 && valori_aco2[i]==26){
    possibilità <- possibilità+1
  }
}
possibilità

valori_aco1==150
max(valori_aco2)
hist(valori_aco2)
valori_massimi_aco1 <- valori_aco1
valori_massimi_aco2 <- valori_aco2
valori_massimi_aco1
valori_massimi_aco2

# shapiro.test(valori_aco2)
qqnorm(valori_aco2)
qqline(valori_aco2, col = "red") 


hist(valori_aco2, probability = TRUE, col = "lightblue", breaks = 15, 
     main = "Istogramma orizzontale con distribuzione Normale", 
     xlab = "Frequenze", ylab = "Valori", horiz = TRUE)
mean(valori_aco2)
sd(valori_aco2)
curve(dnorm(x, mean = mean(valori_aco2), sd = sd(valori_aco2)), 
      col = "red", lwd = 2, add = TRUE)

# i dati sono distribuiti simili ad una Normale, tuttavia c'è una leggera skewness: la coda di destra
# è leggermente più pesante rispetto a quella di sinistra.

probab <- 1-pnorm(48,mean = mean(valori_aco2), sd=sd(valori_aco2))
probab
# da questo metodo possiamo capire alcune cose molto interessanti: prima di tutto i valori ottenute 
# con il primo metodo di valutazione delle faccette (da 0 a 48) è stato decisamente soddisfacente: abbiamo
# raggiunto il valore massimo di 41 (considearando che non puntavamo al massimo ma allo stato a due mosse dalla 
# soluzione. Tuttavia se guardiamo al secondo metodo di valutazione basato sui blocchi,
# ci sono ancora dei miglioramenti da fare: su 100000 mosse effettive, solo 82 volte siamo arrivati 
# al valore di 150 (0.00082%). Molto meno rispetto alle volte che abbiamo raggiunto il valore di 26 con il
# primo metodo di valutazione (8112 volte). Questo ci fa capire quanto sia più difficile fare punteggi
# alti con i gruppi rispetto, ma allo stesso tempo questo secondo metodo aumenta notevolmente la probabilità
# di riuscire a risolvere il cubo una volta raggiunta la condizione necessaria.





######


