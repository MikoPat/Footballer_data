library(tidyverse)
library(StatsBombR) 
library(ggplot2)
library(SBpitch)
library(ggsoccer)
library(soccermatics)
library(dplyr)
library(rlang)
library(sqldf)
library(devtools)


#-------------------------------------------------------------------------------

# Pusta tabela z 
tabela_do_occeny <- data.frame()



#-------------------- metoda z tabelą -----------------------------------------
# trudno zrobić to, by było przejrzyście

procent_celnych_podan_1 <- c(0:30)
procent_celnych_podan_2 <- c(31:40)
procent_celnych_podan_3 <- c(41:50)
procent_celnych_podan_4 <- c(51:60)
procent_celnych_podan_5 <- c(61:70)
procent_celnych_podan_6 <- c(71:77)
procent_celnych_podan_7 <- c(78:84)
procent_celnych_podan_8 <- c(85:90)
procent_celnych_podan_9 <- c(91:96)
procent_celnych_podan_10 <- c(97:100)


max.len = max(length(procent_celnych_podan_1),
              length(procent_celnych_podan_2),
              length(procent_celnych_podan_3),
              length(procent_celnych_podan_4),
              length(procent_celnych_podan_5),
              length(procent_celnych_podan_6),
              length(procent_celnych_podan_7),
              length(procent_celnych_podan_8),
              length(procent_celnych_podan_9),
              length(procent_celnych_podan_10))

procent_celnych_podan_1 <- c(procent_celnych_podan_1, rep(NA, max.len - length(procent_celnych_podan_1)))
procent_celnych_podan_2 <- c(procent_celnych_podan_2, rep(NA, max.len - length(procent_celnych_podan_2)))
procent_celnych_podan_3 <- c(procent_celnych_podan_3, rep(NA, max.len - length(procent_celnych_podan_3)))
procent_celnych_podan_4 <- c(procent_celnych_podan_4, rep(NA, max.len - length(procent_celnych_podan_4)))
procent_celnych_podan_5 <- c(procent_celnych_podan_5, rep(NA, max.len - length(procent_celnych_podan_5)))
procent_celnych_podan_6 <- c(procent_celnych_podan_6, rep(NA, max.len - length(procent_celnych_podan_6)))
procent_celnych_podan_7 <- c(procent_celnych_podan_7, rep(NA, max.len - length(procent_celnych_podan_7)))
procent_celnych_podan_8 <- c(procent_celnych_podan_8, rep(NA, max.len - length(procent_celnych_podan_8)))
procent_celnych_podan_9 <- c(procent_celnych_podan_9, rep(NA, max.len - length(procent_celnych_podan_9)))
procent_celnych_podan_10 <- c(procent_celnych_podan_10, rep(NA, max.len - length(procent_celnych_podan_10)))


tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_1)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_2)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_3)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_4)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_5)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_6)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_7)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_8)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_9)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_10)

indeks <- as.numeric(rownames(tabela_do_occeny))

tabela_do_occeny <- cbind(tabela_do_occeny, indeks)
moveMe(tabela_do_occeny, indeks, "first")
tabela_do_occeny %>% moveme(., "indeks first")

for (row in 1:nrow(tabela_do_occeny)) {
  #if (podania_procen %in% row == TRUE){
  #  print("yes")
  #}else{
  #  print("no")
  #}
  
  print(tabela_do_occeny[row,indeks])
}


as.numeric(rownames(tabela_do_occeny))

podania_procen <- 120

if (podania_procen %in% procent_celnych_podan_8 == TRUE){
  print(tabela_do_occeny[procent_celnych_podan_8,"indeks"])
}else{
  print("no")
}
library(move)

print(tabela_do_occeny[procent_celnych_podan_8,"indeks"])

#------------------------ koniec metody z tablą--------------------------------------

#-------------------- metoda z if-ami------------------------------------------

ocena_elem <- function(podania_procen){
  
  if(podania_procen <= 30){
    print("1")
  }else if (podania_procen>=31 & podania_procen<=40){
    print("2")
  }else if (podania_procen>=41 & podania_procen<=50){
    print("3")
  }else if (podania_procen>=51 & podania_procen<=60){
    print("4")
  }else if (podania_procen>=61 & podania_procen<=70){
    print("5")
  }else if (podania_procen>=71 & podania_procen<=77){
    print("6")
  }else if (podania_procen>=78 & podania_procen<=84){
    print("7")
  }else if (podania_procen>=85 & podania_procen<=91){
    print("8")
  }else if (podania_procen>=92 & podania_procen<=97){
    print("9")
  }else if (podania_procen>=97){
    print("10")
  }
}

ocena_elem(90)

#-------------------- koniec metoda z if-ami--------------------------------------

#-------------------- metoda tabela i if-y---------------------------------------
# najlepiej sprawdza się do procentów


procent_celnych_podan_1 <- c(1,0,30)
procent_celnych_podan_2 <- c(2,31,40)
procent_celnych_podan_3 <- c(3,41,50)
procent_celnych_podan_4 <- c(4,51,60)
procent_celnych_podan_5 <- c(5,61,70)
procent_celnych_podan_6 <- c(6,71,77)
procent_celnych_podan_7 <- c(7,78,84)
procent_celnych_podan_8 <- c(8,85,90)
procent_celnych_podan_9 <- c(9,91,96)
procent_celnych_podan_10 <- c(10,97,100)

tabela_do_occeny <- data.frame()

tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_1)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_2)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_3)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_4)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_5)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_6)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_7)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_8)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_9)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_10)

tabela_do_occeny[1,1]
tabela_do_occeny[1,2]
tabela_do_occeny[1,3]

ocena_elem <- function(podania_procen){
  
  if(podania_procen <= tabela_do_occeny[1,3]){
    print(tabela_do_occeny[1,1])
  }else if (podania_procen>=tabela_do_occeny[2,2] & podania_procen<=tabela_do_occeny[2,3]){
    print(tabela_do_occeny[2,1])
  }else if (podania_procen>=tabela_do_occeny[3,2] & podania_procen<=tabela_do_occeny[3,3]){
    print(tabela_do_occeny[3,1])
  }else if (podania_procen>=tabela_do_occeny[4,2] & podania_procen<=tabela_do_occeny[4,3]){
    print(tabela_do_occeny[4,1])
  }else if (podania_procen>=tabela_do_occeny[5,2] & podania_procen<=tabela_do_occeny[5,3]){
    print(tabela_do_occeny[5,1])
  }else if (podania_procen>=tabela_do_occeny[6,2] & podania_procen<=tabela_do_occeny[6,3]){
    print(tabela_do_occeny[6,1])
  }else if (podania_procen>=tabela_do_occeny[7,2] & podania_procen<=tabela_do_occeny[7,3]){
    print(tabela_do_occeny[7,1])
  }else if (podania_procen>=tabela_do_occeny[8,2] & podania_procen<=tabela_do_occeny[8,3]){
    print(tabela_do_occeny[8,1])
  }else if (podania_procen>=tabela_do_occeny[9,2] & podania_procen<=tabela_do_occeny[9,3]){
    print(tabela_do_occeny[9,1])
  }else if (podania_procen>=tabela_do_occeny[10,2]){
    print(tabela_do_occeny[10,1])
  }
}

ocena_elem(92)

tabela_do_occeny[3,2] <- 41


#-------------------- koniec metody - tabela i if-y------------------------------------------

#-------------------- metoda druga - tabela i if-y--------------------------------------

procent_celnych_podan_1 <- c(1,30)
procent_celnych_podan_2 <- c(2,40)
procent_celnych_podan_3 <- c(3,50)
procent_celnych_podan_4 <- c(4,60)
procent_celnych_podan_5 <- c(5,70)
procent_celnych_podan_6 <- c(6,77)
procent_celnych_podan_7 <- c(7,84)
procent_celnych_podan_8 <- c(8,90)
procent_celnych_podan_9 <- c(9,96)
procent_celnych_podan_10 <- c(10,100)

tabela_do_occeny <- data.frame()

tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_1)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_2)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_3)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_4)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_5)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_6)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_7)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_8)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_9)
tabela_do_occeny <- rbind(tabela_do_occeny, procent_celnych_podan_10)


ocena_elem <- function(podania_procen){
  
  if(podania_procen <= tabela_do_occeny[1,2]){
    print(tabela_do_occeny[1,1])
  }else if (podania_procen>tabela_do_occeny[1,2] & podania_procen<=tabela_do_occeny[2,2]){
    print(tabela_do_occeny[2,1])
  }else if (podania_procen>=tabela_do_occeny[2,2] & podania_procen<=tabela_do_occeny[3,2]){
    print(tabela_do_occeny[3,1])
  }else if (podania_procen>=tabela_do_occeny[3,2] & podania_procen<=tabela_do_occeny[4,2]){
    print(tabela_do_occeny[4,1])
  }else if (podania_procen>=tabela_do_occeny[4,2] & podania_procen<=tabela_do_occeny[5,2]){
    print(tabela_do_occeny[5,1])
  }else if (podania_procen>=tabela_do_occeny[5,2] & podania_procen<=tabela_do_occeny[6,2]){
    print(tabela_do_occeny[6,1])
  }else if (podania_procen>=tabela_do_occeny[6,2] & podania_procen<=tabela_do_occeny[7,2]){
    print(tabela_do_occeny[7,1])
  }else if (podania_procen>=tabela_do_occeny[7,2] & podania_procen<=tabela_do_occeny[8,2]){
    print(tabela_do_occeny[8,1])
  }else if (podania_procen>=tabela_do_occeny[8,2] & podania_procen<=tabela_do_occeny[9,2]){
    print(tabela_do_occeny[9,1])
  }else if (podania_procen>=tabela_do_occeny[9,2]){
    print(tabela_do_occeny[10,1])
  }
}

ocena_elem(98)

# metoda jest najlepsza pod kątem możliwości modyfikacji i niskim stopniem skomplikowania
#--------------------koniec metody drugiej - tabela i if-y--------------------------------------

#-------------------- metoda trzeciej - tabela i if-y--------------------------------------

wartosc_oceny <- c(1:10)
procent_celnych_podan <- c(30,40,50,60,70,77,84,90,96,100)
procent_celnych_dosrodkowan <- c(41,45,55,65,75,80,87,92,97,100)

tabela_do_occeny <- data.frame()
tabela_do_occeny <- data.frame(wartosc_oceny,procent_celnych_podan,procent_celnych_dosrodkowan)

tabela_do_occeny[1,3]
tabela_do_occeny[2,2]

tabela_do_occeny[1,2]
tabela_do_occeny[2,2]
tabela_do_occeny[3,2]

tabela_do_occeny[1,1]

ocena_elem <- function(podania_procen){
  
  if(podania_procen <= tabela_do_occeny[1,2]){
    print(tabela_do_occeny[1,1])
  }else if (podania_procen>tabela_do_occeny[1,2] & podania_procen<=tabela_do_occeny[2,2]){
    print(tabela_do_occeny[2,1])
  }else if (podania_procen>=tabela_do_occeny[2,2] & podania_procen<=tabela_do_occeny[3,2]){
    print(tabela_do_occeny[3,1])
  }else if (podania_procen>=tabela_do_occeny[3,2] & podania_procen<=tabela_do_occeny[4,2]){
    print(tabela_do_occeny[4,1])
  }else if (podania_procen>=tabela_do_occeny[4,2] & podania_procen<=tabela_do_occeny[5,2]){
    print(tabela_do_occeny[5,1])
  }else if (podania_procen>=tabela_do_occeny[5,2] & podania_procen<=tabela_do_occeny[6,2]){
    print(tabela_do_occeny[6,1])
  }else if (podania_procen>=tabela_do_occeny[6,2] & podania_procen<=tabela_do_occeny[7,2]){
    print(tabela_do_occeny[7,1])
  }else if (podania_procen>=tabela_do_occeny[7,2] & podania_procen<=tabela_do_occeny[8,2]){
    print(tabela_do_occeny[8,1])
  }else if (podania_procen>=tabela_do_occeny[8,2] & podania_procen<=tabela_do_occeny[9,2]){
    print(tabela_do_occeny[9,1])
  }else if (podania_procen>=tabela_do_occeny[9,2]){
    print(tabela_do_occeny[10,1])
  }
}

ocena_elem(33)

# metoda jest najlepsza pod kątem możliwości modyfikacji i niskim stopniem skomplikowania
#--------------------koniec metody trzeciej - tabela i if-y--------------------------------------











