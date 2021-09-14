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

dane <- read.csv("Wyciagnie_dane_Liverpool.csv")


#---------- indeks piłkarza ----------
indeks_wiersza_trent <- which(dane$Piłkarz=="Trent Alexander-Arnold")

#---------- tabela ----------
tabela_parametry <- data.frame()

#---------- dane do tabeli ----------

#----- podania -----
indeks_kolumny_podania <- grep("^Podania$", colnames(dane))
podania_wartosc_do_oceny <- dane[indeks_wiersza_trent,indeks_kolumny_podania]

indeks_kolumny_procent_podania <- grep("^Procent.celnych.podań....$", colnames(dane))
podania_procent_wartosc_do_oceny <- dane[indeks_wiersza_trent,indeks_kolumny_procent_podania]

podania_ocena <- podania_wartosc_do_oceny * podania_procent_wartosc_do_oceny


#----- podania pod presją -----
indeks_kolumny_podania_pod_presja <- grep("^Podania.do.przodu$", colnames(dane))
podania_pod_presja_wartosc_do_oceny <- dane[indeks_wiersza_trent,indeks_kolumny_podania]

indeks_kolumny_procent_podania_pod_presja <- grep("^Procent.celnych.podań....$", colnames(dane))
podania_pod_presja_procent_wartosc_do_oceny <- dane[indeks_wiersza_trent,indeks_kolumny_podania_pod_presja]

podania_pod_presja_ocena <- podania_pod_presja_wartosc_do_oceny * podania_pod_presja_procent_wartosc_do_oceny


#----- podania do przodu -----
indeks_kolumny_podania_do_przodu <- grep("^Podania.pod.presją$", colnames(dane))
podania_do_przodu_wartosc_do_oceny <- dane[indeks_wiersza_trent,indeks_kolumny_podania]

# brak warości procentowej udnaych podań do przodu
indeks_kolumny_procent_podania_do_przodu <- grep("^Procent.celnych.podań....$", colnames(dane))
podania_do_przodu_procent_wartosc_do_oceny <- dane[indeks_wiersza_trent,indeks_kolumny_podania_pod_presja]

podania_do_przodu_ocena <- podania_do_przodu_wartosc_do_oceny * podania_do_przodu_procent_wartosc_do_oceny


#----- podania do przodu pod presją-----
podania__do_przodu_pod_presja_ocena <- 0


#----- podania w strefę F3-----
podania_w_strefe_F3_ocena <- 0


#----- dośrodkowania w pole karne -----
dosrodkowania_ocena <- 0


#----- pojedynki w obronie -----
pojedynki_w_obronie_ocena <- 0


#----- straty piłki -----
straty_ocena <- 0


#----- faule -----
faule_ocena <- 0


#----- odbiory piłki -----
odbiory_ocena <- 0



#---------- tworzenie wektora ----------
wektor_dane_do_oceny <- c(podania_ocena,
                          podania_pod_presja_ocena,
                          podania_do_przodu_ocena,
                          podania_w_strefe_F3_ocena,
                          dosrodkowania_ocena,
                          pojedynki_w_obronie_ocena,
                          straty_ocena,
                          faule_ocena,
                          odbiory_ocena)

colnames(dane)

dane[11]












