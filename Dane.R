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

# Pusta tabela początkowa
tabela_wynikowa <- data.frame()

# Funkcja do pozyskiwania danych
tworzenie_tabelii <- function(nazwa_druzyna,nazwa_pilkarz){

  # Wszystkie dostępne mecze
  wszyskie_rozgrywki <- FreeCompetitions()
  
  # Pobranie danych dla danego sezonu i rozgrywek
  rozgrywki_sezon <- FreeCompetitions() %>%
    filter(competition_id==16 & season_name=="2018/2019")
  
  # Baza meczów z rozgrywek i sezonu
  mecz_rozgrywki_sezon <- FreeMatches(rozgrywki_sezon) 
  
  # Dane są tylko z meczy Liverpool- Tottenham
  mecz <- StatsBombFreeEvents(MatchesDF = mecz_rozgrywki_sezon)
  
  # Czyszczenie danych
  mecz <- allclean(mecz)
  
  # Transforamcja pod Statsbomb
  mecz <- mecz %>% 
    soccermatics::soccerTransform(method = "statsbomb")
  
  # Id meczu
  mecz_id <- mecz$match_id[1]
  
  # Dana drużyna
  dana_druzyna = nazwa_druzyna
  
  # Dany piłkarz
  dany_pikarz = nazwa_pilkarz
  
  # Dane dla konkretnej druzyny
  dana_druzyna_tabela <- mecz %>%
    filter(team.name == dana_druzyna)
  
  # Dane danego piłkarza
  pilkarz_tabela <- dana_druzyna_tabela %>%          
    filter(player.name == dany_pikarz)
  
  # Dane konkretnego meczu
  mecz_analizowany <-  mecz_rozgrywki_sezon %>%
    filter(match_id == mecz_id)
  
  #---------------------------------Dane do wektora----------------------------
  
  #------------Ogólny opis------------
  
  data_meczu <- mecz_analizowany$match_date
  lineups <- get.lineupsFree(mecz)
  
  druzyny <- lineups$team_name
  gospodarze <- druzyny[1]
  goscie <- druzyny[2]
  
  pilkarze_gospodarze <- lineups$lineup[[1]]
  pilkarze_goscie <- lineups$lineup[[2]]
  
  pilkarz <- dany_pikarz
  
  team_pilkarza <- dana_druzyna
  
  if ((dim(pilkarz_tabela)[1] == 0)) {
    pozycja <- "nie_okreslono"
  } else{
    pozycja <- pilkarz_tabela$position.name[1]
  }
  
  #-----warunek czy piłkarz grał-----
  
  if ((dim(pilkarz_tabela)[1] == 0)) {
    podania <- 0
    podania_udane <- 0
    podania_udane_procent <- 0
    podania_pod_presja <- 0
    podania_pod_presja_udane <- 0
    podania_pod_presja_udane_procent <- 0
    podania_pod_presja_na_wlasnej_polowie <- 0
    podania_pod_presja_na_wlasnej_polowie_udane <- 0
    podania_pod_presja_na_wlasnej_polowie_udane_procent <- 0
    procent_podan_pod_presja_do_wszystkich <- 0
    procent_udnaych_podan_pod_presja_do_udanych_podan <- 0
    podania_do_przodu <- 0
    podania_do_przodu_pod_presja <- 0
    podania_w_strefe_F3_z_gry <- 0
    podania_w_pole_karne_z_gry <- 0
    drybingi <- 0
    dryblingi_udane <- 0
    dryblingi_pod_presja <- 0
    dyblingi_udane_pod_presja <- 0
    dryblingi_na_wlasnej_polowie <- 0
    dryblingi_na_polowie_przeciwnika <- 0
    dosrodkowania_w_pole_karne <- 0
    dosrodkowania_w_pole_karne_udane <- 0
    procent_udnaych_dosrodkowan_w_pole_karne <- 0
    dosrodkowania_w_pole_karne_z_gry <- 0
    dosrodkowania_w_pole_karne_z_gry_udane <- 0
    procent_udanych_dosrodkowan_w_pole_karne_z_gry <- 0
    podjedynki_w_obronie <- 0
    bycie_przedryblowanym <- 0
    straty_pilki <- 0
    straty_pilki_na_wlasnej_polowie <- 0
    faule <- 0
    faule_na_wlasnej_polowie <- 0
    faule_na_polowie_przeciwnika <- 0
    bycie_sfaulowanym <- 0
    bycie_sfaulowanym_na_wlasnej_polowie <- 0
    bycie_sfaulowanym_na_polowie_przeciwnika <- 0
    pressing <- 0
    pressing_na_wlasnej_polowie <- 0
    pressing_na_polowie_przeciwnika <- 0
    odbiory_pilki <- 0
    odbiory_pilki_na_wlasnej_polowie <- 0
    obriory_pilki_na_polowie_przeciwnika <- 0
    przechwyty_piłki <- 0
    przechwyty_piłki_na_wlasnej_polowie <- 0
    przechwyty_piłki_na_polowie_przeciwnika <- 0
    strzaly <- 0
    strzaly_celne <- 0
  }
  
  
  #------------------inne elementy------------
  
  podania_frame <- pilkarz_tabela%>%
    filter(type.name == "Pass")
  podania <- nrow(podania_frame)
  
  podania_nieudane_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & pass.outcome.name == "Incomplete" | pass.outcome.name == "Out"| pass.outcome.name != "Pass Offside")
  podania_udane_frame <- podania_frame %>% anti_join(podania_nieudane_frame)
  podania_udane <- nrow(podania_udane_frame)
  
  podania_udane_procent <- (podania_udane/podania) * 100
  
  podania_pod_presja_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & under_pressure == "TRUE")
  podania_pod_presja <- nrow(podania_pod_presja_frame)
  
  podania_pod_presja_nieudane_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & under_pressure == "TRUE" & pass.outcome.name == "Incomplete" | pass.outcome.name == "Out"| pass.outcome.name != "Pass Offside")
  podania_pod_presja_udane_frame <- podania_pod_presja_frame %>% anti_join(podania_pod_presja_nieudane_frame)
  podania_pod_presja_udane <- nrow(podania_pod_presja_udane_frame)
  
  podania_pod_presja_udane_procent <- (podania_pod_presja_udane/podania_pod_presja) *100
  
  podania_pod_presja_na_wlasnej_polowie_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & under_pressure == "TRUE" & location.x < 52)
  podania_pod_presja_na_wlasnej_polowie <- nrow(podania_pod_presja_na_wlasnej_polowie_frame)
  
  podania_pod_presja_na_wlasnej_polowie_nieudane_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & under_pressure == "TRUE" & location.x < 52 & pass.outcome.name == "Incomplete" | pass.outcome.name == "Out"| pass.outcome.name != "Pass Offside")
  podania_pod_presja_na_wlasnej_polowie_udane_frame <- podania_pod_presja_na_wlasnej_polowie_frame %>% anti_join(podania_pod_presja_na_wlasnej_polowie_nieudane_frame)
  podania_pod_presja_na_wlasnej_polowie_udane <- nrow(podania_pod_presja_na_wlasnej_polowie_udane_frame)
  
  podania_pod_presja_na_wlasnej_polowie_udane_procent <- (podania_pod_presja_na_wlasnej_polowie_udane/podania_pod_presja_na_wlasnej_polowie) *100
  podania_pod_presja_na_wlasnej_polowie_udane_procent <- round(podania_pod_presja_na_wlasnej_polowie_udane_procent,digits=2)
  
  procent_podan_pod_presja_do_wszystkich <- (podania_pod_presja/podania)*100
  
  procent_udnaych_podan_pod_presja_do_udanych_podan <- (podania_pod_presja_udane/podania_udane)*100
  
  podania_do_przodu_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & pass.angle > -1 & pass.angle < 1)
  podania_do_przodu <- nrow(podania_do_przodu_frame)
  
  podania_do_przodu_pod_presja_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & under_pressure == "TRUE" & pass.angle > -1 & pass.angle < 1)
  podania_do_przodu_pod_presja <- nrow(podania_do_przodu_pod_presja_frame)
  
  
  podania_w_strefe_F3_z_gry_frame <- pilkarz_tabela%>%
    filter(type.name == "Pass" & play_pattern.name == "Regular Play" & pass.end_location.x > 75)
  podania_w_strefe_F3_z_gry <- nrow(podania_w_strefe_F3_z_gry_frame)
  
  
  podania_w_pole_karne_z_gry_frame <- pilkarz_tabela%>%
    filter(type.name == "Pass" & play_pattern.name == "Regular Play" & pass.end_location.x > 89 & pass.end_location.y < 55 & pass.end_location.y > 14)
  podania_w_pole_karne_z_gry <- nrow(podania_w_pole_karne_z_gry_frame)
  
  
  drybingi_frame <- pilkarz_tabela%>%
    filter(type.name == "Dribble")
  drybingi <- nrow(drybingi_frame)
  
  dryblingi_udane_frame <- pilkarz_tabela%>%
    filter(type.name == "Dribble" & dribble.outcome.name == "Complete")
  dryblingi_udane <- nrow(dryblingi_udane_frame)
  
  dryblingi_pod_presja_frame <- pilkarz_tabela%>%
    filter(type.name == "Dribble" & under_pressure == "TRUE")
  dryblingi_pod_presja <- nrow(dryblingi_pod_presja_frame)
  
  dyblingi_udane_pod_presja_frame <- pilkarz_tabela%>%
    filter(type.name == "Dribble" & under_pressure == "TRUE" & dribble.outcome.name == "Complete")
  dyblingi_udane_pod_presja <- nrow(dyblingi_udane_pod_presja_frame)
  
  dryblingi_na_wlasnej_polowie_frame <- pilkarz_tabela%>%
    filter(type.name == "Dribble" & location.x < 52)
  dryblingi_na_wlasnej_polowie <- nrow(dryblingi_na_wlasnej_polowie_frame)
  
  dryblingi_na_polowie_przeciwnika_frame <- pilkarz_tabela%>%
    filter(type.name == "Dribble" & location.x > 52)
  dryblingi_na_polowie_przeciwnika <- nrow(dryblingi_na_polowie_przeciwnika_frame)
  
  
  dosrodkowania_w_pole_karne_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass"& pass.cross == "TRUE" & pass.end_location.x > 89 & pass.end_location.y < 55 & pass.end_location.y > 14)
  dosrodkowania_w_pole_karne <- nrow(dosrodkowania_w_pole_karne_frame)
  
  dosrodkowania_w_pole_karne_nieudane_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & pass.cross == "TRUE" & pass.end_location.x > 89 & pass.end_location.y < 55 & pass.end_location.y > 14 & pass.outcome.name == "Incomplete" | pass.outcome.name == "Out"| pass.outcome.name != "Pass Offside")
  dosrodkowania_w_pole_karne_udane_frame <- dosrodkowania_w_pole_karne_frame %>% anti_join(dosrodkowania_w_pole_karne_nieudane_frame)
  dosrodkowania_w_pole_karne_udane <- nrow(dosrodkowania_w_pole_karne_udane_frame)
  
  procent_udnaych_dosrodkowan_w_pole_karne <- (dosrodkowania_w_pole_karne_udane/dosrodkowania_w_pole_karne)*100
  
  dosrodkowania_w_pole_karne_z_gry_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass"& pass.cross == "TRUE" & play_pattern.name == "Regular Play" & pass.end_location.x > 89 & pass.end_location.y < 55 & pass.end_location.y > 14)
  dosrodkowania_w_pole_karne_z_gry <- nrow(dosrodkowania_w_pole_karne_z_gry_frame)
  
  dosrodkowania_w_pole_karne_z_gry_nieudane_frame <- pilkarz_tabela %>%
    filter(type.name == "Pass" & pass.cross == "TRUE" & play_pattern.name == "Regular Play" & pass.end_location.x > 89 & pass.end_location.y < 55 & pass.end_location.y > 14 & pass.outcome.name == "Incomplete" | pass.outcome.name == "Out"| pass.outcome.name != "Pass Offside")
  dosrodkowania_w_pole_karne_z_gry_udane_frame <- dosrodkowania_w_pole_karne_z_gry_frame %>% anti_join(dosrodkowania_w_pole_karne_z_gry_nieudane_frame)
  dosrodkowania_w_pole_karne_z_gry_udane <- nrow(dosrodkowania_w_pole_karne_z_gry_udane_frame)
  
  procent_udanych_dosrodkowan_w_pole_karne_z_gry <- (dosrodkowania_w_pole_karne_udane/dosrodkowania_w_pole_karne)*100
  
  
  podjedynki_w_obronie_frame <- pilkarz_tabela %>%
    filter(type.name == "Duel")
  podjedynki_w_obronie <- nrow(podjedynki_w_obronie_frame)
  
  bycie_przedryblowanym_frame <- pilkarz_tabela %>%
    filter(type.name == "Dribbled Past")
  bycie_przedryblowanym <- nrow(bycie_przedryblowanym_frame)
  
  
  straty_pilki_frame <- pilkarz_tabela %>%
    filter(type.name == "Dispossessed")
  straty_pilki <- nrow(straty_pilki_frame)
  
  straty_pilki_na_wlasnej_polowie_frame <- pilkarz_tabela %>%
    filter(type.name == "Dispossessed" & location.x < 52)
  straty_pilki_na_wlasnej_polowie <- nrow(straty_pilki_na_wlasnej_polowie_frame)
  
  faule_frame <- pilkarz_tabela %>%
    filter(type.name == "Foul Committed")
  faule <- nrow(faule_frame)
  
  faule_na_wlasnej_polowie_frame <- pilkarz_tabela %>%
    filter(type.name == "Foul Committed" & location.x < 52)
  faule_na_wlasnej_polowie <- nrow(faule_na_wlasnej_polowie_frame)
  
  faule_na_polowie_przeciwnika_frame <- pilkarz_tabela %>%
    filter(type.name == "Foul Committed" & location.x > 52)
  faule_na_polowie_przeciwnika <- nrow(faule_na_polowie_przeciwnika_frame)
  
  bycie_sfaulowanym_frame <- pilkarz_tabela %>%
    filter(type.name == "Foul Won")
  bycie_sfaulowanym <- nrow(bycie_sfaulowanym_frame)
  
  bycie_sfaulowanym_na_wlasnej_polowie_frame <- pilkarz_tabela %>%
    filter(type.name == "Foul Won" & location.x < 52)
  bycie_sfaulowanym_na_wlasnej_polowie <- nrow(bycie_sfaulowanym_na_wlasnej_polowie_frame)
  
  bycie_sfaulowanym_na_polowie_przeciwnika_frame <- pilkarz_tabela %>%
    filter(type.name == "Foul Won" & location.x > 52)
  bycie_sfaulowanym_na_polowie_przeciwnika <- nrow(bycie_sfaulowanym_na_polowie_przeciwnika_frame)
  
  pressing_frame <- pilkarz_tabela %>%
    filter(type.name == "Pressure")
  pressing <- nrow(pressing_frame)
  
  pressing_na_wlasnej_polowie_frame <- pilkarz_tabela %>%
    filter(type.name == "Pressure" & location.x < 52)
  pressing_na_wlasnej_polowie <- nrow(pressing_na_wlasnej_polowie_frame)
  
  pressing_na_polowie_przeciwnika_frame <- pilkarz_tabela %>%
    filter(type.name == "Pressure" & location.x > 52)
  pressing_na_polowie_przeciwnika <- nrow(pressing_na_polowie_przeciwnika_frame)
  
  odbiory_pilki_frame <- pilkarz_tabela %>%
    filter(type.name == "Ball Recovery")
  odbiory_pilki <- nrow(odbiory_pilki_frame)
  
  odbiory_pilki_na_wlasnej_polowie_frame <- pilkarz_tabela %>%
    filter(type.name == "Ball Recovery" & location.x < 52)
  odbiory_pilki_na_wlasnej_polowie <- nrow(odbiory_pilki_na_wlasnej_polowie_frame)
  
  obriory_pilki_na_polowie_przeciwnika_frame <- pilkarz_tabela %>%
    filter(type.name == "Ball Recovery" & location.x > 52)
  obriory_pilki_na_polowie_przeciwnika <- nrow(obriory_pilki_na_polowie_przeciwnika_frame)
  
  przechwyty_piłki_frame <- pilkarz_tabela %>%
    filter(type.name == "Interception")
  przechwyty_piłki <- nrow(przechwyty_piłki_frame)
  
  przechwyty_piłki_na_wlasnej_polowie_frame <- pilkarz_tabela %>%
    filter(type.name == "Interception" & location.x < 52)
  przechwyty_piłki_na_wlasnej_polowie <- nrow(przechwyty_piłki_na_wlasnej_polowie_frame)
  
  przechwyty_piłki_na_polowie_przeciwnika_frame <- pilkarz_tabela %>%
    filter(type.name == "Interception" & location.x > 52)
  przechwyty_piłki_na_polowie_przeciwnika <- nrow(przechwyty_piłki_na_polowie_przeciwnika_frame)
  
  strzaly_frame <- pilkarz_tabela %>%
    filter(type.name == "Shot")
  strzaly <- nrow(strzaly_frame)
  
  strzaly_celne_frame <- pilkarz_tabela %>%
    filter(type.name == "Shot" & shot.outcome.name == "Saved"| shot.outcome.name == "Goal")
  strzaly_celne <- nrow(strzaly_celne_frame)
  
  #-------------koniec elemnetów do tabeli
  
  #-----------łączenie w tabelę
  dane <- data.frame (mecz_id,
                      data_meczu,
                      gospodarze,
                      goscie,
                      pilkarz,
                      team_pilkarza,
                      pozycja,
                      podania,
                      podania_udane,
                      round(podania_udane_procent,digits=2),
                      podania_pod_presja, 
                      podania_pod_presja_udane, 
                      round(podania_pod_presja_udane_procent,digits=2),
                      podania_pod_presja_na_wlasnej_polowie,
                      podania_pod_presja_na_wlasnej_polowie_udane, 
                      round(podania_pod_presja_na_wlasnej_polowie_udane_procent,digits=2),
                      round(procent_podan_pod_presja_do_wszystkich,digits=2),
                      round(procent_udnaych_podan_pod_presja_do_udanych_podan,digits=2),
                      podania_do_przodu,
                      podania_do_przodu_pod_presja,
                      podania_w_strefe_F3_z_gry,
                      podania_w_pole_karne_z_gry,
                      drybingi,
                      dryblingi_udane,
                      dryblingi_pod_presja,
                      dyblingi_udane_pod_presja,
                      dryblingi_na_wlasnej_polowie,
                      dryblingi_na_polowie_przeciwnika,
                      dosrodkowania_w_pole_karne,
                      dosrodkowania_w_pole_karne_udane,
                      round(procent_udnaych_dosrodkowan_w_pole_karne,digits=2),
                      dosrodkowania_w_pole_karne_z_gry,
                      dosrodkowania_w_pole_karne_z_gry_udane,
                      round(procent_udanych_dosrodkowan_w_pole_karne_z_gry,digits=2),
                      podjedynki_w_obronie,
                      bycie_przedryblowanym,
                      straty_pilki,
                      straty_pilki_na_wlasnej_polowie,
                      faule,
                      faule_na_wlasnej_polowie,
                      faule_na_polowie_przeciwnika,
                      bycie_sfaulowanym,
                      bycie_sfaulowanym_na_wlasnej_polowie,
                      bycie_sfaulowanym_na_polowie_przeciwnika,
                      pressing,
                      pressing_na_wlasnej_polowie,
                      pressing_na_polowie_przeciwnika,
                      odbiory_pilki,
                      odbiory_pilki_na_wlasnej_polowie,
                      obriory_pilki_na_polowie_przeciwnika,
                      przechwyty_piłki,
                      przechwyty_piłki_na_wlasnej_polowie,
                      przechwyty_piłki_na_polowie_przeciwnika,
                      strzaly,
                      strzaly_celne)
  
  
  names(dane) <- c("Id mecz",
                   "Data meczu", 
                   "Gospodarz", 
                   "Goście", 
                   "Piłkarz", 
                   "Drużyna piłkarza", 
                   "Pozycja",
                   "Podania", 
                   "Podania celne", 
                   "Procent celnych podań (%)",
                   "Podania pod presją", 
                   "Celne podania pod presja", 
                   "Procent celnych podań pod presją (%)",
                   "Podania pod presją na własnej połowie", 
                   "Celne podania pod presją na własnej połowie",
                   "Procent celnych podań pod presją na własnej połowie (%)",
                   "Procent podan pod presją do wszystkich podań",
                   "Procent udnaych podań pod presja do udanych podań (%)",
                   "Podania do przodu",
                   "Podania do przodu pod presja",
                   "Podania w strefę F3 z gry",
                   "Podania w pole karne z gry",
                   "Drybingi",
                   "Dryblingi udane",
                   "Dryblingi pod presja",
                   "Dyblingi udane pod presja",
                   "Dryblingi na własnej polowie",
                   "Dryblingi na połowie przeciwnika",
                   "Dosrodkowania w pole karne",
                   "Celne dosrodkowania w pole karne",
                   "Procent udnaych dosrodkowań w pole karne (%)",
                   "Dosrodkowania w pole karne z gry",
                   "Celne dosrodkowania w pole karne z gry",
                   "Procent udanych dośrodkowań w pole karne z gry (%)",
                   "Podjedynki w obronie",
                   "Bycie przedryblowanym",
                   "Straty piłki",
                   "Straty piłki na własnej połowie",
                   "Faule",
                   "Faule na własnej połowie",
                   "Faule na połowie przeciwnika",
                   "Bycie sfaulowanym",
                   "Bycie sfaulowanym na własnej połowie",
                   "Bycie sfaulowanym na połowie przeciwnika",
                   "Pressing",
                   "Pressing na własnej połowie",
                   "Pressing na połowie przeciwnika",
                   "Odbiory piłki",
                   "Odbiory piłki na własnej połowie",
                   "Obriory piłki na polowie przeciwnika",
                   "Przechwyty piłki",
                   "Przechwyty piłki na wlasnej polowie",
                   "Przechwyty piłki na polowie przeciwnika",
                   "Strzały",
                   "Strzały celne")
  
  
  dane <<- dane 
  tabela_wynikowa <<- rbind(tabela_wynikowa, dane)
  return(tabela_wynikowa)
}


tworzenie_tabelii("Liverpool","Trent Alexander-Arnold")
tworzenie_tabelii("Liverpool","Andrew Robertson")

write.csv(tabela_wynikowa,"Output/Wyciagnie_dane_Liverpool.csv", row.names = TRUE)










