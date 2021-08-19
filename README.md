# Footballer_data - ocena zawodnika

## **Cel**: 
Stworzenie ocen zawodnika w konkretnym meczu na podstawie danych.


## **Krótki opis**:
Ocena będzie tworzona na postawie danego zawodnika z meczu. Istotnym aspektem będzie możliwość porównania danych i oceny naszego zawodnika do piłkarza z innej drużyny Ekstraklasy.

## **Korzyści**:
- Obiektywna meczowa ocena zawodnika
- Wiedza co dany zawodnik musi poprawić, a co robi na satysfakcjonującym poziomie
- Przy użyciu danych z innych lig, będziemy mogli sprawdzić ile w każdym aspekcie brakuje naszemu zawodnikowi do poziomu w danej lidze i nad czym musi jeszcze pracować

### Potrzebne elementy:
- Profil zawodnika na danej pozycji - określenie profilu przez parametry liczbowe np. liczbę udanych podań progresywnych, udanych i nieudanych dryblingów
- Skalę danego parametru, tzn. kiedy uważamy, że zawodnik uzyskał bardzo dobry wynik, kiedy dobry, a kiedy przeciętny
- Określenie do jakiego poziomu danego parametru chcemy dążyć, a jaki jest na dopuszczalny

### Profil drużyn przeciwnych opisany przez np.:
- Poziom drużyny (bardzo dobra, dobra, przeciętna,…),
- Ustawienie drużyny,
- Czy gra w wysokim czy w niskim pressingu

### Określenie charakterystyki meczu opisane przez np.:
- Z jakim przeciwnikiem gramy (poziom przeciwnika)
- W jakiej formie jest przeciwnik
- Czy styl przeciwnika w meczu był zgodny z naszymi przewidywaniami
- Czy była czerwona kartka dla zawodnika z drużyny przeciwnej
- Czy była czerwona kartka dla zawodnika z naszej drużyny

*Ważne będzie określenie ważności parametru i stworzenie hierarchii, od najbardziej istotnych do najmniej.*


## **Pozycja: Lewy obrońca**
### Charakterystyka:

### Parametry:
- Id mecz
- Data meczu
- Gospodarz
- Goście
- Piłkarz
- Drużyna piłkarza
- Pozycja
- Podania
- Podania celne
- Procent celnych podań (%)
- Podania pod presją
- Celne podania pod presja
- Procent celnych podań pod presją (%)
- Podania pod presją na własnej połowie
- Celne podania pod presją na własnej połowie
- Procent celnych podań pod presją na własnej połowie (%)
- Procent podan pod presją do wszystkich podań
- Procent udnaych podań pod presja do udanych podań (%)
- Podania do przodu
- Podania do przodu pod presja
- Podania w strefę F3 z gry
- Podania w pole karne z gry
- Drybingi
- Dryblingi udane
- Dryblingi pod presja
- Dyblingi udane pod presja
- Dryblingi na własnej polowie
- Dryblingi na połowie przeciwnika
- Dosrodkowania w pole karne
- Celne dosrodkowania w pole karne
- Procent udnaych dosrodkowań w pole karne (%)
- Dosrodkowania w pole karne z gry
- Celne dosrodkowania w pole karne z gry
- Procent udanych dośrodkowań w pole karne z gry (%)
- Podjedynki w obronie
- Bycie przedryblowanym
- Straty piłki
- Straty piłki na własnej połowie
- Faule
- Faule na własnej połowie
- Faule na połowie przeciwnika
- Bycie sfaulowanym
- Bycie sfaulowanym na własnej połowie
- Bycie sfaulowanym na połowie przeciwnika
- Pressing
- Pressing na własnej połowie
- Pressing na połowie przeciwnika
- Odbiory piłki
- Odbiory piłki na własnej połowie
- Obriory piłki na polowie przeciwnika
- Przechwyty piłki
- Przechwyty piłki na wlasnej polowie
- Przechwyty piłki na polowie przeciwnika
- Strzały
- Strzały celne


### Źródło danych
StatsBomb - https://github.com/statsbomb/open-data

![StatsBomb Logo](https://github.com/MikoPat/Footballer_data/blob/main/StatsBombLogo.png)

### Wykorzystane pakiety
- tidyverse
- StatsBombR
- ggplot2
- SBpitch
- ggsoccer
- soccermatics
- dplyr
- rlang
- sqldf
- devtools
