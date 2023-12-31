
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Modoma

In het Modoma project (2021-2023) hebben Floricode, Naktuinbouw en
Biometris/WUR de mogelijkheden onderzocht van automatische identificatie
van bloemkenmerken in rozen en gerbera’s, op grond van beelden. Deze
beelden worden door toepassing van AI gelinkt aan de databases waarin de
kenmerken zijn opgeslagen. Om een overzicht te krijgen van de inhoud van
die databases is een app ontwikkeld op basis van R/Shiny. Met deze app
kun je in je browser een overzicht van de kenmerken bekijken, je kunt
records vergelijken, en je kunt een set waardes definieren voor een
subset van de kenmerken, en dan kijken welke records daar het meest op
lijken.

Er is een demo app beschikbaar via <https://shiny.wur.nl/modoma>.  
In deze app is de Floricode Gerbera data al ingelezen en kan snel een
overzicht worden gekregen van de mogelijkheden van de app. Voor een
uitgebreidere versie waarin ook eigen datasets kunnen worden ingelezen,
moet de app gedraaid worden zoals hieronder bij installatie beschreven.

## Handleidingen

Er zijn twee handleidingen beschikbaar voor de app.

Voor een overzicht van functionaliteiten en het gebruik van de app in
het algemeen: <https://biometris.github.io/modoma/articles/Manual.html>

Voor een uitgebreide beschrijving voor het gebruik van een eigen dataset
binnen de app:
<https://biometris.github.io/modoma/articles/Metadata.html>

## Installatie

De installatie van de app kan worden gedaan in een paar eenvoudige
stappen:

1)  Installeer R op je computer. Uitgebreide instructies voor alle
    besturingssystemen zijn te vinden op <https://cran.r-project.org/>.
    Voor Windows komt het er op neer dat je de installatie-executable
    downloadt voor de meest recente versie van R (op het moment van
    schrijven
    <https://cran.r-project.org/bin/windows/base/R-4.3.0-win.exe>) en
    installeert door te dubbelklikken - binnen een beschermde omgeving
    kan het zijn dat IT support daarbij moet helpen voor de permissies.

2)  Start R op, en installeer het modoma pakket door op de command line
    het volgende te draaien:

``` r
install.packages("remotes")
remotes::install_github("Biometris/modoma", dependencies = TRUE)
```

3)  Nu is het modoma pakket beschikbaar. Het kan worden opgeroepen door
    het volgende commando in R te geven:

``` r
library(modoma)
```

4)  Tenslotte wordt de webpagina geopend door het volgende commando:

``` r
launch()
```
