library(odbc)
library(DBI)
library(leaflet)
library(tidyverse)
library(sf)
library(readxl)


## For å koble til SQL-serveren til HFP må det installerast ein oppdatert ODBC driver for Windows; "ODBC Driver 18 for SQL Server". Ta kontakt med IT for å laste ned.
## Skriptet under er kopla til utviklingsversjonen av HFP og lager nye midlertidige tabeller (slutter med "_temp") for alle steg i prosessen.
conn <- dbConnect(odbc::odbc(),
  .connection_string = "
                 Driver=ODBC Driver 18 for SQL Server;
                 Server=ninsql07;
                 Database=Honsefugl_dev;
                 Trusted_Connection=yes;
                 encrypt=yes;
                 TrustServerCertificate=Yes;
                 "
)

#dbGetQuery(conn, "select top(5) * from Rapporteringsnivaa_temp order by ID desc")
#dbGetQuery(conn, "select top(5) * from Takseringsomrade_temp order by OmradeID desc")
#dbGetQuery(conn, "select top(5) * from Takseringslinje_temp order by LinjeID desc")



# Loading test data -------------------------------------------------------

## Bogn og gilten data
#test_dat_1 <- read_delim("Data/bogn_og_gilten_skogsfugl_2023_WGS84.csv", locale = locale(encoding = "latin1"))

# Jølster data Torsbotn
#shape_torsbotn <- st_read("Data/Torsbotn") # Load shapefile
#test_dat_2 <- tibble(
  #Region = "NINA", Områdenavn = "Jølster", Kommune = "Jølster", Kommunenr = "", Rapporteringsnivå = "Testområde Jølster", # Jølster data 1
  #Linjenr = shape_torsbotn$id, Linjenavn = shape_torsbotn$navn, WKT = st_as_text(shape_torsbotn$geometry)
#)
#test_dat_2
## Jølster data Bjørset
#shape_bjorset <- st_read("Data/Bjørset") # Load shapefile
#test_dat_3 <- tibble(
  #Region = "NINA", Områdenavn = "Jølster", Kommune = "Jølster", Kommunenr = "", Rapporteringsnivå = "Testområde Jølster", # Jølster data 1
 # Linjenr = shape_bjorset$id, Linjenavn = shape_bjorset$Navn, WKT = st_as_text(shape_bjorset$geometry)
#)
#test_dat_3

# Bytte transekt i data Torsbotn for å sjekke update-funksjon

#test_dat_4 <- test_dat_2
#test_dat_4$WKT[1] <- test_dat_3$WKT[1]
#data <- test_dat_1


# Funksjon for å legge til / oppdatere nye linjer i HFP -------------------------------------



append_to_HFP_func <- function(data, RegionID) {
  ## Check if region exists i DB, if not append with new ID
  Rappnivaa <- dbGetQuery(conn, "select * from Rapporteringsnivaa_temp") # Laster ned Rapporteringsnivå fra HFP

  if (any(Rappnivaa$Navn == unique(data$Rapporteringsnivå)) == FALSE) {
    rappnivaa_newID <- max(Rappnivaa$ID) + 1
    print(paste0("Nytt rapporteringsnivå for ", "'", unique(data$Rapporteringsnivå), "'", ": ", rappnivaa_newID))

    newRow_Rappnivaa <- data.frame("Navn" = unique(data$Rapporteringsnivå), "FK_RegionID" = rappnivaa_newID)
    dbWriteTable(conn, "Rapporteringsnivaa_temp", newRow_Rappnivaa, append = TRUE)
    print(dbGetQuery(conn, "select top(5) * from Rapporteringsnivaa_temp order by ID desc"))
  } else {
    rappnivaa_newID <- Rappnivaa$ID[which(Rappnivaa$Navn == unique(data$Rapporteringsnivå))]
    print(paste0("Rapporteringsnivå for ", "'", Rappnivaa$Navn[which(Rappnivaa$Navn == unique(data$Rapporteringsnivå))], "'", " ligger i databasen: ", rappnivaa_newID))
  }

  # 2. Sjekk og ev legg inn nye områder i tabell "Takseringsomrade" ---------

  # Laster inn tabell med FylkeskommuneNR
  fylkekomnr <- dbReadTable(conn, "FYLKEKOMMUNE") %>%
    mutate_if(is.character, str_trim) ## Fjerner mellomrom i Kommunenavn kolonnen

  # Lager ny df med unike områdenavn fra nye takseringslinjedata og legger til fylkeskommuneNR
  taksdat <- data %>% # Ny df med kun unike områdenavn
    select(1:5) %>%
    distinct() %>%
    mutate(Kommunenavn = Kommune, .keep = "unused") %>%
    left_join(., fylkekomnr, by = "Kommunenavn") %>%
    mutate(omradeID = 0, Rappnivaa = rappnivaa_newID)


  ## Sjekker om områdenavn eksisterer i DB, om ikkje leggast det til ny områdeID
  takseringsomrade <- dbReadTable(conn, "Takseringsomrade_temp") # Laster ned "Takseringsomrade" til R-dataframe

  for (i in 1:nrow(taksdat)) {
    if (any(takseringsomrade$OmradeNavn == taksdat$Områdenavn[i]) == FALSE) {
      taksdat$omradeID[i] <- max(takseringsomrade$OmradeID) + i
      print(paste0("Nytt takseringsområde for ", "'", taksdat$Områdenavn[i], "'", ": ", taksdat$omradeID[i]))

      # Legg inn nye rader i "Takseringsomrade_temp"
      newRow_Takseringsomrade <- data.frame("FK_Fylkekomnr" = taksdat$Fylkekomnr, "OmradeNavn" = taksdat$Områdenavn, "FK_RapporteringsnivaaID" = rappnivaa_newID) %>%
        filter(OmradeNavn == taksdat$Områdenavn[i])
      newRow_Takseringsomrade
      dbWriteTable(conn, "Takseringsomrade_temp", newRow_Takseringsomrade, append = TRUE)
    } else {
      taksdat$omradeID[i] <- takseringsomrade$OmradeID[which(takseringsomrade$OmradeNavn == taksdat$Områdenavn[i])]
      print(paste0("Takseringsområde for ", "'", taksdat$Områdenavn[i], "'", "ligger allerede i databasen med ID: ", taksdat$omradeID[i]))
    }
  }


  # 3. Legg inn nye / oppdatere takseringslinjer i tabell "Takseringslinje" -------------
  linedat <- left_join(data, taksdat, by = join_by(Region, Områdenavn, Kommunenr, Rapporteringsnivå)) ## nytt datasett med linjer og områdeID henta frå databasen

  ## Sjekk om linjene ligger der fra før
  linesHFP <- dbReadTable(conn, "Takseringslinje_temp") # Laster ned "Takseringslinje" til R-dataframe
  linedat$Status <- linedat$Linjenavn %in% linesHFP$Linjenavn # Om linjenavn finnes fra før Status = TRUE, om ikkje Status = False

  if (any(linedat$Status == FALSE)) {
  newRow_takseringlinje <- linedat %>%  ### Legger til nye takseringslinjer: Status == FALSE
    filter(Status == FALSE) %>%
    mutate(FK_OmradeID = omradeID,
           STAsText = WKT,
           Linjenavn = Linjenavn,
           Prosjekt = "",
           Rapporteringsniva = Rapporteringsnivå,
           Region = Region,
           Aktiv = 1,
           .keep = 'none')
  print(paste0("Nye takseringslinjer lagt til: ",paste(newRow_takseringlinje$Linjenavn, collapse = " ")))
  dbWriteTable(conn, "Takseringslinje_temp", newRow_takseringlinje, append = TRUE)

  } else if (any(linedat$Status == TRUE)){
  updateRow_takseringlinje <- linedat %>%  ### Oppdaterer eksisterende takseringslinjer: Status == TRUE
    filter(Status == TRUE) %>%
    mutate(FK_OmradeID = omradeID,
           STAsText = WKT,
           Linjenavn = Linjenavn,
           Prosjekt = "",
           Rapporteringsniva = Rapporteringsnivå,
           Region = Region,
           Aktiv = 0,
           .keep = 'none')

  for (i in 1:nrow(updateRow_takseringlinje)){ ## SQL-kode for å oppdatere linjer som allerie ligg inne i HFP
    dbExecute(conn,
              paste0("UPDATE Takseringslinje_temp SET STAsText = ", "'", updateRow_takseringlinje$STAsText[i], "' ",
                     "WHERE Linjenavn =", "'", paste(updateRow_takseringlinje$Linjenavn[i]), "'"))
  }
  print(paste0("Takseringslinjer oppdatert: ",paste(updateRow_takseringlinje$Linjenavn, collapse = " ")))

  }

  # Lagar kart over nye og oppdaterte linjer
  linjekart <- dbGetQuery(conn, paste0("select LinjeID, STAsText from Takseringslinje_temp where Linjenavn in (", paste0(sprintf("'%s'",linedat$Linjenavn), collapse = ", "), ")")) %>%
    st_as_sf(., wkt = "STAsText")

  leaflet(linjekart) %>%
    addTiles(group = "OSM") %>%
    addPolygons() %>%
    print()

  # Konverterar WKT til Geom i HFP for nye linjer
  dbExecute(conn, paste0("update Takseringslinje_temp
          set Geom = geography::STGeomFromText(STAsText, 4326)
          where FK_OmradeID in (", paste0(sprintf("'%s'", unique(linedat$omradeID)), collapse = ", "), ")")) ## Konverterer WKT til Geom

  # 4. SQL kode for å oppdatere GEOM (omriss av område) i tabell "Rapporteringsniva" -------
  dbExecute(conn, paste("
Update Rapporteringsnivaa_temp Set geom =
(
	select geography::ConvexHullAggregate(Takseringslinje_temp.Geom) from Takseringslinje_temp
	inner join Takseringsomrade_temp on Takseringsomrade_temp.OmradeID = Takseringslinje_temp.FK_OmradeID
	where Takseringsomrade_temp.FK_RapporteringsnivaaID = ", unique(linedat$Rappnivaa), "
)
where Rapporteringsnivaa_temp.ID = ", unique(linedat$Rappnivaa)))

  # Lager kart over oppdatert omriss av Rapporteringsnivå
  omriss <- st_read(conn, query = paste0("SELECT ID, geom.STAsBinary() AS Shape FROM Rapporteringsnivaa_temp WHERE ID =", unique(linedat$Rappnivaa)))
  leaflet(omriss) %>%
    addTiles(group = "OSM") %>%
    addPolygons()
}

append_to_HFP_func(test_dat_4, 3)



# Reset alle midlertidige tabellar i HFP -------------------------------------------------------------------

dbGetQuery(conn, "select top(15) * from Rapporteringsnivaa_temp order by ID desc")
dbExecute(conn, "drop table Rapporteringsnivaa_temp;") # sletter tabell "Rapporteringsnivaa_temp"
dbExecute(conn, "select * into Rapporteringsnivaa_temp from Rapporteringsnivaa;") # Kopierer tabell "Rapporteringsnivaa" til "Rapporteringsnivaa_temp"
dbListFields(conn, "Rapporteringsnivaa_temp")

dbGetQuery(conn, "select top(5) * from Takseringsomrade_temp order by OmradeID desc")
dbExecute(conn, "drop table Takseringsomrade_temp;") # sletter tabell "Takseringsomrade_temp"
dbExecute(conn, "select * into Takseringsomrade_temp from Takseringsomrade") # Kopierer tabell "Takseringsomrade" til "Takseringsomrade_temp"
dbGetQuery(conn, "select top(5) * from Takseringsomrade_temp")



dbGetQuery(conn, "select top(15) * from Takseringslinje_temp order by LinjeID desc")
dbExecute(conn, "drop table Takseringslinje_temp;") # sletter tabell "Takseringslinje_temp"
dbExecute(conn, "select * into Takseringslinje_temp from Takseringslinje") # Kopierer tabell "Takseringslinje" til "Takseringslinje_temp"
dbGetQuery(conn, "select top(5) * from Takseringslinje_temp")
