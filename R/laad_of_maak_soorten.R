#' @title laad soortenlijst of maak ze aan
#' @description
#' Deze functie controleert of een soortenlijst specifiek voor de opnamen al
#' bestaat. Ze heeft
#' Als dat niet het geval is, wordt ze aangemaakt.
#'
#' @param datanaam String. Naam van de soortenlijst. Als het een eigen
#' ingebrachte soortenlijst is dan moet het er een volledige verwijzing
#' naar zijn (inclusief pad en bestandsnaam en extensie), alleen csv-bestanden.
#' @param ... Extra argumenten die direct worden doorgegeven aan
#' `maak_soortenlijst` (zoals `naam_opnamen`, `dir_invoerdata` en
#' `dir_achtergronddata`).

#' @importFrom utils read.csv2
#' @returns Een soortenlijst als dataframe
laad_of_maak_soorten <- function(datanaam = "soorten", ...) {
  # Scenario 1: Interne caching of nieuw aanmaken
  if (is.null(datanaam) || datanaam == "soorten") {
    # 1. Probeer op te halen uit de cache
    cache_naam <- if (is.null(datanaam)) "soorten" else datanaam
    soorten <- haal_interim_data(cache_naam)

    # 2. Check of ze nog gemaakt moet worden
    if (is.null(soorten)) {
      message("Soortenlijst wordt aangemaakt...")

      # Maak de lijst. De `...` vangt alle specifieke argumenten af
      soorten <- maak_soortenlijst(...)

      # Sla ze op in de sessie-cache zodat dit de volgende keer wordt
      # overgeslagen
      bewaar_interim_data(soorten, cache_naam)
    }

    return(soorten)

    # Scenario 2: Gebruiker levert een eigen CSV-bestand aan
  } else {
    if (!file.exists(datanaam)) {
      stop(sprintf("Geen soortenlijst gevonden op deze locatie: %s", datanaam))
    }

    message("Eigen soortenlijst wordt ingeladen vanuit CSV...")
    soorten <- read.csv2(datanaam, stringsAsFactors = FALSE)
    return(soorten)
  }
}
