# Stap 1 Creëer een interne, lege environment
# Waarom emptyenv()? Dit zorgt ervoor dat R niet per ongeluk in hogere
# environments gaat zoeken als een variabele niet wordt gevonden.
# Het is een veilige, geïsoleerde "kluis".
.mijn_pkg_env <- new.env(parent = emptyenv())


# Stap 2: Maak Setter- en Getter-functies
# In plaats van dat je andere scripts direct in .mijn_pkg_env laat wroeten,
# schrijf je twee simpele interne functies om de data op te slaan (setter) en op
# te halen (getter).
# Dit is veel robuuster.

# Functie om de data op te slaan (Setter)
bewaar_interim_data <- function(data, naam = "tijdelijke_df",
                                overschrijf = TRUE) {
  # Check of het object al bestaat EN of we het mogen overschrijven
  if (!overschrijf && !is.null(.mijn_pkg_env[[naam]])) {
    # nolint start
    stop(sprintf("Let op: Interim data '%s' bestaat al en mag niet overschreven worden!", naam))
    # nolint end
  }

  # Wijs de data toe aan onze veilige kluis
  .mijn_pkg_env[[naam]] <- data
}

# Functie om de data op te halen (Getter)
haal_interim_data <- function(naam = "tijdelijke_df") {
  # Haal de data op
  data <- .mijn_pkg_env[[naam]]

  # data is automatisch NULL als de naam nog niet bestond
  return(data)
}

# Functie om het werkgeheugen weer leeg te maken
wis_interim_data <- function(herbereken = FALSE) {
  if (herbereken) {
    # 1. Haal een lijst op met de namen van alles wat in de kluis zit
    # all.names = TRUE zorgt dat ook verborgen objecten
    # (die met een punt beginnen) worden gezien
    huidige_objecten <- ls(envir = .mijn_pkg_env, all.names = TRUE)

    # 2. Check of er überhaupt iets in zit
    if (length(huidige_objecten) > 0) {
      # 3. Gooi alles weg uit die specifieke environment
      rm(list = huidige_objecten, envir = .mijn_pkg_env)
    }
  }
}
