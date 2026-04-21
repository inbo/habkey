#' maak een soortenlijst op basis van de opname(n)
#'
#' De functie stelt een soortenlijst samen op basis van de standaard flora-lijst
#' , een soortenlijst die specifiek voor dit package werd samengesteld en
#' (optioneel) een soortenlijst van de gebruiker.#'
#' De soortenlijst bevat alleen soortnamen die aanwezig zijn in het bestand met
#' de opname(n). Naast de standaardnaam bevat de tabel ook soortinformatie die
#' nuttig is bij het determineren van de opname(n).
#'
#' Deze lijst wordt bewaard in een door de gebruiker opgegeven locatie.
#'
#' @inheritParams laad_opnamen
#' @param dir_achtergronddata String. De naam van de map met basisgegevens,
#' zoals standaardlijsten.
#' Standaardlijsten worden met het package meegeleverd.
#'
#' @importFrom assertthat assert_that is.dir
#' @importFrom dplyr bind_rows group_by if_else mutate rename row_number
#' select slice_min ungroup
#' @importFrom tibble rowid_to_column
#' @importFrom stats na.omit
#' @importFrom rlang .data
#' @importFrom utils read.csv2
#' @export

maak_soortenlijst <- function(naam_opnamen,
                              dir_invoerdata,
                              dir_achtergronddata =
                                file.path(
                                  system.file(package = "habkey"),
                                  "basisdata"
                                )) {
  # check of map dir_achtergronddata bestaat
  assert_that(is.dir(dir_achtergronddata))

  # inlezen van dataset met de opnamen (incl. foutafhandeling)
  df_opnamen_bron <- laad_opnamen(naam_opnamen, dir_invoerdata)

  # inlezen van verschillende soortenlijsten
  #   standaardlijst inbo
  # zie taxonlijst_importeren() om een nieuwe standaardlijst aan te maken.

  foutmelding <- sprintf(
    "Bestand '%s.csv' ontbreekt in %s.", "taxa", dir_achtergronddata
  )

  soorten_inbo <- lees_csv2(dir_achtergronddata, "taxa.csv",
    foutboodschap = foutmelding
  )


  #   soortenlijst Access
  foutmelding <- sprintf(
    "Bestand '%s.csv' ontbreekt in %s.", "specsyno", dir_achtergronddata
  )

  soorten_acc <- lees_csv2(dir_achtergronddata, "specsyno.csv",
    foutboodschap = foutmelding
  )

  #   eigen soortenlijst
  # als die bestaat wordt deze ingeladen, anders wordt een lege tabel gemaakt.
  # en een volgnummer begonnen
  file_custom <- file.path(dir_invoerdata, "custom_taxonlijst.csv")
  prefix_id <- "CU"
  if (file.exists(file_custom)) {
    soorten_eigen <- read.csv2(file_custom, stringsAsFactors = FALSE)
    ids_num <- gsub(paste0("^", prefix_id), "", soorten_eigen$taxonid)

    if (length(ids_num) == 0) {
      laatste_nummer <- 0
    } else {
      laatste_nummer <- max(as.numeric(ids_num), na.rm = TRUE)
    }
  } else {
    soorten_eigen <- soorten_inbo[0, ]
    laatste_nummer <- 0
  }


  # Controle van de soortnamen
  # Komen de soortnamen uit de opnamen allemaal voor in de inbo-standaardlijst?

  # Referentie-pool opbouwen voor de check
  # We maken een vector van ALLE bekende namen en zetten deze om naar
  # kleine letters
  bekende_namen_raw <- unique(c(
    soorten_inbo$tax_orig,
    soorten_inbo$tax_canon,
    soorten_inbo$taxon,
    soorten_acc$originalna,
    soorten_acc$specieszonderauteur_ori,
    soorten_eigen$tax_orig,
    soorten_eigen$tax_canon
  ))

  # omzetten van de soortnamen in kleine letters omdat dit anders onterecht
  # aanleiding kan geven voor een (verkeerde) nieuwe soort-melding
  bekende_namen_lower <- tolower(na.omit(bekende_namen_raw))

  # we herhalen dit maar nu alleen voor de namen van de officiele inbo-lijst
  officiele_taxa_lower <- tolower(unique(soorten_inbo$taxon))

  # soortnamen ophalen uit de opnamen
  soorten_opg <- unique(df_opnamen_bron$speciesnaam)

  # vegetatienamen checken

  # nolint start
  soorten_eigen <-
    check_soortnamen(
      custom_taxa = soorten_eigen,
      soorten_opg,
      bekende_namen_lower = bekende_namen_lower,
      officiele_taxa_lower = officiele_taxa_lower,
      prefix_id = prefix_id,
      laatste_nummer = laatste_nummer,
      file_custom = file_custom
    )
  # nolint end

  # We hebben de soortnamen gecontroleerd.
  # We gaan nu een soortenlijst opbouwen die in de eerste plaats is opgehangen
  # aan de soortenlijst in het package (specsyno), omdat deze tabel belangrijke
  # soortinfo bevat, die niet in de andere lijsten zit.

  # basis uit access
  soorten_acc_opg <- soorten_acc[soorten_acc$originalna %in% soorten_opg, ]

  # volgnr toekennen en kolomnamen met soortnamen wijzigen
  # wet_naam verwijst naar de officiële naam
  # speciesnam verwijst naar de gebruikte naam in de opname

  soorten <- soorten_acc_opg |>
    rowid_to_column(var = "soortnr") |>
    rename(
      wet_naam = "speciesnam",
      speciesnaam = "originalna"
    )

  # hoogste soortnr zoeken
  soortnr_max <- nrow(soorten)

  # Hieraan worden eventueel nog soorten uit de standaardlijst en de eigen lijst
  # aan toegevoegd. We gaan het technisch eenvoudig doen, door geen rekening te
  # houden met de andere lijsten => we introduceren eerst veel dubbels, die we
  # daarna wel wegwerken.

  soorten_inbo_opg <- soorten_inbo[soorten_inbo$tax_orig %in% soorten_opg, ]
  soorten_eigen_opg <- soorten_eigen[soorten_eigen$tax_orig %in% soorten_opg, ]

  soorten_extra_opg <- bind_rows(soorten_inbo_opg, soorten_eigen_opg)

  soorten_extra_opg <- soorten_extra_opg |>
    select(
      wet_naam = "taxon",
      speciesnaam = "tax_orig",
      "taxon_group"
    ) |>
    mutate(
      vaatplant = if_else(.data$taxon_group == "VP", 1, 0),
      is_mos = if_else(.data$taxon_group == "BR", 1, 0),
      is_lichen = if_else(.data$taxon_group == "LI", 1, 0),
      is_kranswier = if_else(.data$taxon_group == "CH", 1, 0)
    ) |>
    mutate(soortnr = soortnr_max + row_number()) |>
    select(-"taxon_group")

  toon <- soorten$speciesnaam[!soorten$speciesnaam %in% soorten_inbo$tax_orig]
  if (length(toon) > 0) {
    message(
      "\nOverzicht van de soorten die alleen in de package-lijst voorkomen\n"
    )
    print(toon)
  }

  toon <- soorten_inbo_opg$tax_orig[
    !soorten_inbo_opg$tax_orig %in% soorten$speciesnaam
  ]
  if (length(toon) > 0) {
    # nolint start
    warning(
      paste(
        "\nOverzicht van de soorten die wel in de standaardlijst staan",
        "maar niet in de package-lijst\n", "Ze tellen alleen mee voor
          het soortenaantal.\n"
      )
    )
    print(toon)
    # nolint end
  }

  # samenvoegen, waarbij bij de dubbels de prioriteit gegeven wordt aan de
  # Package-lijst
  soorten <- soorten |>
    bind_rows(soorten_extra_opg) |>
    group_by(.data$speciesnaam) |>
    slice_min(order_by = .data$soortnr) |>
    ungroup() |>
    select(-.data$id)

  # bewaren van het resultaat in tijdelijke omgeving
  bewaar_interim_data(soorten, "soorten")

  return(soorten)
}
