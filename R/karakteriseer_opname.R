#' @title karakteriseer een of meer opname(n)
#' @description Deze functie typeert een vegetatieopname aan de hand van een
#' aantal kenmerken.
#' Deze kenmerken zijn de uitkomsten = antwoorden op een reeks vragen van een
#' determineersleutel.
#' @details
#' De vragen zijn in eerste instantie omgezet in een reeks SQL-queries binnen
#' MS-Access. Deze sql-regels moeten hier ingevoerd worden.
#'
#' @returns een dataframe waarin elke vraag een eigen kolom heeft en elke
#' opname een eigen regel
#'
#' @inheritParams maak_soortenlijst
#' @inheritParams laad_kopdata
#' @inheritParams laad_opnamen
#' @inheritParams bereken_indicator

#' @param df_regels Dataframe. De tabel met de SQL-regels waarmee een opname kan
#' gekarakteriseerd worden.
#' @param df_regel_selectie Dataframe. De tabel waarin alle kolommen die
#' beginnen met 'Regel' verwijzen naar een vraag/regel in de determineersleutel.
#' In deze functie wordt alleen beroep gedaan op de kolomnamen van de tabel.
#' @param sleutel_naam_sql String. Een karakter dat verwijst naar een specifieke
#' determineersleutel. Mogelijke waarden zijn c("O", "G", "H", "M"), die resp.
#' verwijzen naar de hoofd-, grasland-, heide- en moerassleutel.
#' @param ... overige parameters, bijv. overschrijf = TRUE of herbereken = TRUE
#'
#' @importFrom dplyr arrange filter
#' @importFrom assertthat assert_that is.string
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbListTables dbReadTable
#' dbWriteTable
#' @importFrom rlang .data
#' @importFrom RSQLite SQLite
#'
#' @export
#'
karakteriseer_opname <- function(naam_kopdata,
                                 naam_opnamen,
                                 dir_invoerdata,
                                 df_soorten = NULL,
                                 df_regels = NULL,
                                 df_soortenlijstjes = NULL,
                                 df_regel_selectie = file.path(
                                   system.file(package = "habkey"),
                                   "basisdata",
                                   "tbl_sleutel_uitkomst_hoofdtabel.csv"
                                 ),
                                 sleutel_naam_sql = c("O", "G", "H", "M"),
                                 ...) {
  # 00 mogelijke reset (via argument `herbereken`), standaard wordt niets gewist
  wis_interim_data()

  # 0. controle
  # Pak de default (de eerste waarde) als de gebruiker niets heeft ingevuld
  sleutel_naam_sql <- sleutel_naam_sql[1]
  beschikbare_sleutels <- c("O", "G", "H", "M")
  msg <- sprintf(
    "sleutel_naam_sql moet \u00e9\u00e9n van volgende waarden zijn: %s",
    paste(beschikbare_sleutels, collapse = ", ")
  )
  assert_that(is.string(sleutel_naam_sql),
    sleutel_naam_sql %in% c("O", "G", "H", "M"),
    msg = msg
  )

  # inlezen van dataset met de kopgegevens (incl. foutafhandeling)
  assert_that(is.string(naam_kopdata),
    msg = paste(
      "Het argument naam_kopdata is leeg.",
      "Geef hier de naam van het bestand met de kopgegevens."
    )
  )
  df_kop <- haal_interim_data("kopdata_met_indicatoren")
  if (is.null(df_kop)) {
    df_kop <- bereken_indicator(
      naam_kopdata,
      naam_opnamen,
      dir_invoerdata
    )
  }

  # inlezen van dataset met de opnamen (incl. foutafhandeling)
  df_opname <- laad_opnamen(naam_opnamen, dir_invoerdata)

  if (is.null(df_soorten)) {
    df_soorten <- laad_of_maak_soorten(
      datanaam = "soorten"
    )
  } else {
    msg <- "df_soorten is geen data.frame"
    assert_that(is.data.frame(df_soorten), msg = msg)
  }

  msg <- "df_regel_selectie is geen data.frame"
  assert_that(is.data.frame(df_regel_selectie), msg = msg)

  df_file <- "tbl_soorten_in_lijst.csv"
  df_soortenlijstjes <- laad_of_check_df(df_soortenlijstjes, df_file)

  df_file <- "tbl_sql_regels.csv"
  df_regels <- laad_of_check_df(df_regels, df_file)

  # controleer of alle te indexeren velden wel bestaan
  vereiste_kolommen <- c("recordinggivid")
  msg <- sprintf(
    "De vereiste kolom (%s) ontbreekt in %s.",
    vereiste_kolommen, "df_kop"
  )
  assert_that(
    all(vereiste_kolommen %in% names(df_kop)),
    msg = msg
  )

  msg <- sprintf(
    "De vereiste kolom (%s) ontbreekt in %s.",
    vereiste_kolommen, "df_opname"
  )
  assert_that(
    all(vereiste_kolommen %in% names(df_opname)),
    msg = msg
  )

  vereiste_kolommen <- c("speciesnaam")
  msg <- sprintf(
    "De vereiste kolom (%s) ontbreekt in %s.",
    vereiste_kolommen, "df_soorten"
  )
  assert_that(
    all(vereiste_kolommen %in% names(df_soorten)),
    msg = msg
  )

  vereiste_kolommen <- c("tabel", "gecheckt", "regel")
  msg <- sprintf(
    "Niet alle vereiste kolommen (%s) zijn aanwezig in %s.",
    paste(vereiste_kolommen, collapse = ","), "df_regels"
  )
  assert_that(
    all(vereiste_kolommen %in% names(df_regels)),
    msg = msg
  )

  vereiste_kolommen <- c("soortenlijstnr", "wet_naam")
  msg <- sprintf(
    "Niet alle vereiste kolommen (%s) zijn aanwezig in %s.",
    paste(vereiste_kolommen, collapse = ","), "df_soortenlijstjes"
  )
  assert_that(
    all(vereiste_kolommen %in% names(df_soortenlijstjes)),
    msg = msg
  )

  sleutelvragen <- grep("Regel", colnames(df_regel_selectie),
    value = TRUE,
    ignore.case = TRUE
  )
  # nolint start
  assert_that(
    length(sleutelvragen) > 0,
    msg = "Er zijn in 'df_regel_selectie' geen kolommen gevonden die het woord 'Regel' bevatten."
  )
  # nolint end
  message(sprintf("%d regels gevonden om te verwerken", length(sleutelvragen)))

  # 1. Database Setup
  con <- dbConnect(SQLite(), ":memory:")
  on.exit(dbDisconnect(con))

  import_sqlite_db(
    con = con,
    df_kop_indicator = df_kop,
    df_opname = df_opname,
    df_soorten = df_soorten,
    df_soortenlijstjes = df_soortenlijstjes,
    df_regels = df_regels
  )


  # 2. Resultaattabel voorbereiden

  df_resultaat_init <- df_kop |> select(.data$recordinggivid)
  for (regel in sleutelvragen) df_resultaat_init[[regel]] <- 0

  naam_doeltabel <- paste0("resultaat_kop_", sleutel_naam_sql)
  dbWriteTable(con, naam_doeltabel, df_resultaat_init, overwrite = TRUE)
  dbExecute(con, paste0(
    "CREATE INDEX idx_res_id ON ", naam_doeltabel,
    "(recordinggivid)"
  ))

  # 3.  Aanmaak lijst met te wijzigen Accessveldnamen:
  # "AccessNaam" wordt "R_Naam"

  access_naar_r_mapping <- c(
    "SPECIESNAM" = "wet_naam",
    "ORIGINALNA" = "speciesnaam",
    "WetNaam" = "wet_naam",
    "RecordingGivid" = "recordinggivid",
    "Percentage" = "percentage",
    "CoverPctValue" = "cover_pctvalue",
    "CoverageCode" = "coverage_code",
    "Layer" = "layer",
    "VAATPLANT" = "vaatplant",
    "IsBoomStruik" = "is_boomstruik",
    "IsEenBoomStruik" = "is_boomstruik",
    "IsEenDwergstruik" = "is_dwergstruik",
    "IsEenGras" = "is_gras",
    "IsEenHogeplant" = "is_hogeplant",
    "IsEenMeerjarigKruid" = "is_meerjarigkruid",
    "IsEenSchijngras" = "is_schijngras",
    "IsEenjarig" = "is_jarig",
    "IsInvasieveExoot" = "is_invasieve_exoot",
    "IsMos" = "is_mos",
    "IsLichen" = "is_lichen",
    "IsKranswier" = "is_kranswier",
    "IsBossoort" = "is_bossoort",
    "BosBedekking" = "bos_bedekking",
    "DwergstruikBedekking" = "dwergstruik_bedekking",
    "KruidlaagBedekking_vast" = "kruidlaag_bedekking_vast",
    "NietKruidenInKLBedekking_vast" = "nietkruideninkl_bedekking_vast",
    "KruidenInKLBedekking_vast" = "kruideninkl_bedekking_vast",
    "KruidlaagBedekkingSom_Vast" = "kruidlaag_bedekkingsom_Vast",
    "IsKapvlakte" = "iskapvlakte",
    "IsPlagplek_vast" = "isplagplek_vast",
    "IsLandduin_vast" = "islandduin_vast",
    "IsWater_vast" = "iswater_vast",
    "IsDuin_vast" = "isduin_vast",
    "IsUrbaan_vast" = "isurbaan_vast",
    "IsAkker_vast" = "isakker_vast",
    "IsMarien_vast" = "ismarien_vast",
    "IsStrandduin_vast" = "isstrandduin_vast",
    "Ngetal" = "N_getal",
    "Fgetal" = "F_getal",
    "Rgetal" = "R_getal",
    "RNgetal" = "RN_getal",
    "Lgetal" = "L_getal",
    "Sgetal" = "S_getal"
  )

  # 4. Loop door de regels
  verwerk_sql_regels(
    con = con,
    sleutelvragen = sleutelvragen,
    df_regels = df_regels,
    sleutel_naam_sql = sleutel_naam_sql,
    naam_doeltabel = naam_doeltabel,
    mapping_list = access_naar_r_mapping
  )

  # 5. Resultaat ophalen
  df_resultaat_final <- dbReadTable(con, naam_doeltabel)

  # resultaat bewaren in tijdelijke omgeving
  bewaar_interim_data(df_resultaat_final, naam = naam_doeltabel)

  message("Karakterisatie afgerond.")
  return(df_resultaat_final)
}
