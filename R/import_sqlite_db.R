#' @title importeer basisdata naar een SQLite databank
#' @description Deze functie importeert een reeks basisgegevens in een
#' SQLite-databank. Deze gegevens dienen om de opnamen te karakteriseren met
#' behulp van een set SQL-regels.
#'
#' @importFrom assertthat assert_that
#' @importFrom DBI dbExecute dbWriteTable
#'
#' @inheritParams bereken_indicator
#' @inheritParams laad_kopdata
#' @inheritParams laad_opnamen
#' @inheritParams karakteriseer_opname
#' @param con Een `DBIConnection` object (SQLite-databank)
#' @param df_kop_indicator Dataframe. De tabel met de kopgegevens
#' met ook de berekende indicatorwaarden ervan. Default verwijst het naar een
#' dataframe dat het resultaat is van `bereken_indicator`
#' @param df_opname Dataframe. De tabel met opname(n)

import_sqlite_db <- function(con,
                             df_kop_indicator = NULL,
                             df_opname = NULL,
                             dir_invoerdata = NULL,
                             df_soorten,
                             df_soortenlijstjes,
                             df_regels) {
  # inlezen van dataset met de kopgegevens (incl. foutafhandeling)
  # eerst checken of het bestand met de kopdata al eens werd ingeladen
  df_kop <- df_kop_indicator
  if (is.null(df_kop)) {
    df_kop <- haal_interim_data("kopdata_met_indicatoren")
  } else {
    msg <- "df_kop_indicator is geen data.frame"
    assert_that(is.data.frame(df_kop_indicator), msg = msg)
  }

  # inlezen van dataset met de opnamen (incl. foutafhandeling)
  if (is.null(df_opname)) naam_opnamen <- "opnamen"
  msg <- "df_opname en dir_invoerdata mogen beide niet NULL zijn."
  assert_that(!is.null(df_opname), !is.null(dir_invoerdata), msg = msg)
  df_opname <- laad_opnamen(naam_opnamen, dir_invoerdata)

  msg <- "df_soorten is geen data.frame"
  assert_that(is.data.frame(df_soorten), msg = msg)

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

  # Tabellen schrijven
  dbWriteTable(con, "tbl_meta", df_kop, overwrite = TRUE)
  dbWriteTable(con, "tbl_opnamen", df_opname, overwrite = TRUE)
  dbWriteTable(con, "tbl_soorten", df_soorten, overwrite = TRUE)
  dbWriteTable(con, "tbl_regels", df_regels, overwrite = TRUE)
  dbWriteTable(con, "tbl_lijstjes", df_soortenlijstjes, overwrite = TRUE)

  # Indexen
  dbExecute(con, "DROP INDEX IF EXISTS idx_meta_id")
  dbExecute(con, "CREATE INDEX idx_meta_id ON tbl_meta (recordinggivid)")

  dbExecute(con, "DROP INDEX IF EXISTS idx_opnamen_id")
  dbExecute(
    con,
    "CREATE INDEX idx_opnamen_id ON tbl_opnamen (recordinggivid)"
  )

  dbExecute(con, "DROP INDEX IF EXISTS idx_soorten_eigennaam")
  dbExecute(
    con,
    "CREATE INDEX idx_soorten_eigennaam ON tbl_soorten (speciesnaam)"
  )

  dbExecute(con, "DROP INDEX IF EXISTS idx_soorten_offnaam")
  dbExecute(
    con,
    "CREATE INDEX idx_soorten_offnaam ON tbl_soorten (wet_naam)"
  )

  dbExecute(con, "DROP INDEX IF EXISTS idx_regels")
  dbExecute(
    con,
    "CREATE INDEX idx_regels ON tbl_regels (tabel, gecheckt, regel)"
  )

  dbExecute(con, "DROP INDEX IF EXISTS idx_soortenl")
  # nolint start
  dbExecute(
    con,
    "CREATE INDEX idx_soortenl ON tbl_lijstjes (soortenlijstnr, wet_naam)"
  )
  # nolint end
}
