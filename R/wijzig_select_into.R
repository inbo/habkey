#' @title converteer "Select ... into"-sql-regels naar SQLite "CREATE TABLE ..."
#' statements.
#' @description Deze functie wijzigt de syntax van queries die in Access
#' gebruikt worden om tabellen te maken in een syntax die hetzelfde doet in
#' SQLite. Indien de naam van de tabel al bekend is in de SQLite zal deze uit de
#' databank zonder waarschuwing verwijderd worden.
#' De conversie slaat alleen op SQL-regels die het INTO-statement bezitten.
#' Andere SQL-regels blijven onveranderd.
#'
#' @importFrom DBI dbExecute
#' @importFrom stringr regex str_match str_replace str_replace_all
#'
#' @param con Een `DBIConnection` object (SQLite-databank)
#' @param sql String. Een SQL-regel die 'INTO' bevat.
#'
#' @returns een string met de gewijzigde SQL-regel
#'
#' @family converteer_sql


# "Select ... into"-sql-regels vervangen

wijzig_select_into <- function(con, sql) {
  if (!grepl("\\bINTO\\b", sql, ignore.case = TRUE)) {
    return(sql)
  }

  # De regex "[\r\n]+" zoekt naar alle opeenvolgende combinaties van regeleinden
  # (CR en LF) en vervangt deze door een spatie.
  proc_sql_clean <- str_replace_all(sql, "[\r\n]+", " ")

  # Tabelnaam detecteren
  match_data <-
    str_match(proc_sql_clean, regex("INTO\\s+(\\S+)",
      ignore_case = TRUE
    ))
  tabel_te_maken <- match_data[2]

  # Drop table logic (Side effect!)
  if (!is.na(tabel_te_maken)) {
    clean_tbl_name <- str_replace_all(tabel_te_maken, "\\[|\\]|;", "")
    tryCatch(
      {
        dbExecute(con, paste("DROP TABLE IF EXISTS", clean_tbl_name))
      },
      error = function(e) {}
    )
  }

  # String replacement
  str_replace(
    proc_sql_clean,
    regex("SELECT\\s+(.+?)\\s+INTO\\s+(\\S+)\\s+(FROM.+)",
      ignore_case = TRUE, dotall = TRUE
    ),
    "CREATE TABLE \\2 AS SELECT \\1 \\3"
  )
}
