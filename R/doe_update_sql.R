#' @title voer bijwerk-SQLregels geschreven voor SQL-Access uit in SQLite
#' @description Deze functie voert bijwerkqueries in een SQLite databank uit op
#' basis van bijwerkqueries ('Update ... set') voor Access.
#' @details
#' Dit wordt in twee stappen uitgevoerd.
#' Eerst wordt van de updatequery omgezet in een create-query die een tijdelijke
#' tabel aanmaakt (met de soms complexe linken). Ze bevat een eenduidige unieke
#' link met de feitelijke doeltabel die moet bijgewerkt worden.
#' Het bijwerken van deze doeltabel wordt dan in tweede instantie uitgevoerd.
#' Doordat tussen de tijdelijke tabel en de doeltabel een eenduidige, simpele
#' link bestaat, is dit een vrij eenvoudige operatie, veel eenvoudiger en dus
#' betrouwbaarder dan de update-query willen vertalen in één keer.
#'
#' @importFrom DBI dbExecute
#' @importFrom stringr regex str_extract str_locate str_match str_squish str_sub
#' str_replace_all str_trim
#'
#' @param con Een `DBIConnection` object (SQLite-databank)
#' @param sql String. Een SQL-regel die een Access bijwerk-bewerking kan
#' uitvoeren.
#' @param temp_tabelnaam String. Een naam voor de tijdelijke tabel in de
#' SQLite-databank
#'
#' @returns het resultaat van de bijwerkquery is een tabel onder de vorm van een
#' dataframe
#'
#' @family converteer_sql
doe_update_sql <-
  function(con, sql, temp_tabelnaam = "tmp_update_logic") {
    # 1. Schoonmaak
    sql_clean <- str_replace_all(sql, "[\r\n]+", " ") |>
      str_squish()

    # 2. Vind positie van SET (Het ankerpunt)
    loc_set <- str_locate(sql_clean, regex("\\sSET\\s", ignore_case = TRUE))

    if (is.na(loc_set[1])) {
      stop(
        "Geen SET gevonden in UPDATE query: ",
        sql
      )
    }

    # 3. Analyseer deel VOOR de SET (UPDATE ... JOIN ...)
    # We halen "UPDATE " eraf (eerste 7 tekens)
    part_before_set <- str_sub(sql_clean, 7, loc_set[1] - 1) |> str_squish()

    # A. De Tabelnaam (Target) is het EERSTE woord
    target_table_raw <- str_extract(part_before_set, "^[\\(\\s]*[\\w\\[\\]]+")
    target_table <-
      str_replace_all(target_table_raw, "[\\[\\]\\(\\)]", "") |>
      str_trim()

    # B. De 'Source Definition' (Target + Joins)
    # Dit hele stuk gebruiken we in de FROM van de SELECT
    from_source <- part_before_set

    # 4. Analyseer deel NA de SET (Toewijzing + WHERE)
    part_after_set <- str_sub(sql_clean, loc_set[2] + 1)

    loc_where <- str_locate(part_after_set, regex("\\sWHERE\\s",
      ignore_case = TRUE
    ))

    if (!is.na(loc_where[1])) {
      assignments <- str_sub(part_after_set, 1, loc_where[1] - 1) |>
        str_squish()
      where_clause <- str_sub(part_after_set, loc_where[1]) # inclusief" WHERE "
    } else {
      assignments <- part_after_set |> str_squish()
      where_clause <- ""
    }

    # 5. Haal kolom en waarde uit de assignments
    # Verwacht: [Tabel].Kolom = Waarde
    match_assign <-
      stringr::str_match(assignments, "([\\w\\.]+)\\s*=\\s*(.+)")
    target_col_raw <- match_assign[1, 2]
    new_value <- match_assign[1, 3]

    target_col <- str_extract(target_col_raw, "[^\\.]+$") # Alleen de kolomnaam
    target_col <- str_replace_all(target_col, "[\\[\\]]", "")

    # 6. BOUW DE SELECT (STAGING)
    # We gebruiken de originele join-structuur in de FROM
    create_sql <- paste0(
      "CREATE TEMP TABLE ", temp_tabelnaam, " AS ",
      "SELECT DISTINCT [", target_table, "].recordinggivid, ", new_value,
      " AS new_val ",
      "FROM ", from_source, " ",
      where_clause
    )

    # 7. UITVOEREN
    # A. Oude temp weg
    tryCatch(
      {
        dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp_tabelnaam))
      },
      error = function(e) {}
    )

    # B. Maak nieuwe temp (Hier treden eventuele syntaxfouten op, nu leesbaar)
    tryCatch(
      {
        dbExecute(con, create_sql)
      },
      error = function(e) {
        cat("\nFOUT IN CREATE SQL:\n", create_sql, "\n")
        stop(e)
      }
    )

    # C. Index (Snelheidswinst bij update)
    dbExecute(con, paste0(
      "CREATE INDEX idx_", temp_tabelnaam, " ON ",
      temp_tabelnaam, "(recordinggivid)"
    ))

    # D. De Update (Simpele 1-op-1 join)
    update_sql <- paste0(
      "UPDATE ", target_table, " ",
      "SET ", target_col, " = ", temp_tabelnaam, ".new_val ",
      "FROM ", temp_tabelnaam, " ",
      "WHERE ", target_table, ".recordinggivid = ", temp_tabelnaam,
      ".recordinggivid"
    )

    rows_affected <- dbExecute(con, update_sql)

    # E. Opruimen
    dbExecute(con, paste0("DROP TABLE ", temp_tabelnaam))

    return(rows_affected)
  }
