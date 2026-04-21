#' @title Voer SQL regels iteratief uit
#' @description Interne hulpfunctie om de geneste SQL-loops te verwerken.
#'
#' @inheritParams karakteriseer_opname
#' @inheritParams wijzig_veldnamen_acc
#' @inheritParams import_sqlite_db
#' @param sleutelvragen String. Een label waarmee eenduidig een link kan gelegd
#' worden met een vraag in de determinatiesleutel
#' @param naam_doeltabel String. De naam voor de tabel (dataframe) waarin de
#' resultaten van de karakterisatie van een determinatiesleutel bewaard worden.
#'
#' @family converteer_sql
verwerk_sql_regels <- function(con, sleutelvragen, df_regels, sleutel_naam_sql,
                               naam_doeltabel, mapping_list) {
  total_regels <- length(sleutelvragen)

  for (i in seq_along(sleutelvragen)) {
    huidige_regel <- sleutelvragen[i]
    if (i %% 10 == 0) {
      message(sprintf(
        "Verwerken regel %d/%d: %s", i,
        total_regels, huidige_regel
      ))
    }

    regel_code <- sub("^Regel", "", huidige_regel, ignore.case = TRUE)
    regel_code <- sub("_.*$", "", regel_code)

    sqls_to_run <- df_regels |>
      filter(
        .data$regel == regel_code, .data$tabel == sleutel_naam_sql,
        .data$gecheckt == TRUE
      ) |>
      arrange(.data$sub) # (Kleine tip: gebruik .data$sub net als hierboven)

    if (nrow(sqls_to_run) > 0) {
      for (k in seq_len(nrow(sqls_to_run))) {
        raw_sql <- sqls_to_run$sql[k]

        base_sql <- raw_sql |>
          zoekvervang_sql(naam_doeltabel, huidige_regel) |>
          wijzig_veldnamen_acc(mapping_list)

        is_update <- grepl("^UPDATE\\b", base_sql, ignore.case = TRUE)

        tryCatch(
          {
            if (is_update) {
              doe_update_sql(con, base_sql,
                temp_tabelnaam = paste0("tmp_upd_", i, "_", k)
              )
            } else {
              proc_sql <- wijzig_select_into(con = con, sql = base_sql)
              dbExecute(con, proc_sql)
            }
          },
          error = function(e) {
            warning(paste(
              "SQL Fout in regel", huidige_regel, "(Sub", k, "):",
              e$message, "\nQuery:", base_sql
            ))
          }
        )
      }

      # Opruimen tijdelijke tabellen
      tables_in_db <- dbListTables(con)
      temp_tables <- tables_in_db[grepl("^(t[0-9]+|tmp_upd_.*)$", tables_in_db)]
      for (tbl in temp_tables) dbExecute(con, paste("DROP TABLE", tbl))
    }
  }
}
