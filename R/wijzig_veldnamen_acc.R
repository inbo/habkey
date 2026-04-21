#' @title wijzig veldnamen gebruikt in Access door namen SQLite
#' @description Deze functie wijzigt een aantal veldnamen die gebruikt worden
#' in de Access-databank, door de namen die in de SQLite-databank van toepassing
#' zijn.
#' @details
#' De vervanging gebeurt op basis van een "named vector" van strings, waarbij de
#' namen van de strings verwijzen naar de nieuwe namen.

#'
#' @importFrom stringr str_replace_all
#'
#' @param sql String. Een SQL-regel
#' @param mapping_list Named vector. Een vector met veldnamen die
#' gebruikt worden in Access-SQL, maar die in R een andere naam krijgen:
#' "AccessNaam" = "R_Naam". In het package is het standaard een systeemvector,
#' zie `karakteriseer_opname`.
#'
#' @returns een string met de gewijzigde SQL-regel
#'
#' @family converteer_sql


wijzig_veldnamen_acc <- function(sql, mapping_list) {
  # styler: off
  for (acc_col in names(mapping_list)) {
    r_col <- mapping_list[acc_col]
    sql <- str_replace_all(sql, regex(paste0("\\b", acc_col, "\\b"),
                                      ignore_case = TRUE), r_col)
  }
  # styler: on
  return(sql)
}
