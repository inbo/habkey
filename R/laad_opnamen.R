#' @title laad opnamebestand in
#' @description
#' Deze functie controleert het opnamebestand en laadt ze in het werkgeheugen.
#'
#' @param naam_opnamen String. De naam van de tabel met de opnamen/relevés
#' Dit is een csv-bestand.
#' @param dir_invoerdata String. De naam van de map (inclusief pad) met de
#' gegevens van de gebruiker
#'
#' @importFrom assertthat assert_that is.dir
#' @returns Een opnamebestand als dataframe
#' @export
laad_opnamen <- function(naam_opnamen, dir_invoerdata) {
  # eerst checken of het opnamebestand al werd ingeladen
  df_opnamen <- haal_interim_data("opnamen")

  if (!is.null(df_opnamen)) {
    # df_opnamen werd al eens ingeladen
    return(df_opnamen)
  } else {
    # check of map bestaat
    assert_that(is.dir(dir_invoerdata))

    # inlezen van dataset met de opnamen
    foutmelding <- sprintf(
      "Bestand '%s.csv' ontbreekt in %s.", naam_opnamen, dir_invoerdata
    )
    df_opnamen <- lees_csv2(dir_invoerdata, naam_opnamen,
      foutboodschap = foutmelding
    )

    # Sla ze op in de sessie-cache zodat dit de volgende keer wordt
    # overgeslagen
    bewaar_interim_data(df_opnamen, "opnamen")

    return(df_opnamen)
  }
}
