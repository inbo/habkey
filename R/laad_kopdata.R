#' @title laad data met de kopgegevens in
#' @description
#' Deze functie controleert het opnamebestand en laadt ze in het werkgeheugen.
#'
#' @inheritParams laad_opnamen
#' @param naam_kopdata String. De naam van de tabel met de kopgegevens
#' (metadata)
#' Dit is een csv-bestand.
#'
#' @importFrom assertthat assert_that is.dir
#' @returns Een bestand met de kopgegevens als dataframe
#' @export
laad_kopdata <- function(naam_kopdata, dir_invoerdata) {
  # eerst checken of het bestand met de kopdata al eens werd ingeladen
  df_kop <- haal_interim_data("kopdata")

  if (!is.null(df_kop)) {
    # df_opnamen werd al eens ingeladen
    return(df_kop)
  } else {
    # check of map bestaat
    assert_that(is.dir(dir_invoerdata))

    # inlezen van dataset met de opnamen
    foutmelding <- sprintf(
      "Bestand '%s.csv' ontbreekt in %s.", naam_kopdata, dir_invoerdata
    )
    df_kop <- lees_csv2(dir_invoerdata, naam_kopdata,
      foutboodschap = foutmelding
    )

    # Sla ze op in de sessie-cache zodat dit de volgende keer wordt
    # overgeslagen
    bewaar_interim_data(df_kop, "kopdata")

    return(df_kop)
  }
}
