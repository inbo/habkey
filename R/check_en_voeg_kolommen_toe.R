#' @title helper functie om ontbrekende kolommen toe te voegen
#' @description
#' Deze functie inspecteert bij een dataframe of de opgegeven kolomnamen
#' aanwezig zijn.
#' Als ze ontbreken worden ze toegevoegd.
#'
#' @param df Dataframe
#' @param benodigde_kolommen Vector met de namen van kolommen die moeten
#' aanwezig zijn

check_en_voeg_kolommen_toe <- function(df, benodigde_kolommen) {
  ontbrekende_kolommen <- setdiff(benodigde_kolommen, colnames(df))

  if (length(ontbrekende_kolommen) > 0) {
    # Voeg ontbrekende kolommen toe met standaardwaarde 0
    df[ontbrekende_kolommen] <- 0
    # styler: off
    message(paste("Toegevoegde kolommen:", paste(ontbrekende_kolommen,
                                                 collapse = ", ")))
    # styler: on
  }
  return(df)
}
