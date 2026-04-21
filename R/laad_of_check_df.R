#' @title laad basis data
#' @description
#' Deze functie controleert eenvoudigweg wanneer de gebruiker eigen data wil
#' gebruiken in plaats van systeemdata of deze eigen data in een dataframe
#' zitten. Als geen bestandsnaam werd opgegeven dan wordt het systeembestand
#' ingeladen.
#' @param df Dataframe
#' @param bestandsnaam String. Naam van het systeembestand
#' @param package_naam String. Naam van het package met het systeembestand
#' @param custom_msg String. Foutboodschap
#' @importFrom assertthat assert_that

laad_of_check_df <- function(df, bestandsnaam, package_naam = "habkey",
                             custom_msg = NULL) {
  # 1. Is het object leeg (NULL)? Laad dan de basisdata.
  if (is.null(df)) {
    df_file <- system.file(file.path("basisdata", bestandsnaam),
      package = package_naam
    )

    # Extra check: voorkom een cryptische fout als system.file niets vindt
    assert_that(
      df_file != "",
      msg = paste(
        "Bestand", bestandsnaam, "niet gevonden in package",
        package_naam
      )
    )

    df <- read.csv2(df_file)
  } else {
    # 2. Het is niet NULL, dus we checken of het een data.frame is

    # Als de gebruiker geen custom_msg heeft opgegeven, bouwen we er zelf een.
    # deparse(substitute(df)) is een handige R-truc die de naam van de originele
    # variabele pakt.
    if (is.null(custom_msg)) {
      var_naam <- deparse(substitute(df))
      custom_msg <- paste(var_naam, "is geen data.frame")
    }

    assert_that(is.data.frame(df), msg = custom_msg)
  }

  # 3. Geef het resultaat terug aan het script
  return(df)
}
