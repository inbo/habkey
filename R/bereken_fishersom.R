#' @title bereken fishersom bedekking
#' @description
#' Bij vegetatieanalyse, met name bij gelaagde vegetatie (bijvoorbeeld de
#' ondergroei onder een bladerdak), is de som van de bedekkingsgraden voor
#' verschillende soorten of lagen vaak hoger dan 100% vanwege overlappende
#' begroeiing. Om dit probleem aan te pakken, heeft Fisher (2014) een methode
#' ontwikkeld om een gecombineerde "werkelijke" bedekkingswaarde te berekenen,
#' die ervoor zorgt dat deze niet hoger is dan 100% en rekening houdt met de
#' verticale structuur.
#' @param p bedekking in procent (numeric)

bereken_fishersom <- function(p) {
  if (length(p) == 0 || all(is.na(p))) {
    return(0)
  }
  res <- 1 - exp(sum(log((100 - p + 0.01) / 100), na.rm = TRUE))
  return(as.integer(round(res * 100)))
}
