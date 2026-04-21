#' controleer soortnamen
#'
#' De functie vergelijkt de soortnamen van de opgegeven opnamen met de lijst
#' aanwezig in het package en met de standaard inbo-soortenlijst.
#' Ontbreekt een soortnaam in deze lijsten dan kan men deze toevoegen aan een
#' eigen soortenlijst.
#' Deze lijst wordt bewaard in een door de gebruiker opgegeven locatie.
#'
#' @param custom_taxa dataframe met de soortnamen enkel bekend bij de gebruiker
#' @param opname_soorten vector van de soortnamen gebruikt in de tabel met de
#' opnamen
#' @param bekende_namen_lower vector van de soortnamen (in kleine letters)
#' die reeds bekend zijn.
#' @param officiele_taxa_lower vector van de soortnamen (in kleine letters),
#' beperkt tot de standaard inbo-lijst
#' @param prefix_id character de prefix die aan de soortnummers wordt toegevoegd
#' @param laatste_nummer numeric het huidige hoogste soortnummer van custom_taxa
#' @param file_custom character de bestandsnaam voor custom_taxa (inclusief pad)
#' @importFrom utils write.csv2

check_soortnamen <-
  function(custom_taxa,
           opname_soorten,
           bekende_namen_lower,
           officiele_taxa_lower,
           prefix_id = "CU",
           laatste_nummer = 0,
           file_custom) {
    # 1. Hulpfunctie voor ID
    generate_next_id <- function(nr, prefix) {
      nieuw_nr <- nr + 1
      return(list(
        id = paste0(prefix, sprintf("%05d", nieuw_nr)),
        nr = nieuw_nr
      ))
    }

    # 2. Interactieve loop
    nieuwe_soorten_gevonden <- FALSE
    unieke_input <- unique(na.omit(opname_soorten))

    for (soort_input in unieke_input) {
      soort_schoon <- trimws(soort_input)
      soort_lower <- tolower(soort_schoon)

      # Controleer of de naam voorkomt (ongeacht hoofdletters)
      if (soort_lower %in% bekende_namen_lower) {
        # Match gevonden, ga naar de volgende soort
        next
      }

      # Indien niet gevonden: Gebruiker om actie vragen
      message("\n", rep("-", 60), "\n", sep = "")
      warning("NIET GEVONDEN: '", soort_schoon, "'\n", sep = "")
      # nolint start
      keuze <- readline(prompt = "Wilt u deze soort toevoegen aan de eigen lijst? (j/n): ")
      # nolint end

      if (tolower(keuze) == "j") {
        nieuwe_soorten_gevonden <- TRUE
        id_info <- generate_next_id(laatste_nummer, prefix_id)
        nieuw_id <- id_info$id
        laatste_nummer <- id_info$nr

        # Vraag om taal van de oorspronkelijke naam
        message("Stap: Taal van oorspronkelijke naam bepalen\n")
        # nolint start
        orig_ndl_taal <-
          readline(prompt = paste0(" Is het een Nederlandse soortnaam ? (j/n):"))
        if (tolower(orig_ndl_taal) == "j") {
          tax_orig_taal <- "nl"
          soort <- soort_lower
        } else {
          tax_orig_taal <- "Sci"
        }
        # nolint end

        # Preferred Taxon opvragen
        message("Stap: Offici\u00eble wetenschappelijke naam bepalen\n")
        # nolint start
        taxon_input <-
          readline(prompt = paste0(" Offici\u00eble WETENSCHAPPELIJKE naam (ENTER voor '", soort_schoon, "'): "))
        # nolint end

        # Gebruik de originele naam als er alleen op Enter wordt gedrukt
        if (taxon_input == "") {
          taxon_gekozen <- soort_schoon
        } else {
          taxon_gekozen <- taxon_input
        }

        # Check Preferred Taxon (case-insensitief)
        # nolint start
        if (!(tolower(taxon_gekozen) %in% officiele_taxa_lower)) {
          warning(taxon_gekozen, "' is een nieuwe naam die niet in de offici\u00eble taxon-kolom voorkomt.\n",
            sep = ""
          )
        }

        # Overige velden opvragen
        taxon_group <-
          readline(prompt = " taxon_group (VP= vaatplant, BR = mos, LI = lichen, CH = kranswier) (ENTER voor 'VP'):\n")
        # nolint end
        taxon_group <- toupper(taxon_group)
        if (taxon_group == "") taxon_group <- "VP"

        canon_std <- parse_taxonnaam(taxon_gekozen)

        tax_canon <- readline(prompt = paste0(
          " Wetenschappelijke naam zonder auteur, (ENTER voor '",
          canon_std, "'): \n"
        ))
        if (tax_canon == "") tax_canon <- canon_std

        nieuwe_rij <- data.frame(
          taxon = taxon_gekozen,
          taxonid = nieuw_id,
          taxon_group = taxon_group,
          tax_orig = dplyr::if_else(tax_orig_taal == "nl",
            # Omzetting naam uit opnamen naar kleine letters
            soort_lower,
            # Behoud originele schrijfwijze uit opname
            soort_schoon
          ),
          tax_origid = nieuw_id,
          tax_orig_taal = tax_orig_taal,
          tax_canon = tax_canon,
          stringsAsFactors = FALSE
        )

        custom_taxa <- rbind(custom_taxa, nieuwe_rij)

        # Update de check-lijst direct voor de volgende iteratie
        bekende_namen_lower <-
          c(
            bekende_namen_lower, soort_lower, tolower(tax_canon),
            tolower(taxon_gekozen)
          )

        cat("Toegevoegd: ", taxon_gekozen, " [ID: ", nieuw_id, "]\n", sep = "")
      }
    }


    # 3. Opslaan en retourneren
    if (nieuwe_soorten_gevonden) {
      write.csv2(custom_taxa, file_custom, row.names = FALSE)
      message("\nUpdate voltooid: '", file_custom, "' is bijgewerkt.\n",
        sep = ""
      )
    } else {
      # nolint start
      message("\nAlle soorten waren al bekend en/of geen wijzigingen aangebracht.\n")
      # nolint end
    }

    return(custom_taxa)
  }
