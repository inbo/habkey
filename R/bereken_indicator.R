#' @title bereken indicatoren van een of meer opname(n)
#' @description
#' Deze functie berekent voor een of meer opname(n) | relevé(s) een aantal
#' standaard indicatorwaarden. Zie de details-sectie voor meer info.
#'
#' @details
#' We maken onderscheid tussen indicatoren die rechtstreeks ('on the fly')
#' tijdens de uitvoering van een SQL-opdracht worden berekend en indicatoren
#' die op voorhand worden berekend en die tijdens een SQL-opdracht opgehaald
#' kunnen worden. Het gaat hier over de tweede groep: we noemen ze de 'vaste'
#' indicatoren. De andere noemen we 'tijdelijke' indicatoren.
#' Vaste indicatoren zijn indicatoren die ofwel meermaals worden aangeroepen
#' ofwel indicatoren met een diagnostische waarde. Het onderscheid tussen een
#' tijdelijke en vaste indicator is wat subjectief.
#' In de tabel 'df_indicatoren' zijn de vaste indicatoren van de tijdelijke te
#' onderscheiden doordat de kolom 'veldnaam' niet leeg is.
#' Deze verwijst naar de kolom van de tabel met de kopgegevens waarin deze
#' indicatorwaarde moet bewaard worden.
#'
#' Daarenboven maken we een onderscheid tussen indicatoren die een aantal
#' en die een som van bedekkingen uitdrukken. Dit wordt aangegeven in de kolom
#' aard.
#'
#' @inheritParams maak_soortenlijst
#' @inheritParams laad_kopdata
#' @inheritParams laad_opnamen
#'
#' @param df_soorten Dataframe. Soortenlijst, zie `maak_soortenlijst()`
#' @param df_soortenlijstjes Dataframe. Tabel (lang formaat) met de
#' standaardnaam van de soorten die tot een soortengroep gerekend worden, die
#' gebruikt wordt bij de determinatie en een uniek identificatienummer voor deze
#' soortengroep. Het is een systeembestand
#' @param df_indicatoren Dataframe. Overzichtstabel van de soortenlijstnummers
#' en met de opgave van een naam. Het is een systeembestand.
#' @param df_habsleutels Dataframe. Lijst van de (potentiële)
#' determineersleutels. Deze lijst bevat per vegetatiegroep (bijv. graslanden,
#' bossen) een lijstcode (1 karakter), een naam en een aanduiding of voor deze
#' vegetatiegroep er effectief al een digitale determineersleutel bestaat.
#' Het is een systeembestand.
#' @param herbereken_lagen Logical. Indicatoren die op vegetatielagen slaan
#' herberekenen (default = TRUE)?
#'
#' @importFrom assertthat assert_that
#' @importFrom dplyr across anti_join case_when coalesce count distinct filter
#' first group_by if_else inner_join left_join mutate n n_distinct rows_update
#' select slice_max summarise transmute ungroup
#' @importFrom rlang .data
#' @importFrom tidyr pivot_wider
#' @importFrom tidyselect contains matches where
#'
#' @returns Een dataframe waarbij de kopgegevens aangevuld/bijgewerkt werden met
#' betrekking tot een aantal indicatoren
#'
# TODO @examples ex_bereken_indicator.R (geeft fout)
#'
#' @export

bereken_indicator <- function(naam_kopdata,
                              naam_opnamen,
                              dir_invoerdata,
                              df_soorten = NULL,
                              df_soortenlijstjes = NULL,
                              df_indicatoren = NULL,
                              df_habsleutels = NULL,
                              herbereken_lagen = TRUE) {
  # inlezen van dataset met de kopgegevens (incl. foutafhandeling)
  df_kop <- laad_kopdata(naam_kopdata, dir_invoerdata)

  # inlezen van dataset met de opnamen (incl. foutafhandeling)
  df_opname <- laad_opnamen(naam_opnamen, dir_invoerdata)

  if (is.null(df_soorten)) {
    df_soorten <- laad_of_maak_soorten(
      datanaam = "soorten"
    )
  } else {
    msg <- "df_soorten is geen data.frame"
    assert_that(is.data.frame(df_soorten), msg = msg)
  }

  df_file <- "tbl_soorten_in_lijst.csv"
  df_soortenlijstjes <- laad_of_check_df(df_soortenlijstjes, df_file)
  df_file <- "tbl_indicatoren.csv"
  df_indicatoren <- laad_of_check_df(df_indicatoren, df_file)
  df_file <- "cde_sleutels.csv"
  df_habsleutels <- laad_of_check_df(df_habsleutels, df_file)

  # Oplijsting sleutels
  sleutels <- unique(df_habsleutels$code)

  # 1. Reset indicatoren
  # Zet alle numerieke velden die niet in de uitzonderingslijst staan op 0
  cols_to_keep <- c(
    "uniek", "name", "spoc", "opn", "survey", "plaats",
    "recording", "observer", "waarnemer", "user_reference",
    "_vast", "regel"
  )

  df_kop <- df_kop |>
    mutate(across(where(is.numeric) & !contains(cols_to_keep), ~0))


  # 2. Herberekenen bedekking lagen (indien gekozen)
  if (herbereken_lagen) {
    # Berekenen bedekking kruiden in Kruidlaag met Fisher
    kruiden_kl <- df_opname |>
      inner_join(df_soorten, by = "speciesnaam") |>
      # styler: off
      filter((.data$layer == "K" | .data$layer == "X") & .data$vaatplant &
               !.data$is_boomstruik & !.data$is_dwergstruik) |>
      # styler: on
      group_by(.data$recordinggivid) |>
      summarise(
        kruideninkl_bedekking_vast =
          bereken_fishersom(.data$percentage)
      )

    df_kop <- df_kop |> rows_update(kruiden_kl, by = "recordinggivid")

    # Berekenen bedekking niet-kruiden in Kruidlaag met Fisher
    niet_kruiden_kl <- df_opname |>
      inner_join(df_soorten, by = "speciesnaam") |>
      # styler: off
      filter((.data$layer == "K" | .data$layer == "X") & .data$vaatplant &
               (.data$is_boomstruik | .data$is_dwergstruik)) |>
      # styler: on
      group_by(.data$recordinggivid) |>
      summarise(
        nietkruideninkl_bedekking_vast =
          bereken_fishersom(.data$percentage)
      )

    df_kop <- df_kop |> rows_update(niet_kruiden_kl, by = "recordinggivid")
  }

  # --- 2b. Berekening Kruidlaag Bedekking (Complex) ---

  # Voorbereiding: Join opnamen met soort-eigenschappen voor de berekeningen
  # We hebben eigenschappen nodig als 'vaatplant' en 'is_boomstruik'
  opnamen_full <- df_opname |>
    inner_join(df_soorten, by = "speciesnaam")

  # hulp-functie om -Inf te vermijden als alle waarden NA zijn
  safe_max <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    max(x, na.rm = TRUE)
  }

  # Stap 1: Directe overname van cover_pctvalue uit laag 'K'
  step1_direct <- df_opname |>
    filter(.data$layer == "K", !is.na(.data$cover_pctvalue)) |>
    select("recordinggivid", val_step1 = "cover_pctvalue") |>
    distinct()

  # Stap 2: Afleiden uit andere lagen (bijv. 'X') met >50% kruiden
  step2_other_layer <- opnamen_full |>
    filter(.data$layer != "K", !is.na(.data$cover_pctvalue)) |>
    group_by(.data$recordinggivid, .data$layer, .data$cover_pctvalue) |>
    summarise(
      bedekking_nietbomen =
        # styler: off
        sum(
          .data$percentage[.data$is_boomstruik == FALSE |
                             .data$vaatplant == FALSE],
          na.rm = TRUE
        ),
      # styler: on
      bedekking_totaal = sum(.data$percentage, na.rm = TRUE),
      .groups = "drop"
    ) |>
    filter(
      .data$bedekking_totaal > 0,
      (.data$bedekking_nietbomen / .data$bedekking_totaal) > 0.5
    ) |>
    # Als er meerdere lagen voldoen, neem de maximale cover
    group_by(.data$recordinggivid) |>
    summarise(
      val_step2 = safe_max(.data$cover_pctvalue),
      val_step2_som = safe_max(.data$bedekking_totaal)
    )

  # Stap 3: Fisher som van soorten in laag 'K'
  step3_species_k <- opnamen_full |>
    filter(.data$layer == "K", .data$vaatplant == TRUE) |>
    group_by(.data$recordinggivid) |>
    summarise(val_step3 = bereken_fishersom(.data$percentage))

  # Stap 4: Afleiden uit andere lagen (bijv. 'X' of 'B')
  step4_species_other <- opnamen_full |>
    filter(.data$layer != "K", .data$vaatplant == TRUE) |>
    group_by(.data$recordinggivid, .data$layer) |>
    summarise(
      fisher_val = bereken_fishersom(.data$percentage),
      bedekking_nietbomen =
        sum(if_else(.data$is_boomstruik == TRUE, 0, .data$percentage),
          na.rm = TRUE
        ),
      bedekking_totaal = sum(.data$percentage, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Bereken de ratio
    mutate(ratio = if_else(.data$bedekking_totaal == 0, 0,
      .data$bedekking_nietbomen / .data$bedekking_totaal
    )) |>
    # Selecteer per opname de laag met de hoogste 'kruid-achtigheid' (ratio)
    group_by(.data$recordinggivid) |>
    slice_max(order_by = .data$ratio, n = 1, with_ties = FALSE) |>
    ungroup() |>
    # Pas de conditionele logica toe (regels 208-209)
    transmute(
      .data$recordinggivid,
      val_step4 = if_else(.data$ratio > 0.5, .data$fisher_val,
        .data$bedekking_nietbomen
      ),
      val_step4_som = if_else(.data$ratio > 0.5, .data$bedekking_totaal,
        .data$bedekking_nietbomen
      )
    )

  # Stap 5: Gewone som van soorten in laag 'K' voor 'KruidlaagBedekkingSom_Vast'
  step5_sum_k <- opnamen_full |>
    filter(.data$layer == "K", .data$vaatplant == TRUE) |>
    group_by(.data$recordinggivid) |>
    summarise(val_sum_k = round(sum(.data$percentage, na.rm = TRUE)))

  # --- Samenvoegen van alle stappen in df_kop ---

  # We maken een tijdelijke tabel met alle berekende waardes
  kruidlaag_calc <- df_kop |>
    select("recordinggivid") |>
    left_join(step1_direct, by = "recordinggivid") |>
    left_join(step2_other_layer, by = "recordinggivid") |>
    left_join(step3_species_k, by = "recordinggivid") |>
    left_join(step4_species_other, by = "recordinggivid") |>
    left_join(step5_sum_k, by = "recordinggivid") |>
    mutate(
      # De waterval logica: Als 1 bestaat neem 1, anders 2, anders 3, etc.
      kruidlaag_bedekking_vast =
        coalesce(
          .data$val_step1, .data$val_step2, .data$val_step3, .data$val_step4
        ),

      # Specifieke logica voor Som (stap 5)
      kruidlaag_bedekkingsom_vast = case_when(
        # Als afgeleid van K (cover), neem som soorten K
        !is.na(val_step1) ~ val_sum_k,
        !is.na(val_step2) ~ val_step2_som,
        !is.na(val_step3) ~ val_sum_k,
        !is.na(val_step4) ~ val_step4_som,
        TRUE ~ val_sum_k
      )
    ) |>
    select(
      "recordinggivid", "kruidlaag_bedekking_vast",
      "kruidlaag_bedekkingsom_vast"
    )

  # Update de metadata tabel
  df_kop <- df_kop |>
    rows_update(kruidlaag_calc, by = "recordinggivid")

  # 3. Ellenberggetallen (Gewogen gemiddelden)
  ellenberg_calc <- df_opname |>
    inner_join(df_soorten, by = "speciesnaam") |>
    filter(.data$layer == "K" | .data$layer == "X") |>
    group_by(.data$recordinggivid) |>
    # styler: off
    summarise(
      l_percsom = sum(ifelse(!is.na(.data$l_getal), .data$percentage, 0)),
      l_getal = round(sum(.data$l_getal * .data$percentage, na.rm = TRUE) /
                        .data$l_percsom, 2),
      f_percsom = sum(ifelse(!is.na(.data$f_getal), .data$percentage, 0)),
      f_getal = round(sum(.data$f_getal * .data$percentage, na.rm = TRUE) /
                        .data$f_percsom, 2),
      r_percsom = sum(ifelse(!is.na(.data$r_getal), .data$percentage, 0)),
      r_getal = round(sum(.data$r_getal * .data$percentage, na.rm = TRUE) /
                        .data$r_percsom, 2),
      n_percsom = sum(ifelse(!is.na(.data$n_getal), .data$percentage, 0)),
      n_getal = round(sum(.data$n_getal * .data$percentage, na.rm = TRUE) /
                        .data$n_percsom, 2),
      s_percsom = sum(ifelse(!is.na(.data$s_getal), .data$percentage, 0)),
      s_getal = round(sum(.data$s_getal * .data$percentage, na.rm = TRUE) /
                        .data$s_percsom, 2),
      rn_percsom = sum(ifelse(!is.na(.data$n_getal) & !is.na(.data$r_getal),
                              .data$percentage, 0)),
      rn_getal = round(sum(.data$n_getal * .data$r_getal * .data$percentage,
                           na.rm = TRUE) / .data$rn_percsom, 2)
      # styler: on
    ) |>
    dplyr::select(-matches("percsom$"))

  df_kop <- df_kop |> rows_update(ellenberg_calc, by = "recordinggivid")

  # 4. Soortenaantal
  soortenaantal <- df_opname |>
    group_by(.data$recordinggivid) |>
    summarise(soortenaantal = n_distinct(.data$speciesnaam))

  df_kop <- df_kop |> rows_update(soortenaantal, by = "recordinggivid")

  # 5. Aandeel gras
  gras_aandeel <- df_opname |>
    inner_join(df_soorten, by = "speciesnaam") |>
    filter(.data$layer == "K" | .data$layer == "X") |>
    group_by(.data$recordinggivid) |>
    # styler: off
    summarise(
      aandeelgras =
        round((sum(ifelse(.data$is_gras == 1, .data$percentage, 0)) * 100) /
                sum(.data$percentage))
      # styler: on
    )

  df_kop <- df_kop |> rows_update(gras_aandeel, by = "recordinggivid")

  # 6. Bosbedekking (Complexere logica met Fisher som en 100% checks)
  # A. Als de bedekking van bos- of struiklaag bekend is.
  bos_bedekking <- df_opname |>
    filter(.data$layer %in% c("B", "S") & !is.na(.data$cover_pctvalue)) |>
    group_by(.data$recordinggivid) |>
    summarise(
      # styler: off
      b_perc = if (all(!(grepl("B", .data$layer)))) {
        NA_real_
      } else {
        safe_max(ifelse(grepl("B", .data$layer), .data$cover_pctvalue, NA))
      },
      s_perc = if (all(!(grepl("S", .data$layer)))) {
        NA_real_
      } else {
        safe_max(ifelse(grepl("S", .data$layer), .data$cover_pctvalue, NA))
      }
    ) |>
    mutate(bos_bedekking = case_when(
      .data$b_perc == 100 | .data$s_perc == 100 ~ 100,
      is.na(.data$b_perc) ~ .data$s_perc,
      is.na(.data$s_perc) ~ .data$b_perc,
      TRUE ~
        (1 - exp(log((100 - .data$b_perc) / 100) +
                   log((100 - .data$s_perc) / 100))) * 100
      # styler: on
    )) |>
    mutate(bos_bedekking = round(.data$bos_bedekking)) |>
    dplyr::select(-c("b_perc", "s_perc"))

  df_kop <- df_kop |> rows_update(bos_bedekking, by = "recordinggivid")

  # B. Verbraming als bos beschouwen (niet toegepast)
  # Dit geldt voor opnamen waar cover_pctvalue ontbreekt en bramen in
  # niet-B/S lagen staan
  verbraming_bij_verbossing <- FALSE
  if (verbraming_bij_verbossing) {
    # Hulpbronnen voorbereiden
    # We hebben de lijst van 'Verbraming' indicatoren nodig
    verbraming_soorten <- df_indicatoren |>
      filter(.data$criterium_naam == "Verbraming") |>
      inner_join(df_soortenlijstjes,
        by = "soortenlijstnr",
        relationship = "many-to-many"
      ) |>
      distinct(.data$wet_naam)

    calc_verbraming <- df_opname |>
      filter(is.na(.data$cover_pctvalue), !.data$layer %in% c("B", "S")) |>
      inner_join(df_soorten, by = "speciesnaam") |>
      inner_join(verbraming_soorten, by = "wet_naam") |>
      group_by(.data$recordinggivid) |>
      summarise(val_verbraming = bereken_fishersom(.data$percentage))


    df_kop <- df_kop |>
      left_join(calc_verbraming, by = "RecordingGivid") |>
      mutate(
        # Als er een 'verbraming' waarde is berekend, tellen we die op bij
        # de bos_bedekking.
        bos_bedekking = if_else(!is.na(.data$val_verbraming),
          bos_bedekking + .data$val_verbraming,
          bos_bedekking
        )
      ) |>
      select(-"val_verbraming")
  }

  # C. Inschatten op basis van soorten in B/S laag
  # Alleen als de cover_pctvalue (laagbedekking) ontbreekt.
  calc_bs_species <- df_opname |>
    filter(is.na(.data$cover_pctvalue), .data$layer %in% c("B", "S")) |>
    group_by(.data$recordinggivid) |>
    summarise(bos_bedekking = bereken_fishersom(.data$percentage))

  df_kop <- df_kop |> rows_update(calc_bs_species, by = "recordinggivid")

  # D. Bomen/Struiken die in andere lagen (X/K) zijn ingevoerd
  # Dit vangt gevallen op waar bomen/struiken samen met kruiden in één laag
  # zijn gezet.

  # D.1. Bereken bedekking van houtige soorten in
  # NIET-B/S lagen
  t_wood_other <- df_opname |>
    inner_join(df_soorten, by = "speciesnaam") |>
    filter(
      .data$is_boomstruik == TRUE,
      !.data$layer %in% c("B", "S"),
      !nchar(.data$layer) == 0
    ) |>
    group_by(.data$recordinggivid) |>
    summarise(bos_bedekking = bereken_fishersom(.data$percentage))

  # D.2. Identificeer opnamen die WEL degelijk een B of S laag hebben
  # (met houtige soorten)
  t2_has_bs <- df_opname |>
    inner_join(df_soorten, by = "speciesnaam") |>
    filter(.data$is_boomstruik == TRUE, .data$layer %in% c("B", "S")) |>
    distinct(.data$recordinggivid)

  # D.3. Filter en Update
  # We voegen de waarde uit A toe, MAAR ALLEEN als de opname NIET voorkomt
  # in t2_has_bs
  t_wood_other_t2 <- t_wood_other |>
    anti_join(t2_has_bs, by = "recordinggivid")

  df_kop <- df_kop |> rows_update(t_wood_other_t2, by = "recordinggivid")

  # E. Detectie van een kapvlakte (mocht dat niet reeds in de tabel staan)
  # We doen dit op basis van opnamekenmerken:veel bosplanten, maar weinig bomen:
  # bedekking bossoorten minstens 50%, bedekking bos minder dan 15%

  t_bossoort <- df_opname |>
    inner_join(df_soorten, by = "speciesnaam") |>
    filter(.data$is_bossoort == TRUE | .data$is_boomstruik == TRUE) |>
    group_by(.data$recordinggivid) |>
    # toch voor een gewone optelling en geen bereken_fishersom gekozen, omdat
    # de totaalbedekking soms sterk afwijkt van 100%
    summarise(bosplant_bedekking = sum(.data$percentage))

  t_totperc <- df_opname |>
    group_by(.data$recordinggivid) |>
    summarise(tot_perc = sum(.data$percentage))

  t_kapvlakte <- df_kop |>
    inner_join(
      t_totperc |>
        filter(.data$tot_perc > 0),
      by = "recordinggivid"
    ) |>
    inner_join(t_bossoort,
      by = "recordinggivid"
    ) |>
    # styler: off
    filter(.data$bos_bedekking < 15 & .data$bosplant_bedekking /
             .data$tot_perc >= 0.5) |>
    # styler: on
    transmute(iskapvlakte = TRUE, .data$recordinggivid)

  df_kop <- df_kop |> rows_update(t_kapvlakte, by = "recordinggivid")


  # 7. Berekening Bedekking Dwergstruiken

  # Bereken de som van de percentages van dwergstruiken per opname.
  # Hier wordt de 'SomVanMaxVanPctValue' gebruikt (gewone som),
  # en NIET de Fisher-transformatie.
  calc_dwergstruiken <- df_opname |>
    inner_join(df_soorten, by = "speciesnaam") |>
    filter(.data$is_dwergstruik == TRUE) |>
    group_by(.data$recordinggivid) |>
    summarise(
      dwergstruik_bedekking = sum(.data$percentage, na.rm = TRUE)
    )

  df_kop <- df_kop |> rows_update(calc_dwergstruiken, by = "recordinggivid")


  # 8. Dynamische Indicatoren (Lussen door de soortenlijsten)


  indicator_results <- df_indicatoren |>
    filter(!is.na(.data$soortenlijstnr)) |>
    mutate(veldnaam = tolower(.data$veldnaam)) |>
    # opzoeken van de vaste indicatoren voor de sleutel in kwestie
    # styler: off
    filter(.data$hrl_code %in% sleutels & !is.na(.data$veldnaam) &
             nchar(.data$veldnaam) > 0) |>
    # styler: on
    inner_join(
      df_soortenlijstjes |>
        dplyr::select("soortenlijstnr", "wet_naam"),
      by = "soortenlijstnr",
      relationship = "many-to-many"
    ) |>
    inner_join(df_soorten, by = "wet_naam") |>
    inner_join(df_opname,
      by = "speciesnaam",
      relationship = "many-to-many"
    ) |>
    group_by(.data$recordinggivid, .data$veldnaam, .data$aard)

  # nolint start
  check <- indicator_results |>
    count(
      .data$hrl_code, .data$recordinggivid, .data$soortenlijstnr,
      .data$wet_naam
    )
  check_coln <- colnames(df_kop) |> data.frame()
  # vrijwel steeds unieke records, behalve wanneer een soort in meerdere
  # vegetatielagen zit, bijv. een boomsoort die zowel in de struik- als in de
  # boomlaag zit. Dit is ok, want het zijn geen dubbeltellingen.
  # nolint end

  # Bereken bedekking of aantal per indicator
  dyn_summary <- indicator_results |>
    summarise(
      waarde = if_else(first(.data$aard) == "bedekking",
        # nolint start
        # Bedekking: Som van percentages over alle lagen
        round(sum(.data$percentage, na.rm = TRUE)),

        # Aantal: Tel unieke soortsnamen i.p.v. aantal rijen
        # nolint end
        as.integer(n())
      ),
      .groups = "drop"
    ) |>
    select(-"aard") |>
    pivot_wider(
      names_from = .data$veldnaam, values_from = .data$waarde,
      values_fill = 0
    )

  # Identificeer welke kolommen in 'dyn_summary' nieuw zijn
  nieuwe_kolommen <- setdiff(names(dyn_summary), names(df_kop))

  # Voeg deze kolommen toe aan df_kop als ze nog niet bestaan
  if (length(nieuwe_kolommen) > 0) {
    # We gebruiken een matrix-assignatie trucje dat heel snel werkt
    df_kop[nieuwe_kolommen] <- 0

    # Berichtje voor jezelf (optioneel)
    message(paste(
      "Nieuwe kolommen toegevoegd:",
      paste(nieuwe_kolommen, collapse = ", ")
    ))
  }

  # Nu kunnen met rows_update de vaste indicatoren bijgewerkt worden
  df_kop <- df_kop |>
    rows_update(dyn_summary, by = "recordinggivid")

  # bewaren in werksessie
  bewaar_interim_data(df_kop, "kopdata_met_indicatoren")

  return(df_kop)
}
