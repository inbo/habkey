#' @title zoek en vervang tekst in SQL
#' @description Deze functie voert een aantal generieke zoek en vervangfuncties
#' uit in de SQL-regels
#' @details
#' De SQLregels zijn opgemaakt in een Access-databank. De aanpassingen omvatten
#' twee soorten van generieke aanpassingen.
#' 1) De namen voor tabellen en velden verschillen daar met deze die in de
#' SQL-databank gebruikt worden.
#' 2) De SQL-taal van MS-Access en SQLite zijn verschillend. Er zijn enkele
#' functies in Access die met een (vrij) eenvoudige zoek en vervang-methode
#' SQLite-compatibel kunnen gemaakt worden:
#'  - aanpassen van wildcarts (enkel *) door SQL-taal (%)
#'  - aanpassen van de nz()-functie van Access door de SQL-functie COALESCE()
#'  - aanpassen van Iif()-functie
#' 3) De logische variabelen (TRUE/FALSE) worden door resp. 1 en 0 vervangen.
#'
#'
#' @importFrom stringr fixed regex str_replace_all
#'
#' @param sql String. Een SQL-regel
#' @param naam_doeltabel String. De naam van de tabel waarin de karakterisering
#' van de opname(n) wordt bewaard.
#' @param huidige_regel String. De kolomnaam waarin het antwoord op een
#' specifieke vraag van de determineersleutel wordt bewaard. Deze bestaat
#' standaard uit een regelnummer (verwijzend naar de determineersleutel) en een
#' label die toepasselijk is voor de vraag in de sleutel.
#' @returns een string met de gewijzigde SQL-tekst
#' @family converteer_sql

zoekvervang_sql <- function(sql, naam_doeltabel, huidige_regel) {
  sql |>
    str_replace_all(
      fixed("_tbl_HS_Opnamen_Grasland_G"),
      naam_doeltabel
    ) |>
    str_replace_all(fixed("_tbl_HS_Opnamen_Grasland"), "tbl_meta") |>
    # "tbl_iv2021_soortgegevens", "tbl_opnamen"
    # "tbl_iv2021_kopgegevens", "tbl_meta"
    str_replace_all(fixed("tbl_BasisDataset_RBB"), "tbl_opnamen") |>
    str_replace_all(
      regex("SPECSYNO", ignore_case = TRUE),
      "tbl_soorten"
    ) |>
    str_replace_all(fixed("tbl_LSVI3_soorten"), "tbl_lijstjes") |>
    str_replace_all("fld.name", huidige_regel) |>
    # de ';' op het einde van een vba-sql moeten geschrapt worden.
    str_replace_all(fixed(";"), "") |>
    # Integer-deling
    # Vervang " / " door " * 1.0 / " om te voorkomen dat 15/50 resulteert in 0.
    # Verklaring: als in de database waarden zijn opgeslagen als gehele getallen
    # (Integers), dan behandelt SQLite de deling ook als een integer-bewerking,
    # waardoor waarden worden afgekapt.
    # Voor de zekerheid vervangen we "/" globaal.
    # Dit is veilig voor wiskundige formules, maar kan een probleem geven bij
    # datum-notaties. Die zitten gelukkig niet in de SQL-regels.
    str_replace_all(fixed("/"), " * 1.0 / ") |>
    # Booleans
    str_replace_all(fixed("=True"), "=1") |>
    str_replace_all(fixed("=False"), "=0") |>
    str_replace_all(fixed(" True"), " 1") |>
    str_replace_all(fixed(" False"), " 0") |>
    # Wildcards
    str_replace_all(fixed(" Like '*"), " Like '%") |>
    str_replace_all(fixed("*'"), "%'") |>
    str_replace_all(fixed(' Like "*'), ' Like "%') |>
    str_replace_all(fixed('*"'), '%"') |>
    # Fix voor aliassen die beginnen met een cijfer
    # Probleem: SELECT Count(*) AS 6430groep - > Error
    # Oplossing: SELECT Count(*) AS [6430groep]
    # Regex: Zoek AS, spaties, een cijfer (0-9), gevolgd door woordtekens.
    # styler: off
    str_replace_all(regex("AS\\s+([0-9][a-zA-Z0-9_]*)",
                          ignore_case = TRUE), "AS [\\1]") |>
    # styler: on
    # Dubbele quotes naar enkele quotes voor strings
    # Access gebruikt vaak "tekst", SQLite wil 'tekst'.
    # een globale vervanging voor quoted strings die geen spaties bevatten :
    str_replace_all(regex('"([a-zA-Z0-9_]+)"'), "'\\1'") |>
    # Vervang ="k" door IN ('k', 'K')
    # Vervang ="x" door IN ('x', 'X')
    # Vervang ="m" door IN ('m', 'M')
    # Vervang ="s" door IN ('s', 'S')
    # Vervang ="b" door IN ('b', 'B')
    # Dit lost zowel de quotes als de hoofdlettergevoeligheid in één klap op.
    str_replace_all(
      regex("=\\s*[\"']k[\"']", ignore_case = TRUE),
      " IN ('k'###COMMA### 'K')"
    ) |>
    str_replace_all(
      regex("=\\s*[\"']x[\"']", ignore_case = TRUE),
      " IN ('x'###COMMA### 'X')"
    ) |>
    str_replace_all(
      regex("=\\s*[\"']m[\"']", ignore_case = TRUE),
      " IN ('m'###COMMA### 'M')"
    ) |>
    str_replace_all(
      regex("=\\s*[\"']s[\"']", ignore_case = TRUE),
      " IN ('s'###COMMA### 'S')"
    ) |>
    str_replace_all(
      regex("=\\s*[\"']b[\"']", ignore_case = TRUE),
      " IN ('b'###COMMA### 'B')"
    ) |>
    # Access/VBA-functies door SQL-functies vervangen
    # A. Nz -> COALESCE
    # styler: off
    # nolint start
    str_replace_all(
      regex("Nz\\s*\\(\\s*([^,)]+)\\s*,\\s*([^)]+)\\s*\\)",
            ignore_case = TRUE), "COALESCE(\\1, \\2)") |>
    # nolint end
    str_replace_all(regex("Nz\\s*\\(\\s*([^,)]+)\\s*\\)",
                          ignore_case = TRUE), "COALESCE(\\1, 0)") |>
    str_replace_all(regex("COALESCE\\s*\\(\\s*([^,)]+)\\s*\\)",
                          ignore_case = TRUE), "COALESCE(\\1, 0)") |>
    # BESCHERM KOMMA'S BINNEN COALESCE ---
    # Voordat we IIf gaan splitsen, verbergen we komma's die BINNEN een
    # COALESCE staan. Anders denkt de IIf-regex dat dit het scheidingsteken
    # voor zijn argumenten is.
    str_replace_all(regex("COALESCE\\s*\\(([^,]+),([^)]+)\\)",
                          ignore_case = TRUE),
                    "COALESCE(\\1###COMMA###\\2)") |>
    # styler: on
    # Specifieke IIF met OR Fix ---
    # We pakken specifiek de Iif's die een 'Or' in hun conditie hebben.
    # Patroon: Iif(ConditieA Or ConditieB, WaardeWaar, WaardeOnwaar)
    # Doel: CASE WHEN ConditiA OR ConditieB THEN ValueWaar ELSE WaardeOnwaar END

    # Uitleg Regex:
    # Iif\s*\(      -> Zoek naar Iif(
    # ([^,]+)       -> Groep 1: Alles tot de 'Or' (Conditie 1);
    #                  [^,] dwz alles behalve een komma
    # \s+Or\s+      -> De 'Or' scheiding
    # ([^,]+)       -> Groep 2: Alles tot de eerste komma (Conditie 2)
    # ,             -> De eerste komma
    # \s*([^,]+)    -> Groep 3: Waarde als WAAR
    # ,             -> De tweede komma
    # \s*([^)]+)    -> Groep 4: Waarde als ONWAAR (tot het sluithaakje)
    # \)            -> Het sluithaakje


    # nolint start
    str_replace_all(regex("Iif\\s*\\(([^,]+?)\\s+Or\\s+([^,]+?),\\s*([^,]+?),\\s*([^)]+?)\\)", ignore_case = TRUE), "CASE WHEN \\1 OR \\2 THEN \\3 ELSE \\4 END") |>
    # nolint end

    # B. IsNull patronen
    # nolint start
    str_replace_all(regex("IIf\\s*\\(\\s*IsNull\\s*\\(\\s*([^)]+)\\s*\\)\\s*,\\s*([^,)]+)\\s*,\\s*\\1\\s*\\)", ignore_case = TRUE), "COALESCE(\\1, \\2)") |>
    # nolint end
    # styler: off
    str_replace_all(regex("IsNull\\s*\\(\\s*([^)]+)\\s*\\)",
                          ignore_case = TRUE), "(\\1 IS NULL)") |>
    # C. "Capping" logic: IIf(Expr > Max, Max, Expr)
    # nolint start
    str_replace_all(regex("IIf\\s*\\(\\s*(.+?)\\s*>\\s*([0-9\\.]+)\\s*,\\s*\\2\\s*,\\s*\\1\\s*\\)", ignore_case = TRUE), "CASE WHEN \\1 > \\2 THEN \\2 ELSE \\1 END") |>
    # nolint end

    # D. "Flooring" logic: IIf(Expr < Min, Min, Expr)
    # nolint start
    str_replace_all(regex("IIf\\s*\\(\\s*(.+?)\\s*<\\s*([0-9\\.]+)\\s*,\\s*\\2\\s*,\\s*\\1\\s*\\)", ignore_case = TRUE), "CASE WHEN \\1 < \\2 THEN \\2 ELSE \\1 END") |>
    # nolint end
    # styler: on

    # E. Generieke IIf (REST-AFVANG)
    # Deze regex zoekt nu naar komma's.
    # Omdat de komma in COALESCE nu '###COMMA###' heet, zal hij hier niet
    # stoppen, maar netjes doorgaan tot de échte komma van de IIf.
    # nolint start
    str_replace_all(regex("IIf\\s*\\(\\s*([^,]+)\\s*,\\s*([^,]+)\\s*,\\s*([^)]+)\\s*\\)",
      ignore_case = TRUE
    ), "CASE WHEN \\1 THEN \\2 ELSE \\3 END") |>
    # nolint end

    # HERSTEL DE KOMMA'S
    str_replace_all(fixed("###COMMA###"), ",")
}
