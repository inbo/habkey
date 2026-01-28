# Dit is een hulpfunctie waarmee van de soortnaam 1) de canonische naam wordt
# afgeleid en 2) de canonische naam wordt opgesplitst in genus species subsp/var

specnieuw <- read.csv2(
  file.path(path_data, "temp", "t_spec_nieuw.csv")
)

specnieuw <- specnieuw |>
  mutate(tax_canon = LSVI::parseTaxonnaam(SPECSYNO_ORIGINALNA))

subspecies_markers <- c("subsp\\.", "var\\.", "groep", "s\\.l\\.")
# We maken er één regex-string van: "subsp\.|var\.|groep|s\.l\.|s\.s\."
sub_pattern <- paste(subspecies_markers, collapse = "|")

split_taxa <- function(data, column = "tax_canon") {
  stopifnot(require(dplyr))
  stopifnot(require(stringr))
  data %>%
    mutate(
      # 1. Genus: eerste woord
      genus = str_extract(!!sym(column), "^[^ ]+"),

      # 2. Check of er een marker aanwezig is (voorafgegaan door een spatie)
      # We kijken of de marker gevolgd wordt door een spatie OF
      # door het einde van de regel ($)
      has_sub = str_detect(!!sym(column), paste0(" (", sub_pattern, ")( |$)")),

      # 3. Species:
      # We extraheren wat tussen het eerste woord en de marker staat
      species = ifelse(has_sub,
        str_match(
          !!sym(column),
          paste0("^[^ ]+ (.*?) (", sub_pattern, ")")
        )[, 2],
        str_replace(!!sym(column), "^[^ ]+ ", "")
      ),

      # 4. Subspecies:
      # Alles vanaf de marker tot het einde
      subspecies = ifelse(has_sub,
        str_extract(!!sym(column), paste0("(", sub_pattern, ").*$")),
        ""
      )
    ) %>%
    select(-has_sub)
}

# Test met je data
df_split <- split_taxa(specnieuw)


write.csv2(df_split, file.path(path_data, "temp", "specnieuw.csv"),
  row.names = FALSE
)
