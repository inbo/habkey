library(dplyr)
library(readxl)

path_data <- file.path("data", "bron", "soortenlijsten")

# brondata: het geeft niet alleen de officiële namen, maar ook synoniemen
# waaronder de Nederlandse namen
taxa_bron <- read_xlsx(file.path(path_data, "synoniemenlijst_taxa2025.xlsx"))

# opsmuk van de tabel
colnames(taxa_bron) <- snakecase::to_snake_case(colnames(taxa_bron))

# voor het gebruik zijn zes kolommen nuttig
# preferred_taxon: de officiële wetenschappelijke naam. Dit is dus de standaard
# preferred_taxon_givid: id van de officiële naam, gebruikt in INBOVEG
# taxon_group: geeft aan of het een vaatplant, mos, lichen of kranswier is
# synonym_taxon: het synoniem, de standaardnaam wordt hier niet in herhaald !
# synonym_taxon_givid: id van het synoniem, gebruikt in INBOVEG
# synonym_language: de taal van het synoniem. Kan mogelijk handig zijn om Neder-
#   landse namen van de andere te kunnen onderscheiden.

# we gaan deze variabelen voor intern gebruik wel hernoemen
# preferred_taxon - > taxon
# preferred_taxon_givid - > taxonid
# taxon_group ok
# synonym_taxon - > tax_orig
# synonym_taxon_givid - > tax_origid
# synonym_language - > tax_orig_taal

taxa <- taxa_bron |>
  select(
    taxon = preferred_taxon,
    taxonid = preferred_taxon_givid,
    taxon_group,
    tax_orig = synonym_taxon,
    tax_origid = synonym_taxon_givid,
    tax_orig_taal = synonym_language
  )
# Omdat in de taxalijst de standaardnamen niet als synoniem vermeld worden,
# moeten we ze nog toevoegen. Als er geen synoniem voor bestaat, tax_ori NULL.

# toevoegen standaardnamen bij de synoniemen
# 1. NULL-waarden vervangen door taxon
taxa <- taxa |>
  mutate(
    tax_orig_taal = ifelse(tax_orig == "NULL", "Sci", tax_orig_taal),
    tax_origid = ifelse(tax_orig == "NULL", taxonid, tax_origid),
    tax_orig = ifelse(tax_orig == "NULL", taxon, tax_orig)
  )
# 2. taxon die niet voorkomen in tax_ori toevoegen
taxa <- bind_rows(
  taxa,
  taxa |>
    anti_join(taxa |>
                # styler: off
                distinct(taxon), join_by(tax_orig == taxon)) |>
    # styler: on
    mutate(
      tax_orig = taxon,
      tax_origid = taxonid,
      tax_orig_taal = "Sci"
    ) |>
    distinct()
)

# check originele taxa
taxa |>
  count(tax_origid) |>
  filter(n > 1)
# ok !

# check op mogelijke dubbels
taxa |>
  count(tax_orig, taxon_group) |>
  filter(n > 1)

# oplossen van het speciaal geval waarbij de Nederlandse naam dezelfde is als
# de wetenschappelijke naam van het geslacht. Bijv. Parnassia, Taxus (19 cases)
# Het zijn geen dubbels, want parnassia komt overeen met Parnassia palustris(L.)
# We lossen dit op door voor de Nederlandse namen alleen kleine letters te
# gebruiken.

taxa <- taxa |>
  mutate(tax_orig = if_else(tax_orig_taal == "nl", tolower(tax_orig), tax_orig))

# check op mogelijke dubbels, rekeninghoudende met de taxongroep
taxa |>
  count(tax_orig, taxon_group) |>
  filter(n > 1)
# ok

# check op mogelijke dubbels, zonder met de taxongroep rekening te houden
taxa |>
  count(tax_orig) |>
  filter(n > 1)
# ook ok

# nog voor de wetenschappelijke namen de canonische naam toevoegen
# we doen hiervoor beroep op functionaliteit van het LSVI-package
# nolint start
# remotes::install_github("inbo/LSVI@rbb")
# nolint end

library(LSVI) # voor het opstellen van de lijst sleutelsoorten
maakConnectiePool()

taxa <- taxa |>
  mutate(tax_canon = parseTaxonnaam(taxon))

# bewaren van deze taxonlijst
# als rds
saveRDS(taxa, file.path("data", "interim", "taxa.rds"))
# als csv
readr::write_csv2(taxa, file.path("data", "processed", "taxa.csv"))
