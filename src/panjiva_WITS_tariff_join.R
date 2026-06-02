#### Cleaning and merging HS Codes
#### Data on tariffs downloaded from https://wits.worldbank.org/
#### Data on import transactions downloaded from http://panjiva.com
### The following code runs on a sample (1,000 transactions) of US-KOR Imports

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(readr)

# ── 1. Load data ───────────────────────────────────────────────
### Change directory
panjiva <- read_excel("C:/Users/s3ren/Downloads/Panjiva-US_Imports-sample.xlsx",sheet="US Imports Shipments")
tariff <- read_csv("C:/Users/s3ren/Downloads/US_KORImports.csv",
                   locale = locale(encoding = "UTF-8"),
                   show_col_types = FALSE)

# ── 2. Extract and clean HS codes ─────────────────────────────
panjiva_hs <- panjiva %>% 
  # Extract year
  mutate(year = as.integer(str_sub(as.Date(as.numeric(`Arrival Date`),origin = "1899-12-30"),1,4))) %>% 
  mutate(row_id = row_number()) %>%
  # Extract all XXXX.XX(.XX)(.XX) patterns
  mutate(hs_raw = str_extract_all(`HS Code`, "\\d{4}\\.\\d{2}(?:\\.\\d+)*")) %>%
  unnest(hs_raw) %>%                          # one row per HS code
  # Standardize to 6-digit: remove dots, take first 6 chars
  mutate(hs6 = str_remove_all(hs_raw, "\\.") %>% str_sub(1, 6)) %>%
  distinct(row_id, hs6, .keep_all = TRUE)     # drop duplicate HS codes per row

cat("Panjiva rows after HS expansion:", nrow(panjiva_hs), "\n")
cat("Unique HS6 codes:", n_distinct(panjiva_hs$hs6), "\n")

# ── 3. Prepare WITS tariff data ───────────────────────────────
tariff_clean <- tariff %>%
  filter(DutyType == "AHS") %>%
  mutate(hs6 = str_pad(as.character(Product), 6, pad = "0")) %>%
  select(hs6, `Tariff Year`, `Simple Average`, `Weighted Average`) %>%
  rename(tariff_year    = `Tariff Year`,
         tariff_simple  = `Simple Average`,
         tariff_wtd     = `Weighted Average`)

cat("WITS years available:", sort(unique(tariff_clean$tariff_year)), "\n")

# ── 4. Join ───────────────────────────────────────────────────
merged <- panjiva_hs %>%
  inner_join(tariff_clean, 
             by = c("hs6" = "hs6", 
                    "year" = "tariff_year"))

cat("Matched rows:", sum(!is.na(merged$tariff_simple)), 
    "of", nrow(merged), "\n")
cat("Unmatched HS6 codes:", 
    merged %>% filter(is.na(tariff_simple)) %>% 
      pull(hs6) %>% unique() %>% length(), "\n")

# ── 5. Quick summary ──────────────────────────────────────────
merged %>%
  summarise(
    mean_tariff  = mean(tariff_simple, na.rm = TRUE),
    median_tariff = median(tariff_simple, na.rm = TRUE),
    n_matched    = sum(!is.na(tariff_simple)),
    n_total      = n()
  ) %>%
  print()

rm(panjiva,panjiva_hs,tariff,tariff_clean)