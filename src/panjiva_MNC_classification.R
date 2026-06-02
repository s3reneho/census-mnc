# ══════════════════════════════════════════════════════════════
# MNC CLASSIFICATION VIA CAPITAL IQ (WRDS)
# Classifies Panjiva consignees as MNC or non-MNC based on
# the number of corporate relationships in CIQ's company 
# relationship table (companyreltypeid = 5: subsidiary/operating unit)
# Data on import transactions downloaded from http://panjiva.com
# The following code runs on a sample (1,000 transactions) of US-KOR Imports
# ══════════════════════════════════════════════════════════════
rm(list=ls())
library(dplyr)
library(DBI)
library(RPostgres)
library(keyring)

# ── 1. Load Panjiva data ───────────────────────────────────────
# TODO: update path as needed
panjiva <- read_excel("C:/Users/s3ren/Downloads/Panjiva-US_Imports-sample.xlsx",sheet="US Imports Shipments")

# ── 2. Extract unique CIQ company IDs from Panjiva ────────────
# Consignee.SPCIQ.ID maps directly to CIQ's companyid
spciq_ids <- panjiva %>%
  filter(!is.na(`Consignee SPCIQ ID`)) %>%
  mutate(companyid = as.integer(`Consignee SPCIQ ID`)) %>%
  pull(companyid) %>%
  unique()

ids_str <- paste(spciq_ids, collapse = ",")
cat("Unique consignees with SPCIQ ID:", length(spciq_ids), "\n")

# ── 3. Connect to WRDS ────────────────────────────────────────
wrds <- dbConnect(Postgres(),
                  host     = "wrds-pgdata.wharton.upenn.edu",
                  port     = 9737,
                  dbname   = "wrds",
                  user     = "s3reneho",
                  password = key_get("wrds", username = "s3reneho"),
                  sslmode  = "require")

# ── 4. Query CIQ for corporate relationships ──────────────────
# A firm is classified as MNC if it has more than one relationship
# in ciqcompanyrel (type 5 = current subsidiary/operating unit),
# either as the subsidiary (companyid) or parent (companyid2).
# Firms with multiple relationships across countries are MNCs.
# NOTE: this is a proxy -- a cleaner approach would use GUO country
# counts, but requires resolving ownership chain direction in CIQ.
result <- dbGetQuery(wrds, sprintf("
  SELECT c.companyid, 
         c.companyname, 
         COUNT(*) AS n_relationships
  FROM ciq.ciqcompany c
  JOIN ciq.ciqcompanyrel rel
    ON c.companyid = rel.companyid
    OR c.companyid = rel.companyid2
  WHERE c.companyid IN (%s)
    AND rel.companyreltypeid = 5
  GROUP BY c.companyid, c.companyname
", ids_str))

# ── 5. Code MNC indicator ─────────────────────────────────────
# MNC = 1 if firm has more than one subsidiary/parent relationship
# MNC = 0 if only one relationship (standalone) or unmatched in CIQ
result$mnc <- ifelse(result$n_relationships > 1, 1, 0)
cat("MNC distribution (matched firms):\n")
print(table(result$mnc))

# ── 6. Merge MNC indicator back to Panjiva ────────────────────
# Unmatched firms (no SPCIQ ID or not found in CIQ) default to 0
# NOTE: these are not confirmed non-MNCs -- match rate should be
# reported in any empirical writeup
panjiva <- panjiva %>%
  mutate(companyid = as.integer(`Consignee SPCIQ ID`)) %>%
  left_join(result %>% select(companyid, mnc), by = "companyid") %>%
  mutate(mnc = ifelse(is.na(mnc), 0, mnc))

cat("MNC distribution (full Panjiva sample):\n")
print(table(panjiva$mnc))

# ── 7. Disconnect ─────────────────────────────────────────────
dbDisconnect(wrds)

