# _shared.R
# Ejecutado via source() desde los .qmd del proyecto
# Requiere que `params` exista en el entorno del .qmd que lo llama

# Librerías ────────────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(limma)
library(ggplot2)
library(ggvenn)
library(auxprot)
library(knitr)
library(plotly)
library(tidyverse)
library(edgeR)

# Parámetros directos ──────────────────────────────────────────────────────────
org_key    <- params$org_key
exp_id     <- params$exp_id
tmt        <- params$tmt
astral     <- params$astral
use.unique <- params$use_unique
covars     <- if (params$covars == "") NULL else params$covars

# Parámetros derivados ─────────────────────────────────────────────────────────
org_db <- switch(org_key,
                 human    = "org.Hs.eg.db",
                 mouse    = "org.Mm.eg.db",
                 rat      = "org.Rn.eg.db",
                 celegans = "org.Ce.eg.db",
                 drome    = "org.Dm.eg.db",
                 yeast    = "org.Sc.sgd.db"
)

kegg_code <- switch(org_key,
                    human    = "hsa",
                    mouse    = "mmu",
                    rat      = "rno",
                    celegans = "cel",
                    drome    = "dme",
                    yeast    = "sce"
)

# Carga de datos ───────────────────────────────────────────────────────────────
metadata <- import(file.path(params$data_path, "metadata.txt"))

psm <- read_delim(
  file.path(params$data_path, "psm.tsv"),
  delim        = "\t",
  escape_double = FALSE,
  trim_ws      = TRUE
)

psm_proteinFiltered <- psm %>%
  select(`Protein ID`, matches("^Intensity [A-Za-z]")) %>% 
  rename_with(~ gsub("Intensity ", "", .))

raw <- tmt_integrator(
  psm,
  metadata,
  org        = org_key,
  tmt        = tmt,
  use.unique = use.unique,
  astral     = astral
)

comp <- import(file.path(params$data_path, "comparisons.txt")) %>%
  clean_comp()

df <- process_raw("fp", raw$protein, metadata)


metadata <- clean_key(metadata)

# Normalización ────────────────────────────────────────────────────────────────
df_norm_sl <- SL_norm(df, metadata)
df_norm    <- tmm_norm(df_norm_sl, metadata)

# Directorio de resultados ─────────────────────────────────────────────────────
if (!dir.exists("results")) dir.create("results")

message("protflow | _shared.R cargado: ", exp_id, " [", org_key, "]")


# Obtención de df_norm_peptide normalizado ─────────────────────────────────────
psm_clean <- psm %>%
  select(Peptide, starts_with("Intensity ")) %>%
  select(-`Intensity _134N`, -`Intensity _134C`, -`Intensity _135N`)

df_peptide_raw <- psm_clean %>%
  group_by(Protein.IDs = Peptide) %>%
  summarise(across(starts_with("Intensity "), sum, na.rm = TRUE), .groups = "drop")

mat_raw <- as.matrix(df_peptide_raw[, -1])
rownames(mat_raw) <- df_peptide_raw$Protein.IDs

target_effort <- mean(colSums(mat_raw, na.rm = TRUE))
sl_factors <- target_effort / colSums(mat_raw, na.rm = TRUE)
mat_sl <- t(t(mat_raw) * sl_factors)

tmm_factors <- calcNormFactors(mat_sl, method = "TMM")

mat_norm <- t(t(mat_sl) / tmm_factors)

df_norm_peptide <- as.data.frame(mat_norm) %>%
  rownames_to_column(var = "Protein.IDs") %>%
  rename_with(~ str_remove(., "^Intensity "), starts_with("Intensity "))