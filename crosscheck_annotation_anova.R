library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

# Load data
features <- read_excel("~/Desktop/Master thesis/Raw data/Annotation/processed/POS_C24P24.xlsx")
db <- read_excel("~/Desktop/Master thesis/Raw data/Annotation/POS_C24P24_lipidmaps.xlsx")   # db có mz_theoretical + adduct

# Convert characters into numeric
features[[1]] <- as.numeric(features[[1]])
features[[2]] <- as.numeric(features[[2]])
str(features)


# ppm tolerance
ppm_tol <- 20


annot <- features %>%
  rowwise() %>%
  mutate(
    matches = list(
      db %>%
        mutate(
          ppm_error = abs(mz_theoretical - `m/z`) / mz_theoretical * 1e6
        ) %>%
        filter(ppm_error < ppm_tol)
    )
  ) %>%
  ungroup() %>%
  unnest(matches, keep_empty = TRUE) %>%
  mutate(
    Name = replace_na(Name, "Cannot find any matching candidates"),
    Mode = "POS",
    Label = "C24P24") #Replace NA in Name column => Cannot find....
#######ANOVA
anova <- read_excel("~/Desktop/Master thesis/Raw data/Annotation/processed/POS_ANOVA.xlsx")

tukey_yes_no <- function(tukey_str, interest_pairs = c("P1-C1","P6-C6","P24-C24"), min_hits = 2) {
  if (is.na(tukey_str) || tukey_str == "") 
    return("No")
  items <- unlist(strsplit(tukey_str, "\\s*;\\s*"))
  hits  <- sum(items %in% interest_pairs)
  if (hits >= min_hits) "Yes" 
    else "No"
}

anova_flag <- anova %>%
  mutate(`Tukey's HSD` = vapply(`Tukey's HSD`, tukey_yes_no, character(1))) %>%
  select(`m/z`, `retention time`, `Tukey's HSD`)

anova_flag[[1]] <- as.numeric(anova_flag[[1]])
anova_flag[[2]] <- as.numeric(anova_flag[[2]])

annot_2 <- annot %>%
  left_join(anova_flag, by = c("m/z", "retention time")) %>% 
  mutate(`Tukey's HSD` = replace_na(`Tukey's HSD`, "No"))

# Generate output
write_xlsx(annot_2, "~/Desktop/Master thesis/Raw data/Annotation/anova_added/POS_C24P24_annotation_1.xlsx")
