# To test my productivity score we are going to do the following:

# Take a random sample of 40 papers from each of the
# following journals in the social sciences:

# - Psychological Bulletin
# - American Journal of Sociology
# - American Economic Review
# - American Ethnologist
# - American Historical Review

# Subset to citable articles (no book reviews or commentary)
# published in the last ten years,

# Take the first author from each of the sampled papers then

# 1. calculate productivity score
# 2. Calculate hindex
# 3. return total number of citations to these schoalrs
# 4. total number of articles published

library(openalexR)
library(tidyr)
library(dplyr)
library(stringr)

source('/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/scripts/functions.r')

options(openalexR.apikey = "SlDFTCWvxFsq5SVUGSNFm7")

author_works_results <- readRDS("/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/data/author_works_results.rds")

source_years_df <- author_works_results %>%
    filter(publication_year >= 2015) %>%
    select(source_id, publication_year) %>%
    distinct() %>%
    filter(!is.na(source_id), !is.na(publication_year))

###############################################################################
# Sequential Impact Factor Retrieval ------------------------------------------
###############################################################################

output_file <- "/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/data/sampled_impact_factors.csv"

# Initialize file with header if it doesn't exist, otherwise load completed pairs
if (!file.exists(output_file)) {
  write.csv(
    data.frame(source_id = character(), publication_year = integer(), impact = numeric()),
    output_file,
    row.names = FALSE
  )
  completed_pairs <- data.frame(source_id = character(), publication_year = integer())
} else {
  existing <- read.csv(output_file)
  # Only count rows where impact was actually retrieved (not NA)
  completed_pairs <- existing %>%
    filter(!is.na(impact)) %>%
    select(source_id, publication_year)
  message(nrow(completed_pairs), " rows already completed, resuming...")
}

for (i in seq_len(nrow(source_years_df))) {

  already_done <- any(
    completed_pairs$source_id == source_years_df$source_id[i] &
    completed_pairs$publication_year == source_years_df$publication_year[i]
  )
  if (already_done) next

  result <- tryCatch(
    fetch_and_calculate_if(source_years_df$publication_year[i], source_years_df$source_id[i]),
    error = function(e) {
      message("Failed on row ", i, " source: ", source_years_df$source_id[i], " — ", e$message)
      return(NA)
    }
  )

  row <- data.frame(
    source_id = source_years_df$source_id[i],
    publication_year = source_years_df$publication_year[i],
    impact = result
  )

  write.table(row, output_file, append = TRUE, sep = ",",
              col.names = FALSE, row.names = FALSE)
}