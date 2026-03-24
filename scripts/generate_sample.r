run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")

library(openalexR)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(DBI)
library(RSQLite)

source('/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/scripts/functions.r')

options(openalexR.apikey = "SlDFTCWvxFsq5SVUGSNFm7")

###############################################################################
# Journal Definitions ----------------------------------------------------------
###############################################################################

journal_names <- c("Psychological Bulletin",
                   "American Journal of Sociology",
                   "American Economic Review",
                   "American Ethnologist",
                   "American Historical Review")

journal_ids <- c("https://openalex.org/S75627607",
                 "https://openalex.org/S122471516",
                 "https://openalex.org/S23254222",
                 "https://openalex.org/S114801684",
                 "https://openalex.org/S197437610")

###############################################################################
# Sample Articles --------------------------------------------------------------
###############################################################################

journal_sample <- vector("list", length(journal_ids))

for (i in seq_along(journal_ids)) {
  journal_sample[[i]] <- oa_fetch(entity = "works",
                                  locations.source.id = journal_ids[i],
                                  type = "article",
                                  options = list(sample = 100, seed = 123),
                                  from_publication_date = "2015-01-01",
                                  to_publication_date   = "2025-12-31",
                                  output = "dataframe",
                                  verbose = TRUE)
}

journal_sample <- dplyr::bind_rows(journal_sample)

###############################################################################
# Extract First Authors --------------------------------------------------------
###############################################################################

author_ids <- journal_sample %>%
  rename(article_id = id) %>%
  select(article_id, authorships) %>%
  unnest(authorships) %>%
  filter(author_position == "first") %>%
  drop_na(id) %>%
  pull(id)

###############################################################################
# Fetch Author Metadata --------------------------------------------------------
###############################################################################

authors <- oa_fetch(entity     = "authors",
                    identifier = author_ids,
                    verbose    = TRUE)

saveRDS(authors, 
  sprintf("/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/data/sampled_authors_%s.rds", run_id))

###############################################################################
# Fetch Author Works -----------------------------------------------------------
###############################################################################

author_works_results <- vector("list", nrow(authors))

for (i in seq_len(nrow(authors))) {
  author_works_results[[i]] <- oa_fetch(entity    = "works",
                                        type      = "article",
                                        author.id = authors$id[i],
                                        output    = "dataframe",
                                        verbose   = TRUE)
}

author_works_results <- dplyr::bind_rows(author_works_results)

saveRDS(author_works_results, 
  sprintf("/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/data/author_works_results_%s.rds", run_id))

###############################################################################
# Build Source-Year Jobs -------------------------------------------------------
###############################################################################

source_years_df <- author_works_results %>%
  filter(publication_year >= 2015) %>%
  select(source_id, publication_year) %>%
  distinct() %>%
  filter(!is.na(source_id), !is.na(publication_year))

###############################################################################
# Impact Factor Retrieval with SQLite Cache ------------------------------------
###############################################################################

conn <- init_db("/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/data/impact_factors.db")

completed_pairs <- dbGetQuery(conn, 
  "SELECT journal_id, year FROM impact_factors WHERE impact_factor IS NOT NULL"
)

remaining <- source_years_df %>%
  anti_join(completed_pairs,
            by = c("source_id" = "journal_id",
                   "publication_year" = "year"))

message(nrow(completed_pairs), " journal-year pairs already cached")
message(nrow(remaining), " remaining to fetch")

for (i in seq_len(nrow(remaining))) {

  journal_id <- remaining$source_id[i]
  year       <- remaining$publication_year[i]

  result <- tryCatch(
    fetch_and_calculate_if(year, journal_id),
    error = function(e) {
      message("Failed on row ", i, " source: ", journal_id, " — ", e$message)
      return(NA_real_)
    }
  )

  set_impact_factor(conn, journal_id, year, result)
}

dbDisconnect(conn)