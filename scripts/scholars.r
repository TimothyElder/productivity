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
library(readr)

source('/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/scripts/functions.r')

options(openalexR.apikey = "SlDFTCWvxFsq5SVUGSNFm7")

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

results <- vector("list", length(journal_ids))

for(i in 1:length(journal_ids)) {
  results[[i]] <- oa_fetch(entity = "works",
                     locations.source.id = journal_ids[i],
                     type = "article",
                     options = list(sample = 40, seed = 123),
                     from_publication_date = "2015-01-01",
                     to_publication_date = "2025-12-31",
                     output = "dataframe",
                     verbose = TRUE)
}

results <- dplyr::bind_rows(results)

author_ids <- results %>%
  rename(article_id = id) %>%
  select(article_id, authorships) %>%
  unnest(authorships) %>%
  filter(author_position == "first") %>%
  drop_na(id) %>%
  pull(id)

authors <- oa_fetch(
  entity = "authors",
  identifier = author_ids,
  verbose = TRUE)

saveRDS(authors, "data/sampled_authors.rds")

authors %>%
  unnest(works_api_url)

author_works_results <- vector("list", nrow(authors))

for(i in seq_len(nrow(authors))) {
  author_works_results[[i]] <- oa_fetch(entity = "works",
                           type = "article",
                           author.id = authors$id[i],
                           output = "dataframe",
                           verbose = TRUE)
}

author_works_results <- dplyr::bind_rows(author_works_results)

saveRDS(author_works_results, "data/author_works_results.rds")

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

for (i in seq_len(source_years_df)) {

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

library(ggplot2)

articles <- readRDS("data/author_works_results.rds")

impacts <- read.csv("data/sampled_impact_factors.csv")

impacts %>%
  mutate(value = case_when(is.na(impact) ~ 0,
                           impact == 0 ~ 0,
                           TRUE ~ 1)) %>%
  group_by(publication_year) %>%
  summarise(total = n(), with_impact = sum(value)) %>%
  ungroup() %>%
  mutate(percent_with_impact = with_impact / total * 100) %>%
  ggplot(aes(x = publication_year, y = percent_with_impact)) +
  geom_line() +
  geom_point() +
  labs(title = "Percentage of Sources with Impact Factor by Publication Year",
       x = "Publication Year",
       y = "Percentage with Impact Factor") +
  theme_minimal()

impacts <- distinct(impacts)

unnested_articles <- articles %>%
  rename(article_id = id) %>%
  select(-display_name) %>%
  unnest(authorships) %>%
  left_join(impacts, by = c("source_id", "publication_year"))
  
scores <- list()
for(i in 1:nrow(authors)) {

  
  author_id <- authors$id[i]
  
  author_articles <- unnested_articles %>%
    # filter(id == author_id & publication_year >= 2015 & !is.na(impact))
    filter(id == author_id, publication_year >= 2015, !is.na(impact))

  print(nrow(author_articles))

   scores[[i]] <- calc_productivity(author_articles$cited_by_count, author_articles$impact)
  
}

authors$score <- as.numeric(scores)

authors_test <- authors %>%
  filter(!is.na(score))

cor.test(authors_test$score, authors_test$cited_by_count)

hist(authors_test$score)
range(authors_test$score)
hist(authors_test$cited_by_count)

plot(authors_test$score, authors_test$cited_by_count)

cor.test(authors_test$score, authors_test$h_index)

plot(authors_test$score, authors_test$h_index)

cor.test(authors$h_index, authors$cited_by_count)

plot(authors_test$h_index, authors_test$cited_by_count)
hist(authors_test$h_index)


hist(impacts$impact)
