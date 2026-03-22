library(openalexR)
library(tidyr)
library(dplyr)
library(magrittr)
library(readr)

source('/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/scripts/functions.r')

options(openalexR.apikey = "SlDFTCWvxFsq5SVUGSNFm7")

###############################################################################
# Trying to Generalize The Functions ------------------------------------------
###############################################################################

author_ids <- c("https://openalex.org/A5043342879", "https://openalex.org/A5101509948",
                "https://openalex.org/A5063104254", "https://openalex.org/A5112248657")

authors <- oa_fetch(
  entity = "authors",
  identifier = author_ids,
  verbose = TRUE)

articles <- oa_fetch(
  entity = "works",
  author.id = author_ids,
  abstract = FALSE,
  verbose = TRUE)

sources <- articles %>%
  select(source_id, publication_year) %>%
  distinct()

sources_clean <- sources %>%
  filter(!is.na(source_id), !is.na(publication_year))

###############################################################################
# Sequential Impact Factor Retrieval ------------------------------------------
###############################################################################

output_file <- "/dartfs-hpc/rc/home/n/f007dcn/productivity-scores/data/impact_factors.csv"

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

for (i in 1:nrow(sources_clean)) {

  already_done <- any(
    completed_pairs$source_id == sources_clean$source_id[i] &
    completed_pairs$publication_year == sources_clean$publication_year[i]
  )
  if (already_done) next

  result <- tryCatch(
    fetch_and_calculate_if(sources_clean$publication_year[i], sources_clean$source_id[i]),
    error = function(e) {
      message("Failed on row ", i, " source: ", sources_clean$source_id[i], " — ", e$message)
      return(NA)
    }
  )

  row <- data.frame(
    source_id = sources_clean$source_id[i],
    publication_year = sources_clean$publication_year[i],
    impact = result
  )

  write.table(row, output_file, append = TRUE, sep = ",",
              col.names = FALSE, row.names = FALSE)
}

impacts <- read_csv("data/impact_factors.csv")

impacts %>%
  mutate(value = case_when(is.na(impact) ~ 0,
                           impact == 0 ~ 0,
                           TRUE ~ 1)) %>%
  group_by(publication_year) %>%
  summarise(total = n(), with_impact = sum(value)) %>%
  ungroup() %>%
  mutate(percent_with_impact = with_impact / total * 100) %>% View()
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
    filter(id == author_id & publication_year >= 2012 & !is.na(impact))

   scores[[i]] <- calc_productivity(author_articles$cited_by_count, author_articles$impact)
  
}

authors$score <- as.numeric(scores)

cor.test(authors$score, authors$cited_by_count)

cor.test(authors$score, authors$h_index)

cor.test(authors$h_index, authors$cited_by_count)
