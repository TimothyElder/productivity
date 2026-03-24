library(openalexR)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(igraph)
library(ggplot2)

source('scripts/functions.r')

options(openalexR.apikey = "SlDFTCWvxFsq5SVUGSNFm7")


sample_journals <- read_csv("data/sampled_impact_factors.csv")

author_works_results <- readRDS("data/author_works_results.rds")

sample_authors <- readRDS("data/sampled_authors.rds")

df <- author_works_results %>%
  rename(article_id = id, article_display_name = display_name) %>%
  unnest(authorships) %>%
  filter(id %in% sample_authors$id, publication_year %in% sample_journals$publication_year) %>%
  left_join(sample_journals)

author_ids <- unique(sample_authors$id)
productivity_score <- numeric(length(author_ids))

for(i in seq_along(author_ids)){
  temp_df <- filter(df, id == author_ids[i])
  
  productivity_score[i] <- calc_productivity(temp_df$cited_by_count, temp_df$impact)

}

sample_authors$ps <- productivity_score

normalize <- function(x) {
  result <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  return(result)
}

# which is subtracting the mean and dividing by the SD

standardize <- function(x) {
  result <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(result)
}



sample_authors <- sample_authors %>%
  mutate(outlier = case_when(abs(ps - mean(ps, na.rm = TRUE)) <= 2 * sd(ps, na.rm = TRUE) ~ 0, 
                             TRUE ~ 1),
         ps_normalized = normalize(ps),
         ps_standardized = standardize(ps),
         ps_logged = log(ps))

sample_authors %>%
  ggplot(aes(x = ps)) +
  geom_histogram()

sample_authors %>%
  ggplot(aes(x = ps_standardized)) +
  geom_histogram()

sample_authors %>%
  ggplot(aes(x = ps_normalized)) +
  geom_histogram()

sample_authors %>%
  ggplot(aes(x = ps_logged)) +
  geom_histogram()

sample_authors %>%
  ggplot(aes(x = h_index)) +
  geom_histogram()

sample_authors %>%
  ggplot(aes(x = `2yr_mean_citedness`)) +
  geom_histogram()

sample_authors %>%
  ggplot(aes(x = cited_by_count)) +
  geom_histogram()

###############################################################################
# Associations ---------------------------------------------------------------- 
###############################################################################

sample_authors %>%
  ggplot(aes(x = ps, y = h_index)) +
  geom_point() +
  geom_smooth(method = "lm")

sample_authors %>%
  filter(outlier == 0) %>%
  ggplot(aes(x = ps, y = h_index)) +
  geom_point() +
  geom_smooth(method = "lm")

sample_authors %>%
  ggplot(aes(x = ps_logged, y = h_index)) +
  geom_point() +
  geom_smooth(method = "lm")

sample_authors %>%
  filter(outlier == 0) %>%
  ggplot(aes(x = ps_logged, y = h_index)) +
  geom_point() +
  geom_smooth(method = "lm")

sample_authors %>%
  ggplot(aes(x = ps, y = `2yr_mean_citedness`)) +
  geom_point() +
  geom_smooth(method = "lm")

sample_authors %>%
  filter(outlier == 0) %>%
  ggplot(aes(x = ps, y = `2yr_mean_citedness`)) +
  geom_point() +
  geom_smooth(method = "lm")

sample_authors %>%
  ggplot(aes(x = ps, y = cited_by_count)) +
  geom_point() +
  geom_smooth(method = "lm")

sample_authors %>%
  filter(outlier == 0) %>%
  ggplot(aes(x = ps, y = cited_by_count)) +
  geom_point() +
  geom_smooth(method = "lm")


cor.test(sample_authors$h_index, sample_authors$ps)

cor.test(sample_authors$`2yr_mean_citedness`, sample_authors$ps)

cor.test(sample_authors$cited_by_count, sample_authors$ps)



ggplot(df, aes(x = cited_by_count, y = cited_by_count * impact)) +
  geom_point()


ggplot(df, aes(x = impact)) +
  geom_histogram()


df$cite_by_impact <- df$cited_by_count * df$impact


ggplot(df, aes(x = cite_by_impact)) +
  geom_histogram()

df %>%
  group_by(cite_by_impact) %>%
  count() %>%
  View()
