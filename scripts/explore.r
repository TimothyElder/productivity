library(openalexR)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(igraph)
library(ggplot2)
library(ggtext)
library(showtext)
library(future) # For parallel calls
library(furrr)  # for parallel calls
font_add(family = "Fira Sans",
         regular = "/Users/timothyelder/Library/Fonts/FiraSans-Regular.otf",
         bold = "/Users/timothyelder/Library/Fonts/FiraSans-Bold.otf",
         italic = "/Users/timothyelder/Library/Fonts/FiraSans-Italic.otf")

showtext_auto()
showtext_opts(dpi = 300)

theme_set(
  theme_minimal(base_family = "Fira Sans", base_size = 20) +
    theme(
      plot.title      = element_text(face = "bold"),
      plot.subtitle   = element_text(color = "gray40"),
      plot.caption    = element_markdown(color = "gray50"),
      legend.position = "bottom"
    )
)

source('scripts/functions.r')

options(openalexR.apikey = "SlDFTCWvxFsq5SVUGSNFm7")

###############################################################################
# Explore Away ----------------------------------------------------------------
###############################################################################

soc <- oa_fetch(entity = "concepts",
                search = "Sociology")

soc_id <- "https://openalex.org/C144024400"

df <- oa_fetch(entity = "institution",
              search = "University of Chicago",
              sam)

uc <- "https://openalex.org/I40347166"

uc_soc <- oa_fetch(entity = "works",
                   authorships.institutions.id = uc,
                   concepts.id = soc_id,
                   count_only = FALSE,
                   verbose = TRUE)

subset_by_concept(uc_soc, "Sociology", 0.5)

df <- subset_by_concept(uc_soc, "Sociology", 0.5)

soc_journals <- oa_fetch(entity = "venue",
                         concepts.id = soc_id,
                         #works_count = "> 1000",
                         display_name.search = c("Sociology", "Sociological",
                                                 "Socio"),
                         count_only = FALSE,
                         type = "journal",
                         verbose = TRUE)

View(subset_by_concept(soc_journals, "Sociology", 0.6))

df <- oa_fetch(entity = "author",
         display_name.search = "Edward O. Laumann")

df %>%
  select(id, last_known_institutions, topics) %>%
  rename(author_id = id) %>%
  unnest(last_known_institutions)

View(df)

abbott_id  <- "https://openalex.org/A5101509948"

author_ids <- c("https://openalex.org/A5043342879", "https://openalex.org/A5101509948",
                "https://openalex.org/A5063104254", "https://openalex.org/A5112248657")

abbott_articles <- oa_fetch(
  entity = "works",
  author.id = abbott_id
  )

journals <- oa_fetch(
        entity = "sources",
        id = unique(na.omit(abbott_articles$source_id))
)

# Find which record has a duplicate id field
problem_records <- sapply(works_raw, function(x) sum(names(x) == "id") > 1)
which(problem_records)

sf_2025 <- oa_fetch(
        entity = "works",
        locations.source.id = "https://openalex.org/S193359815",
        verbose = TRUE,
        from_publication_date = "2023-01-01",
        to_publication_date = "2024-12-31"
)

sf_2023_2024 <- oa_fetch(
    entity = "works",
    locations.source.id = "https://openalex.org/S193359815",
    from_publication_date = "2023-01-01",
    to_publication_date = "2024-12-31",
    type = "article",  # restrict to citable items
    verbose = TRUE
)

# citations received in 2025 to those articles
citations_2025 <- sf_2023_2024 %>%
    unnest(counts_by_year, names_sep = "_") %>%
    filter(counts_by_year_year == 2025) %>%
    summarise(total = sum(counts_by_year_cited_by_count, na.rm = TRUE)) %>%
    pull(total)

# denominator: citable items in 2023-2024
n_articles <- nrow(sf_2023_2024)

calc_if(citations_2025, n_articles)




source_id <- "https://openalex.org/S193359815"
year <- 2025

paste0(year, "-01-01")

fetch_and_calculate_if <- function(year, source_id) {

    df_y_1_2 <- oa_fetch(
    entity = "works",
    locations.source.id = source_id,
    from_publication_date = paste0(year - 2, "-01-01"),
    to_publication_date = paste0(year - 1, "-12-31"),
    type = "article",  # restrict to citable items
    verbose = TRUE
    )

    # citations received in 2025 to those articles
    citation_count <- df_y_1_2 %>%
        unnest(counts_by_year, names_sep = "_") %>%
        filter(counts_by_year_year == year) %>%
        summarise(total = sum(counts_by_year_cited_by_count, na.rm = TRUE)) %>%
        pull(total)

    # denominator: citable items in 2023-2024
    n_articles <- nrow(df_y_1_2)

    impact_factor <- calc_if(citation_count, n_articles)

    return(impact_factor)

}

fetch_and_calculate_if(year = year, source_id = source_id)



years <- c(2020, 2021, 2022, 2023, 2024)

for(i in years){
    print(fetch_and_calculate_if(year = i, source_id = source_id))

}

impacts <- runif(n = 10, min = 0, max = 3)

citations <- sample.int(20, 10, replace = FALSE)

calc_productivity(citations, impacts)


###############################################################################
# Trying to Generalize The Functions ------------------------------------------ 
###############################################################################

author_ids <- c("https://openalex.org/A5043342879", "https://openalex.org/A5101509948",
                "https://openalex.org/A5063104254", "https://openalex.org/A5112248657")

authors <- oa_fetch(
  entity = "authors",
  identifier = author_ids,
  verbose = TRUE
  )

authors %>%
    ggplot(aes(y = display_name, x = works_count)) +
    geom_bar(stat = "identity")


articles <- oa_fetch(
  entity = "works",
  author.id = author_ids,
  abstract = FALSE,
  verbose = TRUE
  )

sources <- articles %>%
    pull(source_id) %>%
    unique()
    
all_articles <- oa_fetch(
 entity = "works",
 locations.source.id = sources[1],
 type = "article",
 verbose = TRUE
)

oa_snowball(identifier = author_ids, 
            verbose = TRUE)

###############################################################################
# Clearly Defining the Task --------------------------------------------------- 
###############################################################################

# 1. Pull your target researchers and their publication records
# 2. Extract the set of journals Z and the relevant publication years
# 3. For each journal-year combination, pull enough data to compute IF
# 4. Join back to your researcher-article records

for(i in seq_along(author_ids)) {
    print(author_ids[i])
}


# 2. Get all the sources in which the authors articles appear
articles <- oa_fetch(
  entity = "works",
  author.id = author_ids,
  abstract = FALSE,
  verbose = TRUE
  )

sources <- articles %>%
    select(source_id, publication_year) %>%
    distinct()



sources_clean <- sources %>% 
  filter(!is.na(source_id), !is.na(publication_year))

plan(multisession, workers = 4)

ids <- list()
impacts <- list()
years <- list()

for(i in 1:nrow(sources_clean)) {
  result <- tryCatch(
    fetch_and_calculate_if(sources_clean$publication_year[i], sources_clean$source_id[i]),
    error = function(e) {
      message("Failed on row ", i, " source: ", sources_clean$source_id[i], " — ", e$message)
      return(NA)
    }
  )
  impacts[[i]] <- result
  ids[[i]] <- sources_clean$source_id[i]
  years[[i]] <- sources_clean$publication_year[i]
}




# Making this parallel 
plan(multisession, workers = availableCores() - 1)

results <- future_map_dfr(
  1:nrow(sources_clean),
  function(i) {
    tryCatch(
      {
        impact <- fetch_and_calculate_if(
          sources_clean$publication_year[i],
          sources_clean$source_id[i]
        )
        tibble(
          source_id = sources_clean$source_id[i],
          publication_year = sources_clean$publication_year[i],
          impact_factor = impact
        )
      },
      error = function(e) {
        message("Failed on row ", i, " source: ", sources_clean$source_id[i], " — ", e$message)
        tibble(
          source_id = sources_clean$source_id[i],
          publication_year = sources_clean$publication_year[i],
          impact_factor = NA
        )
      }
    )
  },
  .options = furrr_options(seed = NULL)
)

plan(sequential)

###############################################################################
# Getting the full script started---------------------------------------------- 
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

write.csv(results, "data/sampled_articles.csv", row.names = FALSE)

authors <- results %>%
  rename(article_id = id) %>%
  select(article_id, authorships) %>%
  unnest(authorships) %>%
  filter(author_position == "first")
