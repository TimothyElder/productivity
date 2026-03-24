library(DBI)
library(RSQLite)

init_db <- function(db_path = "impact_factors.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS impact_factors (
      journal_id TEXT NOT NULL,
      year INTEGER NOT NULL,
      impact_factor REAL,
      PRIMARY KEY (journal_id, year)
    )
  ")
  conn
}

get_impact_factor <- function(conn, journal_id, year) {
  result <- dbGetQuery(conn, 
    "SELECT impact_factor FROM impact_factors WHERE journal_id = ? AND year = ?",
    params = list(journal_id, year)
  )
  if (nrow(result) == 0) NULL else result$impact_factor
}

set_impact_factor <- function(conn, journal_id, year, impact_factor) {
  dbExecute(conn,
    "INSERT OR REPLACE INTO impact_factors (journal_id, year, impact_factor) VALUES (?, ?, ?)",
    params = list(journal_id, year, impact_factor)
  )
}

###############################################################################
# How to use in pipeline 
###############################################################################

conn <- init_db()

for (i in seq_len(nrow(jobs))) {
  journal_id <- jobs$journal_id[i]
  year <- jobs$year[i]
  
  cached <- get_impact_factor(conn, journal_id, year)
  
  if (!is.null(cached)) {
    if_score <- cached
  } else {
    if_score <- compute_impact_factor(journal_id, year)  # your existing logic
    set_impact_factor(conn, journal_id, year, if_score)
  }
}

dbDisconnect(conn)

###############################################################################
# Package Installer 
###############################################################################

install.packages(c("DBI", "RSQLite"))