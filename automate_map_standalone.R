# automate_map_standalone.R — standalone/terminal runner for the CBR map's
# Google Sheet -> git push automation. Run via:  Rscript automate_map_standalone.R
# (or via run_map_automation.bat). Same logic as automate_map.R, plus an
# explicit setwd() since Rscript doesn't inherit the RStudio project's cwd.

PROJ_DIR <- "E:/Abhinandan/BCI/eBird-projects/CBR/CBR-map"
setwd(PROJ_DIR)

library(googlesheets4)
library(dplyr)

UPDATE_INTERVAL <- 14400 # 4 hours in seconds
TARGET_EMAIL <- "alenalex@ncf-india.org" # Your preferred auth account

run_sync_cycle <- function() {
  tryCatch({
    cat("\n--- Cycle Started:", as.character(Sys.time()), "---\n")
    gs4_auth(email = TARGET_EMAIL)

    # 1. Source the file and call the function
    source("source_data.R")
    changes_made <- update_map_data()

    # 2. ONLY proceed to Git if changes_made is TRUE
    if (changes_made) {

      update_time <- Sys.time()
      writeLines(as.character(update_time), "last_update.txt")

      system('git config user.email "skimmer@birdcount.in"')
      system('git config user.name "Bird Count India"')

      cat("Changes detected. Proceeding to Git Push...\n")
      system("git add .") # Corrected space here

      commit_msg <- sprintf('git commit -m "Auto-update: %s"', Sys.time())
      system(commit_msg)
      system("git push origin main")
      cat("--- Cycle Completed Successfully ---\n")
    } else {
      cat("--- Cycle ended: No new data to push. ---\n")
    }

  }, error = function(e) {
    cat("ERROR in cycle:", e$message, "\n")
  })
}

cat("Starting automation loop. Press Ctrl+C to stop.\n")
repeat {
  run_sync_cycle()
  Sys.sleep(UPDATE_INTERVAL)
}
