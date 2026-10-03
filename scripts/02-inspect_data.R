# inspect-data.R
#
# Step 1 of the data processing: open every dataset and get a quick overview.
# Nothing is changed here - this script only LOOKS at the raw data.
#
# Run from the project root (open the .Rproj first).

source("scripts/00-packages.R")
source("scripts/01-get-data.R")  # loads the four datasets


# ---- a reusable inspection function ----------------------------------------
#
# Works on any data frame. Prints:
#   - number of rows and columns
#   - every column with its type and first values
#   - missing values per column (only columns that have any)
#   - hidden "<5" values per column (only columns that have any)
#   - optionally: how many rows each ID has (is one row one unit?)

inspect_data <- function(df, name, id_col = NULL) {
  cat("\n\n==========", name, "==========\n")
  cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n\n")
  
  glimpse(df)
  
  na_counts <- colSums(is.na(df))
  cat("\nMissing values (columns with any):\n")
  print(na_counts[na_counts > 0])
  
  lt5_counts <- colSums(df == "<5", na.rm = TRUE)
  cat("\nHidden '<5' values (columns with any):\n")
  print(lt5_counts[lt5_counts > 0])
  
  if (!is.null(id_col)) {
    cat("\nRows per", id_col, "(n = rows per ID, n_ids = how many IDs):\n")
    df |>
      count(.data[[id_col]]) |>
      count(n, name = "n_ids") |>
      print()
  }
  
  invisible(df)
}


# ---- inspect each dataset ---------------------------------------------------

inspect_data(eindscores, "eindscores", id_col = "INSTELLINGSCODE")
inspect_data(referentieniveaus, "referentieniveaus", id_col = "INSTELLINGSCODE")
inspect_data(schooladviezen, "schooladviezen", id_col = "INSTELLINGSCODE")
inspect_data(schoolweging, "schoolweging", id_col = "OVT")


# ---- open them as spreadsheets (optional) ----------------------------------
# Uncomment to browse a dataset in RStudio:
# View(schooladviezen)