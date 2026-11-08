# Check the rendered site for exam question text before publishing.
# Run after `quarto render`. Returns the pages that contain any question.

library(tidyverse)
library(here)

titles <- list.files(
  here("data"),
  pattern = "_combined\\.csv$",
  recursive = TRUE,
  full.names = TRUE
) |>
  map(\(f) read_csv(f, show_col_types = FALSE) |> select(question_title)) |>
  list_rbind() |>
  distinct(question_title) |>
  pull(question_title)

# First 30 characters of each title, keeping only plain-text fragments
# so HTML escaping of quotes and symbols doesn't hide a match
fragments <- titles |>
  str_squish() |>
  str_sub(1, 30) |>
  keep(\(x) str_detect(x, "^[A-Za-z0-9 ,.]+$")) |>
  unique()

pages <- list.files(
  here("docs"),
  pattern = "\\.(html|json)$",
  recursive = TRUE,
  full.names = TRUE
)

leaks <- pages |>
  set_names() |>
  map(\(p) {
    text <- read_file(p)
    fragments[str_detect(text, fixed(fragments))]
  }) |>
  compact()

cat(length(fragments), "question fragments checked across", length(pages), "files\n")

if (length(leaks) == 0) {
  cat("No question text found in docs/\n")
} else {
  leaks |> map_int(length) |> print()
}
