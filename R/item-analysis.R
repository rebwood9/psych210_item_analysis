# Item analysis helpers for PSYC 210 exams
# Source from a report with: source(here::here("R", "item-analysis.R"))

library(tidyverse)
library(here)

# Read + reshape ----

# One instructor file (wide Canvas export) -> long, one row per student x question
reshape_section <- function(df) {
  df <- df |>
    slice_max(attempt, n = 1, with_ties = FALSE, by = id)

  all_names <- names(df)
  q_positions <- which(str_detect(all_names, "^\\d+:"))

  pairs <- tibble(
    question_raw = all_names[q_positions],
    points_col = all_names[q_positions + 1],
    question_id = str_extract(question_raw, "^\\d+")
  )

  pairs |>
    pmap(\(question_raw, points_col, question_id) {
      df |>
        mutate(
          question_id = question_id,
          question_title = str_remove(question_raw, "^\\d+:\\s*"),
          selection = as.character(.data[[question_raw]]),
          points = as.numeric(.data[[points_col]]),
          answered = !is.na(selection) | !is.na(points),
          correct = if_else(!is.na(points) & points == 1, 1L, 0L)
        ) |>
        select(
          id, section_id, submitted, attempt, question_id,
          question_title, selection, points, answered, correct, score
        )
    }) |>
    list_rbind()
}

# All instructor files for one exam; instructor comes from the file name
read_exam <- function(exam, term = "spring") {
  list.files(here("data", term, exam), pattern = "\\.csv$", full.names = TRUE) |>
    set_names(\(f) tools::file_path_sans_ext(basename(f))) |>
    map(\(f) read_csv(f, show_col_types = FALSE) |> reshape_section()) |>
    list_rbind(names_to = "instructor") |>
    mutate(exam = exam, .before = 1)
}

# Bad items ----

# Flags titles that (a) had more than one answer scored correct (regraded /
# credit given) or (b) were answered more than once by the same student
flag_bad_items <- function(long) {
  multi_correct <- long |>
    filter(answered, correct == 1, !is.na(selection)) |>
    distinct(exam, question_title, selection) |>
    count(exam, question_title) |>
    filter(n > 1) |>
    transmute(exam, question_title, reason = "more than one answer scored correct")

  seen_twice <- long |>
    filter(answered) |>
    count(exam, instructor, id, question_title) |>
    filter(n > 1) |>
    distinct(exam, question_title) |>
    mutate(reason = "some students saw it twice")

  bind_rows(multi_correct, seen_twice) |>
    summarise(
      reason = str_flatten(unique(reason), collapse = "; "),
      .by = c(exam, question_title)
    )
}

# Item statistics ----

# Top / middle / bottom thirds of total score, computed within exam (and `by`)
add_tiers <- function(long, by = NULL) {
  groups <- c("exam", by)

  tiers <- long |>
    distinct(across(all_of(c(groups, "id", "score")))) |>
    mutate(
      tier = case_when(
        score >= quantile(score, 2 / 3, na.rm = TRUE) ~ "top",
        score >= quantile(score, 1 / 3, na.rm = TRUE) ~ "middle",
        .default = "bottom"
      ),
      .by = all_of(groups)
    )

  long |>
    left_join(tiers, by = c(groups, "id", "score"))
}

# Difficulty, discrimination index, and option point-biserials.
# `by = NULL` gives pooled results; `by = "instructor"` or "section_id"
# gives one row per item per group.
item_summary <- function(long, by = NULL) {
  groups <- c("exam", by, "question_title")
  answered <- long |>
    add_tiers(by = by) |>
    filter(answered)

  item_stats <- answered |>
    summarise(
      answered_student_count = n(),
      top_student_count = sum(tier == "top"),
      middle_student_count = sum(tier == "middle"),
      bottom_student_count = sum(tier == "bottom"),
      correct_student_count = sum(correct),
      wrong_student_count = answered_student_count - correct_student_count,
      correct_student_ratio = correct_student_count / answered_student_count,
      wrong_student_ratio = wrong_student_count / answered_student_count,
      correct_top_count = sum(correct == 1 & tier == "top"),
      correct_middle_count = sum(correct == 1 & tier == "middle"),
      correct_bottom_count = sum(correct == 1 & tier == "bottom"),
      difficulty_index = correct_student_ratio,
      variance = difficulty_index * (1 - difficulty_index),
      standard_deviation = sqrt(variance),
      .by = all_of(groups)
    )

  # Every student crossed with every option their item offered
  options_per_q <- answered |>
    filter(!is.na(selection)) |>
    distinct(across(all_of(groups)), option = selection)

  option_pbis <- answered |>
    filter(!is.na(selection)) |>
    select(all_of(groups), id, score, selection, correct) |>
    left_join(options_per_q, by = groups, relationship = "many-to-many") |>
    mutate(chose_option = if_else(selection == option, 1L, 0L)) |>
    summarise(
      point_biserial = cor(chose_option, score, use = "complete.obs"),
      n_chose = sum(chose_option),
      is_correct_option = any(correct == 1L & chose_option == 1L),
      .by = all_of(c(groups, "option"))
    )

  pbis_wide <- option_pbis |>
    arrange(across(all_of(groups)), desc(is_correct_option)) |>
    mutate(
      distractor_num = cumsum(!is_correct_option),
      label = if_else(
        is_correct_option,
        "point_biserial_correct",
        paste0("point_biserial_distractor_", distractor_num)
      ),
      .by = all_of(groups)
    ) |>
    select(all_of(groups), label, point_biserial) |>
    pivot_wider(names_from = label, values_from = point_biserial)

  disc_index <- answered |>
    filter(tier %in% c("top", "bottom")) |>
    summarise(p_correct = mean(correct), .by = all_of(c(groups, "tier"))) |>
    pivot_wider(names_from = tier, values_from = p_correct) |>
    transmute(across(all_of(groups)), discrimination_index = top - bottom)

  items <- item_stats |>
    left_join(disc_index, by = groups) |>
    left_join(pbis_wide, by = groups)

  list(items = items, options = option_pbis)
}

# Plots ----

plot_difficulty <- function(df, subtitle_note = "All items", y_max = NULL) {
  mu <- mean(df$difficulty_index, na.rm = TRUE)
  sdv <- sd(df$difficulty_index, na.rm = TRUE)

  ggplot(df, aes(x = difficulty_index)) +
    geom_histogram(bins = 30, fill = "#4C72B0", color = "white", alpha = 0.9) +
    geom_vline(xintercept = mu, color = "#C44E52", linetype = "dashed", linewidth = 0.8) +
    geom_vline(
      xintercept = c(mu - sdv, mu + sdv),
      color = "grey30", linetype = "dashed", linewidth = 0.8
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(
      title = "Distribution of Difficulty Index",
      subtitle = paste0(
        subtitle_note, "\n",
        "Mean = ", round(mu, 2),
        "   ± 1 SD = [", round(mu - sdv, 2), ", ", round(mu + sdv, 2), "]"
      ),
      x = "Difficulty Index",
      y = "Count",
      caption = paste0("N = ", sum(!is.na(df$difficulty_index)))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40"),
      panel.grid.minor = element_blank()
    )
}

plot_chapters <- function(df, subtitle = NULL) {
  ggplot(df, aes(x = factor(chapter), y = after_stat(count / sum(count)))) +
    geom_bar(fill = "#4C72B0", color = "white", alpha = 0.9) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      title = "Distribution of Questions by Chapter",
      subtitle = subtitle,
      x = "Chapter",
      y = "Proportion of Questions",
      caption = paste0("N = ", sum(!is.na(df$chapter)))
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40"),
      panel.grid.minor = element_blank()
    )
}
