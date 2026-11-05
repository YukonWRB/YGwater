# Internal helpers for water temperature report ExtendedTasks.
# Keep these in the package namespace so mirai workers can call them without
# serializing a Shiny module environment.

water_temp_build_report_filename <- function(site_name, lang) {
  site_name <- as.character(site_name)
  if (
    length(site_name) == 0 ||
      is.na(site_name[[1]]) ||
      !nzchar(site_name[[1]])
  ) {
    site_name <- tr("water_temp_selected_site", lang)
  } else {
    site_name <- site_name[[1]]
  }
  site_name <- gsub("[\\\\/:*?\"<>|]+", " ", site_name)
  site_name <- gsub("\\s+", " ", site_name)
  paste0(
    trimws(site_name),
    " ",
    tr("water_temp_report_bundle", lang),
    " ",
    Sys.Date(),
    ".zip"
  )
}


complete_fake_dates <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  tidyr::complete(
    data,
    fake_date = seq(
      min(data$fake_date, na.rm = TRUE),
      max(data$fake_date, na.rm = TRUE),
      by = "day"
    )
  )
}

safe_min <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

summarise_fake_date <- function(data, value_col) {
  if (nrow(data) == 0) {
    return(data.frame(
      fake_date = as.Date(character()),
      tmin = numeric(),
      tmax = numeric(),
      tmean = numeric()
    ))
  }

  out <- data |>
    dplyr::group_by(fake_date) |>
    dplyr::summarise(
      tmin = safe_min(.data[[value_col]]),
      tmax = safe_max(.data[[value_col]]),
      tmean = safe_mean(.data[[value_col]]),
      .groups = "drop"
    )

  complete_fake_dates(out)
}

add_threshold_lines <- function(plot_obj, thresholds) {
  thresholds <- as.character(thresholds)
  threshold_cols <- c(
    "20" = "#8b0000",
    "18" = "#fe0100",
    "13" = "#ffa405"
  )

  for (threshold in names(threshold_cols)) {
    if (threshold %in% thresholds) {
      plot_obj <- plot_obj +
        ggplot2::geom_hline(
          yintercept = as.numeric(threshold),
          color = threshold_cols[[threshold]],
          linetype = "dashed"
        )
    }
  }

  plot_obj
}

water_temp_build_plot_bundle <- function(
  ts_data,
  highlight_range,
  thresholds,
  language_abbrev
) {
  report_language <- lengthenLanguage(language_abbrev)
  if (is.null(ts_data) || nrow(ts_data) == 0) {
    stop(
      tr("water_temp_no_data", report_language),
      call. = FALSE
    )
  }

  highlight_range <- as.Date(highlight_range)
  if (length(highlight_range) != 2 || any(is.na(highlight_range))) {
    stop(
      tr("water_temp_highlight_range_invalid", report_language),
      call. = FALSE
    )
  }

  site_col <- if (
    identical(language_abbrev, "fr") &&
      "name_fr" %in% names(ts_data)
  ) {
    "name_fr"
  } else {
    "name"
  }

  display_name <- as.character(ts_data[[site_col]])
  if ("name" %in% names(ts_data)) {
    fallback_name <- as.character(ts_data[["name"]])
    display_name[is.na(display_name) | !nzchar(display_name)] <-
      fallback_name[is.na(display_name) | !nzchar(display_name)]
  }

  ts_data <- ts_data |>
    dplyr::mutate(
      datetime = as.POSIXct(datetime, tz = "UTC"),
      value_corrected = as.numeric(value_corrected),
      name = display_name
    )

  daily_stats <- ts_data |>
    dplyr::mutate(date = as.Date(datetime)) |>
    dplyr::group_by(date, name) |>
    dplyr::summarise(
      daily_mean = safe_mean(value_corrected),
      daily_max = safe_max(value_corrected),
      daily_min = safe_min(value_corrected),
      .groups = "drop"
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      fake_date = as.Date(
        paste0(
          "1996-",
          lubridate::month(date),
          "-",
          lubridate::day(date)
        ),
        "%Y-%m-%d"
      ),
      seven_day_max_avg = zoo::rollmean(
        daily_max,
        7,
        align = "right",
        na.pad = TRUE
      )
    )

  site_name <- stats::na.omit(unique(as.character(daily_stats$name)))
  if (length(site_name) == 0) {
    site_name <- tr("water_temp_selected_site", report_language)
  } else {
    site_name <- site_name[[1]]
  }

  historic <- daily_stats |>
    dplyr::filter(
      date < highlight_range[[1]] | date > highlight_range[[2]]
    )
  historic_mean <- summarise_fake_date(historic, "daily_mean")
  historic_max <- summarise_fake_date(historic, "daily_max")
  historic_seven_day <- summarise_fake_date(historic, "seven_day_max_avg")

  highlight <- daily_stats |>
    dplyr::filter(
      date >= highlight_range[[1]] & date <= highlight_range[[2]]
    )
  highlight <- complete_fake_dates(highlight)

  daily_mean_plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = historic_mean,
      ggplot2::aes(x = fake_date, ymin = tmin, ymax = tmax),
      fill = "grey80"
    ) +
    ggplot2::geom_line(
      data = historic_mean,
      ggplot2::aes(x = fake_date, y = tmean),
      color = "darkgrey",
      linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = highlight,
      ggplot2::aes(x = fake_date, y = daily_mean),
      color = "blue",
      linewidth = 1.2
    ) +
    ggplot2::labs(
      title = paste0(
        site_name,
        "\n - ",
        tr("water_temp_daily_mean", report_language)
      ),
      x = tr("date", report_language),
      y = tr("temperature_c", report_language)
    ) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 13
      ),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 18),
      axis.line = ggplot2::element_line()
    )
  daily_mean_plot <- add_threshold_lines(daily_mean_plot, thresholds)

  daily_max_plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = historic_max,
      ggplot2::aes(x = fake_date, ymin = tmin, ymax = tmax),
      fill = "grey80"
    ) +
    ggplot2::geom_line(
      data = historic_max,
      ggplot2::aes(x = fake_date, y = tmean),
      color = "darkgrey",
      linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = highlight,
      ggplot2::aes(x = fake_date, y = daily_max),
      color = "blue",
      linewidth = 1.2
    ) +
    ggplot2::labs(
      title = paste0(
        site_name,
        "\n - ",
        tr("water_temp_daily_max", report_language)
      ),
      x = tr("date", report_language),
      y = tr("temperature_c", report_language)
    ) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 13
      ),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 18),
      axis.line = ggplot2::element_line()
    )
  daily_max_plot <- add_threshold_lines(daily_max_plot, thresholds)

  seven_day_max_plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = historic_seven_day,
      ggplot2::aes(x = fake_date, ymin = tmin, ymax = tmax),
      fill = "grey80"
    ) +
    ggplot2::geom_line(
      data = historic_seven_day,
      ggplot2::aes(x = fake_date, y = tmean),
      color = "darkgrey",
      linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = highlight,
      ggplot2::aes(x = fake_date, y = seven_day_max_avg),
      color = "blue",
      linewidth = 1.2
    ) +
    ggplot2::labs(
      title = paste0(
        site_name,
        "\n - ",
        tr("water_temp_seven_day_max", report_language)
      ),
      x = tr("date", report_language),
      y = tr("temperature_c", report_language)
    ) +
    ggplot2::scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 13
      ),
      axis.text.y = ggplot2::element_text(size = 13),
      axis.title = ggplot2::element_text(size = 16),
      plot.title = ggplot2::element_text(size = 18),
      axis.line = ggplot2::element_line()
    )
  seven_day_max_plot <- add_threshold_lines(seven_day_max_plot, thresholds)

  list(
    raw_ts = ts_data,
    daily_stats = daily_stats,
    site_name = site_name,
    language = language_abbrev,
    highlight_range = highlight_range,
    daily_mean_plot = daily_mean_plot,
    daily_max_plot = daily_max_plot,
    seven_day_max_plot = seven_day_max_plot
  )
}

