#' Predict the Top Days and Times to Spot a Wildlife Organism
#'
#' Takes occurrence and weather data from the ecotourism package and returns
#' the top five recommended calendar periods and times of day to spot a given
#' organism, based on either a weighted scoring model or a Poisson GLM.
#'
#' The organism name is derived automatically from the name of the object
#' passed to `occurrence` — no separate label argument needed.
#'
#' Records where hour == 0 are excluded from time-of-day analysis, as these
#' almost certainly represent missing time data that defaulted to midnight
#' rather than genuine midnight sightings.
#'
#' @param occurrence A data frame of occurrence records from the ecotourism
#'   package (e.g. manta_rays, gouldian_finch). Must contain columns:
#'   ws_id, date, month, hour, weekday, record_type.
#' @param weather A data frame of daily weather observations from the
#'   ecotourism package. Must contain columns: ws_id, date, temp, prcp.
#' @param temp_range Numeric vector of length 2. The optimal temperature
#'   range in Celsius. Used only when method = "score". Defaults to c(15, 35).
#' @param method Character. Either "score" (default) or "glm".
#'   "score" uses a weighted composite of sighting frequency and weather
#'   favourability - works reliably on all organisms including those with
#'   sparse weather coverage.
#'   "glm" fits a Poisson GLM with weather variables as predictors and ranks
#'   combinations by predicted sighting count - requires sufficient matched
#'   weather records to converge.
#' @param top_n Integer. Number of top results to return. Defaults to 5.
#'
#' @return A named list with:
#'   \item{best_days}{Top top_n month + weekday combinations.}
#'   \item{best_times}{Top top_n hours of day.}
#'   \item{scores}{Full scored or predicted data frames for exploration.}
#'
#' @examples
#' library(ecotourism)
#' data(manta_rays,     package = "ecotourism")
#' data(gouldian_finch, package = "ecotourism")
#' data(glowworms,      package = "ecotourism")
#' data(weather,        package = "ecotourism")
#'
#' # Default: weighted scoring
#' predict_best_sighting_times(gouldian_finch, weather, temp_range = c(18, 30))
#'
#' # Poisson GLM on gouldian finch (sufficient weather coverage)
#' predict_best_sighting_times(gouldian_finch, weather,
#'                             temp_range = c(18, 30),
#'                             method = "glm")
#'
predict_best_sighting_times <- function(occurrence,
                                        weather,
                                        temp_range = c(15, 35),
                                        method     = "score",
                                        top_n      = 5) {
  # Organism name cleaning from occurrence
  raw_name      <- deparse(substitute(occurrence))
  organism_name <- paste(
    toupper(substring(gsub("_", " ", raw_name), 1, 1)),
    substring(gsub("_", " ", raw_name), 2),
    sep = ""
  )

  # Input validation
  required_occ <- c("ws_id", "date", "month", "hour", "weekday", "record_type")
  required_wth <- c("ws_id", "date", "temp", "prcp")

  missing_occ <- setdiff(required_occ, names(occurrence))
  missing_wth <- setdiff(required_wth, names(weather))

  if (length(missing_occ) > 0)
    stop("occurrence is missing columns: ", paste(missing_occ, collapse = ", "))
  if (length(missing_wth) > 0)
    stop("weather is missing columns: ", paste(missing_wth, collapse = ", "))
  if (!method %in% c("score", "glm"))
    stop('method must be either "score" or "glm"')
  if (length(temp_range) != 2 || temp_range[1] >= temp_range[2])
    stop("temp_range must be a numeric vector of length 2 with temp_range[1] < temp_range[2]")

  month_names <- c("January", "February", "March", "April", "May", "June",
                   "July", "August", "September", "October", "November", "December")

  # Filter hour == 0 for time-of-day analysis (most likely means time was not recorded)
  n_zero_hour <- sum(occurrence$hour == 0, na.rm = TRUE)
  pct_zero    <- round(100 * n_zero_hour / nrow(occurrence), 1)
  occ_timed   <- occurrence[!is.na(occurrence$hour) & occurrence$hour != 0, ]

  if (nrow(occ_timed) < 10)
    warning("Fewer than 10 records with valid time data after removing hour == 0. ",
            "Time-of-day results may not be reliable.")

  # Select only the weather columns needed before joining.
  wth_slim <- weather[, intersect(
    c("ws_id", "date", "temp", "max", "min", "prcp", "rainy", "wind_speed", "dewp"),
    names(weather)
  )]

  # Join occurrence with weather
  joined_full  <- merge(occurrence, wth_slim, by = c("ws_id", "date"), all.x = TRUE)
  joined_timed <- merge(occ_timed,  wth_slim, by = c("ws_id", "date"), all.x = TRUE)

  # Branch on method
  if (method == "score") {

    # Weather favourability score per row (0 to 1)
    # temp_score: triangular decay centred on midpoint of temp_range
    # prcp_score: exponential decay, half score at ~5.5mm/day
    score_weather <- function(df) {
      temp_mid   <- mean(temp_range)
      temp_width <- diff(temp_range) / 2
      df$temp_score <- ifelse(is.na(df$temp), 0.5,
                        pmax(0, 1 - abs(df$temp - temp_mid) / temp_width))
      df$prcp_score <- ifelse(is.na(df$prcp), 0.5,
                        exp(-df$prcp / 8))
      df$weather_score <- 0.5 * df$temp_score + 0.5 * df$prcp_score
      df
    }

    joined_full  <- score_weather(joined_full)
    joined_timed <- score_weather(joined_timed)

    # Seasonal scoring: aggregate by month x weekday
    seasonal <- aggregate(
      list(sighting_count = rep(1, nrow(joined_full)),
           weather_score  = joined_full$weather_score),
      by  = list(month = joined_full$month, weekday = joined_full$weekday),
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    seasonal$freq_score         <- seasonal$sighting_count /
                                   max(seasonal$sighting_count, na.rm = TRUE)
    seasonal$weather_score_norm <- seasonal$weather_score /
                                   max(seasonal$weather_score, na.rm = TRUE)
    seasonal$composite_score    <- 0.6 * seasonal$freq_score +
                                   0.4 * seasonal$weather_score_norm
    seasonal$month_name         <- month_names[seasonal$month]
    seasonal                    <- seasonal[order(seasonal$composite_score,
                                                  decreasing = TRUE), ]
    best_days                   <- head(seasonal, top_n)
    rownames(best_days)         <- NULL

    # Time-of-day scoring: aggregate by hour
    hourly <- aggregate(
      list(sighting_count = rep(1, nrow(joined_timed)),
           weather_score  = joined_timed$weather_score),
      by  = list(hour = joined_timed$hour),
      FUN = function(x) sum(x, na.rm = TRUE)
    )
    hourly$freq_score         <- hourly$sighting_count /
                                 max(hourly$sighting_count, na.rm = TRUE)
    hourly$weather_score_norm <- hourly$weather_score /
                                 max(hourly$weather_score, na.rm = TRUE)
    hourly$composite_score    <- 0.6 * hourly$freq_score +
                                 0.4 * hourly$weather_score_norm
    hourly$time_label         <- sprintf("%02d:00", hourly$hour)
    hourly$period             <- ifelse(hourly$hour < 12, "Morning",
                                 ifelse(hourly$hour < 17, "Afternoon", "Evening"))
    hourly                    <- hourly[order(hourly$composite_score,
                                             decreasing = TRUE), ]
    best_times                <- head(hourly, top_n)
    rownames(best_times)      <- NULL

    scores <- list(seasonal = seasonal, hourly = hourly)

  } else {

    # Poisson GLM method, appropriate because sighting counts are non-negative integers.
    glm_vars <- intersect(c("date", "ws_id", "hour", "month",
                             "temp", "prcp", "wind_speed", "dewp"),
                           names(joined_timed))
    glm_data <- joined_timed[, glm_vars]
    glm_data <- glm_data[stats::complete.cases(glm_data), ]

    if (nrow(glm_data) < 50)
      stop("Insufficient matched weather records to fit a Poisson GLM (",
           nrow(glm_data), " complete rows). Try method = 'score' instead.")

    # Aggregate to one row per unique date-hour-weather combination
    count_data <- aggregate(
      list(n_sightings = rep(1, nrow(glm_data))),
      by  = glm_data[, setdiff(glm_vars, c("date", "ws_id"))],
      FUN = sum
    )

    # Fit the model and generate predictions
    predictors  <- intersect(c("temp", "prcp", "wind_speed", "dewp",
                                "month", "hour"), names(count_data))
    formula_str <- paste("n_sightings ~", paste(predictors, collapse = " + "))
    model       <- stats::glm(stats::as.formula(formula_str),
                              data   = count_data,
                              family = stats::poisson(link = "log"))

    count_data$predicted  <- stats::predict(model, type = "response")
    count_data$month_name <- month_names[count_data$month]
    count_data$time_label <- sprintf("%02d:00", count_data$hour)
    count_data$period     <- ifelse(count_data$hour < 12, "Morning",
                             ifelse(count_data$hour < 17, "Afternoon", "Evening"))
    count_data            <- count_data[order(count_data$predicted,
                                             decreasing = TRUE), ]

    # Seasonal: sum predicted scores per month (not mean)
    seasonal_glm <- aggregate(
      list(predicted   = count_data$predicted * count_data$n_sightings,
           n_sightings = count_data$n_sightings),
      by  = list(month      = count_data$month,
                 month_name = count_data$month_name),
      FUN = function(x) round(sum(x, na.rm = TRUE), 2)
    )
    seasonal_glm <- seasonal_glm[order(seasonal_glm$predicted, decreasing = TRUE), ]
    best_days    <- head(seasonal_glm, top_n)
    rownames(best_days) <- NULL

    # Hourly: sum predicted scores per hour (frequency-weighted)
    hourly_glm <- aggregate(
      list(predicted   = count_data$predicted * count_data$n_sightings,
           n_sightings = count_data$n_sightings),
      by  = list(hour       = count_data$hour,
                 time_label = count_data$time_label,
                 period     = count_data$period),
      FUN = function(x) round(sum(x, na.rm = TRUE), 2)
    )
    hourly_glm <- hourly_glm[order(hourly_glm$predicted, decreasing = TRUE), ]
    best_times <- head(hourly_glm, top_n)
    rownames(best_times) <- NULL

    scores <- list(model = model, full_predictions = count_data)
  }

  # Print summary
  cat("\n")
  cat("  Best Times to Spot", organism_name, "\n")
  cat("  Method:", ifelse(method == "score",
      "Weighted scoring (frequency + weather)",
      "Poisson GLM (predicted counts)"), "\n")
  cat("\n")
  cat("\nTop", top_n, "month + day combinations:\n")
  cat(rep("-", 40), "\n", sep = "")
  for (i in seq_len(nrow(best_days))) {
    row <- best_days[i, ]
    if (method == "score") {
      cat(sprintf("  %d. %-10s on %-10s  (score: %.2f, sightings: %d)\n",
                  i, row$month_name, row$weekday,
                  row$composite_score, row$sighting_count))
    } else {
      cat(sprintf("  %d. %-10s  (predicted: %.1f, observed: %.0f)\n",
                  i, row$month_name, row$predicted, row$n_sightings))
    }
  }

  cat("\nTop", top_n, "times of day:\n")
  cat(rep("-", 40), "\n", sep = "")
  if (nrow(best_times) == 0) {
    cat("  No valid time data available.\n")
  } else {
    for (i in seq_len(nrow(best_times))) {
      row <- best_times[i, ]
      if (method == "score") {
        cat(sprintf("  %d. %s (%s)  (score: %.2f, sightings: %d)\n",
                    i, row$time_label, row$period,
                    row$composite_score, row$sighting_count))
      } else {
        cat(sprintf("  %d. %s (%s)  (predicted: %.1f, observed: %.0f)\n",
                    i, row$time_label, row$period,
                    row$predicted, row$n_sightings))
      }
    }
  }

  cat("\nNote:", n_zero_hour, "records (", pct_zero,
      "%) had hour == 0 and were excluded from time-of-day analysis.\n")
  machine_n <- sum(occurrence$record_type == "MACHINE_OBSERVATION", na.rm = TRUE)
  if (machine_n > 0)
    cat("Tip:", machine_n, "MACHINE_OBSERVATION records detected.",
        "Time-of-day results reflect human observer patterns only.\n")
  if (method == "score") {
    cat("\nScoring: composite = 0.6 x frequency + 0.4 x weather favourability\n")
    cat("Temperature optimal range:", temp_range[1], "to", temp_range[2], "C\n")
    cat("Rainfall penalty: exponential decay (half score at ~5.5mm/day)\n")
  }
  # Return 
  invisible(list(
    best_days  = best_days,
    best_times = best_times,
    scores     = scores
  ))
}