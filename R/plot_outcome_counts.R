#' Produce a cumulative plot of study events
#'
#' An appropriate analysis must take into account the amount of data available
#' for analysis. For continuous outcomes, this may involve the number of
#' individuals with baseline covariates and primary outcomes, while binary and
#' time to events may additionally depend on the number of observed events.
#' \code{plot_outcome_counts} provides a way to visualize the amount of data
#' available for analysis.
#'
#' @param prepared_data A prepeared dataset: see
#' [impart::prepare_monitored_study_data]
#' @param study_time A \code{numeric} scalar indicating the study time at which
#' the data should be reconstructed and events should be counted
#' @param type A \code{character} scalar containing "t" for the total number of
#' observations, "c" for the number of complete observations, and "e" for the
#' number of events (for time-to-event or binary outcomes)
#' @param count_increment Plots have horizontal lines at regular increments to
#' assist in reading of counts. \code{count_increment} is a \code{numeric}
#' scalar indicating the increment between horizontal lines on the count axis.
#' @param time_increment Plots have horizontal lines at regular increments to
#' assist in reading of counts. \code{time_increment} is a \code{numeric}
#' scalar indicating the increment between vertical lines on the time axis.
#' @param color_palette A vector of colors for each event.
#' @param legend_placement Location of the plot legend. See \code{?legend}
#'
#' @return NULL
#' @export
#'
#' @examples
#' # To be added

plot_outcome_counts <-
  function(
    prepared_data,
    study_time = NULL,
    type = "tc",
    count_increment = 10,
    time_increment = 30,
    color_palette = NULL,
    legend_placement = "topleft"
  ) {

    outcome_counts <-
      count_outcomes(
        prepared_data = prepared_data,
        study_time = study_time
      )

    time_to_event <- prepared_data$time_to_event

    type <- tolower(type)
    plot_total <- length(grep(pattern = "t", x =  type)) > 0
    plot_complete <- length(grep(pattern = "c", x =  type)) > 0
    plot_events <- length(grep(pattern = "e", x =  type)) > 0

    plot_elements <- plot_total + plot_complete + plot_events
    if(plot_elements < 1){
      stop("`type` must contain at least one of the following characters: ",
           "'t' (total observations), 'c' (complete observations), or 'e' ",
           "(event count).")
    }


    if(plot_events){
      if(!("count_events" %in% names(outcome_counts))){
        stop("`type` contains 'e' (event counts), which is for time-to-event and ",
             "binary outcomes, but `count_outcomes()` did not detect a ",
             "time-to-event or binary outcome.")
      }
    }

    events <- levels(outcome_counts$event)

    if(length(color_palette) < length(events)){
      if(!is.null(color_palette)){
        warning("Length of `color_palette (", length(color_palette), ") must be ",
                "at least ", length(events), ".")
      }
      color_palette <- 1:length(events)
    }

    y_max <-
      if(plot_total){
        max(outcome_counts$count_total, na.rm = TRUE)
      } else if(plot_complete){
        max(outcome_counts$count_complete, na.rm = TRUE)
      } else if(plot_events){
        max(outcome_counts$count_events, na.rm = TRUE)
      }

    graphics::par(mar = c(5, 4, 4, 4) + 0.25)

    base::plot(
      x = NULL,
      y = NULL,
      xlim = range(outcome_counts$time),
      ylim = c(0, y_max),
      xlab = "Study Time",
      ylab = "Count",
      las = 1
    )

    graphics::abline(
      h = seq(from = 0, to = y_max, by = count_increment),
      col = grDevices::gray(level = 0, alpha = 0.125)
    )

    graphics::abline(
      v = seq(from = 0, to = max(outcome_counts$time), by = time_increment),
      col = grDevices::gray(level = 0, alpha = 0.125)
    )

    for(i in 1:length(events)){
      plot_i_data <-
        outcome_counts[which(outcome_counts$event == events[i]),]

      if(plot_total){
        if(!time_to_event){
          graphics::lines(
            count_total ~ time,
            data = plot_i_data[order(plot_i_data$count_total),],
            col = i,
            type = "S"
          )
        }
      }

      if(plot_complete){
        graphics::lines(
          count_complete ~ time,
          data = plot_i_data[order(plot_i_data$count_complete),],
          col = i,
          lty = 2,
          type = "S"
        )
      }

      if(plot_events){
        graphics::lines(
          count_events ~ time,
          data = plot_i_data[order(plot_i_data$count_events),],
          col = i,
          lty = 3,
          type = "S"
        )
      }
    }

    graphics::axis(side = 4, las = 1)

    legend_text <- legend_lty <- NULL

    if(plot_complete) {
      legend_text <- c(legend_text, "Complete")
      legend_lty <- c(legend_lty, 2)
    }
    if(plot_total) {
      legend_text <-
        c(legend_text, "Total")
      legend_lty <- c(legend_lty, 1)
    }
    if(plot_events) {
      legend_text <- c(legend_text, "Events")
      legend_lty <- c(legend_lty, 3)
    }

    graphics::legend(
      x = "bottomright",
      legend = legend_text,
      lty = legend_lty,
      inset = c(0, 1),
      xpd = TRUE,
      horiz = TRUE,
      bty = "n"
    )

    events[1] <- "RND"

    graphics::legend(
      x = legend_placement,
      legend = events,
      col = color_palette,
      lty = 1,
      lwd = 1.75
    )

  }
