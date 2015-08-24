### Cohort Activity Heatmap ###
## Dataset mapped as data$survey
## (TODO) Takes in job settings (currently params$job_settings): num_colors, num_bins
#' @export
cohort_activity_heatmap <- function(data, params, ...) {
  # Subset to only survey submissions
  data$survey <- subset(data$survey, event_type == "survey_submitted")

  # Add day column to dataframe
  data$survey$day <- substring(data$survey$timestamp, 0, 10)

  # Create fill color spectrum based on job settings
  heatcolors <- colorRampPalette(c("#FFCC00","#006600"))

  # Calculate surveys per day per participant
  totals <- data.frame(table(data$survey$pt, data$survey$day))
  names(totals) <- c("pt", "day", "total")

  # Set all 0 values to black
  totals$daycolor[totals$total == 0] = "#FF0000"

  # Set other values to appropriate colors from red to green
  totals$daycolor[totals$total > 0] =
    as.character(cut(totals$total[totals$total > 0],
                     seq(0, max(totals$total, na.rm=TRUE)+0.1, length.out=11),
                     labels=heatcolors(10)))

  # Create heatmap
  totals %>%
    ggvis(~day, ~pt, fill := ~daycolor) %>%
    layer_rects(height = band(), width = band()) %>%
    layer_text(
      x = prop("x", ~day, scale = "xcenter"),
      y = prop("y", ~pt, scale = "ycenter"),
      text:=~total, fontSize := 20, fill:="white", baseline:="middle", align:="center") %>%
    scale_numeric("fill", range=c("#FFCC00","#00CC00")) %>%
    scale_nominal("x", padding = 0, points = FALSE) %>%
    scale_nominal("y", padding = 0, points = FALSE) %>%
    scale_nominal("x", name = "xcenter", padding = 1, points = TRUE) %>%
    scale_nominal("y", name = "ycenter", padding = 1, points = TRUE) %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "Participant") %>%
    hide_legend("fill")
}

### Actual vs Expected Per Day Bar Chart ###
## Expected calculated by maximum per day * total participants
#' @export
actual_expected_bar <- function(data, params, ...) {
  # Subset to only survey submissions
  data$survey <- subset(data$survey, event_type == "survey_submitted")

  # Add day column to dataframe
  data$survey$day <- substring(data$survey$timestamp, 0, 10)

  # Sum surveys per day per participant
  totals <- data.frame(table(data$survey$pt, data$survey$day))
  # Find maximum and calculate Expected
  expected <- max(totals$Freq) * length(unique(data$survey$pt))

  # Sum surveys per day over all participants
  totals <- data.frame(table(data$survey$day))
  names(totals) <- c("day", "actual")

  # Add column for expected
  totals$expected <- expected

  totals %>%
    ggvis(~day, ~expected, fill = "estimated", fillOpacity := 0.15) %>%
    layer_bars() %>%
    layer_bars(~day, ~actual, fillOpacity := 1, fill = "actual") %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "Submitted Surveys")
}
