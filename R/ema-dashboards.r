### Cohort Activity Heatmap ###
## Dataset mapped as data$survey
## (TODO) Takes in job settings (currently params$job_settings): num_colors, num_bins
#' @export
cohort_activity_heatmap <- function(data, params, ...) {
  # Bind data and metadata by column
  data$survey <- cbind(data$survey$data, data$survey$metadata)

  # Subset to only survey submissions
  data$survey <- subset(data$survey, event_type == "survey_submitted")

  # Add day column to dataframe
  data$survey$day <- substring(data$survey$timestamp, 0, 10)

  # Create fill color spectrum based on job settings
  heatcolors <- colorRampPalette(c("#FFCC00","#006600"))

  # Calculate surveys per day per participant
  totals <- data.frame(table(data$survey$pt, data$survey$day))
  names(totals) <- c("pt", "day", "total")

  # Set all 0 values to red
  totals$daycolor[totals$total == 0] = "#FF0000"

  totals$day <- as.Date(totals$day)
  if (length(params$params$date)) {
    g <- seq(as.Date(params$params$date$gt), as.Date(params$params$date$lt), by = 1)
  }
  else {
    g <- seq(totals$day[1], totals$day[nrow(totals)], by = 1)
  }
  for (i in 1:length(g)) {
    if (!nrow(totals[totals$day == g[i],])) {
      for (j in 1:length(unique(totals$pt))) {
        totals <- rbind(totals, data.frame(pt = unique(totals$pt)[j],
                                           day = g[i],
                                           total = 0,
                                           daycolor = "#FF0000"))
      }
    }
  }
  totals$day <- as.factor(totals$day)

  # If more than 30 days, subset to last 30
  if (length(unique(totals$day)) > 30) {
    include_days <- g[(length(g)-29):length(g)]
    totals <- totals[as.Date(totals$day) %in% include_days,]
  }

  # Set other values to appropriate colors from red to green
  totals$daycolor[totals$total > 0] =
    as.character(cut(totals$total[totals$total > 0],
                     seq(0, max(totals$total, na.rm=TRUE)+0.1, length.out=11),
                     labels=heatcolors(10)))

  totals$day <- as.character(totals$day)
  totals <- arrange(totals, day)
  # Create heatmap
  totals %>%
    ggvis(~day, ~pt, fill := ~daycolor) %>%
    layer_rects(height = band(), width = band()) %>%
    layer_text(
      x = prop("x", ~day, scale = "xcenter"),
      y = prop("y", ~pt, scale = "ycenter"),
      text:=~total, fontSize := 15, fill:="white", baseline:="middle", align:="center") %>%
    scale_numeric("fill", range=c("#FFCC00","#00CC00")) %>%
    scale_nominal("x", padding = 0, points = FALSE) %>%
    scale_nominal("y", padding = 0, points = FALSE) %>%
    scale_nominal("x", name = "xcenter", padding = 1, points = TRUE) %>%
    scale_nominal("y", name = "ycenter", padding = 1, points = TRUE) %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "Participant") %>%
    hide_legend("fill") %>% set_options(renderer=NULL)
}

### Actual vs Expected Per Day Bar Chart ###
## Expected calculated by maximum per day * total participants
#' @export
actual_expected_bar <- function(data, params, ...) {
  # Bind data and metadata by column
  data$survey <- cbind(data$survey$data, data$survey$metadata)

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

  # Add days with no surveys for any participants
  totals$day <- as.Date(totals$day)
  if (length(params$params$date)) {
    g <- seq(as.Date(params$params$date$gt), as.Date(params$params$date$lt), by = 1)
  }
  else {
    g <- seq(totals$day[1], totals$day[nrow(totals)], by = 1)
  }
  for (i in 1:length(g)) {
    if (!nrow(totals[totals$day == g[i],])) {
      totals <- rbind(totals, data.frame(day = g[i],
                                         actual = 0))
    }
  }

  # Add column for expected
  totals$expected <- expected

  # If more than 30 days, subset to last 30
  if (length(unique(totals$day)) > 30) {
    include_days <- g[(length(g)-29):length(g)]
    totals <- totals[as.Date(totals$day) %in% include_days,]
  }

  totals$day <- as.character(totals$day)
  totals <- arrange(totals, day)
  totals %>%
    ggvis(~day, ~expected, fill = "estimated", fillOpacity := 0.15) %>%
    layer_bars() %>%
    layer_bars(~day, ~actual, fillOpacity := 1, fill = "actual") %>%
    add_axis("x", title = "",
             properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
    add_axis("y", title = "Submitted Surveys")
}

### Leaflet Map Plot ###
##
#' @export
surveys_leaflet_map <- function(data, params, ...) {
  # Bind data and metadata by column
  #data$survey <- cbind(data$survey$data, data$survey$metadata)

  # Subset to only survey submissions
  data$survey <- subset(data$survey, event_type == "survey_submitted")

  # Rename columns to lat/lon for leaflet pkg
  data$survey <- rename(data$survey, c("more_lat" = "lat", "more_lon" = "lon"))

  # Generate map
  leaflet(data$survey) %>% addTiles() %>% addMarkers(
    clusterOptions = markerClusterOptions(),
    popup = ~htmlEscape(pt)
  )
}

surveys_leaflet_map_2 <- function(data, params, ...) {
  # Subset to only survey submissions
  data$survey <- subset(data$survey, event_type == "survey_submitted")

  # Rename columns to lat/lon for leaflet pkg
  data$survey <- rename(data$survey, c("more_lat" = "lat", "more_lon" = "lon"))

  # Generate map
  leaflet(data$survey) %>% addTiles() %>% addMarkers(
    clusterOptions = markerClusterOptions(),
    popup = ~htmlEscape(pt)
  )
}

ema_html_test <- function(data, params, ...) {
  data$survey <- cbind(data$survey$data, data$survey$metadata)
  data$survey <- subset(data$survey, event_type == "survey_submitted")

  pt_counts <- count(data$survey, 'pt')

  print(xtable(pt_counts), type = "html", comment = FALSE, include.rownames=FALSE)
}
