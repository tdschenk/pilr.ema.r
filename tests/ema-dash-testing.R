library(devtools)
library(pilr.api.r)
options(pilr_server_default = "http://qa.pilrhealth.com")
options(pilr_project_default = "ema_branch_testing")
options(pilr_default_access_code = "40c17cbc-0df2-497d-9fbd-83a8b92cf3e6")

data <- list(survey = read_pilr(data_set = "pilrhealth:mobile:survey_data", schema = "1",
                                query_params = list(group = "template_assignments_enrolled")))
params <- list(params = list(date = list(gt = "2015-06-19", lt = "2015-09-02")))

cohort_activity_heatmap(data, params)
surveys_leaflet_map(data, params)

# Add days with no surveys for any participants
totals$day <- as.Date(totals$day)
g <- seq(totals$day[1], totals$day[nrow(totals)], by = 1)
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

  zym <- zoo(1:5, as.yearmon("2000-01-01") + c(0, 1, 2, 4, 5)/12)
  g <- seq(start(zym), end(zym), by = 1/12)
  na.locf(zym, xout = g)

  zym <- zoo(1:5, as.Date("2000-01-01") + c(0, 1, 2, 4, 5))
  g <- seq(start(zym), end(zym), by = 1)
  na.locf(zym, xout = g)

  zym <- zoo(1:length(unique(totals$day)), as.Date())
  g <- seq(totals$day[1], totals$day[nrow(totals)], by = 1)

### Leaflet Stuff
  leaflet(test) %>% addTiles() %>% addMarkers(
    clusterOptions = markerClusterOptions()
  )
