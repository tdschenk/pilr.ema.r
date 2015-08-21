library(devtools)
library(pilr.api.r)
options(pilr_server_default = "http://qa.pilrhealth.com")
options(pilr_project_default = "ema_development_1")
options(pilr_default_access_code = "4557a4ca-b783-4ed4-8e57-14fd1465205d")

data <- list(survey = read_pilr(data_set = "pilrhealth:mobile:survey_data", schema = "1",
                                query_params = list(group = "template_assignments_enrolled")))
params <- ""

cohort_activity_heatmap(data, params)


# Testing overlapping bar chart
totals %>%
  ggvis(~day, ~total, fillOpacity := 0.25) %>%
  layer_bars(stack = FALSE) %>%
  layer_bars(data = test, x = ~day, y = ~total)
  scale_nominal("fill",
                domain = c("actual", "expected"),
                range = c("darkred", "lightgray")) %>%
  add_axis("x", title = "",
           properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
  add_axis("y", title = "Submitted Surveys")

test <- data.frame(x = c(1,2,3), y1 = c(5,6,7), y2 = c(10,11,12))

test %>%
  ggvis(~x, ~y1, fillOpacity := 1) %>%
  layer_bars() %>%
  layer_bars(~x, ~y2, fillOpacity := 0.5)




# Testing side by side bar chart
totals %>%
  mutate(day_category = factor(paste(day, category))) %>%
  ggvis(x= ~day_category, y= ~total, fill = ~category) %>%
  layer_bars(stack = FALSE)  %>%
  add_axis("x", title = "",
           properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
  add_axis("y", title = "Submitted Surveys")

palette <- c("#FF0000", Yellow(10), Green(10))
pos.cut.values <- seq(0, max(totals$total, na.rm = TRUE) + 0.1, length.out = 11)
neg.cut.values <- seq(min(totals$total - 0.1, na.rm = TRUE), 0, length.out = 11)
legend.values <- c(paste(neg.cut.values[1:10], '..', neg.cut.values[2:11]),
                   'NA', paste(pos.cut.values[1:10], '..', pos.cut.values[2:11]))
totals %>%
  ggvis(~day, ~pt, fill:=~daycolor) %>%
  scale_ordinal('fill', range = heatcolors) %>%
  add_legend(scales = 'fill', values = legend.values) %>%
  layer_rects(width = band(), height = band()) %>%
  scale_nominal("x", padding = 0, points = FALSE) %>%
  scale_nominal("y", padding = 0, points = FALSE) %>%
  layer_text(text := ~defense, stroke := "white", align := "left",
             baseline := "top")

# Create fill color spectrum based on job settings
heatcolors <- colorRampPalette(c("#FFCC00","#006600"))

# Set all 0 values to black
totals$daycolor[totals$total == 0] = "#FF0000"

# Set other values to appropriate colors from red to green
totals$daycolor[totals$total > 0] =
  as.character(cut(totals$total[totals$total > 0],
                   seq(0, max(totals$total, na.rm=TRUE)+0.1, length.out=11),
                   labels=heatcolors(10)))


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
  add_axis("y", title = "Participant")
