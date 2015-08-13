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
  ggvis(~day, ~total, fill = ~category, fillOpacity := 0.25) %>%
  layer_bars(stack = FALSE) %>%
  scale_nominal("fill",
                domain = c("actual", "expected"),
                range = c("darkred", "lightgray")) %>%
  add_axis("x", title = "",
           properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
  add_axis("y", title = "Submitted Surveys")

# Testing side by side bar chart
totals %>%
  mutate(day_category = factor(paste(day, category))) %>%
  ggvis(x= ~day_category, y= ~total, fill = ~category) %>%
  layer_bars(stack = FALSE)  %>%
  add_axis("x", title = "",
           properties = axis_props(labels = list(angle = 45, align = "left"))) %>%
  add_axis("y", title = "Submitted Surveys")
