library(devtools)
library(pilr.api.r)
options(pilr_server_default = "http://qa.pilrhealth.com")
options(pilr_project_default = "ema_development_1")
options(pilr_default_access_code = "4557a4ca-b783-4ed4-8e57-14fd1465205d")

data <- list(survey = read_pilr(data_set = "pilrhealth:mobile:survey_data", schema = "1",
                                query_params = list(group = "template_assignments_enrolled")))

