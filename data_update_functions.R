library(feather)


# replace this with the updated list of surveys
survey_list <- c("102 Petty France April 19",
                 "102 Petty France March 19",
                 "102 Petty France v2.3b",
                 "10 South Colonnade",
                 "5WP; Leeds",
                 "HMPPS in Southern House v2.0",
                 "Home Office; Southern House",
                 "Home Office; Apollo House",
                 "Home Office; Croydon",
                 "Victory House; OSPT",
                 "Edward House; APVU")

update_selected_survey_list <- function(survey_list) {

my_df <- data.frame(surveyname = survey_list)
feather::write_feather(my_df, "active surveys.feather")
s3tools::write_file_to_s3("active surveys.feather", "alpha-app-occupeye-automation/active surveys.feather", overwrite = TRUE)
}

update_selected_survey_list(survey_list)
