######################## PACKAGES

library(janitor)    # clean column names
library(dplyr)      # data wrangling
library(stringr)    # string manipulation
library(countrycode) # ISO3 country code standardization
library(forcats)    # factor recoding and ordering




######################## READ AND CLEAN NAMES AND STRIP PII

# read data
dei = read.csv("data/survey-raw.csv", stringsAsFactors = TRUE)

# clean names
dei = clean_names(dei)

# remove PII -- potential re-id of individuals
dei = dplyr::select(dei, -please_share_any_additional_comments_on_diversity_equity_and_or_inclusivity_inclusion_in_the_gccr)




######################## RENAME AND LABEL VARIABLES

# Date
dei = rename(dei, Date = timestamp)
dei$Date = as.character(dei$Date)
dei$Date = as.POSIXct(dei$Date, format = "%m/%d/%Y", tz = "EST")

# Structural variables
dei = rename(dei, Structural_Country = in_which_country_do_you_engage_in_your_primary_professional_activity)

dei = rename(dei, Structural_Institution = what_type_of_institution_are_you_affiliated_with_please_choose_the_closest_option_from_the_list_and_if_you_work_at_more_than_one_please_choose_the_option_that_describes_your_primary_affiliation_or_where_most_of_your_work_gets_done)

# Demographic variables
dei = rename(dei, Demographic_Role = are_you_a_member_of_a_gccr_committee_this_includes_members_of_the_leadership_team)

dei = rename(dei, Demographic_PrimaryField = what_is_your_primary_field_of_research_or_practice_please_choose_the_one_that_best_describes_your_research_or_practice)

dei = rename(dei, Demographic_CareerStage = how_long_have_you_been_involved_in_research_practice_or_creative_activity)

dei = rename(dei, Demographic_Language = is_english_your_first_language)

# Participation variables
dei = rename(dei, Participation_Formed_Collaboration = what_have_you_found_to_be_the_single_best_way_to_form_or_join_a_new_collaboration_on_slack_via_the_gccr)

dei = rename(dei, Participation_Interaction = the_primary_people_i_interact_with_not_on_a_project_are)

dei = rename(dei, Participation_Collaboration = the_primary_people_i_collaborate_with_on_a_project_other_than_gccr_wide_papers_are)

dei = rename(dei, Participation_Consortium = how_do_you_participate_in_the_gccr)

dei = rename(dei, Participation_Information_Source = what_is_your_main_source_of_gccr_information_and_activities)

# Barriers
dei = rename(dei, Barriers_Experience = if_you_are_experiencing_barriers_to_participation_in_the_gccr_which_of_the_following_apply)

dei = rename(dei, Barriers_Observe = if_you_have_observed_others_experiencing_barriers_to_participation_in_the_gccr_which_of_the_following_apply)




######################## CHECK FOR DUPLICATE RESPONSES

# count rows
print(paste("Rows:", nrow(dei)))

# screened for duplicates -- email not collected for privacy so duplicates were possible
dupes = dei |>
    group_by(across(-Date)) |>
    filter(n() > 1) |>
    ungroup()
print(paste("Duplicate pairs found:", nrow(dupes) / 2))

# retain one row from each duplicate pair
dei = dei |>
    distinct(across(-Date), .keep_all = TRUE)
print(paste("Rows after deduplication:", nrow(dei)))




######################## BARRIERS BINARY AND COUNT

# binary: barrier present or absent
target_string = "I am not experiencing barriers to participation."
dei$Barriers_Experience_Binary = ifelse(grepl(target_string, dei$Barriers_Experience, fixed = TRUE),
                                        "No",
                                        "Yes")

target_string2 = "I have not observed others experiencing barriers to participation."
dei$Barriers_Observe_Binary = ifelse(grepl(target_string2, dei$Barriers_Observe, fixed = TRUE),
                                     "No",
                                     "Yes")

# count: number of barriers selected
dei$Barriers_Experience_Count = sapply(dei$Barriers_Experience, function(x) {
    if (is.na(x) || x == "") return(0)
    if (grepl(target_string, x, fixed = TRUE)) return(0)
    parts = unlist(strsplit(as.character(x), ","))
    return(length(unique(trimws(parts))))
})

dei$Barriers_Observe_Count = sapply(dei$Barriers_Observe, function(x) {
    if (is.na(x) || x == "") return(0)
    if (grepl(target_string2, x, fixed = TRUE)) return(0)
    parts = unlist(strsplit(as.character(x), ","))
    return(length(unique(trimws(parts))))
})

rm(target_string, target_string2)




######################## COUNTRY

# manual data entry cleaning
dei = dei |>
    mutate(Structural_Country = case_when(
        str_detect(Structural_Country, "Italy and US") ~ "Italy",
        str_detect(Structural_Country, "Congo") ~ "Congo, Democratic Republic of the",
        str_detect(Structural_Country, "Francr") ~ "France",
        TRUE ~ as.character(Structural_Country)
    ))

# ISO3 codes
dei = dei |>
    mutate(Map_Country_ISO3 = countrycode(Structural_Country,
                                          origin = 'country.name',
                                          destination = 'iso3c'))

# standard map names
dei = dei |>
    mutate(Map_Country_Standard = countrycode(Map_Country_ISO3,
                                              origin = 'iso3c',
                                              destination = 'country.name'))




######################## RECODE FACTORS

# reorder columns alphabetically
dei = dei |> select(order(colnames(dei)))

# career stage collapsed to three groups
dei$Demographic_CareerStage = fct_collapse(dei$Demographic_CareerStage,
                                           "Early"  = c("0-4 years", "5-9 years"),
                                           "Middle" = "10-14 years",
                                           "Late"   = "15+ years"
)

# role recoded from committee membership question
dei$Demographic_Role = fct_recode(dei$Demographic_Role,
                                  "Member" = "No",
                                  "Leader" = "Yes"
)

# information source collapsed -- small sample sizes for non-Slack/Newsletter sources
dei = dei |>
    mutate(Participation_Information_Source = case_when(
        str_detect(Participation_Information_Source, "Slack") ~ "Slack",
        str_detect(Participation_Information_Source, "Newsletter") ~ "Newsletter",
        TRUE ~ "Other"
    )) |>
    mutate(Participation_Information_Source = as.factor(Participation_Information_Source))

# interaction and collaboration recoded
dei = dei |> mutate(across(c(Participation_Interaction, Participation_Collaboration), ~fct_recode(.x,
                                                                                                  "Network"     = "people I did know before becoming a GCCR member.",
                                                                                                  "New_Unknown" = "people I did not know before becoming a GCCR member.",
                                                                                                  "New_Known"   = "people I knew about before becoming a GCCR member (but did not talk to)."
)))

# collaboration formation collapsed to project-based categories
dei = dei |> mutate(Participation_Formed_Collaboration = fct_recode(Participation_Formed_Collaboration,
                                                                    "Project"          = "Being a project member on a new or existing GCCR data study",
                                                                    "Consortium_Paper" = "Contributing to GCCR-wide papers",
                                                                    "Messaged"         = "Direct contact via messaging",
                                                                    "Solicited"        = "Direct solicitation for specific expertise",
                                                                    "Unable_To_Form"   = "I have not found a way to form or join a new collaboration on Slack",
                                                                    "Project"          = "Meeting on a project idea page"
))

# barriers -- shorten response labels for analysis
dei = dei |>
    mutate(
        Barriers_Experience = str_replace(Barriers_Experience, 'A barrier not listed \\(if wished, you can provide the barrier in the comments at the end\\)', 'Not_listed'),
        Barriers_Experience = str_replace(Barriers_Experience, 'Time to participate', 'Time'),
        Barriers_Experience = str_replace(Barriers_Experience, 'I am not experiencing barriers to participation\\.', 'None'),
        Barriers_Experience = str_replace(Barriers_Experience, 'Disability \\(mental and\\/or physical\\)', 'Disability'),
        Barriers_Experience = str_replace(Barriers_Experience, 'Time zone differences', 'Time_Zone'),

        Barriers_Observe = str_replace(Barriers_Observe, 'I have not observed others experiencing barriers to participation\\.', 'None_observed'),
        Barriers_Observe = str_replace(Barriers_Observe, 'A barrier not listed \\(if wished, you can provide the barrier in the comments at the end\\)', 'Not_listed'),
        Barriers_Observe = str_replace(Barriers_Observe, 'Time to participate', 'Time'),
        Barriers_Observe = str_replace(Barriers_Observe, 'Disability \\(mental and\\/or physical\\)', 'Disability'),
        Barriers_Observe = str_replace(Barriers_Observe, 'Time zone differences', 'Time_Zone'),

        # remove implausible answer (no barrier but list time as a barrier) n=4
        Barriers_Experience = str_replace(Barriers_Experience, 'None\\, Time', 'Time')
    )

# consortium participation collapsed
dei = dei |> mutate(Participation_Consortium = str_replace(Participation_Consortium, 'Active participation \\(e\\.g\\.\\, GCCR committee or Leadership Team\\, research collaboration due to being a GCCR member\\)', 'Active'))

dei = dei |> mutate(Participation_Consortium = str_replace(Participation_Consortium, 'Passive participation \\(e\\.g\\.\\, checking Slack daily\\/weekly or reading the newsletter\\)', 'Passive'))

dei = dei |> mutate(Participation_Consortium = str_replace(Participation_Consortium, 'Passive\\, Active', 'Both'))

# language recoded
dei$Demographic_Language = fct_recode(dei$Demographic_Language,
                                      "Other" = "Bilingual or multilingual from birth/early childhood",
                                      "EFL"   = "Yes",
                                      "ESL"   = "No"
)

# primary field -- excluded from analysis due to small sample sizes but retained for reference
dei = dei |> mutate(Demographic_PrimaryField = case_match(Demographic_PrimaryField,
                                                          "Creative Activity (e.g., writing, art or cooking)" ~ "Creative",
                                                          "Innovation and/or Research and Development" ~ "Innovation",
                                                          "Life Sciences (including biochemistry, genetics/genomics)" ~ "LifeSciences",
                                                          "Social and Perceptual Sciences (including human behavior and culture)" ~ "SocialSciences",
                                                          .default = Demographic_PrimaryField))

# institution
dei = dei |> mutate(Structural_Institution = case_match(Structural_Institution,
                                                        "Academic Institution" ~ "Academic",
                                                        "Granting organization or Funding Agency" ~ "Funding",
                                                        "Industry/For profit" ~ "Enterprise",
                                                        "Medical/Clinical" ~ "Medical",
                                                        "Non-Academic Institution (e.g., not degree granting) or Research Institution" ~ "ResearchOnly",
                                                        "Non-profit organization" ~ "Nonprofit",
                                                        "Patient Organization" ~ "Patient",
                                                        "Self-employed or freelance" ~ "Freelance",
                                                        .default = Structural_Institution))




######################## SAVE DATA

# full clean dataset with country mapping
write.csv(dei, "data/survey-clean-map.csv", row.names = FALSE)

# crosstabs file -- remove geography, barriers, and primary field
dei_xtabs = dplyr::select(dei, -Date, -Map_Country_Standard, -Map_Country_ISO3, -Structural_Country, -Barriers_Observe, -Barriers_Experience, -Demographic_PrimaryField)
write.csv(dei_xtabs, "data/survey-clean-xtabs.csv", row.names = FALSE)

# barriers file
dei_barriers = dplyr::select(dei, Demographic_CareerStage, Demographic_Role, Demographic_Language, Barriers_Experience, Barriers_Experience_Binary, Barriers_Experience_Count, Barriers_Observe, Barriers_Observe_Binary, Barriers_Observe_Count)
write.csv(dei_barriers, "data/survey-clean-barriers.csv", row.names = FALSE)




######################## TIDY
rm(list = ls())
gc()