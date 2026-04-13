######################## PACKAGES

library(dplyr)       # data wrangling
library(tidyr)       # separate_rows and pivot_wider
library(forcats)     # factor wrangling
library(FactoMineR)  # multiple correspondence analysis (MCA)
library(crosstable)  # contingency tables with statistical tests
library(cowplot)     # multi-panel figure composition
library(ggplot2)     # figures
library(openxlsx)    # save results to Excel




######################## READ DATA

dei_barriers = read.csv("data/survey-clean-barriers.csv", stringsAsFactors = TRUE)

# strip variable prefix for cleaner column names
dei_barriers = dei_barriers |> rename_with(~ sub("^[^_]+_", "", .x))




######################## SUBSET BY DEMOGRAPHIC

deiEqRole  = dplyr::select(dei_barriers, Role, Experience_Binary, Experience_Count, Observe_Binary, Observe_Count)

deiEqLang  = dplyr::select(dei_barriers, Language, Experience_Binary, Experience_Count, Observe_Binary, Observe_Count)
deiEqLang  = deiEqLang |>
    filter(Language != "Other") |>   # omit Other due to small sample size (n=7)
    mutate(Language = fct_drop(Language))

deiEqStage = dplyr::select(dei_barriers, CareerStage, Experience_Binary, Experience_Count, Observe_Binary, Observe_Count)




######################## CONTINGENCY TABLES

# tests determined by data type and sample size -- see crosstable documentation
# results used to select variables for MCA

roleBarrier     = crosstable(deiEqRole,  by = Role,        test = TRUE)
languageBarrier = crosstable(deiEqLang,  by = Language,    test = TRUE)
stageBarrier    = crosstable(deiEqStage, by = CareerStage, test = TRUE)

# save results -- ST1 in supplemental materials
wb = createWorkbook()
addWorksheet(wb, "Role")
writeData(wb, "Role", as.data.frame(roleBarrier))
addWorksheet(wb, "Language")
writeData(wb, "Language", as.data.frame(languageBarrier))
addWorksheet(wb, "CareerStage")
writeData(wb, "CareerStage", as.data.frame(stageBarrier))
saveWorkbook(wb, "outputs/st-survey-equity-crosstabs.xlsx", overwrite = TRUE)




######################## MCA

# role -- significant variables only (observe binary and count)
mca_eqRole = deiEqRole |>
    mutate(Observe_Count_Group = case_when(
        Observe_Count == 0  ~ "0",
        Observe_Count == 1  ~ "1",
        Observe_Count >= 2  ~ "2+"
    )) |>
    select(Role, Observe_Count_Group) |>
    mutate(across(everything(), as.factor))

eq_mca_role = MCA(mca_eqRole, quali.sup = 1, graph = FALSE)
c = plot(eq_mca_role, invisible = "ind", col.var = "#21918c", col.quali.sup = "#440154")

# career stage -- significant variables only
mca_eqStage = deiEqStage |>
    mutate(Observe_Count_Group = case_when(
        Observe_Count == 0  ~ "0",
        Observe_Count == 1  ~ "1",
        Observe_Count >= 2  ~ "2+"
    )) |>
    select(CareerStage, Observe_Count_Group) |>
    mutate(across(everything(), as.factor))

eq_mca_stage = MCA(mca_eqStage, quali.sup = 1, graph = FALSE)
d = plot(eq_mca_stage, invisible = "ind", col.var = "#21918c", col.quali.sup = "#440154")

# save MCA coordinates as artifacts
write.csv(as.data.frame(eq_mca_role$var$coord),  "artifacts/survey-mca-equity-role-coords.csv")
write.csv(as.data.frame(eq_mca_stage$var$coord), "artifacts/survey-mca-equity-stage-coords.csv")

# save MCA panel figure -- SF1 in supplemental materials
png("outputs/sf-survey-mca-equity.png", width = 8, height = 4, units = "in", res = 300)
plot_grid(c, d,
          nrow = 1,
          labels = c('A', 'B'),
          label_size = 14,
          label_x = 0,
          label_y = 0.5)
dev.off()




######################## DIVERGENCE DUMBBELL PLOT

dei_barriers_list = dplyr::select(dei_barriers, Role, Language, CareerStage, Experience, Observe)

# calculate proportions by group and barrier type
get_proportions = function(data, group_var, survey_col) {
    data |>
        select(all_of(group_var), all_of(survey_col)) |>
        separate_rows(!!sym(survey_col), sep = ",\\s*") |>
        filter(!!sym(survey_col) != "" & !is.na(!!sym(survey_col))) |>
        group_by(.data[[group_var]], !!sym(survey_col)) |>
        summarise(Count = n(), .groups = "drop_last") |>
        filter(Count >= 2) |>   # remove cells n<2 for anonymity
        mutate(Prop = round((Count / sum(Count)), 4)) |>
        select(-Count) |>
        arrange(.data[[group_var]], desc(Prop))
}

exp_prop_role  = get_proportions(dei_barriers_list, "Role",        "Experience")
exp_prop_lang  = get_proportions(dei_barriers_list, "Language",    "Experience")
exp_prop_stage = get_proportions(dei_barriers_list, "CareerStage", "Experience")
obs_prop_role  = get_proportions(dei_barriers_list, "Role",        "Observe")
obs_prop_lang  = get_proportions(dei_barriers_list, "Language",    "Observe")
obs_prop_stage = get_proportions(dei_barriers_list, "CareerStage", "Observe")

# calculate divergence (spread) between min and max group proportions per barrier
get_divergence = function(prop_df, group_col) {
    barrier_col = setdiff(names(prop_df), c(group_col, "Prop"))
    wide_data = prop_df |>
        select(all_of(group_col), all_of(barrier_col), Prop) |>
        pivot_wider(names_from = all_of(group_col),
                    values_from = Prop,
                    values_fill = 0)
    group_names = setdiff(names(wide_data), barrier_col)
    wide_data |>
        rowwise() |>
        mutate(
            Max_Prop  = max(c_across(all_of(group_names)), na.rm = TRUE),
            Min_Prop  = min(c_across(all_of(group_names)), na.rm = TRUE),
            Spread    = Max_Prop - Min_Prop,
            Max_Group = group_names[which.max(c_across(all_of(group_names)))],
            Min_Group = group_names[which.min(c_across(all_of(group_names)))],
            Mid_Prop  = ifelse(length(group_names) == 3,
                               sort(c_across(all_of(group_names)))[2],
                               NA_real_),
            Mid_Group = ifelse(length(group_names) == 3,
                               group_names[order(c_across(all_of(group_names)))[2]],
                               NA_character_)
        ) |>
        ungroup() |>
        filter(Max_Prop >= 0.10) |>
        arrange(desc(Spread))
}

exp_div_role  = get_divergence(exp_prop_role,  "Role")
exp_div_lang  = get_divergence(exp_prop_lang,  "Language")
exp_div_stage = get_divergence(exp_prop_stage, "CareerStage")
obs_div_role  = get_divergence(obs_prop_role,  "Role")
obs_div_lang  = get_divergence(obs_prop_lang,  "Language")
obs_div_stage = get_divergence(obs_prop_stage, "CareerStage")

# combine and standardize
plot_data = bind_rows(
    exp_div_role  |> rename(Barrier = Experience) |> mutate(Category = "Role",         Type = "Experienced"),
    exp_div_lang  |> rename(Barrier = Experience) |> mutate(Category = "Language",     Type = "Experienced"),
    exp_div_stage |> rename(Barrier = Experience) |> mutate(Category = "Career Stage", Type = "Experienced"),
    obs_div_role  |> rename(Barrier = Observe)    |> mutate(Category = "Role",         Type = "Observed"),
    obs_div_lang  |> rename(Barrier = Observe)    |> mutate(Category = "Language",     Type = "Observed"),
    obs_div_stage |> rename(Barrier = Observe)    |> mutate(Category = "Career Stage", Type = "Observed")
)

# clean barrier labels
plot_data = plot_data |>
    mutate(Barrier = case_when(
        Barrier == "None_observed" ~ "None",
        Barrier == "Time_Zone"     ~ "TimeZone",
        Barrier == "Not_listed"    ~ "Other",
        TRUE                       ~ Barrier
    )) |>
    relocate(Type, Barrier, Category, Min_Prop, Max_Prop, Spread)

# save plot data as artifact
write.table(plot_data, "artifacts/survey-mca-equity-divergence-values.csv")

# experienced barriers panel
p_exp = ggplot(plot_data |> filter(Type == "Experienced"),
               aes(x = Min_Prop, xend = Max_Prop, y = reorder(Barrier, Spread))) +
    geom_segment(color = "grey", linewidth = 2, alpha = 0.6) +
    geom_point(aes(x = Min_Prop), color = "#3DB4AD", fill = "#3DB4AD", size = 2, shape = 25) +
    geom_point(aes(x = Max_Prop), color = "#440154", fill = "#440154", size = 2, shape = 24) +
    geom_point(aes(x = Mid_Prop), color = "#357BA2", fill = "#357BA2", size = 2, shape = 23, na.rm = TRUE) +
    geom_text(aes(x = Min_Prop, label = Min_Group), hjust = 1.3,  vjust = -0.8, size = 3.5, color = "#3DB4AD") +
    geom_text(aes(x = Mid_Prop, label = Mid_Group), hjust = 0.5,  vjust = -0.8, size = 3,   color = "#357BA2", na.rm = TRUE) +
    geom_text(aes(x = Max_Prop, label = Max_Group), hjust = -0.4, vjust = -0.8, size = 3.5, color = "#440154") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1),
                       expand = expansion(mult = c(0.15, 0.15))) +
    facet_grid(Category ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(title = "", x = NULL, y = "Experienced") +
    theme_minimal() +
    theme(
        panel.grid.minor   = element_blank(),
        strip.placement    = "outside",
        plot.margin        = margin(t = 5, b = 20, l = 5, r = 5, unit = "pt"),
        axis.text.y        = element_text(color = "black", size = 11),
        axis.title.y       = element_text(size = 16),
        strip.text.y.left  = element_text(angle = 90, size = 13),
        strip.text.x       = element_text(size = 13),
        panel.spacing      = unit(1.5, "lines")
    )

# observed barriers panel
p_obs = ggplot(plot_data |> filter(Type == "Observed"),
               aes(x = Min_Prop, xend = Max_Prop, y = reorder(Barrier, Spread))) +
    geom_segment(color = "grey", linewidth = 2, alpha = 0.6) +
    geom_point(aes(x = Min_Prop), color = "#3DB4AD", fill = "#3DB4AD", size = 2, shape = 25) +
    geom_point(aes(x = Max_Prop), color = "#440154", fill = "#440154", size = 2, shape = 24) +
    geom_point(aes(x = Mid_Prop), color = "#357BA2", fill = "#357BA2", size = 2, shape = 23, na.rm = TRUE) +
    geom_text(aes(x = Min_Prop, label = Min_Group), hjust = 1.3,  vjust = -0.8, size = 3.5, color = "#3DB4AD") +
    geom_text(aes(x = Mid_Prop, label = Mid_Group), hjust = 0.5,  vjust = -0.8, size = 3,   color = "#357BA2", na.rm = TRUE) +
    geom_text(aes(x = Max_Prop, label = Max_Group), hjust = -0.4, vjust = -0.8, size = 3.5, color = "#440154") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1),
                       expand = expansion(mult = c(0.15, 0.15))) +
    facet_grid(Category ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(title = "", x = NULL, y = "Observed") +
    theme_minimal() +
    theme(
        panel.grid.minor   = element_blank(),
        strip.placement    = "outside",
        plot.margin        = margin(t = 5, b = 20, l = 5, r = 5, unit = "pt"),
        axis.text.y        = element_text(color = "black", size = 11),
        axis.title.y       = element_text(size = 16),
        strip.text.y.left  = element_text(angle = 90, size = 13),
        strip.text.x       = element_text(size = 13),
        panel.spacing      = unit(1.5, "lines")
    )

# save combined dumbbell figure -- paper figure
ggsave(filename = "outputs/f-survey-mca-equity-dumbbell.png",
       plot = plot_grid(p_exp, p_obs,
                        labels = "AUTO",
                        nrow = 2,
                        align = "v",
                        axis = "lr") +
           draw_label("Proportion of Respondents", x = 0.5, y = 0.02, size = 13),
       device = "png",
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)




######################## TIDY
rm(list = ls())
gc()