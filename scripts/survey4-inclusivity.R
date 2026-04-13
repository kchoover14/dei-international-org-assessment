######################## PACKAGES

library(dplyr)       # data wrangling
library(tidyr)       # data reshaping
library(forcats)     # factor wrangling
library(FactoMineR)  # multiple correspondence analysis (MCA)
library(crosstable)  # contingency tables with statistical tests
library(cowplot)     # multi-panel figure composition
library(ggplot2)     # figures
library(openxlsx)    # save results to Excel




######################## READ DATA

dei_xtabs = read.csv("data/survey-clean-xtabs.csv", stringsAsFactors = TRUE)

# strip variable prefix for cleaner column names
dei_xtabs = dei_xtabs |> rename_with(~ sub("^[^_]+_", "", .x))




######################## SUBSET BY DEMOGRAPHIC

deiIncRole = dplyr::select(dei_xtabs, Role, Collaboration, Consortium, Formed_Collaboration, Information_Source, Interaction)

deiIncLang = dplyr::select(dei_xtabs, Language, Collaboration, Consortium, Formed_Collaboration, Information_Source, Interaction)
deiIncLang = deiIncLang |>
    filter(Language != "Other") |>   # omit Other due to small sample size (n=7)
    mutate(Language = fct_drop(Language))

deiIncStage = dplyr::select(dei_xtabs, CareerStage, Collaboration, Consortium, Formed_Collaboration, Information_Source, Interaction)




######################## CONTINGENCY TABLES

# remove empty consortium responses before testing
deiIncRole = deiIncRole |>
    filter(Consortium != "") |>
    droplevels()
role = crosstable(deiIncRole, by = Role, test = TRUE)

deiIncLang = deiIncLang |>
    filter(Consortium != "") |>
    droplevels()
language = crosstable(deiIncLang, by = Language, test = TRUE)

deiIncStage = deiIncStage |>
    filter(Consortium != "") |>
    droplevels()
stage = crosstable(deiIncStage, by = CareerStage, test = TRUE)

# save results -- ST2 in supplemental materials
wb = createWorkbook()
addWorksheet(wb, "Role")
writeData(wb, "Role", as.data.frame(role))
addWorksheet(wb, "Language")
writeData(wb, "Language", as.data.frame(language))
addWorksheet(wb, "CareerStage")
writeData(wb, "CareerStage", as.data.frame(stage))
saveWorkbook(wb, "outputs/st-survey-inclusivity-crosstabs.xlsx", overwrite = TRUE)




######################## MCA

# role -- significant variables: collaboration, consortium, interaction
mca_role = deiIncRole |>
    select(Role, Collaboration, Consortium, Interaction)

inc_mca_role = MCA(mca_role, quali.sup = 1, graph = FALSE)
a = plot(inc_mca_role, invisible = "ind", col.var = "#21918c", col.quali.sup = "#440154")

# language -- significant variable: information source
mca_lang = deiIncLang |>
    select(Language, Information_Source)

inc_mca_lang = MCA(mca_lang, quali.sup = 1, graph = FALSE)
b = plot(inc_mca_lang, invisible = "ind", col.var = "#21918c", col.quali.sup = "#440154")

# save MCA coordinates as artifacts
write.csv(as.data.frame(inc_mca_role$var$coord), "artifacts/survey-mca-inclusivity-role-coords.csv")
write.csv(as.data.frame(inc_mca_lang$var$coord), "artifacts/survey-mca-inclusivity-lang-coords.csv")

# save MCA panel figure -- paper figure (Figure 3)
png("outputs/f-survey-mca-inclusivity.png", width = 9, height = 6, units = "in", res = 300)
plot_grid(a, b,
          nrow = 1,
          rel_widths = c(1.5, 1),
          labels = c('A', 'B'),
          label_size = 14,
          label_x = 0,
          label_y = 0.5)
dev.off()




######################## TIDY
rm(list = ls())
gc()