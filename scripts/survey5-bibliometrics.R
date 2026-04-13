######################## PACKAGES

library(bibliometrix) # bibliometric analysis and thematic evolution
library(dplyr)        # data wrangling
library(ggplot2)      # figures
library(janitor)      # clean column names
library(tidyr)        # data reshaping
library(stringr)      # string cleaning
library(purrr)        # functional iteration (map)
library(countrycode)  # standardize country names
library(rworldmap)    # geographic mapping and country coordinates
library(ggalluvial)   # Sankey / alluvial plot
library(classInt)     # Jenks natural breaks classification




######################## LOAD DATA

# WoS data -- links only visible with WoS subscription
# https://www.webofscience.com/wos/woscc/summary/6806fa57-3edb-408d-802b-54458855247b-25226194/relevance/1

# convert WoS plain text export to bibliometrix dataframe
bibdf = convert2df('data/wos-raw.txt', db = "wos", format = "plaintext")




######################## CLEAN DATES

resultsdf  = biblioAnalysis(bibdf, sep = ";")
summarydf  = summary(object = resultsdf, k = 20, pause = FALSE) # 417 documents

# 32 early access documents have no publication year -- assign 2022 for inclusion
sum(!is.na(bibdf$PY))
print(summarydf$MainInformationDF) # confirms 32 NA years are early access
bibdf$PY[is.na(bibdf$PY)] = 2022
sum(!is.na(bibdf$PY)) # verify all 417 now have dates




######################## FALSE POSITIVE SCREENING -- DEI SCORE

# clean abstract field
bibdf = bibdf |>
    mutate(AB = ifelse(trimws(AB) == "", NA, trimws(AB)))

sum(is.na(bibdf$AB))

# review missing abstracts using author keywords -- all have DEI keywords, keep
bibdf |>
    filter(is.na(AB)) |>
    select(AU, TI, PY, DE)

# score each abstract by count of core DEI stem terms
dei_terms = c(
    "divers",   # diversity, diverse, diversification
    "equit",    # equity, equitable
    "inclus"    # inclusion, inclusive, inclusivity
)

bibdf = bibdf |>
    mutate(
        dei_score = sapply(AB, function(x) {
            if (is.na(x)) return(NA)
            sum(sapply(dei_terms, function(term) {
                grepl(term, x, ignore.case = TRUE)
            }))
        })
    )

table(bibdf$dei_score)

# score 0 -- flag for manual review
bibdf |>
    filter(dei_score == 0 & !is.na(AB)) |>
    select(AU, TI, PY, DE) |>
    write.csv("artifacts/bibmet-review_score0.csv", row.names = FALSE)

# score 1 -- flag for manual review
bibdf |>
    filter(dei_score == 1 & !is.na(AB)) |>
    select(AU, TI, PY, DE) |>
    write.csv("artifacts/bibmet-review_score1.csv", row.names = FALSE)

# remove articles confirmed as irrelevant after manual review
noise_dei_score = c(
    "GHAFOOR A",
    "MCGUINNESS PB",
    "BABIER A",
    "BOSMAN LB",
    "WU J",
    "BILLINGSLEY S",
    "CHALAMANDARIS G",
    "MANN VVS",
    "TAECHARUNGROJ V",
    "D'AMATO M",
    "KOLOTOUCHKINA O",
    "KALFAGIANNI A",
    "SZAJNFARBER Z"
)

bibdf_clean = bibdf |>
    filter(!grepl(paste(noise_dei_score, collapse = "|"), AU, ignore.case = TRUE))

# confirm removals
bibdf_clean |>
    filter(grepl(paste(noise_dei_score, collapse = "|"), AU, ignore.case = TRUE)) |>
    select(AU, TI)




######################## FALSE POSITIVE SCREENING -- THEMATIC CLUSTERS

# helper to extract top keywords per cluster per time slice
get_slice_keywords = function(slice_data, slice_num) {
    slice_data |>
        group_by(Cluster_Label) |>
        arrange(desc(Occurrences)) |>
        slice_head(n = 5) |>
        mutate(Period = slice_num) |>
        select(Period, Cluster_Label, Words, Occurrences)
}

# Jenks breaks to determine chronological periods
breaks = classIntervals(bibdf$PY, n = 3, style = "jenks")
breaks$brks # 1994 2007 2016 2022

# thematic evolution of abstracts to identify noise clusters
thematic_evo = thematicEvolution(bibdf,
                                 field = "AB",
                                 years = c(2007, 2016),
                                 n = 150,
                                 ngrams = 2,
                                 minFreq = 1)

lapply(1:3, function(i) unique(thematic_evo$TM[[i]]$words$Cluster_Label))

comprehensive_check = bind_rows(
    get_slice_keywords(thematic_evo$TM[[1]]$words, "1994-2007"),
    get_slice_keywords(thematic_evo$TM[[2]]$words, "2008-2016"),
    get_slice_keywords(thematic_evo$TM[[3]]$words, "2017-2022")
) |> print(n = Inf)

# audit noise phrases per period
# slice 1 (1994-2007)
bibdf_clean |>
    filter(PY <= 2007) |>
    filter(grepl("paper argues|paper explores|urban planning", AB, ignore.case = TRUE)) |>
    select(AU, TI, SO, PY, DE, AB) |>
    write.csv("artifacts/bibmet-audit_slice1.csv", row.names = FALSE)

# slice 2 (2008-2016)
bibdf_clean |>
    filter(PY >= 2008 & PY <= 2016) |>
    filter(grepl("urban form|sustainable urban|decision makers|public administration|study examines|regional council", AB, ignore.case = TRUE)) |>
    select(AU, TI, SO, PY, DE, AB) |>
    write.csv("artifacts/bibmet-audit_slice2.csv", row.names = FALSE)

# slice 3 (2017-2022)
bibdf_clean |>
    filter(PY >= 2017) |>
    filter(grepl("climate change|sustainable development|future research", AB, ignore.case = TRUE)) |>
    select(AU, TI, SO, PY, DE, AB) |>
    write.csv("artifacts/bibmet-audit_slice3.csv", row.names = FALSE)

# remove articles confirmed as irrelevant after manual review
noise_audit = c(
    "DZINGAI I",  # corporate governance/JSE financial performance
    "ESFANDI S"   # urban energy resilience, equity as technical principle
)

bibdf_clean = bibdf_clean |>
    filter(!grepl(paste(noise_audit, collapse = "|"), AU, ignore.case = TRUE))

# confirm removals
bibdf_clean |>
    filter(grepl(paste(noise_dei_score, collapse = "|"), AU, ignore.case = TRUE)) |>
    select(AU, TI)




######################## FINAL THEMATIC EVOLUTION ON AUTHOR KEYWORDS

# re-run thematic evolution on author keywords (DE field) with education synonyms consolidated
thematic_evo_final = thematicEvolution(bibdf_clean,
                                       field = "DE",
                                       years = c(2007, 2016),
                                       n = 150,
                                       ngrams = 2,
                                       minFreq = 1,
                                       synonyms = c(
                                           "higher education;education",
                                           "medical education;education",
                                           "teacher education;education",
                                           "educational equity;education",
                                           "inclusive education;education",
                                           "continuing education;education",
                                           "social justice education;education",
                                           "diversity education;education",
                                           "higher-education;education"
                                       ))

# standardize node names
thematic_evo_final$Nodes$name = gsub("higher education", "education",
                                     thematic_evo_final$Nodes$name,
                                     ignore.case = TRUE)
thematic_evo_final$Nodes$name
lapply(1:3, function(i) unique(thematic_evo_final$TM[[i]]$words$Cluster_Label))




######################## CORPUS METADATA

# confirm Jenks breaks unchanged after cleaning
breaks = classIntervals(bibdf_clean$PY, n = 3, style = "jenks")
breaks$brks # same: 2007, 2016

# generate summary statistics
resultsdf_clean = biblioAnalysis(bibdf_clean, sep = ";")
summarydf_clean = summary(object = resultsdf_clean, k = 20, pause = FALSE)

# save corpus summary -- ST4 in supplemental materials
write.csv(summarydf_clean$MainInformationDF, "outputs/st-bibmet-corpus summary.csv", row.names = FALSE)

rm(bibdf, comprehensive_check, breaks, summarydf, summarydf_clean, thematic_evo,
   resultsdf_clean, resultsdf, dei_terms, noise_audit, noise_dei_score)




######################## SANKEY FIGURE

# build node and edge data for Sankey
node_names = thematic_evo_final$Nodes$name
names(node_names) = thematic_evo_final$Nodes$id

sankey_data = thematic_evo_final$Edges |>
    left_join(thematic_evo_final$Nodes |> select(id, slice), by = c("from" = "id")) |>
    mutate(
        from_lab = str_wrap(str_to_title(str_replace_all(node_names[as.character(from)], "_", " ")), 10),
        to_lab   = str_wrap(str_to_title(str_replace_all(node_names[as.character(to)],   "_", " ")), 10),
        axis1    = ifelse(slice == 1, from_lab, NA_character_),
        axis2    = ifelse(slice == 1, to_lab,   from_lab),
        axis3    = ifelse(slice == 2, to_lab,   NA_character_),
        flow_id  = row_number()
    ) |>
    to_lodes_form(key = "Timeline", value = "Theme", axes = c("axis1", "axis2", "axis3")) |>
    filter(!is.na(Theme))

# save Sankey figure -- paper figure (Figure 8)
ggplot(sankey_data,
       aes(x = Timeline,
           stratum = Theme,
           alluvium = flow_id,
           y = Inc_Weighted)) +
    geom_flow(aes(fill = Theme), width = 1/5, alpha = 0.4, na.rm = TRUE) +
    geom_stratum(width = 1/4, fill = "white", color = "gray30", linewidth = 0.4) +
    geom_text(stat = "stratum", aes(label = str_to_title(Theme)),
              size = 3.2, lineheight = 0.9, fontface = "bold") +
    scale_fill_viridis_d(option = "D", guide = "none") +
    scale_x_discrete(labels = c("1994-2007", "2008-2016", "2017-2022"),
                     expand = c(.2, .2)) +
    theme_minimal() +
    labs(x = "Timeline of Thematic Evolution", y = "") +
    theme(
        axis.text.y  = element_blank(),
        panel.grid   = element_blank(),
        axis.text.x  = element_text(size = 12, face = "bold")
    )

ggsave(filename = "outputs/f-bibmet-sankey.png",
       width = 12,
       height = 10,
       units = "in",
       dpi = 300)




######################## COLLABORATION NETWORK MAP

# extract country affiliations from author address field
bibdf_geo = bibdf_clean |>
    mutate(countries_extracted = str_extract_all(C1, "(?<=, )[^,;]+(?=;|$)")) |>
    mutate(countries_std = map(countries_extracted, function(x) {
        if(length(x) == 0) return(NA)
        countrycode(trimws(x), origin = 'country.name', destination = 'country.name')
    }))

# create collaboration links (co-authorship pairs)
links = bibdf_geo |>
    select(countries_std) |>
    filter(!is.na(countries_std)) |>
    mutate(pairs = map(countries_std, function(x) {
        x = unique(na.omit(x))
        if(length(x) > 1) {
            as.data.frame(t(combn(sort(x), 2)))
        } else {
            NULL
        }
    })) |>
    filter(!map_lgl(pairs, is.null)) |>
    unnest(pairs) |>
    rename(from = V1, to = V2) |>
    group_by(from, to) |>
    summarise(weight = n(), .groups = "drop") |>
    filter(weight > 1)

# map coordinates from rworldmap country centroids
data(countriesLow)
coords = data.frame(
    name = countriesLow$NAME,
    lon  = coordinates(countriesLow)[,1],
    lat  = coordinates(countriesLow)[,2]
) |>
    mutate(name_std = countrycode(name, "country.name", "country.name")) |>
    filter(!is.na(name_std))

# join coordinates to collaboration links
links_coords = links |>
    left_join(coords, by = c("from" = "name_std")) |>
    rename(x = lon, y = lat) |>
    left_join(coords, by = c("to" = "name_std")) |>
    rename(xend = lon, yend = lat) |>
    filter(!is.na(x) & !is.na(xend))

# publication density by country for choropleth base
prod_map_data = bibdf_geo |>
    select(countries_std) |>
    unnest(countries_std) |>
    filter(!is.na(countries_std)) |>
    group_by(countries_std) |>
    summarise(articles = n(), .groups = "drop") |>
    rename(country_std = countries_std)

node_data = prod_map_data |>
    left_join(coords, by = c("country_std" = "name_std")) |>
    filter(!is.na(lon))

# build choropleth base map
world_data = map_data("world") |>
    filter(region != "Antarctica")

world_choropleth = world_data |>
    mutate(region_std = countrycode(region, "country.name", "country.name")) |>
    left_join(prod_map_data, by = c("region_std" = "country_std"))

# save collaboration network map -- paper figure (Figure 7)
ggplot() +
    geom_polygon(data = world_choropleth,
                 aes(x = long, y = lat, group = group, fill = articles),
                 color = "white", linewidth = 0.1) +
    scale_fill_gradient(low = "gray90", high = "gray40",
                        na.value = "gray95",
                        trans = "log10",
                        name = "Pub Density") +
    geom_curve(data = links_coords,
               aes(x = x, y = y, xend = xend, yend = yend, linewidth = weight),
               color = "#440154FF", alpha = 0.3, curvature = 0.2) +
    geom_point(data = node_data,
               aes(x = lon, y = lat, size = articles),
               color = "#440154FF", alpha = 0.6) +
    geom_text(data = node_data |> slice_max(articles, n = 12),
              aes(x = lon, y = lat, label = country_std),
              size = 3, vjust = -1, fontface = "bold", check_overlap = TRUE) +
    scale_size_continuous(range = c(0.5, 6), name = "Network Weight") +
    theme_classic() +
    theme(
        legend.position   = "bottom",
        legend.box        = "horizontal",
        axis.title        = element_blank(),
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        axis.line         = element_blank(),
        legend.key.width  = unit(1.5, "cm")
    ) +
    guides(
        fill      = guide_colorbar(title = "Pub Density (Log)", order = 1),
        size      = guide_legend(title = "Article Count",       order = 2),
        linewidth = guide_legend(title = "Collab Strength",     order = 3)
    )

ggsave(filename = "outputs/f-bibmet-map.png",
       plot = last_plot(),
       device = "png",
       width = 12,
       height = 6,
       units = "in",
       dpi = 300)




######################## TIDY
rm(list = ls())
gc()