######################## PACKAGES

library(dplyr)      # data wrangling
library(countrycode) # ISO3 country code standardization
library(rworldmap)  # choropleth world maps
library(viridis)    # color-blind friendly color palettes
library(classInt)   # Jenks natural breaks classification
library(shadowtext)  # map label masking for readability
library(tidyr)      # replace_na for missing survey proportions
library(janitor)    # clean column names for membership file



######################## READ DATA

survey    = read.csv("data/survey-clean-map.csv", stringsAsFactors = TRUE)
membership = read.csv("data/membership-raw.csv", stringsAsFactors = TRUE) |> clean_names()




######################## CREATE SURVEY PROPORTIONAL DATA

# check for unmapped countries
sum(is.na(survey$Map_Country_ISO3))

# survey proportion by country
mapdf = survey |>
    group_by(Map_Country_ISO3) |>
    summarise(Survey_N = n()) |>
    ungroup() |>
    mutate(Survey_Prop = (Survey_N / sum(Survey_N)))




######################## CREATE MEMBERSHIP PROPORTIONAL DATA

# standardize country names and ISO3 codes
mapdf2 = membership |>
    mutate(Map_Country_ISO3 = countrycode(country_of_institution,
                                          origin = 'country.name',
                                          destination = 'iso3c')) |>
    mutate(Map_Country_Standard = countrycode(Map_Country_ISO3,
                                              origin = 'iso3c',
                                              destination = 'country.name'))

# check for unmapped countries
sum(is.na(mapdf2$Map_Country_ISO3))

# membership proportion by country
mapdf2 = mapdf2 |>
    group_by(Map_Country_ISO3) |>
    summarise(Member_N = n()) |>
    ungroup() |>
    mutate(Member_Prop = (Member_N / sum(Member_N)))

# join survey and membership -- representation ratio = survey prop / membership prop
ubermapdf = mapdf2 |>
    left_join(mapdf, by = "Map_Country_ISO3") |>
    mutate(
        Survey_Prop = tidyr::replace_na(Survey_Prop, 0),
        Rep_Ratio = Survey_Prop / Member_Prop
    )




######################## PEARSON CORRELATION

# confirm response patterns are independent of national membership size
cor_data = ubermapdf[ubermapdf$Rep_Ratio > 0, ]
print(cor.test(as.numeric(cor_data$Member_N), as.numeric(cor_data$Rep_Ratio), method = "pearson"))




######################## PREPARE MAP DATA

rm(mapdf, mapdf2, membership, survey, cor_data)

# reduce to mapping variables only
ubermapdf = dplyr::select(ubermapdf, Map_Country_ISO3, Rep_Ratio)
ubermapdf = as.data.frame(ubermapdf)




######################## JENKS NATURAL BREAKS

# function to calculate Goodness of Variance Fit (GVF) to identify optimal k
get_gvf = function(k, data) {
    if (k < 2) return(0)
    res = classIntervals(data, n = k, style = "jenks")
    sdam = sum((data - mean(data))^2)
    sub_means = rep(0, k)
    for(i in 1:k) {
        vals = data[data >= res$brks[i] & data <= res$brks[i+1]]
        if(length(vals) > 0) {
            sub_means[i] = sum((vals - mean(vals))^2)
        }
    }
    sdcm = sum(sub_means)
    return((sdam - sdcm) / sdam)
}

# test k from 2 to 10
ratios = ubermapdf$Rep_Ratio
gvf_scores = sapply(2:10, get_gvf, data = ratios)
names(gvf_scores) = 2:10
print(gvf_scores)

# optimal k=3 confirmed -- add category for 0 (no representation)
natural_k = classIntervals(ubermapdf$Rep_Ratio, n = 3, style = "jenks")
print(natural_k)
my_breaks = c(0, 0.00001, 0.7712, 3.47, 6.941)




######################## LABEL PRIORITY

# European ISO3 codes -- label selectively to avoid crowding
europe_iso3 = c("ALB","AND","AUT","BLR","BEL","BIH","BGR","HRV","CYP","CZE",
                "DNK","EST","FIN","FRA","DEU","GRC","HUN","ISL","IRL","ITA",
                "LVA","LIE","LTU","LUX","MLT","MDA","MCO","MNE","NLD","MKD",
                "NOR","POL","PRT","ROU","SMR","SRB","SVK","SVN","ESP","SWE",
                "CHE","UKR","GBR","VAT","TUR")

# large European countries labeled even for mid categories
europe_large = c("FRA","SWE","NOR")

ubermapdf = ubermapdf |>
    mutate(Label_Me = case_when(
        !(Map_Country_ISO3 %in% europe_iso3) & Rep_Ratio > 0 ~ TRUE,
        Map_Country_ISO3 %in% europe_iso3 & Rep_Ratio >= 3.47 ~ TRUE,
        Map_Country_ISO3 %in% europe_iso3 & Map_Country_ISO3 %in% europe_large ~ TRUE,
        TRUE ~ FALSE
    ))




######################## MAP COLORS

# mako palette, color-blind friendly, 4 categories
my_colors = viridis::viridis(4, option = "G", direction = -1, begin = .3, end = .9, alpha = .9)




######################## MAKE THE MAP

# add map geometry
map_data = joinCountryData2Map(ubermapdf,
                               joinCode = "ISO3",
                               nameJoinColumn = "Map_Country_ISO3")

# save map as PNG
png(filename = "outputs/f-survey-diversity.png", width = 15, height = 8,
    units = "in", res = 300, type = "cairo")
par(mar = c(2, 0, 0, 0))
mapParams = mapCountryData(map_data,
                           nameColumnToPlot = "Rep_Ratio",
                           catMethod = my_breaks,
                           colourPalette = my_colors,
                           addLegend = FALSE,
                           missingCountryCol = "white",
                           oceanCol = "aliceblue",
                           mapTitle = "",
                           mapRegion = "world",
                           xlim = c(-180, 180),
                           ylim = c(-60, 90))

# cover Antarctica with ocean color rectangle
rect(-180, -90, 180, -60, col = "aliceblue", border = NA)

# country labels with selective offsets for small or crowded countries
labels_to_plot = map_data[which(map_data$Label_Me == TRUE), ]
coords = coordinates(labels_to_plot)

offsets = data.frame(
    iso3 = c("IRL", "DNK", "NOR", "SWE", "ARG", "URY"),
    dx   = c(-4,     3,     3,     3,      2,     4),
    dy   = c( 2,     3,     3,    -3,     -3,     0),
    stringsAsFactors = FALSE
)

# draw leader lines for offset labels
for(i in 1:nrow(coords)) {
    label = as.character(labels_to_plot$Map_Country_ISO3[i])
    off = offsets[offsets$iso3 == label, ]
    if(nrow(off) > 0) {
        segments(coords[i,1], coords[i,2],
                 coords[i,1] + off$dx, coords[i,2] + off$dy,
                 col = "black", lwd = 0.5)
    }
}

# draw white mask behind labels for readability (shadowtext approach)
for(i in 1:nrow(coords)) {
    label = as.character(labels_to_plot$Map_Country_ISO3[i])
    x = coords[i,1]
    y = coords[i,2]
    off = offsets[offsets$iso3 == label, ]
    if(nrow(off) > 0) { x = x + off$dx; y = y + off$dy }
    text_width  = strwidth(label, cex = 0.8, font = 2) * 1.0
    text_height = strheight(label, cex = 0.8, font = 2) * 2.0
    rect(x - text_width/2, y - text_height/2,
         x + text_width/2, y + text_height/2,
         col = adjustcolor("white", alpha.f = 0.6), border = NA)
    text(x, y, labels = label, cex = 0.8, col = "black", font = 2)
}

# legend
legend(x = -180, y = -35,
       legend = c("Over-Represented",
                  "Balanced Representation",
                  "Under-Represented",
                  "Not Represented",
                  "No Members"),
       fill = c(rev(my_colors), "white"),
       title = "",
       cex = 0.8,
       bty = "n",
       y.intersp = .9)
dev.off()




######################## TIDY
rm(list = ls())
gc()