## DEI Assessment of an International Scientific Organization

This paper presents an evidence-based DEI policy framework for international scientific organizations, developed through a mixed-methods case study of the Global Consortium for Chemosensory Research (GCCR). We assessed geographic diversity, equity, and inclusivity using three independent methods — a member survey, behavioral analysis of Slack platform data, and a bibliometric review of the DEI action literature — and found that while geographic participation broadly reflects membership composition, equity barriers and communication patterns are unevenly distributed across roles, career stages, and language backgrounds, with the DEI literature itself concentrated in geopolitical silos despite its global relevance.

---

### Paper in Review

Hoover, K.C. et al. (under review). A Mixed-Methods Toolkit for Evidence-Based Diversity, Equity, and Inclusivity Policy in International Organizations. *Diversity, Equity, and Inclusion: A Research and Practice Journal*.

---

### Portfolio Page

The [portfolio page](https://kchoover14.github.io/dei-international-org-assessment) includes a full project narrative, key findings, and figures.

---

### Scripts

| File | Language | Purpose |
|---|---|---|
| `scripts/survey1-clean.R` | R | Survey data cleaning, PII removal, variable recoding |
| `scripts/survey2-diversity.R` | R | Geographic diversity map (representation ratio) |
| `scripts/survey3-equity.R` | R | Equity analysis: crosstabs, MCA, dumbbell figure |
| `scripts/survey4-inclusivity.R` | R | Inclusivity analysis: crosstabs, MCA figure |
| `scripts/survey5-bibliometrics.R` | R | Bibliometric cleaning, corpus filtering, Sankey and network map |
| `scripts/slack1-clean.ipynb` | Python | Slack export flattening, anonymization, participation ratios |
| `scripts/slack2-explore.ipynb` | Python | Exploratory figures (voice, reactions, response, temporal) |
| `scripts/slack3-diversity.ipynb` | Python | Geographic diversity choropleth map |
| `scripts/slack4-analysis.ipynb` | Python | Network metrics, equity and inclusivity network figures |

### Data — Not Shared

Raw data are not shared due to participant confidentiality and organizational data agreements.

| File | Description |
|---|---|
| `data/survey-raw.csv` | Raw survey export |
| `data/membership-raw.csv` | Organizational membership list |
| `data/slack-raw-json.zip` | Raw Slack workspace export (nested JSON) |
| `data/slack-raw-flat.xlsx` | Flattened Slack export prior to anonymization |
| `data/slack-explore.csv` | Intermediate Slack file for exploratory analysis |

### Data — Shared

Intermediate cleaned files used as inputs to analysis scripts.

| File | Description |
|---|---|
| `data/survey-clean-map.csv` | Cleaned survey data with ISO3 country codes |
| `data/survey-clean-xtabs.csv` | Cleaned survey data for inclusivity crosstabs |
| `data/survey-clean-barriers.csv` | Cleaned survey data for equity/barriers analysis |
| `data/wos-raw.txt` | Web of Science export — requires WoS subscription to reproduce |
| `data/slack-analysis.csv` | Anonymized Slack data for network analysis |
| `data/slack-diversity.csv` | Anonymized Slack data for geographic diversity analysis |

### Outputs

| File | Description |
|---|---|
| `outputs/f-survey-diversity.png` | Survey geographic diversity map (paper figure) |
| `outputs/f-survey-mca-equity-dumbbell.png` | Equity barrier dumbbell plot (paper figure) |
| `outputs/f-slack-diversity.png` | Slack geographic participation map (paper figure) |
| `outputs/f-slack-equity.png` | Slack equity network map (paper figure) |
| `outputs/f-slack-inclusivity.png` | Slack inclusivity network map — two panel (paper figure) |
| `outputs/f-bibmet-sankey.png` | Bibliometric thematic evolution Sankey (paper figure) |
| `outputs/f-bibmet-map.png` | Bibliometric collaboration network map (paper figure) |
| `outputs/f-survey-mca-inclusivity.png` | Inclusivity MCA — SF1 (supplemental figure) |
| `outputs/sf-survey-mca-equity.png` | Equity MCA (supplemental figure) |
| `outputs/st-survey-equity-crosstabs.xlsx` | Survey equity crosstabs — ST1 |
| `outputs/st-survey-inclusivity-crosstabs.xlsx` | Survey inclusivity crosstabs — ST2 |
| `outputs/st-slack-network-metrics.csv` | Slack network metrics — ST3 |
| `outputs/st-bibmet-corpus summary.csv` | Bibliometric corpus summary — ST4 |

### Artifacts

Intermediate and exploratory files retained for transparency and reproduction verification.

| File | Description |
|---|---|
| `artifacts/survey-mca-equity-role-coords.csv` | MCA coordinates: equity by role |
| `artifacts/survey-mca-equity-stage-coords.csv` | MCA coordinates: equity by career stage |
| `artifacts/survey-mca-equity-divergence-values.csv` | Barrier divergence values for dumbbell plot |
| `artifacts/survey-mca-inclusivity-role-coords.csv` | MCA coordinates: inclusivity by role |
| `artifacts/survey-mca-inclusivity-lang-coords.csv` | MCA coordinates: inclusivity by language |
| `artifacts/bibmet-review_score0.csv` | Manual review list: DEI score = 0 |
| `artifacts/bibmet-review_score1.csv` | Manual review list: DEI score = 1 |
| `artifacts/bibmet-audit_slice1.csv` | Thematic audit review list: 1994–2007 |
| `artifacts/bibmet-audit_slice2.csv` | Thematic audit review list: 2008–2016 |
| `artifacts/bibmet-audit_slice3.csv` | Thematic audit review list: 2017–2022 |
| `artifacts/Slack-explore-equity metric definitions.docx` | Metric definitions developed during exploration |
| `artifacts/slack-explore-diversity-bubble.png` | Exploratory: diversity bubble chart |
| `artifacts/slack-explore-diversity-threepanel.png` | Exploratory: diversity three-panel |
| `artifacts/slack-explore-gini-reciprocity.png` | Exploratory: Gini and reciprocity |
| `artifacts/slack-explore-networkCentrality.png` | Exploratory: network centrality |
| `artifacts/slack-explore-participation.png` | Exploratory: participation patterns |
| `artifacts/slack-explore-reactionEquity.png` | Exploratory: reaction equity |
| `artifacts/slack-explore-responsePatterns.png` | Exploratory: response patterns |
| `artifacts/slack-explore-voiceDistribution.png` | Exploratory: voice distribution |

---

### Tools & Technologies

**Languages:** R | Python

**Tools:** Jupyter

**R Packages:** bibliometrix | classInt | countrycode | cowplot | crosstable | dplyr | FactoMineR | forcats | ggalluvial | ggplot2 | janitor | openxlsx | purrr | rworldmap | shadowtext | stringr | tidyr | tidytext | viridis

**Python Libraries:** countrycode | geopandas | jenkspy | matplotlib | networkx | numpy | pandas | pytz | scipy | seaborn

**Environments:** `renv.lock` (R) | `requirements.txt` (Python)

---

### Expertise

Demonstrates end-to-end mixed-methods research design for organizational DEI assessment — from literature synthesis and survey analytics through network analysis and bibliometrics to evidence-based policy output.

---

### License

- Code and scripts are licensed under the [MIT License](LICENSE).
- Data, figures, and written content © Kara C. Hoover, licensed under [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/).
