NHL Win Probability Modeling
================
Bangguo Jin
三月 03, 2026

## Overview

This cleaned version is designed for GitHub/portfolio use. It:

- uses a repo-friendly data path search
- keeps the author name as **Bangguo Jin**
- saves selected figures to a `figures/` folder
- keeps the original analysis logic while making the code easier to
  reuse

``` r
find_data_file <- function() {
  candidates <- c(
    "data/game_teams_stats.csv",
    "game_teams_stats.csv",
    "data/game_teams_stats_sample.csv",
    "game_teams_stats_sample.csv",
    "data/game_teams_stats.xlsx",
    "game_teams_stats.xlsx"
  )

  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit)) {
    stop(
      "No dataset found. Place one of these files in the project folder:\n",
      "- data/game_teams_stats.csv\n",
      "- game_teams_stats.csv\n",
      "- data/game_teams_stats_sample.csv\n",
      "- game_teams_stats_sample.csv\n",
      "- data/game_teams_stats.xlsx\n",
      "- game_teams_stats.xlsx"
    )
  }
  hit
}

load_nhl_data <- function(path) {
  if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    df <- read_excel(path, sheet = 1)
    colnames(df) <- c(
      "game_id", "team_id", "HoA", "won", "settled_in", "head_coach",
      "goals", "shots", "hits", "pim", "powerPlayOpportunities",
      "powerPlayGoals", "faceOffWinPercentage", "giveaways",
      "takeaways", "blocked", "startRinkSide"
    )
  } else {
    df <- read_csv(path, show_col_types = FALSE)
  }

  df %>%
    mutate(
      game_id = as.character(game_id),
      team_id = as.numeric(team_id),
      HoA = as.character(HoA),
      won = as.logical(won),
      goals = as.numeric(goals),
      shots = as.numeric(shots),
      pim = as.numeric(pim),
      powerPlayOpportunities = as.numeric(powerPlayOpportunities),
      powerPlayGoals = as.numeric(powerPlayGoals)
    ) %>%
    filter(
      !is.na(team_id), !is.na(goals), !is.na(shots),
      !is.na(pim), !is.na(won)
    ) %>%
    distinct(game_id, team_id, .keep_all = TRUE)
}

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

data_path <- find_data_file()
df <- load_nhl_data(data_path)

cat("Loaded file:", data_path, "\n")
```

    ## Loaded file: game_teams_stats.csv

``` r
cat("Rows after cleaning:", nrow(df), "\n")
```

    ## Rows after cleaning: 47462

## Question 1

**Does higher total penalty minutes (PIM) per game decrease win
probability after controlling for goal differential and shots?**

``` r
df_wide_q1 <- df %>%
  select(game_id, team_id, HoA, won, goals, shots, pim) %>%
  pivot_wider(
    names_from = HoA,
    values_from = c(team_id, won, goals, shots, pim),
    names_glue = "{HoA}_{.value}"
  ) %>%
  filter(!is.na(home_team_id), !is.na(away_team_id))

analysis_q1 <- bind_rows(
  df_wide_q1 %>%
    transmute(
      game_id,
      location = "home",
      won = home_won,
      goal_diff = home_goals - away_goals,
      shots_self = home_shots,
      shots_opp = away_shots,
      pim_self = home_pim
    ),
  df_wide_q1 %>%
    transmute(
      game_id,
      location = "away",
      won = away_won,
      goal_diff = away_goals - home_goals,
      shots_self = away_shots,
      shots_opp = home_shots,
      pim_self = away_pim
    )
) %>%
  filter(complete.cases(.))

q1_model <- glm(
  won ~ pim_self + goal_diff + shots_self + shots_opp,
  data = analysis_q1,
  family = binomial
)

q1_results <- tidy(q1_model, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

q1_results %>%
  select(
    Term = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  ) %>%
  gt() %>%
  tab_header(title = "Question 1 Results")
```

<div id="qeympyxxos" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#qeympyxxos table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#qeympyxxos thead, #qeympyxxos tbody, #qeympyxxos tfoot, #qeympyxxos tr, #qeympyxxos td, #qeympyxxos th {
  border-style: none;
}
&#10;#qeympyxxos p {
  margin: 0;
  padding: 0;
}
&#10;#qeympyxxos .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#qeympyxxos .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#qeympyxxos .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#qeympyxxos .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#qeympyxxos .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#qeympyxxos .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#qeympyxxos .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#qeympyxxos .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#qeympyxxos .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#qeympyxxos .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#qeympyxxos .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#qeympyxxos .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#qeympyxxos .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#qeympyxxos .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#qeympyxxos .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeympyxxos .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#qeympyxxos .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#qeympyxxos .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#qeympyxxos .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeympyxxos .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#qeympyxxos .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeympyxxos .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#qeympyxxos .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeympyxxos .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#qeympyxxos .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#qeympyxxos .gt_left {
  text-align: left;
}
&#10;#qeympyxxos .gt_center {
  text-align: center;
}
&#10;#qeympyxxos .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#qeympyxxos .gt_font_normal {
  font-weight: normal;
}
&#10;#qeympyxxos .gt_font_bold {
  font-weight: bold;
}
&#10;#qeympyxxos .gt_font_italic {
  font-style: italic;
}
&#10;#qeympyxxos .gt_super {
  font-size: 65%;
}
&#10;#qeympyxxos .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#qeympyxxos .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#qeympyxxos .gt_indent_1 {
  text-indent: 5px;
}
&#10;#qeympyxxos .gt_indent_2 {
  text-indent: 10px;
}
&#10;#qeympyxxos .gt_indent_3 {
  text-indent: 15px;
}
&#10;#qeympyxxos .gt_indent_4 {
  text-indent: 20px;
}
&#10;#qeympyxxos .gt_indent_5 {
  text-indent: 25px;
}
&#10;#qeympyxxos .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#qeympyxxos div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Question 1 Results</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Term">Term</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Odds-Ratio">Odds Ratio</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Lower">95% CI Lower</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Upper">95% CI Upper</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Term" class="gt_row gt_left">(Intercept)</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.890000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">1.290000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">2.756000e-01</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">pim_self</td>
<td headers="Odds Ratio" class="gt_row gt_right">9.822000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">9.745000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">9.898000e-01</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">goal_diff</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.795015e+09</td>
<td headers="95% CI Lower" class="gt_row gt_right">1.280267e+141</td>
<td headers="95% CI Upper" class="gt_row gt_right">5.003245e+146</td>
<td headers="p-value" class="gt_row gt_right">0.9071</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">shots_self</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.017800e+00</td>
<td headers="95% CI Lower" class="gt_row gt_right">1.009700e+00</td>
<td headers="95% CI Upper" class="gt_row gt_right">1.025900e+00</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">shots_opp</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.026800e+00</td>
<td headers="95% CI Lower" class="gt_row gt_right">1.018700e+00</td>
<td headers="95% CI Upper" class="gt_row gt_right">1.035000e+00</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td></tr>
  </tbody>
  &#10;</table>
</div>

``` r
# Figure A1: distribution of PIM
fig_a1 <- ggplot(analysis_q1, aes(x = pim_self)) +
  geom_histogram(binwidth = 7, fill = "#E69F00", color = NA) +
  labs(
    title = "Figure A1. Distribution of team penalty minutes per game",
    x = "Team penalty minutes in game (PIM)",
    y = "Number of team-games"
  ) +
  theme_minimal(base_size = 16)

# Figure 1A: observed win percentage by PIM
fig_1a_data <- analysis_q1 %>%
  group_by(pim_self) %>%
  summarise(
    win_pct = mean(as.numeric(won)),
    .groups = "drop"
  )

fig_1a <- ggplot(fig_1a_data, aes(x = pim_self, y = win_pct)) +
  geom_line(color = "#E69F00", linewidth = 1) +
  geom_point(color = "#E69F00", size = 2.8) +
  labs(
    title = "Figure 1A. Win percentage by team penalty minutes",
    x = "Team penalty minutes in game (PIM)",
    y = "Observed win percentage"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 16)

# Figure 1B: predicted probability vs PIM
pred_q1 <- expand.grid(
  pim_self = seq(0, 40, by = 1),
  goal_diff = 0,
  shots_self = mean(analysis_q1$shots_self, na.rm = TRUE),
  shots_opp = mean(analysis_q1$shots_opp, na.rm = TRUE)
)

pred_q1$prob <- predict(q1_model, newdata = pred_q1, type = "response")

fig_1b <- ggplot(pred_q1, aes(x = pim_self, y = prob)) +
  geom_line(color = "#E69F00", linewidth = 1.2) +
  labs(
    title = "Figure 1B. Model-predicted win probability vs team PIM\n(Goal differential = 0, average shots)",
    x = "Team penalty minutes in game (PIM)",
    y = "Predicted probability of winning"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 16)

fig_a1
```

![](final_portfolio_cleaned_files/figure-gfm/q1-figures-1.png)<!-- -->

``` r
fig_1a
```

![](final_portfolio_cleaned_files/figure-gfm/q1-figures-2.png)<!-- -->

``` r
fig_1b
```

![](final_portfolio_cleaned_files/figure-gfm/q1-figures-3.png)<!-- -->

``` r
ggsave("figures/pim_distribution.png", fig_a1, width = 11, height = 8, dpi = 200)
ggsave("figures/observed_win_percentage_by_pim.png", fig_1a, width = 11, height = 8, dpi = 200)
ggsave("figures/predicted_win_probability_vs_pim.png", fig_1b, width = 11, height = 8, dpi = 200)
```

## Question 2

**Among teams that take more penalties than their opponent, does high
power-play efficiency mitigate the negative effect?**

``` r
q2_wide <- df %>%
  select(game_id, HoA, team_id, won, pim, powerPlayOpportunities, powerPlayGoals, goals) %>%
  pivot_wider(
    names_from = HoA,
    values_from = c(pim, powerPlayOpportunities, powerPlayGoals, won, goals, team_id),
    names_glue = "{HoA}_{.value}"
  ) %>%
  filter(
    !is.na(home_pim), !is.na(away_pim),
    !is.na(home_powerPlayOpportunities), !is.na(away_powerPlayOpportunities)
  )

analysis_q2 <- bind_rows(
  q2_wide %>%
    transmute(
      game_id,
      location = "home",
      won = home_won,
      goal_diff = home_goals - away_goals,
      pim_self = home_pim,
      pim_opp = away_pim,
      pp_opps_self = home_powerPlayOpportunities,
      pp_goals_self = home_powerPlayGoals
    ),
  q2_wide %>%
    transmute(
      game_id,
      location = "away",
      won = away_won,
      goal_diff = away_goals - home_goals,
      pim_self = away_pim,
      pim_opp = home_pim,
      pp_opps_self = away_powerPlayOpportunities,
      pp_goals_self = away_powerPlayGoals
    )
) %>%
  filter(pp_opps_self > 0) %>%
  mutate(
    more_penalties = pim_self > pim_opp,
    pp_conversion = pp_goals_self / pp_opps_self,
    high_pp_conv = pp_conversion >= 0.25
  ) %>%
  filter(complete.cases(.))

q2_subset <- analysis_q2 %>% filter(more_penalties)

q2_model <- glm(
  won ~ high_pp_conv + pim_self + goal_diff,
  data = q2_subset,
  family = binomial
)

q2_results <- tidy(q2_model, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

q2_results %>%
  select(
    Term = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  ) %>%
  gt() %>%
  tab_header(title = "Question 2 Results")
```

<div id="coqmqrfugu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#coqmqrfugu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#coqmqrfugu thead, #coqmqrfugu tbody, #coqmqrfugu tfoot, #coqmqrfugu tr, #coqmqrfugu td, #coqmqrfugu th {
  border-style: none;
}
&#10;#coqmqrfugu p {
  margin: 0;
  padding: 0;
}
&#10;#coqmqrfugu .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#coqmqrfugu .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#coqmqrfugu .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#coqmqrfugu .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#coqmqrfugu .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#coqmqrfugu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#coqmqrfugu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#coqmqrfugu .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#coqmqrfugu .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#coqmqrfugu .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#coqmqrfugu .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#coqmqrfugu .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#coqmqrfugu .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#coqmqrfugu .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#coqmqrfugu .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#coqmqrfugu .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#coqmqrfugu .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#coqmqrfugu .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#coqmqrfugu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#coqmqrfugu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#coqmqrfugu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#coqmqrfugu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#coqmqrfugu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#coqmqrfugu .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#coqmqrfugu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#coqmqrfugu .gt_left {
  text-align: left;
}
&#10;#coqmqrfugu .gt_center {
  text-align: center;
}
&#10;#coqmqrfugu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#coqmqrfugu .gt_font_normal {
  font-weight: normal;
}
&#10;#coqmqrfugu .gt_font_bold {
  font-weight: bold;
}
&#10;#coqmqrfugu .gt_font_italic {
  font-style: italic;
}
&#10;#coqmqrfugu .gt_super {
  font-size: 65%;
}
&#10;#coqmqrfugu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#coqmqrfugu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#coqmqrfugu .gt_indent_1 {
  text-indent: 5px;
}
&#10;#coqmqrfugu .gt_indent_2 {
  text-indent: 10px;
}
&#10;#coqmqrfugu .gt_indent_3 {
  text-indent: 15px;
}
&#10;#coqmqrfugu .gt_indent_4 {
  text-indent: 20px;
}
&#10;#coqmqrfugu .gt_indent_5 {
  text-indent: 25px;
}
&#10;#coqmqrfugu .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#coqmqrfugu div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Question 2 Results</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Term">Term</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Odds-Ratio">Odds Ratio</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Lower">95% CI Lower</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Upper">95% CI Upper</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Term" class="gt_row gt_left">(Intercept)</td>
<td headers="Odds Ratio" class="gt_row gt_right">7.004000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">5.809000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">8.461000e-01</td>
<td headers="p-value" class="gt_row gt_right">0.0002</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">high_pp_convTRUE</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.065800e+00</td>
<td headers="95% CI Lower" class="gt_row gt_right">8.880000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">1.278400e+00</td>
<td headers="p-value" class="gt_row gt_right">0.4927</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">pim_self</td>
<td headers="Odds Ratio" class="gt_row gt_right">9.867000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">9.756000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">9.974000e-01</td>
<td headers="p-value" class="gt_row gt_right">0.0171</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">goal_diff</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.655634e+09</td>
<td headers="95% CI Lower" class="gt_row gt_right">5.964894e+185</td>
<td headers="95% CI Upper" class="gt_row gt_right">3.492465e+144</td>
<td headers="p-value" class="gt_row gt_right">0.9411</td></tr>
  </tbody>
  &#10;</table>
</div>

``` r
win_rate_summary <- q2_subset %>%
  group_by(high_pp_conv) %>%
  summarise(
    win_rate = mean(won),
    n_games = n(),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(high_pp_conv, "High PP teams", "Other teams")
  )

fig_2a <- ggplot(win_rate_summary, aes(x = group, y = win_rate)) +
  geom_col(fill = "#E69F00", alpha = 0.95) +
  labs(
    title = "Figure 2A. Win percentage when taking more PIM than opponent\nby team power-play efficiency group",
    x = NULL,
    y = "Win percentage"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 16)

pred_q2 <- expand.grid(
  pim_self = seq(0, 40, by = 1),
  high_pp_conv = c(FALSE, TRUE),
  goal_diff = 0
)

pred_q2$prob <- predict(q2_model, newdata = pred_q2, type = "response")

fig_2b <- ggplot(pred_q2, aes(x = pim_self, y = prob, color = high_pp_conv)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("#E69F00", "#56B4E9"),
    labels = c("Other teams", "High PP teams"),
    name = NULL
  ) +
  labs(
    title = "Figure 2B. Predicted win probability vs PIM\nwhen team has more PIM than opponent",
    x = "Team penalty minutes in game (PIM)",
    y = "Predicted probability of winning"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 16)

fig_2a
```

![](final_portfolio_cleaned_files/figure-gfm/q2-figures-1.png)<!-- -->

``` r
fig_2b
```

![](final_portfolio_cleaned_files/figure-gfm/q2-figures-2.png)<!-- -->

``` r
ggsave("figures/high_pp_win_percentage_bar.png", fig_2a, width = 11, height = 8, dpi = 200)
ggsave("figures/high_pp_predicted_win_probability.png", fig_2b, width = 11, height = 8, dpi = 200)
```

## Question 3

**Does the negative effect of PIM differ for home and away teams?**

``` r
analysis_q3_base <- df %>%
  select(game_id, HoA, team_id, won, pim, goals, shots) %>%
  mutate(
    HoA = factor(HoA, levels = c("away", "home")),
    won = as.factor(won),
    across(c(pim, goals, shots), as.numeric)
  ) %>%
  filter(complete.cases(.))

q3_wide <- analysis_q3_base %>%
  select(game_id, HoA, goals, shots, pim, won) %>%
  pivot_wider(
    names_from = HoA,
    values_from = c(goals, shots, pim, won),
    names_glue = "{HoA}_{.value}"
  ) %>%
  filter(!is.na(home_goals), !is.na(away_goals))

analysis_q3 <- bind_rows(
  q3_wide %>%
    transmute(
      game_id,
      HoA = "home",
      won = as.factor(home_won),
      pim = home_pim,
      shots = home_shots,
      goal_diff = home_goals - away_goals
    ),
  q3_wide %>%
    transmute(
      game_id,
      HoA = "away",
      won = as.factor(away_won),
      pim = away_pim,
      shots = away_shots,
      goal_diff = away_goals - home_goals
    )
) %>%
  mutate(HoA = factor(HoA, levels = c("away", "home")))

q3_model_main <- glm(
  won ~ HoA + pim + goal_diff + shots,
  data = analysis_q3,
  family = binomial
)

q3_model_interaction <- glm(
  won ~ HoA * pim + goal_diff + shots,
  data = analysis_q3,
  family = binomial
)

q3_results <- tidy(q3_model_interaction, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

q3_results %>%
  select(
    Term = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  ) %>%
  gt() %>%
  tab_header(title = "Question 3 Results")
```

<div id="zxqqtoxkmp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zxqqtoxkmp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zxqqtoxkmp thead, #zxqqtoxkmp tbody, #zxqqtoxkmp tfoot, #zxqqtoxkmp tr, #zxqqtoxkmp td, #zxqqtoxkmp th {
  border-style: none;
}
&#10;#zxqqtoxkmp p {
  margin: 0;
  padding: 0;
}
&#10;#zxqqtoxkmp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zxqqtoxkmp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#zxqqtoxkmp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#zxqqtoxkmp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#zxqqtoxkmp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#zxqqtoxkmp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zxqqtoxkmp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zxqqtoxkmp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#zxqqtoxkmp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zxqqtoxkmp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#zxqqtoxkmp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#zxqqtoxkmp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zxqqtoxkmp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zxqqtoxkmp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#zxqqtoxkmp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zxqqtoxkmp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#zxqqtoxkmp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zxqqtoxkmp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zxqqtoxkmp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zxqqtoxkmp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zxqqtoxkmp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zxqqtoxkmp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zxqqtoxkmp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zxqqtoxkmp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zxqqtoxkmp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zxqqtoxkmp .gt_left {
  text-align: left;
}
&#10;#zxqqtoxkmp .gt_center {
  text-align: center;
}
&#10;#zxqqtoxkmp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zxqqtoxkmp .gt_font_normal {
  font-weight: normal;
}
&#10;#zxqqtoxkmp .gt_font_bold {
  font-weight: bold;
}
&#10;#zxqqtoxkmp .gt_font_italic {
  font-style: italic;
}
&#10;#zxqqtoxkmp .gt_super {
  font-size: 65%;
}
&#10;#zxqqtoxkmp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zxqqtoxkmp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zxqqtoxkmp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zxqqtoxkmp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zxqqtoxkmp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zxqqtoxkmp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zxqqtoxkmp .gt_indent_5 {
  text-indent: 25px;
}
&#10;#zxqqtoxkmp .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#zxqqtoxkmp div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_heading">
      <td colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Question 3 Results</td>
    </tr>
    &#10;    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" scope="col" id="Term">Term</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="Odds-Ratio">Odds Ratio</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Lower">95% CI Lower</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="a95%-CI-Upper">95% CI Upper</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1" scope="col" id="p-value">p-value</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Term" class="gt_row gt_left">(Intercept)</td>
<td headers="Odds Ratio" class="gt_row gt_right">4.411000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">3.318000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">5.860000e-01</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">HoAhome</td>
<td headers="Odds Ratio" class="gt_row gt_right">9.873000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">8.056000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">1.209800e+00</td>
<td headers="p-value" class="gt_row gt_right">0.9020</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">pim</td>
<td headers="Odds Ratio" class="gt_row gt_right">9.871000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">9.767000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">9.973000e-01</td>
<td headers="p-value" class="gt_row gt_right">0.0144</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">goal_diff</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.748019e+09</td>
<td headers="95% CI Lower" class="gt_row gt_right">7.357061e+135</td>
<td headers="95% CI Upper" class="gt_row gt_right">8.076835e+132</td>
<td headers="p-value" class="gt_row gt_right">0.9074</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">shots</td>
<td headers="Odds Ratio" class="gt_row gt_right">1.016900e+00</td>
<td headers="95% CI Lower" class="gt_row gt_right">1.009000e+00</td>
<td headers="95% CI Upper" class="gt_row gt_right">1.024800e+00</td>
<td headers="p-value" class="gt_row gt_right">0.0000</td></tr>
    <tr><td headers="Term" class="gt_row gt_left">HoAhome:pim</td>
<td headers="Odds Ratio" class="gt_row gt_right">9.934000e-01</td>
<td headers="95% CI Lower" class="gt_row gt_right">9.781000e-01</td>
<td headers="95% CI Upper" class="gt_row gt_right">1.008800e+00</td>
<td headers="p-value" class="gt_row gt_right">0.3975</td></tr>
  </tbody>
  &#10;</table>
</div>

``` r
# Figure 3A: observed home vs away win percentage by PIM
fig_3a_data <- analysis_q3 %>%
  group_by(HoA, pim) %>%
  summarise(
    win_pct = mean(as.numeric(won) - 1),
    .groups = "drop"
  )

fig_3a <- ggplot(fig_3a_data, aes(x = pim, y = win_pct, color = HoA)) +
  geom_line(linewidth = 1.1) +
  geom_point(
    data = fig_3a_data %>% filter(HoA == "home"),
    size = 2.5
  ) +
  scale_color_manual(
    values = c("home" = "#E69F00", "away" = "#56B4E9"),
    labels = c("Away teams", "Home teams"),
    name = NULL
  ) +
  labs(
    title = "Figure 3A. Win percentage vs PIM for home and away teams",
    x = "Team penalty minutes in game (PIM)",
    y = "Observed win percentage"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 16)

# Figure 3B: model-predicted home vs away win probability
pred_q3 <- ggpredict(q3_model_interaction, terms = c("pim [0:40 by=1]", "HoA")) %>%
  as_tibble()

fig_3b <- ggplot(pred_q3, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c("home" = "#E69F00", "away" = "#56B4E9"),
    labels = c("Away teams", "Home teams"),
    name = NULL
  ) +
  labs(
    title = "Figure 3B. Predicted win probability vs PIM for home and away teams",
    x = "Team penalty minutes in game (PIM)",
    y = "Predicted probability of winning"
  ) +
  ylim(0, 1) +
  theme_minimal(base_size = 16)

fig_3a
```

![](final_portfolio_cleaned_files/figure-gfm/q3-figures-1.png)<!-- -->

``` r
fig_3b
```

![](final_portfolio_cleaned_files/figure-gfm/q3-figures-2.png)<!-- -->

``` r
ggsave("figures/home_away_observed_win_percentage.png", fig_3a, width = 11, height = 8, dpi = 200)
ggsave("figures/home_away_predicted_win_probability.png", fig_3b, width = 11, height = 8, dpi = 200)
```

## Saved figure files

``` r
list.files("figures")
```

    ## [1] "high_pp_predicted_win_probability.png"  
    ## [2] "high_pp_win_percentage_bar.png"         
    ## [3] "home_away_observed_win_percentage.png"  
    ## [4] "home_away_predicted_win_probability.png"
    ## [5] "observed_win_percentage_by_pim.png"     
    ## [6] "pim_distribution.png"                   
    ## [7] "predicted_win_probability_vs_pim.png"
