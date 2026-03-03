# Portfolio Report — NHL Win Probability Modeling (R)

## 1. Project goal
This project answers a practical analytics question:

**Which in-game factors are most strongly associated with winning an NHL game, and how does team discipline (penalty minutes) relate to win probability after controlling for performance?**

This is an applied econometrics-style problem: game outcomes are binary (win/loss), and many predictors are correlated. A modeling approach helps quantify associations while holding key confounders constant.

## 2. Why this matters
In sports analytics, teams and analysts often want to separate:
- “We commit more penalties and lose more” (a raw pattern), versus
- “Penalties reduce win probability even after controlling for how well we played” (a controlled estimate)

This is the same logic used in business/finance analytics:
- observed correlation vs. controlled association
- isolating the effect of one variable while holding others constant
- interpreting results in probability/odds rather than only coefficients

## 3. Data structure
- **Unit:** team-game observation (each match appears twice, one row per team).
- **Outcome:** `win` ∈ {0,1}.
- **Predictors:** discipline (PIM), shots, power-play variables, etc.
- **Key controls:**  
  - **Goal differential** (a direct summary of scoring advantage)  
  - **Shots for/against** (proxy for puck control and pressure)

### Cleaning & preparation
Typical preparation steps include:
- removing missing values
- ensuring consistent team perspective (variables measured “for the team” and “for the opponent”)
- building opponent variables by pairing each team with its opponent in the same game
- verifying each game contributes exactly two rows after processing

## 4. Modeling approach

### 4.1 Logistic regression for win probability
Because `win` is binary, the main model is logistic regression:

\[
P(\text{win}=1 \mid X) = \frac{1}{1 + e^{-\eta}}, \quad
\eta = \beta_0 + \beta_1 \text{PIM} + \beta_2 \text{GoalDiff} + \beta_3 \text{ShotsFor} + \beta_4 \text{ShotsAgainst} + \cdots
\]

Interpretation is easiest using **odds ratios**:
- If \(\exp(\beta_1) < 1\), higher PIM is associated with **lower odds of winning**.
- An odds ratio of 0.98 means each additional unit reduces odds by ~2% (approx).

### 4.2 Interaction terms
To test whether an effect differs by context (e.g., home vs away), the model includes interaction terms:

\[
\eta = \cdots + \beta_5 \text{Home} + \beta_6 (\text{Home} \times \text{PIM})
\]

This tests whether penalties “hurt” differently at home vs away, after controlling for performance.

### 4.3 Extensions 
Depending on the question, the project can include:
- Poisson GLM for goals (count outcome)
- subset analysis (e.g., games where team has more PIM than opponent)
- alternative variable definitions (net opportunities, conversion rate thresholds)

## 5. Findings & interpretation (portfolio level)

### 5.1 Discipline (PIM) and win probability
Across controlled logistic regression specifications, **penalty minutes are negatively associated with winning** after controlling for major performance factors. In practical terms:

- When two teams have the same goal differential and similar shot profiles, the model still predicts lower win probability for the team that takes more penalties.
- The per-minute effect may look small, but it accumulates when teams take many penalties.

### 5.2 The role of goal differential
Goal differential dominates win probability, which is expected because it summarizes the scoring outcome. In controlled models, goal differential typically explains a large share of the variation in win outcomes.

**Key point:** This is why controlling for goal differential matters—otherwise many predictors appear significant simply because they correlate with scoring.

### 5.3 Special teams variables and confounding
A common pattern in these data is:
- raw win rates may increase with more power-play opportunities,
- but once goal differential (and sometimes power-play goals) is controlled for, opportunity counts alone may add little predictive value.

This shows a core analytics idea:
> a variable can look important descriptively, but its effect can shrink after accounting for the main drivers.

## 6. Diagnostics & limitations
This is a predictive/associational model using observational data, so I present results as **associations**, not causal claims.

Key limitations:
1. **Endogeneity / reverse causality:** teams that are losing may commit more penalties (frustration, aggressive play), making PIM partly a result of game state.
2. **Omitted variables:** team strength, player injuries, travel fatigue, referee tendencies, and season effects can influence outcomes.
3. **Multicollinearity:** related special-teams variables can be highly correlated and destabilize coefficient estimates.
4. **Independence:** repeated games by the same team suggest considering clustered standard errors or fixed effects in a more advanced version.

## 7. Improvements / next steps 
If extending this project, I would:
- add **team fixed effects** and **season fixed effects**
- cluster standard errors at the **team** or **game** level
- incorporate a **game-state** control (e.g., period-by-period score) to reduce reverse causality
- compare logit vs tree-based models for prediction, while keeping logit for interpretability
- build a small dashboard summarizing predicted win probability under user-selected scenarios

## 8. What this project demonstrates 
This repo demonstrates the same skills used in analytics internships:
- cleaning real-world data, building features, and validating joins
- selecting an appropriate statistical model for the outcome type
- interpreting estimates using odds ratios and uncertainty
- communicating results in clear, decision-oriented language
- writing reproducible analysis in R (script/Rmd)

---

