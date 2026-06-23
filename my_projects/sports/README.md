# Basketball & Sports Analytics

I'm a data scientist and statistician with a focus on basketball analytics — building the kinds of tools, reports, and models that support real front-office and coaching decisions. My work spans player evaluation, team performance analysis, game preparation, and predictive modeling, using R, Python, and SQL across play-by-play, box score, and tracking-level data.

The projects below reflect how I approach basketball as an analytical problem: start with a question that matters operationally, build a rigorous framework around it, and communicate findings in a way that's useful to the people making decisions.

---

## Projects

### 🏆 2026 NBA Finals Performance Analysis — New York Knicks vs. San Antonio Spurs
**Tools:** R, hoopR, flexdashboard, ggplot2, tidyverse

A multi-game interactive dashboard analyzing the 2026 NBA Finals (Games 1–5). Built using ESPN play-by-play and box score data via the `hoopR` package. Features include shot charts with full court geometry, team offensive efficiency metrics, value box summaries, and per-game performance breakdowns for both teams. Designed to mirror the kind of nightly reporting and game-prep output used by analytics staffs during a postseason run.

📁 [View project folder](https://github.com/lindseyh251/My-Data-Science-Portfolio/tree/5c62cb3d788a0b746aa2530d5972ac37f3335841/my_projects/sports/basketball/2026_nba_finals_performance_analysis)

---

### 📊 What Drives Winning in the Modern NBA? (2009–2025)
**Tools:** R, tidyverse, ggplot2

A longitudinal team-level analysis spanning 16 seasons of NBA data, identifying the statistical factors most predictive of regular season success. Structured as a front-office style report with insight-led prose, annotated visualizations, and key takeaway callouts. Examines how the league's winning formula has evolved — from post-heavy to pace-and-space — and what that means for roster construction today.

📄 [View PDF report](./What%20Drives%20Winning%20in%20the%20Modern%20NBA_%20(2009%E2%80%932025%20Analysis).pdf) | 📁 [View Rmd source](./nba_team_analysis.Rmd)

---

### 🏀 Indiana Pacers 2025 — Player Impact & Win Probability Modeling
**Tools:** R, tidyverse, ggplot2, caret (logistic regression, random forest)

Two-part project. First, engineered a composite player impact score for the 2025 Pacers by combining game-level efficiency metrics (eFG%, true shooting %, plus/minus per minute) from full-season box score data. Second, aggregated player scores to the lineup level and used them to train logistic regression and random forest models predicting game outcomes across an 82-game season. Both models achieved AUC = 0.70; plus/minus per minute emerged as the strongest individual predictor.

📁 [View project folder](https://github.com/lindseyh251/My-Data-Science-Portfolio/tree/ad290bff45d092b61f8819246f5f4ce2f35fe180/my_projects/sports/basketball/pacers_impact_score_analysis) | 📄 [View win probability PDF](https://github.com/lindseyh251/My-Data-Science-Portfolio/blob/ad290bff45d092b61f8819246f5f4ce2f35fe180/my_projects/sports/basketball/pacers_impact_score_analysis/Pacers_Impact_Players.png)

---

### ⚾ Baseball Operations — Pitcher/Hitter Matchup Analysis
**Tools:** R, tidyverse, ggplot2, Statcast (Baseball Savant)

Analyzed pitch-level Statcast tracking data to build a data-driven at-bat gameplan for a position player facing a specific left-handed pitcher. Profiled the hitter's season-over-season development (2024–2025) across plate discipline, contact quality, and pitch-type performance, then mapped those tendencies against the pitcher's usage profile and Savant zone distribution. Output: actionable strategy broken down by count and damage zone — the format a coaching staff can actually use.

---

### ⚾ Profiling Kansas City Royals Hitters — PCA & Clustering (2024 Season)
**Tools:** R, tidyverse, ggplot2 (k-means clustering, PCA)

Applied k-means clustering and principal component analysis to classify Royals hitters (minimum 25 AB) into performance archetypes using a feature set spanning plate discipline, contact quality, power, and baserunning. Produced a PCA visualization mapping players into three distinct clusters — useful for understanding roster composition, identifying redundancies, and evaluating development trajectories.

---

## Technical Stack

| Area | Tools |
|---|---|
| Languages | R, Python, SQL |
| R packages | hoopR, tidyverse, ggplot2, caret, flexdashboard |
| Python | Pandas, TensorFlow, PyTorch, Jupyter |
| Visualization | ggplot2, Tableau, R Shiny, flexdashboard |
| Data sources | ESPN (hoopR), Baseball Savant (Statcast), nbastatR |

---

*More projects in progress. For questions or collaboration, connect on [LinkedIn](https://www.linkedin.com/in/lindsey-hornberger) or reach out at ljvbcllh@gmail.com.*
