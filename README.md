# Data Mining: Regression, Clustering & Association Rules

Final project for CSCI 221 (Data Mining) at the Community College of Philadelphia, Spring 2026. It covers three end-to-end analyses on three different datasets, done in Python and R: supervised regression, unsupervised clustering, and market-basket rule mining.

## Results at a glance

| Part | Method | Dataset | Key result |
|---|---|---|---|
| 1 | Multiple linear regression (Python and R) | Developer burnout, ~6.9K rows used to fit the model | Test R² = **0.81**, RMSE = **0.30**. The Python and R implementations agree. |
| 2 | K-means clustering, K = 7 (Python) | Music selections, 32.8K tracks (28.6K after removing outliers) | Seven interpretable audio profiles. Silhouette score = **0.11**, so the clusters overlap. |
| 3 | Apriori association rules (R) | Online store transactions | Rules filtered at support ≥ 0.01, confidence ≥ 0.50 and lift > 1.2. Five rules interpreted. |

## Part 1: Multiple linear regression

**Notebooks:** [Python](Part1_Multilinear_Regression_Python.ipynb) · [R](Part1_Multilinear_Regression_R.ipynb)

This part predicts developer burnout level from five predictors: daily work hours, sleep hours, stress level, screen time and exercise hours. It's implemented twice, once with pandas, scikit-learn and statsmodels and once with R's `lm`, to check that the two give the same answer.

- **Cleaning:** checked for duplicates, filled missing numeric values with the median, and flagged outliers using both IQR and z-score rules.
- **Evaluation:** a train/test split. The R model scored training R² 0.820 and test R² 0.808, and RMSE was 0.30 in both versions.
- **Main finding:** **stress level is the only statistically significant predictor** (t ≈ 120, p < 2e-16). Work hours, sleep, screen time and exercise are not significant (all p > 0.39) once stress is in the model. So the high R² comes almost entirely from one variable. That matters if the model is used to suggest interventions, because the other four inputs barely move the prediction.

## Part 2: K-means clustering

**Notebook:** [Python](Part2_KMeans_MusicSelections_Python.ipynb)

This part clusters 32.8K tracks by their audio features: danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo and duration.

- **Preprocessing:** removed outliers by z-score (32,833 → 28,554 tracks), log-transformed skewed features, and standardized everything with `StandardScaler`.
- **Choosing K:** used the elbow method, which pointed to K = 7. The clusters are visualized in 2-D with PCA.
- **Cluster profiles:** the seven clusters separate into recognizable kinds of track. Examples include loud, high-energy tracks (energy around 0.8, loudness around −5 dB), acoustic and mellow tracks (acousticness 0.45, energy 0.48), and speech-heavy tracks (speechiness 0.28).
- **Caveat:** the silhouette score is only 0.11, so the clusters overlap a lot. Audio features change continuously from track to track, so these profiles are useful summaries rather than clean, separate groups.

## Part 3: Association rule mining

**Script:** [R](Part3_Association_Rule_Mining_Online_Store.R)

This part is a market-basket analysis of online-store transactions using `arules`.

- **Preparation:** removed duplicate line items, since they would inflate support, dropped missing values, screened outliers by z-score, and grouped line items into one basket per invoice.
- **Mining:** ran Apriori with support ≥ 0.01 and confidence ≥ 0.50, kept rules with lift > 1.2, and sorted them by lift.
- **Output:** network and grouped plots of the rules, plus an interpretation of five meaningful product associations.

## Running it

| Language | Libraries |
|---|---|
| Python 3 | pandas, numpy, scipy, scikit-learn, statsmodels, matplotlib |
| R | readr, dplyr, ggplot2, Metrics, arules, arulesViz |

The notebooks expect `developer_burnout.csv`, `music_selections.csv` and `online_store.csv` in the working directory. The datasets aren't included in this repo.
