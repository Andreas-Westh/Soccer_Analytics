# Soccer Analytics: xG, xP & Player Clustering in the Danish Superliga
This project explores football analytics through expected goals (xG), expected points (xP), and player clustering, built entirely on Wyscout data from the Danish Superliga 2023/2024 and the ongoing 2024/2025 season.

![xG Field Plot](Images/xG_Field.png)
*Visualizing shot quality using model-predicted xG values across the pitch.*


The repo contains:
- **xG Model**: A machine learning model predicting the probability of a shot becoming a goal, trained on 80% of the shots from the 2023/2024 season.  
  - Feature engineering includes shot distance, angle, and body part used.  
  - Variable selection was based on domain knowledge, correlation analysis, and the Boruta method.  
  - Several models were tested: logistic regression (GLM), decision trees, Random Forest, and XGBoost.  
  - XGBoost performed best with an AUC of **0.7774** on the test set (20% split), and **0.7193** when tested on a separate ongoing season dataset (around the same amount of shots as in the whole training data).  

- **xP Model**: Uses the xG models predictions to simulate match outcomes and calculate team-level expected points in the 2023/2024 season. It captures over- or underperformance across a the season.

- **Player Clustering**: Uses unsupervised learning to group players by passing behavior and performance profiles. Built mostly for exploration and fun.

Data preprocessing, modeling, and visualization were done in R, with interactive dashboards created in Shiny.


> Other data outside of Wyscout was scraped using a combination of R and Python — including Playwright for scraping team rankings from Flashscore and player profiles from FifaIndex.
