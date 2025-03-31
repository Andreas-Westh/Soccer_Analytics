

# sinuglar tree plot
simple_tree_plot <- rpart.plot(tree_model, type = 2, extra = 104, fallen.leaves = TRUE,
           box.palette = "RdYlGn", main = "Beslutningstræ (singular)")



# RF ntree loop
rf_ntree_loop <- ggplot(auc_df, aes(x = ntree, y = AUC)) +
  geom_line() +
  geom_point() +
  labs(title = "AUC bliver stabil ved 5000 træer",
       x = "Number of Trees",
       y = "AUC") +
  theme_minimal()
