
correlation <- round(cor(DATA[,c(4:9,11,21:25)], use = "complete.obs"),2)
correlation[upper.tri(correlation)] <- NA
correlation <- na.omit(reshape2::melt(correlation))

# Create ggplot without NA values and move y-ticks to the right side
ggplot(data = correlation, aes(x = Var2, y = Var1, fill = value)) + 
  geom_tile() +
  geom_text(aes(label = sprintf("%1.2f", value)), size = 4) + # show correlation values with 2 decimal places
  scale_fill_gradient2(low = "red", high = "green", limit = c(-1, 1), name = "Correlation") +
  scale_x_discrete(expand = c(0,0)) + # remove gray areas in x-axis
  scale_y_discrete(expand = c(0,0)) + # remove gray areas in y-axis
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank())



# perform PCA on the predictors
pca <- PCA(na.omit(DATA[,c(4:9,21:25)]), scale.unit = TRUE, ncp = 12, graph = FALSE)

# extract the principal component scores
scores <- pca$ind$coord

# extract the variable importance measures
var_importance <- pca$var$contrib

# plot the variable importance measures
var_df <- data.frame(var_names = rownames(var_importance), importance = var_importance[,1])
ggplot(data = var_df, aes(x = reorder(var_names, importance), y = importance, fill = importance)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title = "Variable Importance by PCA", x = "Variable", y = "Importance")

data_temp <- na.omit(DATA)
xgb_model <- xgboost(
  data = as.matrix(data_temp[,c(4:9,21:25)]), # Exclude response variable from training data
  label = data_temp$HU_Load_Actual, # Response variable
  objective = "reg:squarederror", # Set objective to regression
  nrounds = 100, # Number of boosting rounds
  max_depth = 3, # Maximum tree depth
  eta = 0.3, # Learning rate
  subsample = 0.7, # Subsampling ratio
  colsample_bytree = 0.7 # Feature subsampling ratio
)

# Calculate variable importance
var_imp <- xgb.importance(
  feature_names = colnames(data_temp[,c(4:9,21:25)]), # Names of predictor variables
  model = xgb_model # Trained xgboost model
)

# Plot variable importance
xgb.plot.importance(var_imp)