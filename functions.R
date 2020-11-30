############################################################################################

#title: 'Price Elastcity of Demand Analysis - WISH e-Commerce Platform'
#author: "Chetana Katpally"
#subtitle: 'Copyright (c) Chetana Katpally'
#comment: 'Copyright (c) Chetana Katpally'

############################################################################################

## Finding Linear combinations of features ##
#
#"rating_count" is the sum of rating_one_count, rating_two_count, rating_three_count, rating_four_count, rating_five_count. Hence eliminating rating_one_count variable to eliminate linear combination of variables

#"badges_count" is the sum of "badge_local_product", "badge_product_quality", "badge_fast_shipping". Hence eliminating "badge_fast_shipping" to eliminate linear combination of variables

#if (!require(corrplot)) install.packages('corrplot', dependencies=TRUE)
#library(corrplot)

linearCombos <- function(df){
  numeric_cols <- unlist(sapply(df, is.numeric))
  df_subset <- df[,numeric_cols]
  linearCombos <- findLinearCombos(df_subset)
  linearCombos
  remove_cols <- names(df_subset[,linearCombos$remove])
  cat("Removed the following variables that are a linear combination of other predictors: \n", remove_cols)
  remove_col_names <- names(df) %in% remove_cols 
  df <- df[!remove_col_names]
  cat("\n Dimensions of the dataset:\n",dim(df), "\n")
  return (df)
  
}



####Remove zero variance predictors

zeroVar <- function(df){
  zero_variance <- apply(df, 2, function(x) length(unique(x)) == 1)
  
  cat("\n\nVariables with zero sum variance: ", sum(zero_variance))
  
  if (sum(zero_variance) > 0)
  {
    df_zerovar <- as.data.frame(zero_variance)
    df_zerovar <- subset(df_zerovar, zero_variance == TRUE)
    df_zerovar
    remove_col_names <- names(df) %in% rownames(df_zerovar)
    
    cat("\n\nThese predictor variables are eliminated due to zero variance:\n", rownames(df_zerovar))
    names(remove_col_names)
    df <- df[!remove_col_names]
  }
  else{
    cat("\n\n No changes since there are no zero variance variables")
    
  }
  
  cat("\n\n Dimensions of the dataset:\n",dim(df), "\n")
  return (df)
  
}


# Find correlations and Colnames included in final data set
# Change the cutoff value as needed

multiCorr <- function(df, cutoff=.70){
  numeric_cols <- unlist(sapply(df, is.numeric))
  df_subset <- df[,numeric_cols]
  cor_matrix <- findCorrelation(cor(df_subset[,-which(names(df_subset) %in% c("units_sold"))]), cutoff = cutoff , exact=FALSE, names=TRUE)
  cor_matrix
  remove_col_names <- names(df) %in% names(df_subset[, cor_matrix])
  cat("\n\nThese variables are eliminated to eliminate/reduce multicollinearity:\n", names(df_subset[, cor_matrix]))
  final_df <- df[!remove_col_names]
  # Dataset size
  cat("\n\nDimensions of the dataset:\n",dim(final_df), "\n")
  return(final_df)
  
}




## Correlation matrix plot


final_df_cor <- function(df, cutoff=0.50)
{
  numeric_cols <- unlist(sapply(df, is.numeric))
  df <- df[,numeric_cols]
  final_cor_matrix <- findCorrelation(cor(df), cutoff = cutoff , exact=FALSE, names=FALSE)
  final_cor_matrix
  names(df[,final_cor_matrix])
  final_df_cor <- df[, final_cor_matrix]
  dim(final_df_cor)
  cat("The final data set contains the below variables:\n", names(final_df_cor))
  cat("\n Dimensions of the dataset:\n", dim(final_df_cor), "\n")
  return(final_df_cor)
}

### Correlation function
corr_df <- function(df, cutoff=0.50)
{
  
  
  ### Get a correlation of int variables
  numeric_cols <- unlist(sapply(df, is.numeric))
  #### Plot the correlations of all numeric variables ####
  df_corr <- cor(df[,numeric_cols], use="pairwise.complete.obs") #correlations of all numeric variables
  #sort on decreasing correlations with Units Sold
  df_corr_sorted <- as.matrix(sort(df_corr[,'units_sold'], decreasing = TRUE))
  #select only high corelations with Units Sold
  df_high_corr <- names(which(apply(df_corr_sorted, 1, function(x) abs(x)>cutoff)))
  df_corr_matrix <- df_corr[df_high_corr, df_high_corr]
  #return(df_corr_matrix)
  #corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", upper="circle", lower="number")
  corrplot::corrplot(df_corr_matrix, method="number", type="upper", tl.col="black", tl.srt=45)
  
  
}


### Exploratory Data Analysis 

EDA <- function(df)
{
  ### Plot Histograms on the canvas####
  par(mfrow=c(3,2))
  for (i in names(df)){
    #print (is.numeric(df[[i]]))
    if (is.numeric(df[[i]]))
    {
      hist(df[[i]], xlab = i, main = paste("Histogram of ", i, sep=" "))
      
    }
    
  }
  
  ### Plot Boxplots on the canvas####
  par(mfrow=c(3,2))
  for (i in names(df)){
    #print (is.numeric(df[[i]]))
    if (is.numeric(df[[i]]))
    {
      
      boxplot(df[[i]], xlab = i, main = paste("Box Plot of ", i, sep=" "))
      
      
    }
    
  }
  
  ### Plot Barplots of Factor variables on the canvas####
  par(mfrow=c(3,1))
  for (i in names(df)){
    #print (is.numeric(df[[i]]))
    if (is.factor(df[[i]]))
    {
      
      plot(df[[i]], xlab = i, main = paste("Bar Plot of ", i, sep=" "))
      
      
    }
    
  }
}




#### Transform Data for select variables


transformData <- function(df){
  preprocessParams <- caret::preProcess(df, method=c("YeoJohnson"), verbose=TRUE)
  transformed <- predict(preprocessParams, df)
  summary(transformed)
  return (list(transformed=transformed, preprocessParams=preprocessParams))
  
}


### LS, PCR and LM regression
pls_pcr<- function(train, test, method){
  
  model.fit <- train(units_sold ~ ., data = df_train, method = method,
                     trControl = trainControl("cv", number = 10),
                     tuneLength = 10
  )
  plot(model.fit)
  
  model.fit$bestTune
  summary(model.fit$finalModel)
  coefs <- coef(model.fit$finalModel, ncomp = model.fit$bestTune$ncomp, intercept = TRUE)
  
  # Make predictions on Train set
  pred.train <- model.fit %>% predict(df_train)
  # Model performance metrics
  
  RMSE.train <-  caret::RMSE(pred.train, df_train$units_sold)
  Rsquare.train <- caret::R2(pred.train, df_train$units_sold)
  
  
  # Make predictions on Test set
  pred.test <- model.fit %>% predict(df_test)
  # Model performance metrics
  
  RMSE.test <- caret::RMSE(pred.test, df_test$units_sold)
  Rsquare.test <- caret::R2(pred.test, df_test$units_sold)
  
  model.stats <- list(mse.train=RMSE.train, rsq.train=Rsquare.train, mse.test=RMSE.test, rsq.test=Rsquare.test, coefs=coefs)
  
  
  return (model.stats)
  
}








### Compute SSE and MSE functions

compute_SSE <- function(predictions, actual){
  return(sum((predictions-actual)^2))
}

compute_MSE <- function(predictions, actual){
  return(mean((predictions-actual)^2))
}


