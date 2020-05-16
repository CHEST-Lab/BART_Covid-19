# Authors: Blake Byron Walker, Sebastian Brinkmann, Tim Große
#          Institute of Geography
#          Friedrich-Alexander-Universitaet Erlangen-Nuernberg
#          Erlangen, Germany
#                               
# e-mail: bastibrinkmann94@gmail.com
# Date:   17.04.2020
#
#
# Note: Here we provide a brief overview of our procedure with the key methods used in our analysis.
#       Due to random seeds, results may vary to a limited degree.
#       Users can set serialize=TRUE in the bartMachine() function to save a specific model for a new session.



# Set to use 45 GB memory - adjust this to your resources
options(java.parameters = "-Xmx45g")

# Load packages
library(bartMachine)
library(dplyr)
library(psych)


# Set to run on 20 threads - adjust this to your resources
set_bart_machine_num_cores(20)

# Function for linear stretching. New range: 0-1
range0_1 <- function(x){(x-min(x))/(max(x)-min(x))}


#### Data ####
data <- read.csv("Data/GermanyTraining.csv", stringsAsFactors = F) %>%
  mutate(Rch_den = range0_1(Rch_den))

# Select variables: Lat/ Long, BL, NUTS2, socioeconomic, build environment and age adjusted case rate
data <- data[c(3, 4, 5, 28, 38, 65:372, 374)]

# Factorise
data$NUTS2_Fact  <- as.factor(data$NUTS2_Fact)
data$BL_ID  <- as.factor(data$BL)
data$S_109  <- as.factor(data$S_109)


# Response variable
#
# Note: AdjRate is highly skewed (skewness = 3.64) and was therefore log-transformed.
#       The results from the BART machine won't differ with the highly skewed data, since it is 
#       non-parametric. However for all the figures in the paper we used the log-rate for higher 
#       visual interpretability. Thus - to get the same units in the PD-Plots later - we used 
#       the log-rate here, too.

psych::skew(data$AdjRate)

y <- log(data$AdjRate)

data_bm <- select(data, -c(AdjRate))


#### 1st BART machine ####
# Grid-search to obtain the optimal hyperparameters
bm_cv_All <- bartMachineCV(X = data_bm, y = y, 
                           num_tree_cvs = c(200, 300, 400, 500, 600), 
                           num_iterations_after_burn_in=2000, 
                           num_burn_in = 300)

# Winning bartMachine: k: 3 nu: 3, q: 0.9 m: 300 
bm_All <- bartMachine(X = data_bm, y = y, 
                      k=3, nu=3, q=0.90, num_trees=300, 
                      num_iterations_after_burn_in=2000, 
                      num_burn_in = 300)

summary(bm_All)


# Basic statistic plots:
plot_convergence_diagnostics(bm_All)
check_bart_error_assumptions(bm_All)
plot_y_vs_yhat(bm_All,credible_intervals = T)
get_sigsqs(bm_All, plot_hist = T, plot_sigma = T)


# Returns variable importance for tree structure by "splits". 
splits_all <- investigate_var_importance(bm_All, type="splits", num_replicates_for_avg = 50, num_trees_bottleneck = 20)


# Variable selection - Returns the most important variables for the bm_All model.
#
# Note: Since I didn't set a seed the results from the var_selection might slightly differ!
#       However the "Top-10" variables proved to be consistent even when iterating over the
#       var_selection_by_permute_cv()-function multiple times with slightly different settings for
#       the number of trees. However this empirical "test" is very time consuming even with a high
#       performance system! Therefore we did not include this step in this summary.
#
#       Furthermore we removed NUTS2 and BL_ID since only a few of these factors were important.
#       Down below you can find our subset of the important variables.

# Leave the num_trees_for_permute small, to force variables to compete for entry into the model!
var_sel <- bartMachine::var_selection_by_permute_cv(bm_All, num_trees_for_permute = 20)

# Look at the most important variables
var_sel$important_vars_cv





#### 2nd BART machine ####
# Subset of important variables
data_subset <- data_bm %>%
  select(c(
    # Geographical Units
    X, #Longitude
    Y, #Latitude
    
    # Political units
    S_109, #rural/urban
    
    # Socioeconomic
    EWZ, #Population
    Pop_Den, #Population density
    S_004, #Unemployment rate under 25
    S_006, #Household income per capita 
    S_020, #Employment rate 15-<30
    S_051, #voter participation
    S_054, #Apprenticeship positions
    S_066, #Haushaltseinkommen
    S_070, #Deptors rate
    S_080, #Recreationl Space
    S_104, #Income tax
    S_107, #Steuerkraft
    S_115, #Residents/Workplace density
    S_123, #Child poverty
    S_130, #IC train station access
    S_146, #Commuters >150km
    S_153, #Foreign guests in tourist establishments
    S_170, #Longterm unemployment rate
    
    # Built environment
    Rch_den, #church density
    play_dn, #Playground density
    bir_km2, #Biergarten per km²
    ff_pop, #Fast food places per capita
    hair_pp, #Hairdresser per capita
    thea_pp, #Theatre per capita
    cc_pop, #community centre density
    sch_den, #school density
    kid_den #Kindergarten density
  ))


# Grid-search to obtain the optimal hyperparameters
bm_cv_final <- bartMachineCV(X=data_subset, y=y, 
                             num_tree_cvs = c(50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300))

# Winning bartMachine: k: 5 nu: 3, q: 0.99 m: 200 
bm_final <- bartMachine(X = data_subset, y = y, 
                        k=5, nu=3, q=0.99, num_trees=200)
summary(bm_final)


# Basic statistic plots:
plot_convergence_diagnostics(bm_final)
check_bart_error_assumptions(bm_final)
plot_y_vs_yhat(bm_final,credible_intervals = T)
get_sigsqs(bm_final, plot_hist = T, plot_sigma = T)


# Returns variable importance for tree structure by "splits". 
splits_final <- investigate_var_importance(bm_final, type="splits", num_replicates_for_avg = 50, num_trees_bottleneck = 20)



#### Residual Map ####
library(sf)
library(RColorBrewer)
library(tmap)

# Join residuals to shapefile, then map residuals
shp <- read_sf("Data/RKI_age")  #### Your filepath must be defined here.
shp$resid <- bm_final$residuals
res_map <- tm_shape(shp) + tm_polygons(col="resid", title="BART Machine Residuals\n(log incidence rate)", 
                                       breaks=seq(-1.75, 1.75, 0.5), midpoint=NA, palette="RdBu") + 
  tm_layout(frame = F,
            inner.margins=c(0.02, 0.02, 0.02, 0.30),
            legend.position = c(0.7,0.19),
            legend.frame = T,
            legend.outside = F, 
            bg.color = "white")



#### PD-Plots for the Top-10 variables ####
# Note: We decided to further analyse only the "Top-10" variables. As already mentioned at the
#       var_selection section, the results slightly differ every time. So the order of "Top-10"
#       from the splits_final might vary slightly, too.
#       In the paper we opened the pd_plot()-function and slightly adjusted the return() to
#       generate a ggplot2 version of it.


# Set parameters to plot PDP top and histogram bottom
par(mfrow = c(2,1))

# 1.Rch_den
Rch_den.pd <- pd_plot(bm_final, "Rch_den", levs = c(0.0, seq(0, 1, 0.1), 1))
Rch_den.hist <- hist(bm_final$X$Rch_den, 20)

# 2. Y (Lat)
lat.pd <- pd_plot(bm_final, "Y", levs = c(0.0, seq(0, 1, 0.1), 1))
lat.hist <- hist(bm_final$X$Y, 20)

# 3. X (Long)
long.pd <- pd_plot(bm_final, "X", levs = c(0.0, seq(0, 1, 0.1), 1))
long.hist <- hist(bm_final$X$X, 20)

# 4. S_051
S_051.pd <- pd_plot(bm_final, "S_051", levs = c(0.0, seq(0, 1, 0.1), 1))
S_051.hist <- hist(bm_final$X$S_051, 20)

# 5. S_153
S_153.pd <- pd_plot(bm_final, "S_153", levs = c(0.0, seq(0, 1, 0.1), 1))
S_153.hist <- hist(bm_final$X$S_153, 20)

# 6. S_130
S_130.pd <- pd_plot(bm_final, "S_130", levs = c(0.0, seq(0, 1, 0.1), 1))
S_130.hist <- hist(bm_final$X$S_130, 20)

# 7. S_020
S_020.pd <- pd_plot(bm_final, "S_020", levs = c(0.0, seq(0, 1, 0.1), 1))
S_020.hist <- hist(bm_final$X$S_020, 20)

# 8. S_115
S_115.pd <- pd_plot(bm_final, "S_115", levs = c(0.0, seq(0, 1, 0.1), 1))
S_115.hist <- hist(bm_final$X$S_115, 20)

# 9. S_170
S_170.pd <- pd_plot(bm_final, "S_170", levs = c(0.0, seq(0, 1, 0.1), 1))
S_170.hist <- hist(bm_final$X$S_170, 20)

# 10. S_004
S_004.pd <- pd_plot(bm_final, "S_004", levs = c(0.0, seq(0, 1, 0.1), 1))
S_004.hist <- hist(bm_final$X$S_004, 20)



#### Out of sample model validation ####
ger_north <- data %>% 
  mutate(subregrion = factor(ifelse(BL_ID %in% 5:10, "South", "North"), levels = c("South", "North"))) %>% 
  filter(subregrion == "North")

ger_south <- data %>% 
  mutate(subregrion = factor(ifelse(BL_ID %in% 5:10, "South", "North"), levels = c("South", "North"))) %>% 
  filter(subregrion == "South")


# Create Out-of-Sample DataFrame
OOS_metrics <- data.frame(test_rmse = double(),
                          test_R2 = double(),
                          train_rmse = double(),
                          train_R2 = double(),
                          train_adjR2 = double())


# 20 fold iteration of calculating out-of-sample RMSE and R² by splitting the data in 
# test (N = 100) and train (N = 301).  

iterations = 20
pr = txtProgressBar(min = 0, max = iterations, initial = 0, style = 3)
for (i in 1:iterations) {
  # Take random row-samples for each region and bind together
  sample_north = sample_n(ger_north, 35)
  sample_south = sample_n(ger_south, 65)
  test <- bind_rows(sample_north, sample_south)
  
  # Take the remainder of the data as train data
  train <- anti_join(data, test)
  
  # Exlude test X and y (AdjRate)
  test.y <- log(test$AdjRate)
  test.X <- test %>% 
    select(c(
      # Geographical Units
      X, #Longitude
      Y, #Latitude
      
      # Political units
      S_109, #rural/urban
      
      # Socioeconomic
      EWZ, #Population
      Pop_Den, #Population density
      S_004, #Unemployment rate under 25
      S_006, #Household income per capita 
      S_020, #Employment rate 15-<30
      S_051, #voter participation
      S_054, #Apprenticeship positions
      S_066, #Haushaltseinkommen
      S_070, #Deptors rate
      S_080, #Recreationl Space
      S_104, #Income tax
      S_107, #Steuerkraft
      S_115, #Residents/Workplace density
      S_123, #Child poverty
      S_130, #IC train station access
      S_146, #Commuters >150km
      S_153, #Foreign guests in tourist establishments
      S_170, #Longterm unemployment rate
      
      # Built environment
      Rch_den, #church density
      play_dn, #Playground density
      bir_km2, #Biergarten per km²
      ff_pop, #Fast food places per capita
      hair_pp, #Hairdresser per capita
      thea_pp, #Theatre per capita
      cc_pop, #community centre density
      sch_den, #school density
      kid_den #Kindergarten density
    ))
  
  # Exlude train X and y (AdjRate)
  train.y <- log(train$AdjRate)
  train.X <- train %>% 
    select(c(
      # Geographical Units
      X, #Longitude
      Y, #Latitude
      
      # Political units
      S_109, #rural/urban
      
      # Socioeconomic
      EWZ, #Population
      Pop_Den, #Population density
      S_004, #Unemployment rate under 25
      S_006, #Household income per capita 
      S_020, #Employment rate 15-<30
      S_051, #voter participation
      S_054, #Apprenticeship positions
      S_066, #Haushaltseinkommen
      S_070, #Deptors rate
      S_080, #Recreationl Space
      S_104, #Income tax
      S_107, #Steuerkraft
      S_115, #Residents/Workplace density
      S_123, #Child poverty
      S_130, #IC train station access
      S_146, #Commuters >150km
      S_153, #Foreign guests in tourist establishments
      S_170, #Longterm unemployment rate
      
      # Built environment
      Rch_den, #church density
      play_dn, #Playground density
      bir_km2, #Biergarten per km²
      ff_pop, #Fast food places per capita
      hair_pp, #Hairdresser per capita
      thea_pp, #Theatre per capita
      cc_pop, #community centre density
      sch_den, #school density
      kid_den #Kindergarten density
    ))
  
  # Build BART machine from train data
  bm_train <- bartMachineCV(X = train.X, y = train.y)
  
  # Use the predict function from the BART package to calculate the predicting and rmse 
  bm_test <- bartMachine::bart_predict_for_test_data(bm_train, Xtest = test.X, ytest = test.y)
  
  # Use predicted values of Y to calculate R² / adj. R²
  lm_train <- lm(bm_test$y_hat ~ test.y) %>% 
    summary()
  
  OOS_metrics[i,] <- c(bm_train$rmse_train, bm_train$PseudoRsq, bm_test$rmse, lm_train$r.squared, lm_train$adj.r.squared)
  setTxtProgressBar(pr, i)
}


# Mean
apply(OOS_metrics, 2, mean)

# SD
apply(OOS_metrics, 2, sd)
