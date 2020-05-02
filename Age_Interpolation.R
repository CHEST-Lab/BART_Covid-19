# Authors: Tim Große, Sebastian Brinkmann
#          Institute of Geography
#          Friedrich-Alexander-Universitaet Erlangen-Nuernberg
#          Erlangen, Germany
#                               
# e-mail: bastibrinkmann94@gmail.com
# Date:   20.04.2020
#
#
# Note: Here we provide a the method used for the age interpolation. The RKI used different age groups than those
#       provided by the census data from INKAR. Therefore a probabilistic age interpolation has been conducted.
#       This is based on the assumption, that the distribution within the specific age groups is linear (see plot in line 72).


library(dplyr)

# Load RKI data and select age groups
rki <- read.csv("Data/GermanyTraining.csv", stringsAsFactors = F) %>% 
  tbl_df() %>% 
  select(c(S_026, S_027, S_029, S_030, S_031, S_032, S_033, S_034, S_037, S_038, EWZ, NUTS3))

# Create output data.frame
output <- data.frame(NUTS3=character(),
                     A_0_4=double(),       # Age 0-4
                     A_5_14=double(),      # Age 5-14
                     A_15_34=double(),     # Age 15-34
                     A_35_59=double(),     # Age 35-59
                     A_60_79=double(),     # Age 60-79
                     A_80=double(),      # Age 80+
                     stringsAsFactors=FALSE)


pr = txtProgressBar(min = 0, max = nrow(rki), initial = 0, style = 3)

# Loop for every Landkreis in Germany
for (this_row in 1:nrow(rki)) {
  
  # Select current Landkreis and convert to vector
  this_rki <- rki[this_row,1:11] %>%
    unlist()
  
  # Plot original INKAR age distribution
  'barplot((this_rki[1:10] * this_rki[11]), 
          names = c("0-2", "3-5", "6-17", "18-24", "15-29", "30-49", "50-64", "65-74", "75-84", "84+"), 
          xlab = "Age Groups", 
          ylab = "Population", 
          main = "INKAR Age Distribution", 
          col = "#69b3a2")'
  
  
  #  Create sampels from old age groups:
  # x = ages per groupe
  # size = y*1000
  # prob = rep(1/length(x), length(x))
  A_0_2 <- sample(seq(0,2,1), size = this_rki[1]*10000, replace = T, prob = rep(1/3,3))
  A_3_5 <- sample(seq(3,5,1), size = this_rki[2]*10000, replace = T, prob = rep(1/3,3))
  A_6_17 <- sample(seq(6,17,1), size = this_rki[3]*10000, replace = T, prob = rep(1/12,12))
  A_18_24 <- sample(seq(18,24,1), size = this_rki[4]*10000, replace = T, prob = rep(1/7,7))
  A_25_29 <- sample(seq(25,29,1), size = this_rki[5]*10000, replace = T, prob = rep(1/5,5))
  A_30_49 <- sample(seq(30,49,1), size = this_rki[6]*10000, replace = T, prob = rep(1/20,20))
  A_50_64 <- sample(seq(50,64,1), size = this_rki[7]*10000, replace = T, prob = rep(1/15,15))
  A_65_74 <- sample(seq(65,74,1), size = this_rki[8]*10000, replace = T, prob = rep(1/10,10))
  A_75_84 <- sample(seq(75,84,1), size = this_rki[9]*10000, replace = T, prob = rep(1/10,10))
  A_85_100 <- sample(seq(84,100,1), size = this_rki[10]*10000, replace = T, prob = rep(1/17,17))
  
  age_tests <- c(A_0_2, A_3_5, A_6_17, A_18_24, A_25_29, A_30_49, A_50_64, A_65_74, A_75_84, A_85_100) # combine all tests
  
  # Compute Empirical Cumulative Distribution Function
  P <- ecdf(age_tests)
  
  # Plot cumulated probabilities
  #plot(P, main = "Empirical Cumulative Distribution Function", xlab = "Age", ylab = "Cumulative Probability")
  
  # Create probabilities for new age groups
  New_A_0_4 <-  P(4)
  New_A_5_14 <- P(14) - P(4)
  New_A_15_34 <- P(34) - P(14)
  New_A_35_59 <- P(59) - P(34)
  New_A_60_79 <- P(79) - P(59)
  New_A_80_100 <- 1 - P(79)
  
  # Multiply the probability by population to calculate absolute population per age-group
  age_groups <- c(New_A_0_4, New_A_5_14, New_A_15_34, New_A_35_59, New_A_60_79, New_A_80_100) * this_rki[11]
  
  # Plot population per age-group
  'barplot(age_groups, 
          names=c("0-4", "5-14", "15-34", "35-59", "60-79", "80+"), 
          xlab = "Age Groups", 
          ylab = "Population", 
          main = "Interpolated RKI Age Distribution", 
          col="#69b3a2")'
  
  # Include NUTS2 code and results to output
  output[this_row,1] <- rki[this_row,12]
  output[this_row,2:7] <- age_groups
  
  
  setTxtProgressBar(pr, this_row)
}

# Safe as .csv
write.csv(output, "Data/Age_RKI.csv", row.names = F)
