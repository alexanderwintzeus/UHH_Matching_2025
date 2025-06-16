## Assignment 3 -- Matching Model

# Import Packages
if (!require("affinitymatrix")) install.packages("affinitymatrix")
library(affinitymatrix)

# Problem 1 -- Load in the data set P1_Data.csv.
#setwd("/Users/inessatunieva/Desktop/6_Market Outcomes/Matching Model")
# data <- read.csv("P1_Data_05.06.2025.csv")
data <- read.csv("P1_Data.csv")

#1. -- Infer the number of matches N from the data. Create two matrices i and j containing the
#      observations for workers and jobs, respectively.

N <- nrow(data) # Number of matches
i <- as.matrix(data[, c("cog_skill", "man_skill")]) # Matrix of worker characteristics (cognitive and manual skill)
j <- as.matrix(data[, c("cog_job", "man_job")]) # Matrix of job characteristics (cognitive and manual job requirement)

#2. -- Estimate the affinity matrix using the estimate.affinity.matrix() function and print out
#      the results using show.affinity.matrix().

# Estimate the affinity matrix using worker (i) and job (j) characteristics
res <- estimate.affinity.matrix(i, j, nB = 100)

# Define labels for worker and job characteristics
labels_i <- c("Cognitive Skill", "Manual Skill")   # for workers
labels_j <- c("Cognitive Job", "Manual Job")       # for jobs

# Show the Affinity Matrix with labels
aff_mat <- show.affinity.matrix(res, labels_i, labels_j)

# Print in cleaned-up format
cat(gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat)))

#Result:  see in  pdf file

#3. -- Print out the results of the rank test using the show.test() function.
rank_test =show.test(res) #Rank Test - Print Results
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", rank_test[c(1,2,4),]))


## Problem 2 -- Load in the data sets P2_Data_Couples.csv and P2_Data_Singles. The data contain 
#               information on the educational attainment of a sample of couples and a sample of 
#               single men and women, respectively. Education is discrete, and ranges from 1 (high 
#               school dropouts) to 4 (college education).

#setwd("/Users/inessatunieva/Desktop/6_Market Outcomes/Matching Model")
data_couples <- read.csv("P2_Data_Couples.Csv", header = TRUE, row.names = NULL, check.names = TRUE)
# data_couples <- read.csv("P2_Data_Couples_05.06.2025.csv", header = TRUE, row.names = NULL, check.names = TRUE)
colnames(data_couples) <- c("educ_h", "educ_w") # renaming as it is supposed to be
data_singles <- read.csv("P2_Data_Singles.Csv", header = TRUE, row.names = NULL, check.names = TRUE)
#data_singles <- read.csv("P2_Data_Singles_05.06.2025.csv", header = TRUE, row.names = NULL,check.names = TRUE)
colnames(data_singles) <- c("gender", "educ") # renaming as it is supposed to be

# 1. 

library(dplyr)

mu_ij <- data_couples %>%
  count(educ_h, educ_w) %>%
  tidyr::pivot_wider(names_from = educ_w, values_from = n, values_fill = 0)

# Extract educ levels
male_types <- mu_ij$educ_h
mu_ij_matrix <- as.matrix(mu_ij[,-1])
rownames(mu_ij_matrix) <- male_types

# Single men
mu_i0 <- data_singles %>%
  filter(gender == "Male") %>%
  count(educ) %>%
  tibble::deframe()

# Single women
mu_0j <- data_singles %>%
  filter(gender == "Female") %>%
  count(educ) %>%
  tibble::deframe()

# Create surplus matrix
surplus_matrix <- matrix(NA, 
                         nrow = nrow(mu_ij_matrix), 
                         ncol = ncol(mu_ij_matrix),
                         dimnames = list(rownames(mu_ij_matrix), colnames(mu_ij_matrix)))

for (i in rownames(mu_ij_matrix)) {
  for (j in colnames(mu_ij_matrix)) {
    muij <- mu_ij_matrix[i, j]
    mui0 <- mu_i0[i]
    mu0j <- mu_0j[j]
    
    # Only compute if all values are present
    if (!is.na(muij) && !is.na(mui0) && !is.na(mu0j)) {
      surplus_matrix[i, j] <- muij / sqrt(mui0 * mu0j)
    }
  }
}
surplus_matrix
