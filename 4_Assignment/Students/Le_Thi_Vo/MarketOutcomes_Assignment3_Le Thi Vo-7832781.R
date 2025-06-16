#Le Thi Vo - 7832781
#Assignment 3: Matching Models

############################################################################
############################################################################

library (affinitymatrix)


##----------PROBLEM 1
library(readr)

seed <- 777
set.seed(seed)

#data <- read.csv('/Users/thilepetitail/Documents/Master Study in Germany/MSE at UHH/Machine Learning - Python/P1_Data_05.06.2025.csv')
data <- read.csv("P1_Data.csv")
head(data)
names(data)

## 1. Infer the number of matches N from the data
N = nrow(data)
print(N)

## 1. Create two matrices i and j
# Worker matrix: cog_skill and man_skill
i = data[, c("cog_skill", "man_skill")]

# Job matrix: cog_job and man_job
j = data[, c("cog_job", "man_job")]

# View dimensions
dim(i)  
dim(j)

## 2. Estimate the affinity matrix
# Estimate affinity matrix
res = estimate.affinity.matrix(i, j, nB = 100)  

# Define labels
labels_i = c("Cognitive Skill", "Manual Skill")
labels_j = c("Cognitive Job Demand", "Manual Job Demand")

# Show the affinity matrix
aff_mat = show.affinity.matrix(res, labels_i, labels_j)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat))

## 3. Print rank test
rank_test = show.test(res)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", rank_test[c(1, 2, 4),]))


##----------PROBLEM 2
library(expm)

# couples_data <- read.csv('/Users/thilepetitail/Documents/Master Study in Germany/MSE at UHH/Machine Learning - Python/P2_Data_Couples_05.06.2025.csv', header = TRUE, row.names = NULL)
# singles_data <- read.csv('/Users/thilepetitail/Documents/Master Study in Germany/MSE at UHH/Machine Learning - Python/P2_Data_Singles_05.06.2025.csv', header = TRUE, row.names = NULL)
couples_data = read.csv("P2_Data_Couples.csv", header = TRUE)
singles_data = read.csv("P2_Data_Singles.csv", header = TRUE)

print(names(couples_data))
print(names(singles_data))

colnames(couples_data) <- c("educ_h", "educ_w")
colnames(singles_data) <- c("gender", "educ")

couples_data$educ_h <- as.numeric(as.character(couples_data$educ_h))
couples_data$educ_w <- as.numeric(as.character(couples_data$educ_w))
singles_data$educ <- as.numeric(as.character(singles_data$educ))

edu_levels_husbands <- sort(unique(couples_data$educ_h))
edu_levels_wives <- sort(unique(couples_data$educ_w))
edu_levels_singles <- sort(unique(singles_data$educ))

print(edu_levels_husbands)
print(edu_levels_wives)
print(edu_levels_singles)

estimate_surplus <- function(couples_data, singles_data) {
  
  # Extract unique education levels
  edu_levels_husbands <- sort(unique(couples_data$educ_h))
  edu_levels_wives <- sort(unique(couples_data$educ_w))
  
  # Initialize surplus matrix
  surplus_matrix <- matrix(0, nrow = length(edu_levels_husbands), ncol = length(edu_levels_wives),
                           dimnames = list(edu_levels_husbands, edu_levels_wives))
  
  # Compute surplus for each (i, j) pair
  for (i in edu_levels_husbands) {
    for (j in edu_levels_wives) {
      Hij <- sum(couples_data$educ_h == i & couples_data$educ_w == j)  # Number of couples
      mi <- sum(singles_data$educ == i & singles_data$gender == "Male")  # Number of single men
      mj <- sum(singles_data$educ == j & singles_data$gender == "Female")  # Number of single women
      
      if (Hij > 0 && mi > 0 && mj > 0) {
        surplus_matrix[as.character(i), as.character(j)] <- Hij / sqrt(mi * mj)  # Surplus computation
      }
    }
  }
  
  return(surplus_matrix)
}

# Compute surplus matrix
surplus_matrix <- estimate_surplus(couples_data, singles_data)
print(surplus_matrix)

