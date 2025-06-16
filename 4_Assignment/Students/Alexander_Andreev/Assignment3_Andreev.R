###############################################################################
# Assignment 3
# Matching Models
# Alexander Andreev
###############################################################################

library(affinitymatrix)

##------------------------------------------------------
## Problem 1
##------------------------------------------------------

## 1.1 Create two matrices i and j
# Load the dataset
data <- read.csv("/Users/sanjoktherid/Documents/MASTER ECONOMICS/2. Semester/Market Outcomes/Problem Sets/Assignment 3/P1_Data.csv")

# Infer number of matches
N <- nrow(data)

# Create matrices for workers and jobs
i <- as.matrix(data[, c("cog_skill", "man_skill")])
j <- as.matrix(data[, c("cog_job", "man_job")])

# Optional: check dimensions
dim(i)  # Should be N x 2
dim(j)  # Should be N x 2

## 1.2. Estimate the affinity matrix

# Estimate the affinity matrix
res <- estimate.affinity.matrix(i, j, nB = 100)

# Define labels for rows and columns
labels_i <- c("Cognitive Skill", "Manual Skill")
labels_j <- c("Cognitive Requirement", "Manual Requirement")

# Print the Affinity Matrix
aff_mat <- show.affinity.matrix(res, labels_i, labels_j)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat))

## What can we learn about the matching patterns in our data from these estimates? ##
# The diagonal elements (0.40 and 0.41) are large and positive. This indicates strong positive assortative matching.
# Workers with high cognitive skill tend to be matched with cognitively demanding jobs.
# Workers with strong manual skill tend to be matched with jobs requiring manual intensity.

## Provide a correct interpretation of the estimated coefficient related to the worker’s manual skills and the job’s cognitive demand ##
# The affinity matrix is directly linked to the joint surplus.
# The estimated coefficient is -0.07 with a standard error of 0.02.
# It is statistically significant and economically meaningful.
# A worker with higher manual skill is less likely to match with a job that demands cognitive skills.
# This implies negative cross-sorting:
# Manual and cognitive dimensions are not complements in the matching process.
# Instead, they appear to reflect a trade-off or specialization in the labor market.
# It is suggested that these two traits are substitutes in the match surplus:
# - Increasing the worker's manual skill and the job's cognitive requirement 
#   by one standard deviation each reduces the joint surplus by approximately 0.07 units.
# - This implies negative cross-sorting: jobs that require high cognitive input 
#   are less likely to be matched with workers who are strong in manual skills.

# Overall, it can be concluded that the results suggest the labor market rewards specialization:
# - High skill in one dimension aligns with job demands in that same dimension.
# - High manual skill is penalized when cognitive demands are high – and vice versa.

## 1.3. Rank Test

# Perform the rank test
rank_test <- show.test(res)

# Print selected parts of the rank test results
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", rank_test[c(1, 2, 4),]))

## What does this test suggest about the rank of the affinity matrix? ##
# The rank test evaluates the null hypothesis that the affinity matrix A has rank k = 1.
# Since the null is rejected (chi^2 = 140.96, p- < 0.05), 
# we conclude that the matrix has full rank (i.e., rank 2 in this 2×2 case).
# This means that both dimensions—cognitive and manual—are relevant for the matching patterns 
# and contribute independently to the match surplus.

## How does the rank test relate to the singular value decomposition of the affinity matrix? ##
# The rank test is directly related to the saliency analysis.
# In the singular value decomposition, the number of non-zero singular values equals the rank of the matrix.
# Therefore, rejecting rank = 1 implies that at least two singular values are significant, 
# meaning that both latent dimensions are needed to explain sorting in the data.

##------------------------------------------------------
## Problem 2
##------------------------------------------------------

if (!require("readr")) install.packages("readr")
library(readr)

## 2.1. Use the matching function to estimate the surplus
# Load in the data
couples <- read_csv("/Users/sanjoktherid/Documents/MASTER ECONOMICS/2. Semester/Market Outcomes/Problem Sets/Assignment 3/P2_Data_Couples.csv")
singles <- read_csv("/Users/sanjoktherid/Documents/MASTER ECONOMICS/2. Semester/Market Outcomes/Problem Sets/Assignment 3/P2_Data_Singles.csv")

# Define support for education levels
educ_levels = 1:4

# Make sure the education columns are factors with all levels
couples$educ_men   <- factor(couples$educ_h, levels = educ_levels)
couples$educ_women <- factor(couples$educ_w, levels = educ_levels)

singles$educ <- factor(singles$educ, levels = educ_levels)

# Create contingency table for couples mu_ij
mu_ij <- table(couples$educ_men, couples$educ_women)

# Marginal counts for singles
mu_i0 <- table(singles$educ[singles$gender == "Male"])
mu_0j <- table(singles$educ[singles$gender == "Female"])

# Ensure all levels are represented (even with zero counts)
mu_i0 <- mu_i0[as.character(educ_levels)]
mu_0j <- mu_0j[as.character(educ_levels)]

# Replace NAs (if any) with zeros
mu_i0[is.na(mu_i0)] <- 0
mu_0j[is.na(mu_0j)] <- 0

# Initialize Pi matrix
Pi = matrix(NA, nrow = length(educ_levels), ncol = length(educ_levels),
            dimnames = list(paste0("Men_", educ_levels), paste0("Women_", educ_levels)))

# Estimate Pi_ij nonparametrically
for (i in educ_levels) {
  for (j in educ_levels) {
    numerator   = mu_ij[as.character(i), as.character(j)]
    denominator = sqrt(mu_i0[as.character(i)] * mu_0j[as.character(j)])
    
    # Avoid division by zero
    if (denominator > 0) {
      Pi[i, j] = numerator / denominator
    } else {
      Pi[i, j] = NA
    }
  }
}

# Show final matrix
round(Pi, 2)

## 2.2. What can we learn from the Pi_ij estimates?
# The estimated surplus matrix Pi_ij shows clear evidence of systematic matching patterns.
# High and increasing values along the diagonal (e.g., Pi_11 = 0.24, Pi_22 = 0.32, Pi_33 = 0.50, Pi_44 = 0.91)
# suggest positive assortative matching. Individuals with similar education levels are more likely to match.
# Partners tend to assort based on similar education rather than matching randomly.

## How does this relate to random matching?
# Under random matching, Pi_ij would be equal to 1 across all pairs,
# because observed matches would correspond proportionally to singles' marginals.
# As we have mixed the couples dataset with the singles dataset, it is to be expected that all values are smaller than 1. 
# However, that does not mean that the matches are less often than random.
# Rather, one has to look at the relative height of the values compared to other combinations.
# In this case, the elevated diagonal values confirm non-random, positive assortative matching.
# Some off-diagonal values are also non-negligible but generally lower, supporting this pattern.

## 2.3. Does the model allow us to infer anything about male (alpha_ij) and female (gamma_ij) preferences?
# No, the model as currently implemented does not separately identify male preferences and female preferences.
# The surplus estimates Pi_ij capture the joint utility gains from matching pairs,
# but they do not disentangle which part of the surplus comes from the man or the woman.
# To identify alpha_ij and gamma_ij separately, additional structure or assumptions are needed
# (e.g., functional forms, exclusion restrictions, or additional data on individual preferences).
# In the current nonparametric setup, we only observe the total surplus Pi_ij = alpha_ij + gamma_ij.
