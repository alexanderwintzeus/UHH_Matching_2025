###############################################################################
# Introduction to Microeconometrics
# SoSe 2025
# Author: Reimy Liou
###############################################################################
# Assignment 3 : Matching Models
###############################################################################

rm(list = ls())
getwd()
setwd("/Users/reimyliou/market_outcomes/r_studio/Assignment 3")

# load libraries
library(dplyr)
library(tidyverse)
library(affinitymatrix)

###############################################################################

# ------------------------------------------------------
# Problem 1 – Dupuy and Galichon (2014) Matching Model
# ------------------------------------------------------

# Load data
P1_data <- read.csv("P1_Data.csv")

# Q1: Infer number of matches and create i and j matrices
N <- nrow(P1_data)
i <- as.matrix(P1_data[, c("cog_skill", "man_skill")])
j <- as.matrix(P1_data[, c("cog_job", "man_job")])

# Q2: Estimate the affinity matrix
res_P1 <- estimate.affinity.matrix(i, j, nB = 100)

# Labels for table output
labels_worker <- c("Cognitive Skill", "Manual Skill")
labels_job <- c("Cognitive Demand", "Manual Demand")

# Print out Results and Affinity Matrix 
aff_mat_P1 <- show.affinity.matrix(res_P1, labels_worker, labels_job)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat_P1))

# cog skill x cog demand = 0.40
# manual skill x cog demand = -0.07
# cog skill x manual demand = 0.03
# manual skill x manual demand = 0.41

# Print the Diagonal Elements of Affinity Matrix
aff_diag <- show.diagonal(res_P1, labels = labels_worker)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_diag))

# The affinity matrix captures how the characteristics of agents on one side (e.g., workers) interact with those on the other side (e.g., firms or jobs) in the matching process. It summarizes the matching logic in the data.

# Interpretation of matching patterns:
# The diagonal elements (0.40 and 0.41) are large and positive. This indicates strong positive assortative matching (cog skill/demand and manual demand/skill)
# Workers with high cognitive skills tend to be matched with cognitively demanding jobs, and workers with strong manual skills tend to be matched with manual demanding jobs

# Interpretation of cross terms (e.g., manual skill x cognitive demand):
# The estimated coefficient is negative (-0.07) with a standard error of 0.02.
# This implies that workers with high manual skill are less likely to match with jobs that demand higher cognitive specialization.

# Overall Conclusion:
# The results suggest the labor market rewards specialization which can be seen across the diagonal elements.
# High skill in one dimension aligns with job demands in that same dimension.


# Q3: Print Rank Test
rank_test_P1 <- show.test(res_P1)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", rank_test_P1[c(1,2,4),]))
# H_0: rk(A) = 1
# chi^(2) = 140.96, rejected = yes 

# NOTE: If only one singular value is significant, it suggests the affinity matrix is rank 1, indicating a sorting of a single underlying trait. On the other hand, a higher rank means a richer sorting structure where a combination of different traits is present.
# Since the rank test rejects the hypothesis that the rank of the affinity matrix is one, this suggests that the rank is at least two.

# Saliency Analysis
saliency_P1 <- show.saliency(res_P1, labels_x = labels_worker, labels_y = labels_job, ncol_x = 2, ncol_y = 2)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n|$", "", saliency_P1$U.table))
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n|$", "", saliency_P1$V.table))

# Saliency analysis decomposes the affinity matrix into orthogonal components using singular value decomposition (SVD).
# The rank test complements this by statistically testing how many dimensions (singular values) are relevant for matching. In other words, what are people sorting on? vs how many types of sorting exists?
# Since the rank test suggests that rank value = 2, the first two singular values are statistically significant
# Saliency analysis tells you: 
# Which worker traits (e.g., cognitive or manual skill) are dominant in each dimension (U.table for X-side)
# Which job traits (e.g., cognitive or manual demand) align with those dimensions (V.table for Y-side)

# Plot Correlations
show.correlations(res_P1, labels_x = labels_worker, labels_y = labels_job, label_x_axis = "Worker", label_y_axis = "Job", ndims = 2)



# ------------------------------------------------------
# Problem 2 – Choo and Siow (2006) Matching Function
# ------------------------------------------------------

# Load data
couples <- read.csv("P2_Data_Couples.csv", row.names = NULL)
singles <- read.csv("P2_Data_Singles.csv", row.names = NULL)

# Rename column names
colnames(couples) <- c("educ_h", "educ_w")
colnames(singles) <- c("gender", "educ")

# Define education levels (1 = dropout, 4 = college)
educ_levels = 1:4

# Make sure the education columns are factors with all levels
couples$educ_men   <- factor(couples$educ_h, levels = educ_levels)
couples$educ_women <- factor(couples$educ_w, levels = educ_levels)

singles$educ <- factor(singles$educ, levels = educ_levels)

# Create contingency table for couples µ_ij
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

# Initialize Π matrix
Pi = matrix(NA, nrow = length(educ_levels), ncol = length(educ_levels),
            dimnames = list(paste0("Men_", educ_levels), paste0("Women_", educ_levels)))

# Estimate Π_ij nonparametrically
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

# Q2: What can we learn about the matching patterns in our data from these estimates? How does this relate to random matching?
# The nonparametric surplus matrix tells you how often men of education level i match with women of education level j, relative to random matching.
# The diagonal entries (e.g. Π11, Π22, etc) are higher than most off-diagonal values, suggesting strong positive assortative matching: people tend to marry others with similar education levels
# Matches like Π14 = 0.01 and Π41 = 0.00, imply that there is almost no matching between high-school dropouts and college graduates suggesting educational barriers or mismatching against mismatched pairs 
# Relation to random matching: Random matching would imply all Π = 1, but here, most values are less than 1, and the pattern is clear: the closer the education levels, the higher the surplus. This means that match outcomes are not random - there are preferences/frictions that make similar individuals more likely to marry each other.

# Q3: Male and Female Preferences?
# Male and female preferences cannot be inferred from the matrix alone. The estimated surplus matrix only captures joint surplus from the match — it reflects how often matches occur relative to random matching. Perhaps with parametric modeling, instrumental variables/additional data, both sides' preferences would be identified. 








