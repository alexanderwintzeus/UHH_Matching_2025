##Assignment 2 Kexin Guo

# Import Packages
if (!require("affinitymatrix")) install.packages("affinitymatrix")
library(affinitymatrix)

##Q1 Workers And Jobs

rm(list = ls())

# Set a seed for replication
seed = 9999
set.seed(seed)

# Load the data
data <- read.csv("P1_Data.csv")

# Number of matches
N <- nrow(data)

# Type space: 2 worker traits, 2 job traits
n_i <- 2
f_j <- 2

# Create matrices
i <- as.matrix(data[, c("cog_skill", "man_skill")])
j <- as.matrix(data[, c("cog_job", "man_job")])

# Labels
labels_i <- c("Cognitive Skill", "Manual Skill")
labels_j <- c("Cognitive Demand", "Manual Demand")

# Estimate the affinity matrix
res <- estimate.affinity.matrix(i, j, nB = 100)

# Print the affinity matrix
aff_mat <- show.affinity.matrix(res, labels_i, labels_j)
cat(gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat)))

##Q2 Couples And Singles

rm(list = ls())


# Load data
couples <- read.csv("P2_Data_Couples.csv")
singles <- read.csv("P2_Data_Singles.csv")

## I edited the column names for couples into "educ_h" "educ_w"; for singles into "gender" "educ" 

#Check column names for couples
colnames(couples)

# Check column names for singles
colnames(singles)


# Define education levels (1-4)
edu_levels <- 1:4
n_types <- length(edu_levels)

# Ensure factors include all 4 levels
couples$educ_h <- factor(couples$educ_h, levels = edu_levels)
couples$educ_w <- factor(couples$educ_w, levels = edu_levels)
singles$educ <- factor(singles$educ, levels = edu_levels)

# Count matched couples µ_ij
mu_ij <- table(couples$educ_h, couples$educ_w)

# Count unmatched singles µ_i0 and µ_0j
mu_i0 <- sapply(edu_levels, function(e) sum(singles$gender == "male" & singles$educ == e))
mu_0j <- sapply(edu_levels, function(e) sum(singles$gender == "female" & singles$educ == e))

# Construct Π_ij matrix with small epsilon to prevent divide-by-zero
Pi <- matrix(NA, nrow = n_types, ncol = n_types)
epsilon <- 1e-6  # small constant to avoid division by zero

for (i in 1:n_types) {
  for (j in 1:n_types) {
    denom <- sqrt((mu_i0[i] + epsilon) * (mu_0j[j] + epsilon))
    Pi[i, j] <- mu_ij[i, j] / denom
  }
}

# Label rows and columns
rownames(Pi) <- paste0("H_Educ_", edu_levels)
colnames(Pi) <- paste0("W_Educ_", edu_levels)

# Print results
print(round(Pi, 2))