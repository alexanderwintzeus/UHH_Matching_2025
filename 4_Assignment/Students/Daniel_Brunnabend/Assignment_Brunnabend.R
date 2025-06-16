# Assignment Matching Models
# Daniel Brunnabend
# Student ID 7717842

# Install required packages (only run once)
# install.packages("devtools")
# devtools::install_github("mathieugillen/affinitymatrix")
library(affinitymatrix)

set.seed(777)

# PROBLEM 1: Dupuy and Galichon (2014)

# Load the data
# df_p1 <- read.csv("P1_Data_05.06.2025.csv")
df_p1 <- read.csv("P1_Data.csv")

# Inspect the structure
str(df_p1)

# 1. Number of matches N
N <- nrow(df_p1)  # Dataset only includes matched workers, i.e. rows = numbers of matches
cat("Number of matches N =", N, "\n")

# Create matrices i (workers) and j (jobs)
i <- as.matrix(df_p1[, c("cog_skill", "man_skill")])
j <- as.matrix(df_p1[, c("cog_job", "man_job")])

# 2. Estimate the affinity matrix
res <- estimate.affinity.matrix(i, j, nB = 100)

# Convert to matrix for nicer display
aff_mat_raw <- matrix(res$Aopt, nrow = 2, byrow = TRUE)
se_mat <- matrix(res$sdA, nrow = 2, byrow = TRUE)

# Label for interpretation
rownames(aff_mat_raw) <- c("Cognitive skill (worker)", "Manual skill (worker)")
colnames(aff_mat_raw) <- c("Cognitive demand (job)", "Manual demand (job)")

rownames(se_mat) <- rownames(aff_mat_raw)
colnames(se_mat) <- colnames(aff_mat_raw)

# Display results
cat("Affinity Matrix (point estimates):\n")
print(aff_mat_raw, digits = 3)

cat("\nStandard Errors:\n")
print(se_mat, digits = 3)

# Alternatively
aff_mat <- show.affinity.matrix(res)
print(aff_mat)

# The affinity matrix reveals the contribution of each (worker trait, job trait) pair
# to the match surplus. A higher positive value indicates strong complementarities
# between a worker trait and a job requirement. In our dataset,
# we can see that matches of cognitively skiled workers with cognitively demanding jobs 
# or manually skilled with manually demanding generate a higher surplus.
# Matching manually skilled workers with cognitively demanding jobs contributes positively
# to the surplus (0.0289), however, the estimated coefficient is not statistically significantly 
# different than zero. 


# 3. Perform rank test
test_result <- show.test(res)
print(test_result)

# INTERPRETATION:
# The rank test rejects the null hypothesis that the matrix has rank 1, so the rank is higher.
# We have a 2x2 matrix, so we can conclude the matrix is of full rank.
# This means both cognitive and manual skill dimensions matter.

# Saliency analysis:
# Our conclusion from the rank test, that two dimensions matter for matching, 
# corresponds to having two significant singular values in the singular value decomposition.
# If only one singular value is large and the other near zero, the matrix would be of rank 1.


# PROBLEM 2: Choo and Siow (2006)


# Load data
# df_couples <- read.csv("P2_Data_Couples_05.06.2025.csv")
# df_singles <- read.csv("P2_Data_Singles_05.06.2025.csv")
df_couples <- read.csv("P2_Data_Couples.csv")
df_singles <- read.csv("P2_Data_Singles.csv")

# Fix the couple dataset's column names (they appear messy in the file)
colnames(df_couples) <- c("edu_men", "edu_women")

# Education levels range from 1 to 4
edu_levels <- 1:4

# Split singles dataset into men and women
df_singles_men <- subset(df_singles, gender == "Male")
df_singles_women <- subset(df_singles, gender == "Female")

# Construct matching frequency tables
mu_ij <- table(df_couples$edu_men, df_couples$edu_women)
mu_i0 <- table(df_singles_men$educ)
mu_0j <- table(df_singles_women$educ)

# Initialize surplus matrix Π_ij
Pi <- matrix(NA, nrow = 4, ncol = 4)
for (i in edu_levels) {
  for (j in edu_levels) {
    mij <- mu_ij[as.character(i), as.character(j)]
    mi0 <- mu_i0[as.character(i)]
    m0j <- mu_0j[as.character(j)]
    
    if (mij > 0 & mi0 > 0 & m0j > 0) {
      Pi[i, j] <- sqrt(mij / (mi0 * m0j))
    } else {
      Pi[i, j] <- NA  # Avoid undefined entries
    }
  }
}

# Label the matrix
rownames(Pi) <- paste0("M_", edu_levels)
colnames(Pi) <- paste0("F_", edu_levels)

# Display result
print(Pi, digits = 3)


# 2. INTERPRETATION of surplus matrix Π_ij:
# Π_ij is highest on the diagonal (i = j), this supports
# positive assortative matching (people tend to marry similar education levels).
# Therefore the observed matching is not random, but structured by education.

# 3. Preferences (α_ij, γ_ij):
# TThe nonparametric estimation of Π_ij provides an estimate of the joint surplus generated
# by a match between a type i man and a type j woman. But it does not
# separate this surplus into components attributable to male and female preferences.