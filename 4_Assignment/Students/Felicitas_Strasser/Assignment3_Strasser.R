###############################################################################
# Assignment 2 - Matching Models
###############################################################################

# Load necessary packages
if (!require("affinitymatrix")) install.packages("affinitymatrix")
library(affinitymatrix)

# Set seed for reproducibility
set.seed(1234)

###############################################################################
# Problem 1
###############################################################################

# Load the data
# data <- read.csv("~/2. Semester/Empirical Evaluation of Market Outcomes/Assignment/Assignment 3/P1_Data_05.06.2025.csv")
data <- read.csv("P1_Data.csv")

# Check the structure
str(data)

###############################################################################
# Part 1.

# Infer number of observations (matches)
N <- nrow(data)
cat("Number of matches (N):", N, "\n")

# Create the worker matrix (i): cognitive and manual skills
i <- as.matrix(data[, c("cog_skill", "man_skill")])

# Create the job matrix (j): cognitive and manual job demands
j <- as.matrix(data[, c("cog_job", "man_job")])

###############################################################################
# Part 2.

# Define labels
labels_i <- c("Cognitive Skill", "Manual Skill")
labels_j <- c("Cognitive Job Demand", "Manual Job Demand")

# Estimate the affinity matrix
res <- estimate.affinity.matrix(i, j, nB = 100)

# Print the affinity matrix
aff_mat <- show.affinity.matrix(res, labels_i, labels_j)
cat("\nAffinity Matrix:\n")
cat(gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat)), "\n")

# Worker cognitive skill x Job cognitive demand: 0.40 (0.03)
### Workers with higher cognitive skills are more likely to be matched with jobs that demand cognitive ability.
### 1SD increase in worker cognitive skill and job cognitive demand increases the joint surplus by 0.40 units.

# Worker manual skill x Job cognitive demand: -0.07 (0.02)
### Workers with strong manual skills are less likely to match with cognitively demanding jobs (neg. cross-dimensional sorting).
### Each SD increase in manual skill and cognitive job demand reduces the match surplus by 0.07 units.

# Worker cognitive skill x Job manual demand: 0.03 (0.02) 
### Weak, stat. insignificant tendency for cognitive skilled workers to be matched with manual jobs

# Worker manual skill x Job manual demand: 0.41 (0.03)
### Workers with higher manual skills are more likely to be matched with jobs that demand manual labor
### A 2 SD increase in both traits increases joint surplus by 0.41 units

###############################################################################
# Part 3.

# Run the rank test
rank_test <- show.test(res)

# Clean rank test results
clean_test_matrix <- apply(rank_test, 1:2, function(x) {
  x_clean <- gsub("\\\\|\\$|\\t|\\{|\\}|\\hline|\\\\n|&", "", x)
  trimws(x_clean)
})

# Print structured output
cat("Rank Test Results (Structured):\n")
cat("Null Hypothesis:", clean_test_matrix[1,1], clean_test_matrix[1,2], "\n")
cat("Chi-squared Statistic:", clean_test_matrix[2,1], clean_test_matrix[2,2], "\n")
cat("Degrees of Freedom:", clean_test_matrix[3,1], clean_test_matrix[3,2], "\n")
cat("Rejected?:", clean_test_matrix[4,1], clean_test_matrix[4,2], "\n")

# Rank test rejects null hypothesis that affinity matrix has rank 1
# -> Affinity matrix has a rank higher than 1
# -> matching patterns are driven by more than one latent dimension (at least 2 distinct factors are influencing how workers and jobs are paired)

# The rank test formally tests the number of statistically significant singular values of the affinity matrix,
# while the salient analysis breaks them down to explain what each dimension means and how important it is for sorting.

###############################################################################
# Problem 2
###############################################################################

# I deleted "" from educ_h,"educ_w" in P2_Data_Couples_05.06.2025.csv and from gender,"educ" in P2_Data_Singles_05.06.2025.csv by hand!

# Load couples data
# couples <- read.csv("~/2. Semester/Empirical Evaluation of Market Outcomes/Assignment/Assignment 3/P2_Data_Couples_05.06.2025.csv", 
#                     row.names = NULL, check.names = TRUE)
couples = read.csv("P2_Data_Couples.csv", header = TRUE)

# # singles <- read.csv("~/2. Semester/Empirical Evaluation of Market Outcomes/Assignment/Assignment 3/P2_Data_Singles_05.06.2025.csv", 
#                     row.names = NULL, check.names = TRUE)
singles = read.csv("P2_Data_Singles.csv", header = TRUE)

str(couples)
str(singles)

##############################################################################
# Part 1.

# Define education levels
edu_levels <- 1:4

# Create count tables
mu_ij <- table(couples$educ_h, couples$educ_w)  # Matches (husband i, wife j)
mu_i0 <- table(singles$educ[singles$gender == "Male"])   # Single men
mu_0j <- table(singles$educ[singles$gender == "Female"]) # Single women

# Initialize Pi matrix
Pi <- matrix(NA, nrow = length(edu_levels), ncol = length(edu_levels))
rownames(Pi) <- paste0("Men_", edu_levels)
colnames(Pi) <- paste0("Women_", edu_levels)

# Compute Π_ij for each combination
for (i in edu_levels) {
  for (j in edu_levels) {
    mu_ij_val <- mu_ij[as.character(i), as.character(j)]
    mu_i0_val <- mu_i0[as.character(i)]
    mu_0j_val <- mu_0j[as.character(j)]
    
    if (!is.na(mu_ij_val) && mu_ij_val > 0 &&
        !is.na(mu_i0_val) && mu_i0_val > 0 &&
        !is.na(mu_0j_val) && mu_0j_val > 0) {
      
      Pi[i, j] <- log(mu_ij_val) - 0.5 * log(mu_i0_val) - 0.5 * log(mu_0j_val)
    } else {
      Pi[i, j] <- NA  # Avoid log(0)
    }
  }
}

# Print the result
cat("Surplus Matrix (Pi_ij):\n")
print(round(Pi, 2))

###############################################################################
# Part 2.

# Surplus is generally highest when men and women have similar education levels:
## e.g. Men_4 & Women_4: -0.09 (highest in matrix, best match)
## e.g. Men_3 & Women_3: -0.69 (also relatively high)

# Matches where education levels differ significantly show much lower surplus:
## e.g. Men_1 & Women_4: -4.54 (extremely low surplus)
## e.g. Men_4 & Women_2: -3.68 (also really low)

# People prefer partners with similar levels of education, and matches involving two highly educated individuals generate the greatest surplus.
# This indicates that these pairings are especially preferred and common.

# The surplus estimates show that matching in the data deviates significantly from what would be expected under random matching. 
# While random matching assumes partners pair independently of characteristics like education, the estimates reveal that similarly educated individuals match far more frequently than chance would predict.
# This indicates strong positive assortative matching, driven by preferences or institutional factors.

###############################################################################
# Part 3.

# The model assumes that total match surplus is the sum of male and female utilities.
# Although we cannot separately identify alpha (male preference) and gamma (female preference) from our function alone,
# the structure of the surplus matrix reveals how both sides of the market value different matches.
# For full identification of separate male and female preferences, we would need additional assumptions or data,
# such as variation in singles' utilities or outside options.





