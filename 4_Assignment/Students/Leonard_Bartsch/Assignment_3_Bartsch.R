#Assignment – Matching Models

##------------------------------------------------------
# Problem 1
##------------------------------------------------------

rm(list = ls())
getwd()
setwd('/Users/leo/Desktop/SS 2025/Empirical Evaluation of Market Outcomes/Assignments/Assignment_3')

# 1. Infer the number of matches N from the data. Create two matrices i and j containing the 
# observations for workers and jobs, respectively.

# Import Packages
if (!require("affinitymatrix")) install.packages("affinitymatrix")
library(affinitymatrix)

# Load the data
data <- read.csv("P1_Data.csv", header = TRUE)

# Infer the number of matches N
N <- nrow(data)

# Matrix i for workers 
i <- as.matrix(data[, c("cog_skill", "man_skill")])

# Matrix j for jobs
j <- as.matrix(data[, c("cog_job",   "man_job")])


# 2. Estimate the affinity matrix using the estimate.affinity.matrix() function and print out
# the results using show.affinity.matrix().

# Estimate the affinity matrix
res <- estimate.affinity.matrix(i, j, nB = 100)

# Define labels 
labels_i <- c("Cog. skill", "Man. skill")
labels_j <- c("Cog. demand", "Man. demand")

aff_mat <- show.affinity.matrix(res, labels_i, labels_j)
print(aff_mat)

#                   cognitive demand    manual demand 
# cognitive skill     0.40 (0.03)         0.03 (0.02)
# manual skill        -0.07 (0.02)        0.41 (0.03)

# What can we learn about the matching patterns in our data from these estimates?
# Both diagonal elements of the affinity matrix show positive values, 
# 0.40 for cognitive and 0.41 for manual, and thus indicating strong positive 
# assortative matching. Workers with high cognitive skills tend to sort into jobs with a 
# high cognitive demand while workers with high manual skills sort into jobs 
# with high manual demand. 

# Provide a correct interpretation of the estimated coefficient related to the worker's 
# manual skills and the job's cognitive demand.
# The coefficient for manual skill X cognitive demand yields a value of -0.07 
# and a standard error of 0.02 (it is statistically significant). Since the affinity matrix is related to the match surplus
# the coefficient indicates that an increase in both  manual skill and cognitive demand
# by a standard deviation would decrease the joint surplus by 0.07 units. 


# 3. Print out the results of the rank test using show.test() function. 

# Rank test 
rank_test <- show.test(res)
print(rank_test)

# What does this test suggest about the rank of the affinity matrix? 
# H_0: rk(A) = 1
# chi^(2) = 140.96, df = 1, rejected = yes 
# The rank test shows that we can reject the hypothesis that the rank of the affinity matrix
# is 1, suggesting that the rank is 2 since we have a 2x2 matrix. 

# How does the rank test relate to the singular value decomposition of the affinity matrix (saliency analysis)?
# The rank test is based on jointly testing the significance of the singular values of the 
# affinity matrix. Rejecting H_0: rank(A) = 1 means that there are at least two nonzero 
# singular values in the decomposition. This is because in the singular value decomposition, the rank of the matrix equals
# the number of non-zero singular values.


##------------------------------------------------------
# Problem 2
##------------------------------------------------------

rm(list = ls())
getwd()
setwd('/Users/leo/Desktop/SS 2025/Empirical Evaluation of Market Outcomes/Assignments/Assignment_3')

# 1. Use this matching function to estimate the surplus pi_ij nonparametrically.

# Read the data
couples <- read.csv(
  "P2_Data_Couples.csv",
  header     = FALSE,
  stringsAsFactors = FALSE
)
singles <- read.csv(
  "P2_Data_Singles.csv",
  header     = FALSE,
  stringsAsFactors = FALSE
)

# Assign column names 
names(couples) <- c("man_edu",   "woman_edu")
names(singles) <- c("gender",    "edu")

# Remove the first row
couples <- couples[-1, ]
singles <- singles[-1, ]

# Couples
mu_ij <- table(couples$man_edu, couples$woman_edu)

# Singles
mu_i0 <- table(singles$edu[singles$gender=="Male"])
mu_0j <- table(singles$edu[singles$gender=="Female"])

# Compute 
edulevels <- sort(unique(couples$man_edu))
pi <- matrix(NA,
             nrow = length(edulevels),
             ncol = length(edulevels),
             dimnames = list(
               paste0("Man_",  edulevels),
               paste0("Woman_",edulevels)
             ))

for(i in edulevels) for(j in edulevels) {
  pi[paste0("Man_", i), paste0("Woman_", j)] <-
    mu_ij[as.character(i), as.character(j)] /
    sqrt( mu_i0[as.character(i)] * mu_0j[as.character(j)] )
}

#print
print(round(pi, 3))

#        Woman_1 Woman_2 Woman_3 Woman_4
# Man_1   0.236   0.233   0.074   0.011
# Man_2   0.149   0.322   0.372   0.119
# Man_3   0.038   0.217   0.502   0.467
# Man_4   0.000   0.025   0.248   0.911

# 2. What can we learn about the matching patterns in our data from these estimates? 
# How does this relate to random matching?
# The estimated matrix shows that the main diagonal almost always yields 
# the highest values which indicates positive assortative matching by education. 
# Men and women with the same education level form couples more often relative to persons
# where the education level does not match. We can even see that this difference increases
# as the gap in education rises. Additionally, the values are higher for higher education levels.
# Under complete random matching all the values would be equal to 1. The values differing from 1
# plus the relative magnitude of the main diagonal indicates that matching is not random
# but based on the level of education. The values are smaller than 1 because although there
# is matching based on education, not all people are in a relationship and there are still
# a lot of singles left. 

# 3. Does the model allow us to infer anything about male (αij ) and female (γij ) preferences?
# No, the model does not allow us to infer anything about male and female preferences.
# We do not know if a value is low or high because the woman or the man likes or dislikes 
# the others education level. This means that we can see the total surplus but we can not 
# tell if that surplus is based on the male or female preferences. 



