###################################
### Tutorial on Matching Models ###
###################################

# Import Packages
if (!require("affinitymatrix")) install.packages("affinitymatrix")
library(affinitymatrix)

##------------------------------------------------------
## Problem 1 - Generating a dataset for a single market
##------------------------------------------------------

# Set a seed for replication
seed = 777
set.seed(seed)

# Number of Individuals on Both Sides
N = 500

# Define the Type Space
n_i = 4
f_j = 4

# Set the Parameters for the Data Generating Process
# Means
mu = rep(0, n_i+f_j)

# Covariance matrix
mat <- read.csv("Tutorial_CovMat.csv", header = FALSE)
Sigma <- as.matrix(mat)

# Sigma = matrix(c(1, 0.326, 0.1446, -0.0668, 0.5712, 0.4277, 0.1847, -0.2883,
#                  0.326, 1, -0.0372, 0.0215, 0.2795, 0.8471, 0.1211, -0.0902,
#                  0.1446, -0.0372, 1, -0.0244, 0.2186, 0.0636, 0.1489,
#                  -0.1301, -0.0668, 0.0215, -0.0244, 1, 0.0192, 0.0452,
#                  -0.0553, 0.2717, 0.5712, 0.2795, 0.2186, 0.0192, 1, 0.3309,
#                  0.1324, -0.1896, 0.4277, 0.8471, 0.0636, 0.0452, 0.3309, 1,
#                  0.0915, -0.1299, 0.1847, 0.1211, 0.1489, -0.0553, 0.1324,
#                  0.0915, 1, -0.1959, -0.2883, -0.0902, -0.1301, 0.2717,
#                  -0.1896, -0.1299, -0.1959, 1), nrow=n_i+f_j)

# Draw the Sample
sample = MASS::mvrnorm(N, mu, Sigma)

# Extract Matrices for Men and Women
i = sample[, 1:n_i]
j = sample[, n_i+1:f_j]

# Define Labels for Graphs and Tables
labels_i = c("Educ.", "Age", "Height", "BMI") 
labels_j = c("Educ.", "Age", "Height", "BMI")

##------------------------------------------------------
## Problem 2 - Estimating the Model
##------------------------------------------------------

help(estimate.affinity.matrix)
res = estimate.affinity.matrix(i, j, nB = 100)

## Summarize results
# Print the Affinity Matrix
aff_mat = show.affinity.matrix(res, labels_i, labels_j)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat))

# Print the Diagonal Elements of Affinity Matrix
aff_diag = show.diagonal(res, labels = labels_i)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_diag))

# Print the Results from a Rank Test
rank_test = show.test(res)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", rank_test[c(1,2,4),]))

# Saliency Analysis
saliency = show.saliency(res, labels_x = labels_i, labels_y = labels_j, ncol_x = 4, ncol_y = 4)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n|$", "", saliency$U.table))
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n|$", "", saliency$V.table))

# Plot the Correlations
show.correlations(res, labels_x = labels_i, labels_y = labels_j, label_x_axis = "Husband", label_y_axis = "Wife", ndims = 2)

###################################
###             END             ###   
###################################