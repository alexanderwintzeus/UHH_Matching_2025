#####################################
### Assignment on Matching Models ###
#####################################

# Set working directory
setwd("C:/Users/u0131511/Documents/PhD_AlexanderWintzéus/Teaching/UHH_Matching/4_Assignment/DataCode")

# Import Packages
if (!require("affinitymatrix")) install.packages("affinitymatrix")
library(affinitymatrix)

##======================================================
## Problem 1: Matching on skills (workers and jobs)
##======================================================

# Read in data
df <- read.csv("P1_Data.csv", header = TRUE)

##------------------------------------------------------
## Step 1 - Prepare data for estimation
##------------------------------------------------------

# Define the number of worker-firm matches
N = nrow(df)

# Extract the sample of workers (x) and firms (y)
# NB: Only extract those attributes relevant for the matching model estimation
x = df[c("cog_skill","man_skill")]
y = df[c("cog_job", "man_job")]

# Define Labels for Graphs and Tables
labels_x = c("Cognitive skills", "Manual skills") 
labels_y = c("Cognitive demand", "Manual demand")

##------------------------------------------------------
## Step 2 - Estimation of matching model
##------------------------------------------------------

# Estimation of Affinity Matrix
res = estimate.affinity.matrix(x, y, nB = 100)

# Print Affinity Matrix
aff_mat = show.affinity.matrix(res, labels_x, labels_y)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat))

# Saliency Analysis
saliency = show.saliency(res, labels_x = labels_x, labels_y = labels_y, ncol_x = 2, ncol_y = 2)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n|$", "", saliency$U.table))
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n|$", "", saliency$V.table))

# Rank test
rank_test = show.test(res)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", rank_test[c(1,2,4),]))

##======================================================
## Problem 2: Matching on education (men and women)
##======================================================

# Load in data
couples = read.csv("P2_Data_Couples.csv", header = TRUE)
singles = read.csv("P2_Data_Singles.csv", header = TRUE)

##------------------------------------------------------
## Step 1 - Nonparametric estimation of match surplus
##------------------------------------------------------

# Compute quantities of singles of each gender and education level
tab_singles = table(singles$gender, singles$educ)
tab_female = tab_singles[1,]
tab_male = tab_singles[2,]

q_singles <- outer(tab_male, tab_female, "*") # outer product (denominator match function)

# Compute quantities of couples for each combination of education levels
q_couples = table(couples$educ_h, couples$educ_w)
q_couples_w = table(couples$educ_w) # later use (see below)
q_couples_h = table(couples$educ_h) # later use (see below)

# Compute match surplus matrix
Pi = q_couples/(sqrt(q_singles))
print(Pi)

##------------------------------------------------------
## Step 2 - Shares of observed vs random matches
##------------------------------------------------------

# Number of couples
Nc = nrow(couples)

# Marginal distribution of education levels by gender (across matched individuals)
share_f = (q_couples_w)/(Nc)
share_m = (q_couples_h)/(Nc)

# Matrix of random matches (as a share of total number of matches)
R <- outer(share_m, share_f,"*")
print(R)

# Vector of assortative matches according to random matching
print(diag(R))

# Matrix of observed matches (as a share of total number of matches)
O = q_couples/Nc
print(O)
 
# Vector of observed assortative matches
print(diag(O))

# Clearly, for any pair of education levels, there are more observed assortative
# matches than implied by random matching based on the product of the marginal
# distributions. This is reflected in the match surplus Pi as follows:
#   Take any two education levels: The sum of the two corresponding diagonal 
#   elements will always be greater than the sum of the two associated off-
#   diagonal elements.

#####################################
###              END              ###
#####################################