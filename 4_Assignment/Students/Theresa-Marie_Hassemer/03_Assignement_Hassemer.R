# Load necessary packages
install.packages("devtools")
devtools::install_local("/Users/theresahassemer/Downloads/affinitymatrix-master")
library(affinitymatrix)
library(readr)
install.packages(transport)
library(transport)

# Load data
# data_p1 <- read_csv("/Users/theresahassemer/Desktop/p1_data_05.06.2025.csv")
data_p1 = data

# Step 1: Infer number of matches and create matrices
N <- nrow(data_p1)
i <- as.matrix(data_p1[, c("cog_skill", "man_skill")])
j <- as.matrix(data_p1[, c("cog_job", "man_job")])

# Step 2: Estimate and show the affinity matrix
estimate.affinity.matrix(i, j)
A_hat <- estimate.affinity.matrix(i, j)
show.affinity.matrix(A_hat)

# Interpretation: the diagonal coefficients are large and significant, indicating positive aoosrtative matching; the off-diagonal coefficient is negative and statisticall significant.
# Interpretation: Interpretation: Workers with strong manual skills are less likely to be matched with cognitively demanding jobs, suggesting negative sorting across these dimensions (i.e., skill specialization).

# Step 3: 
show.test(A_hat)
# Interpretation:The null hypothesis that the matrix has rank 1 is strongly rejected.
# Interpretation:This implies that matching cannot be explained by a single skill dimension — both cognitive and manual traits contribute independently to matching patterns.

# Problem 2:

# Load datasets
#couples <- read_csv("/Users/theresahassemer/Desktop/P2_Data_Couples_05.06.2025.csv")
#singles_raw <- read.csv("/Users/theresahassemer/Desktop/P2_Data_Singles_05.06.2025.csv", header = TRUE, stringsAsFactors = FALSE)
couples = read.csv("P2_Data_Couples.csv")
singles_raw = read.csv("P2_Data_Singles.csv")

# Define education levels
edu_levels <- 1:4

table(singles_raw$gender)
str(singles_raw$gender)
unique(singles_raw$educ)

# Construct mu_ij (couples matrix)
mu_ij <- table(couples$educ_h, couples$educ_w)
mu_ij <- mu_ij[as.character(edu_levels), as.character(edu_levels)]
mu_ij <- as.matrix(mu_ij)

# Construct mu_i0 (single men) and mu_0j (single women)
mu_i0 <- table(singles_raw$educ[singles_raw$gender == "Male"])
mu_0j <- table(singles_raw$educ[singles_raw$gender == "Female"])
mu_i0 <- mu_i0[as.character(edu_levels)]
mu_0j <- mu_0j[as.character(edu_levels)]
mu_i0[is.na(mu_i0)] <- 0
mu_0j[is.na(mu_0j)] <- 0
mu_i0 <- as.numeric(mu_i0)
mu_0j <- as.numeric(mu_0j)

# Initialize surplus matrix
Pi <- matrix(NA, 4, 4)

# Estimate Pi_ij using loop
for (i in edu_levels) {
  for (j in edu_levels) {
    if (mu_i0[i] > 0 && mu_0j[j] > 0) {
      Pi[i, j] <- sqrt(mu_ij[i, j] / (mu_i0[i] * mu_0j[j]))
    } else {
      Pi[i, j] <- NA
    }
  }
}

# Add labels
rownames(Pi) <- paste0("Husband_", edu_levels)
colnames(Pi) <- paste0("Wife_", edu_levels)

# Show result
print(round(Pi, 3))

# Interpretation: Couples are more likely to form when both partners share a similar level of educational attainment.

# Step 2: Construct unmatched (single) counts by gender
mu_i0 <- table(singles$educ[singles$gender == "Male"])
mu_0j <- table(singles$educ[singles$gender == "Female"])

# Ensure full support across education levels
mu_i0 <- mu_i0[as.character(edu_levels)]
mu_0j <- mu_0j[as.character(edu_levels)]

# Replace NAs (from missing levels) with zeros
mu_i0[is.na(mu_i0)] <- 0
mu_0j[is.na(mu_0j)] <- 0

# Convert to numeric vectors
mu_i0 <- as.numeric(mu_i0)
mu_0j <- as.numeric(mu_0j)

# Step 3: Estimate Choo and Siow surplus matrix Π_ij
Pi <- matrix(NA, length(edu_levels), length(edu_levels))

for (i in edu_levels) {
  for (j in edu_levels) {
    if (mu_i0[i] > 0 && mu_0j[j] > 0) {
      Pi[i, j] <- sqrt(mu_ij[i, j] / (mu_i0[i] * mu_0j[j]))
    } else {
      Pi[i, j] <- NA
    }
  }
}

# Label the matrix
rownames(Pi) <- paste0("Husband_", edu_levels)
colnames(Pi) <- paste0("Wife_", edu_levels)

# Show the result
print(round(Pi, 3))

# We can learn that matching is selective, education-based, and positively assortative - clearly not random. If it would have been random, the values would be roughly uniform and close to 1.
# Ye, the model allows inference on gender-specific preferences. With log-surplus values, you can back out how desirable each eduation level is for men and women, independently.

