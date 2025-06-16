library(affinitymatrix)

####Exercise 1
data_p1 <- read.csv("P1_Data.csv")

####Infer the number of matches N from the data
N <- nrow(data_p1)

####Create two matrices i and j containing the observations for workers and jobs
i <- data_p1[, c("cog_skill", "man_skill")]
j <- data_p1[, c("cog_job", "man_job")]


labels_i = c("cog_skill", "man_skill")
labels_j = c("cog_job", "man_job")

####Estimate the affinity matrix
affinity_matrix <- estimate.affinity.matrix(i, j)

####Show results
show_aff <- show.affinity.matrix(affinity_matrix, labels_i, labels_j)
gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", show_aff))

###What can we learn about the matching patterns in our data from these estimates?
#The diagonal values are positive and highly significant. This implies:
#Workers with high cognitive skills tend to be matched with jobs that demand high cognitive input.
#Workers with high manual skills tend to be matched with jobs that demand high manual input.

####Provide a correct interpretation of the estimated coefficient related to the worker’s manual skills and the job's cognitive demand.
#The negative and statistically significant value (–0.07) implies that:
#Workers who are more manually skilled are less likely to be placed into jobs with high cognitive demands.

####Print out the results of the rank test
show.test(affinity_matrix)

####What does this test suggest about the rank of the affinity matrix?
#The null hypothesis that the matrix has rank 1 is strongly rejected.
#This means the data require more than one dimension to explain the observed matching patterns.

show.saliency(affinity_matrix, labels_x = labels_i, labels_y = labels_j)

####How does the rank test relate to the singular value decomposition of the affinity matrix (saliency analysis)?
#The saliency analysis (via SVD) confirms this by decomposing the affinity matrix into two orthogonal indices:
#Index 1 and Index 2 each explain ~50% of the matching variation.
#This supports a rank of 2, which matches the rank test result.

library(tidyverse)

####Exercise 2
couples <- read.csv("P2_Data_Couples.csv", row.names = NULL)
names(couples) <- c("educ_men", "educ_women")

singles <- read.csv("P2_Data_Singles.csv", row.names = NULL)
names(singles) <- c("gender", "educ")

mu_ij <- table(couples$educ_men, couples$educ_women)
mu_i0 <- filter(singles, gender == "Male") %>%
  count(educ)
mu_0j <- filter(singles, gender == "Female") %>%
  count(educ)


matching_function <- function(mu_ij, mu_i0, mu_0j) {
  surplus <- matrix(0, nrow = nrow(mu_ij), ncol = ncol(mu_ij))
  
  for (i in 1:nrow(mu_ij)) {
    for (j in 1:ncol(mu_ij)) {
      surplus[i, j] <- mu_ij[i, j] / sqrt(mu_i0$n[i] * mu_0j$n[j])
    }
  }
  
  return(surplus)
}

####Estimate the surplus
Pi_ij <- matching_function(mu_ij, mu_i0, mu_0j)

print(Pi_ij)

####What can we learn about the matching patterns in our data from these estimates? How does this relate to random matching?
#These estimates show positive assorative matching: individuals tend to marry partners with similar education levels.
#The diagonal values are highest, especially for the highly educated, indicating these matches occur more frequently than expected under random matching.
#This suggests that education plays a strong role in partner selection.

####Does this model allow us to infer anything about male and female preferences?
#The model reveals joint preferences.
#High surplus values indicate both men and women prefer similar education levels.
#While alpha_ij and gamma_ij aren't separately identified, the surplus reflects their combined effect.