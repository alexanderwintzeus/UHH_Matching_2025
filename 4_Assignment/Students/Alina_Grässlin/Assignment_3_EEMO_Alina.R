# Load required package
if (!require("affinitymatrix")) install.packages("affinitymatrix")
library(affinitymatrix)

# Problem 1

# data

# df <- read.csv("C:/Users/alina/OneDrive/Desktop/Economics/2. Semester/Evaluation of Market Outcomes/Assignments/3/P1_Data_05.06.2025.csv")
df <- read.csv("P1_Data.csv", header = TRUE)

N <- nrow(df)

#  matrices for workers (i) and firms (j)
i <- as.matrix(df[, c("cog_skill", "man_skill")])
j <- as.matrix(df[, c("cog_job", "man_job")])

#  vectors
labels_i <- c("Cognitive Skill", "Manual Skill")
labels_j <- c("Cognitive Requirement", "Manual Requirement")

##  Affinity Matrix
res <- estimate.affinity.matrix(i, j, nB = 10)

aff_mat <- show.affinity.matrix(res, labels_i, labels_j)
cat(gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", aff_mat)), "\n")

##  Rank Test
rank_test <- show.test(res)
cat(gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", rank_test[c(1,2,4),])), "\n")

# Saliency Analysis 
saliency <- show.saliency(res, labels_x = labels_i, labels_y = labels_j)
cat(gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", saliency$U.table)), "\n")
cat(gsub("\\}", "***", gsub("\\\\|hline|textbf\\{|\t|&|\n", "", saliency$V.table)), "\n")

# Problem 2 

# couples <- read.csv("C:/Users/alina/OneDrive/Desktop/Economics/2. Semester/Evaluation of Market Outcomes/Assignments/3/P2_Data_Couples_05.06.2025.csv")
# singles <- read.csv("C:/Users/alina/OneDrive/Desktop/Economics/2. Semester/Evaluation of Market Outcomes/Assignments/3/P2_Data_Singles_05.06.2025.csv")
couples = read.csv("P2_Data_Couples.csv", header = TRUE)
singles = read.csv("P2_Data_Singles.csv", header = TRUE)

# Education Range
edu_levels <- 1:4

# frequency tables
mu_ij <- table(couples$educ_h, couples$educ_w)
mu_i0 <- table(singles$educ_men)
mu_0j <- table(singles$educ_women)


# Couples
mu_ij <- as.matrix(xtabs(~ educ_h + educ_w, data=couples, drop.unused.levels=FALSE))


# Singles
mu_i0 <- as.numeric(xtabs(~ educ, data = singles[singles$gender == "Male", ], drop.unused.levels = FALSE))
mu_0j <- as.numeric(xtabs(~ educ, data = singles[singles$gender == "Female", ], drop.unused.levels = FALSE))


# Ensure all combinations are included
#mu_ij <- as.matrix(xtabs(~ educ_h + educ_w, data=couples, drop.unused.levels=FALSE))
#mu_i0 <- as.numeric(xtabs(~ educ_h, data=singles, drop.unused.levels=FALSE))
#mu_0j <- as.numeric(xtabs(~ educ_w, data=singles, drop.unused.levels=FALSE))

#  surplus matrix
Pi <- matrix(NA, nrow = 4, ncol = 4)

# Loop surplus matrix
for (i in edu_levels) {
  for (j in edu_levels) {
    if (mu_i0[i] > 0 && mu_0j[j] > 0) {
      Pi[i, j] <- mu_ij[i, j] / sqrt(mu_i0[i] * mu_0j[j])
    } else {
      Pi[i, j] <- NA
    }
  }
}

colnames(Pi) <- paste0("W", edu_levels)
rownames(Pi) <- paste0("M", edu_levels)

# Output surplus matrix
print(round(Pi, 2))

