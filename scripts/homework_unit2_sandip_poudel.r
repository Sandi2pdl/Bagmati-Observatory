# PART 1: Data Ingestion and Structure Inspection

inv <- read.csv("./data/raw/bagmati_raw_inventory.csv") #Assign values to the object inv
nrow(inv)
ncol(inv)
names(inv)
str(inv)
head(inv)

# Checking the parameters class and data type
class(inv$DBH_cm) 
typeof(inv$DBH_cm)

class(inv$Species)
typeof(inv$Species)

# Part 2: Forensic Data Auditing and Missing Values

# Missing Values
is.na(inv$DBH_cm)
sum(is.na(inv$DBH_cm))

is.na(inv$Height_m)
sum(is.na(inv$Height_m))

# Protocol Missing Codes
inv$DBH_cm == -99
sum(inv$DBH_cm == -99)

inv$DBH_cm == 999
sum(inv$DBH_cm == 999)

inv$Height_m == -99
sum(inv$Height_m == -99)

inv$Height_m == 999
sum(inv$Height_m == 999)

# Biological Boundary Violation
inv$DBH_cm <= 0
sum(inv$DBH_cm <= 0, na.rm = TRUE)

# The Plot P17 Field Sheet Transcription Case
inv[inv$Plot_ID == "P17" & inv$Tree_ID == "T203", ]
inv$DBH_cm[inv$Plot_ID == "P17" & inv$Tree_ID == "T203"] <- 29.9

# 5. Millimeter Entry Errors
sum(inv$DBH_cm > 150 & inv$DBH_cm < 900, na.rm = TRUE)
inv[which(inv$DBH_cm > 150 & inv$DBH_cm < 900), ]
rows <- which(inv$DBH_cm > 150 & inv$DBH_cm < 900)
inv$DBH_cm[rows] <- inv$DBH_cm[rows] / 10

# Part 3: Logical Subsetting & Ecological Queries
# clean inventory sheet
inv_clean <- inv[!is.na(inv$DBH_cm) & inv$DBH_cm > 0 & inv$DBH_cm != 999, ]

# Size-class
inv_clean$size_class <- cut (
    inv_clean$DBH_cm,
    breaks = c(0, 10, 30, Inf),
    labels = c("Sapling", "Pole", "Sawtimber")
)
names(inv_clean)
class(inv_clean$size_class)

#Frequency of factor
table(inv_clean$size_class)

which.max(inv_clean$DBH_cm)
inv_clean[which.max(inv_clean$DBH_cm), ]

#Count the species number of repeats
inv_clean$Species[inv_clean$Species == "S. robusta"]
sum(inv_clean$Species == "S. robusta", na.rm = TRUE)
mean(inv_clean$DBH_cm[inv_clean$Species == "S. robusta"], na.rm = TRUE)

#Part 4: Defensive Functions and Variability
calc_ba <- function(DBH_cm) {
    if (any(DBH_cm <= 0, na.rm = TRUE)) {
        stop("Error: DBH must be greater than 0")
    }

    if (any(DBH_cm > 150, na.rm = TRUE)) {
        warning("Warning: DBH > 150 cm detected. Did you record in mm?")
    }
ba_m2 <- pi * (DBH_cm)^2 / 40000
return(ba_m2)
}

# inv_clean$ba_m2 <- calc_ba(inv_clean$DBH_cm)
inv_clean$ba_m2 <- calc_ba(inv_clean$DBH_cm)
round(sum(inv_clean$ba_m2, na.rm = TRUE), 4)

# Coefficient of variation
calc_cv <- function(x) {
    cv <- (sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)) * 100
    return(round(cv, 2))
}

cv <- calc_cv (inv_clean$DBH_cm)
if (cv < 20) {
    print("Low structural variability")
    } else if (cv > 30) {
    print("High structural variability")
    }

# Testing Model Domain Boundaries
calculate_volume_example <- function(DBH_cm, Height_m) {
  if (any(DBH_cm < 10, na.rm = TRUE)) {
    warning("DBH below 10 cm: outside the valid range of this equation")
  }

  ba_m2 <- pi * DBH_cm ^ 2 / 40000
  volume_m3 <- 0.5 * ba_m2 * Height_m
  return(volume_m3)
}

vol_A <- calculate_volume_example(32.4, 12.1)
print(vol_A)
vol_B <- calculate_volume_example(6.5, 4.2)

# Part 5: Exploratory Visualization
png("figures/homework_unit2_diagnostics.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2))

# 1. Histogram of DBH_cm with a dashed red line for the mean
hist(inv_clean$DBH_cm,
     main = "DBH distribution",
     xlab = "DBH (cm)",
     col = "grey80")
abline(v = mean(inv_clean$DBH_cm, na.rm = TRUE),
       col = "red", lty = 2, lwd = 2)

# 2. Histogram of Height_m with a dashed blue line for the median
hist(inv_clean$Height_m,
     main = "Height distribution",
     xlab = "Height (m)",
     col = "grey80")
abline(v = median(inv_clean$Height_m, na.rm = TRUE),
       col = "blue", lty = 2, lwd = 2)

# 3. Boxplot of DBH_cm comparing the 4 stands (Stand_ID)
boxplot(DBH_cm ~ Stand_ID, data = inv_clean,
        main = "DBH by stand",
        xlab = "Stand",
        ylab = "DBH (cm)",
        las = 2,
        cex.axis = 0.7)

# 4. Scatterplot of DBH_cm vs Height_m with a trendline
plot(inv_clean$DBH_cm, inv_clean$Height_m,
     main = "DBH vs Height",
     xlab = "DBH (cm)",
     ylab = "Height (m)",
     pch = 16, cex = 0.5)
abline(lm(Height_m ~ DBH_cm, data = inv_clean),
       col = "red", lwd = 2)

par(mfrow = c(1, 1))
dev.off()
