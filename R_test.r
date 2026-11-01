# Assign variables with values using '<-'
dbh <- 28.4
height <- 20.1

#Calculate Basal Area (m2) from DBH (cm)
ba <- pi * (dbh^2 / 40000)
ba

# Core data types in forestry
dbh_num <- 88.6         #Numeric (data continuous)
tree_count <- 10L       #Integer (L represents no decimal values are in tree count)
species <- "Shorea robusta"     #Character text strings
is_A_grade  <- TRUE

#Inspect types using class() (high-)
class(dbh_num)
typeof(dbh_num)

class(is_A_grade)
typeof(is_A_grade)

# Verify types using is.*() functions
is.numeric(dbh_num)     #TRUE
is.numeric(is_A_grade)  #FALSE

# 2. Why Data Types Matter: The "Numeric"
# Field sheets often import number 
messy_dbh <- c(32,"35","35.1",42,NA)
class(messy_dbh)        #character

#math on text causes an error:
pi * (messy_dbh^2) / 40000 # --> Error: non-numeric argument to binary operator

# 3. Type Casting (Explicityly Changing Types with as.* fucntions)

#Scenerio A: Character -> Numeric
clean_dbh <- as.numeric(messy_dbh)
class(clean_dbh)
ba <- pi * (clean_dbh^2) / 40000
ba

# Scenario E: The Classic Factor-to-Numeric Problem
# If numeric codes are stored as a factor, convert to character FIRST, the numeric:
plot_codes_factor <- factor(c("101","102","105"))
as.numeric(plot_codes_factor)             #WRONG! Returns internal level indicies: 1 2 3
as.numeric(as.character(plot_codes_factor)) #CORRECT! Returns: 101 102 105

# 1. Relational (Comparison) Operators: >, <, >=, <=, == (equal), != (not equal)
dbh > 20         # TRUE (Is DBH greater than 20 cm?)
dbh <=10         # FALSE (Is DBH 10 cm or smaller?)
species_chr
species_chr == "Shorea robusta"     # FALSE (Exact match check; note the double '==')
species_chr != "Pinus roxburghii"   # TRUE (Not equal to Chir Pine)

# 2. Compound logical operators: & (AND), | (OR), ! (NOT)
# Merchantable timber rule: DBH > 30 cm AND Height > 18 m
(dbh > 30) & (height > 18)          # FALSE (both consitions must be TRUE)

# Pole-sized range check: Is DBH between 10 cm and 30 cm?
(dbh >= 10) & (dbh <= 30)           # TRUE

# Flagging rule: Is DBH negative OR absurdly large (> 150 cm)?
(dbh <= 0) | (dbh > 150)            # FALSE (TRUE if at least one condition is met)

dbh = c(12)

# 3. Conditional Decision-Making with if / else (for a single value)
if (dbh >= 30) {
    cat("Classification: Sawtimber tree\n") # cat removes the line number in output
} else if (dbh >= 10) {
    cat("Classification: Pole-sized tree\n")
} else {
    cat("Classification: Sapling\n")
}

trees_dbh <- c(28.4,35.1,17.8,42.0,22.4) 
#Vectorised math: R applies the basal area formula to all 5 trees simultaneously
trees_ba <- pi * (trees_dbh^2) / 40000
round(trees_ba, digits =4)

# Vectorised logical choice with ifelse (test, yes, no)
ifelse(test = trees_dbh >= 30, yes = "Large Stem", no = "Small/Medium Stem")

# Handling Missing Values (NA) in Field Data
heights <- c(19.2, 22.4, NA, 25.1, 17.1)

# Default: failes safely so you know data are missing
mean(heights)       #returns NA 

# The fix: tell R to remove NAs before calculating
mean(heights, na.rm = TRUE) #returns 20.95 m

# How many trees had missing height measurements?
# (is.na() produces TRUE/FALSE; sum() counts the TRUEs because TRUE ==1)
is.na (heights)         # FALSE FALSE TRUE FALSE FALSE
sum(is.na(heights))     # returns 1

# Test how logical checks behave on a vector with planted errors
test_trees <- c(28.4, -12.0, 0, 999, NA)

# Which trees have impossible non-positive diameters?
test_trees <= 0

# Which trees are suspiciously large (>150 cm) AND not missing?
test_trees > 150 & !is.na(test_trees)

# Do ANY trees in the victor violate the  > 0 rule?
any(test_trees <= 0, na.rm = TRUE)  # Returns a sigle TRUE

calculate_quadratic_mean_dia <- function(dbh_cm,n) {
    # Guard 1: Check data type
    if (!is.numeric(dbh_cm) & !is.numeric(n)) {
        stop("Error: Either, DBH or n mush be numeric (measured in cm) !")
    }
    # Guard 2: Stop on biologically impossible negative or zero values
    if (any(dbh_cm <= 0, na.rm = TRUE)) {
        stop("Error: DBH and n must be strictly greater than 0!")
    }
    # Guard 3: Warn on potential unit errors (e.g., mm recorded instead of cm)
    if (any(dbh_cm > 150, na.rm = TRUE)) {
        warning("Warning: DBH > 150 cm detected. Did you record in mm")
    }
    qmd <- ((dbh_cm) / n) ^ 0.5
    return(qmd)
}
dbh_in_cm <- c(28.9, -12.3, 6, 16.5)
n <- 4
calculate_quadratic_mean_dia(dbh_in_cm,n)

trees <- read.csv("./data/raw/unit2_starter_trees.csv")

# Inspecr the table structure before doing any math
head(trees)         # First 6 rows
head(trees, 15)     # Up to 15 rows (shows all 10 rows in out sheet)
nrow(trees)         # Number of trees records (10)
ncol(trees)         # Number of columns (5)
names(trees)        # Column names: "plot_id", "tree_id", "species_id"
str(trees)          # Structure and data type of every column               

# Access a single column as a vector using '$'
trees$dbh_cm

# Apply our custom functions to the inventory column
trees$ba_m2 <- calculate_quadratic_mean_dia(trees$dbh_cm, 10)
head(trees)
