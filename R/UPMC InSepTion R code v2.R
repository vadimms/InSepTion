
library(data.table)
library(mice)
library(tmle)
library(SuperLearner)
library(randomForest)
library(xgboost)
library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)   
library(DMwR) #can't download
library(VIM)

#setorder(wupmc, "hosp_id")


upmc <- fread("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Emergency cohort pre-imputation carryforward V1.csv", 
                 colClasses = c(gender = "factor", icu = "factor", mechvent = "factor", race = "factor", dead = "factor",
                                surg = "factor",  pbc = "factor", pos_pbc = "factor", cx_type = "factor", 
                                cx_source = "factor", admit_year = "factor", stringsAsFactors = TRUE))

upmc2 <- fread("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Emergency cohort original V1.csv", 
               colClasses = c(gender = "factor", icu = "factor", mechvent = "factor", race = "factor", dead = "factor",
                              surg = "factor",  pbc = "factor", pos_pbc = "factor", cx_type = "factor", 
                              cx_source = "factor", admit_year = "factor"))

upmc2 <- subset(upmc2, select = -c(sofa_total, sirs_total, age, alb, alt, ast, base_excess, bicarb, bili, bun, cl, creat, dbp,
                                   elix, fio2, gcs, gender, gluc, hr, hgb, icu, inr, lactate, map, mechvent, paco2, pao2, 
                                   pf_ratio, ph, plt, k, rr, shock_index, na, o2_sat, sbp, temp, wbc, weight, fluid, 
                                   norepi_equiv, dtt0_hours, dead_90, action, interval, empi_id))

upmc2 <- subset(upmc2, select = -c(interval, empi_id))

upmc2 <- subset(upmc2, select = -c(pulsepress, ))


#####New variables
hdupmc <- hdupmc |>
  mutate(pulsepress = sbp - dbp)

calculate_egfr <- function(creat, age, gender) {
  a <- ifelse(gender == "0", 101.9, 138.2)
  b <- ifelse(gender == "0", 0.80, 1.05)
  c <- ifelse(gender == "0", -0.742, -0.918)
  
  egfr <- a * ((creat / b) ^ c) * (0.993 ^ age)
  
  return(egfr)
}

hdupmc <- hdupmc |>
  mutate(egfr = calculate_egfr(creat, age, gender))

#fluid cc/Kg actual body weight
hdupmc <- hdupmc |>
  mutate(fluid_cckg = fluid / weight)





upmc <- group_by(upmc, hosp_id) |> mutate(id = row_number())
upmc2 <- group_by(upmc2, hosp_id) |> mutate(id = row_number())

mupmc <- full_join(upmc, upmc2, by = c("hosp_id", "id"))

length(unique(height$hosp_id))


heights <- fread("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Heights.csv")

heights <- group_by(heights, hosp_id) |> mutate(id = row_number())

cupmc <- right_join(heights, mupmc, by = c("hosp_id", "id"))

str(heights)

#Ideal Body Weight (IBW)
hdupmc <- hdupmc |>
         mutate(ibw = pmax(0.1, if_else(gender == 1,
                               50 + 0.91 * (height - 152.4),
                               45.5 + 0.91 * (height - 152.4))))

# Adjusted Body Weight (AdjBW)
hdupmc$adjbw <- hdupmc$ibw + 0.4 * (hdupmc$weight - hdupmc$ibw)

#fluid_cckg_ibw
hdupmc <- hdupmc |>
  mutate(fluid_cckg_ibw = fluid_cckg / ibw)

setorder(cupmc, hosp_id, id)

write.csv(cupmc, "C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Combined long data.csv")

write.csv(hdupmc, "C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC hot deck complete.csv")



####REMOVE PTS NOT HOTN OR IN SHOCK
fupmc <- cupmc |>
  group_by(hosp_id) |>                
  filter(any(sbp < 101 | map < 66 | norepi_equiv > 0)) |>  
  ungroup()   

length(unique(fupmc$hosp_id))
#43462

table(fupmc$id)
# 1     2     3     4     5     6     7     8     9    10    11    12    13 
# 43462 43462 43078 42872 42603 42254 41776 41309 40951 40627 40082 39260 13601 


#making a new time variable
create_time_column <- function(fupmc) {
  fupmc |>
    group_by(hosp_id) |>
    mutate(first_row = which.min(ifelse(sbp < 101 | map < 66 | norepi_equiv > 0, interval, Inf))) |>
    mutate(time = row_number() - first_row)|>
    ungroup() 
}


fupmc <- create_time_column(fupmc)

fupmc |> relocate(time)

write.csv(fupmc, "C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC combined_pre imputed.csv")


fupmc <- fread("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC long_time.csv")


str(cupmc)

summary(fupmc$time)

str(fupmc)

### CARRYFORWARD HEIGHTS AND THEN RECALUCLATE FLUID_CC_KG_IBW, ADJBW, IBW, ETC

fupmc <- fupmc |> 
  group_by(hosp_id) |>
  fill(height, .direction = "down") |>
  ungroup()

sum(is.na(fupmc$height))


#Hot Deck imputation

fupmc_impuhd <- hotdeck(fupmc)

hdupmc <- fupmc_impuhd

sum(is.na(fupmc_impuhd))

str(hdupmc)

####KNN imputation

#knn_fupmc <- fupmc[, !(names(fupmc) %in% c("fluid_cckg_ibw", "adjbw", 
 #                                          "ibw", "fluid_cckg", "egfr", "pulsepress", "pf_ratio", "shock_index"))]

numeric_fupmc <- fupmc[sapply(fupmc, is.numeric)]
nonnumeric_fupmc <- fupmc[sapply(fupmc, Negate(is.numeric))]


knn_normupmc <- as.data.frame(lapply(numeric_fupmc, scale))

#knn_fupmc <- fupmc[, !(names(fupmc) %in% c("pf_ratio", "shock_index", "enc_start_dt", "enc_end_dt", "dt_t0_first",  "death_date",
##                                          "icu_start_dt", "icu_end_dt", "hospital", "empi_id"))]



knn_fupmc <- knn_normupmc[, !(names(knn_normupmc) %in% c("pf_ratio", "shock_index"))]


knn_upmc5 <- kNN(knn_fupmc, k = 5)




#MICE
# Perform MICE
# Use urine_sum and max_prev_norepi_equiv, not urine, there's no missingness in urine_sum and max_prev_norepi_equiv
#no missingness in fluid_balance or fluid_sum or fluid
#calculate "shock_index" as hr/sbp when missing after imputation
#calc  "pulsepress", "fluid_cckg", "egfr","fluid_cckg_ibw"

imp_mice <- mice(fupmc[, c("height", "weight", "alb", "alt", "ast", "base_excess", "elix",        
                   "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", "gcs", 
                   "gluc", "hr", "hgb", "inr", "lactate", "map", "paco2", "pao2",                 
                   "pf_ratio", "ph", "plt", "k", "rr", "na", "o2_sat","sbp", "temp",                 
                   "wbc")], 
            method = 'pmm', 
            m = 5, 
            maxit = 15,
            seed = 1,
            printFlag = TRUE)


#mice on subset
?mice


fupmc_subset <- fupmc[1:200,]

fupmc_subset_mice <- mice(fupmc_subset[, c("height", "weight", "alb", "alt", "ast", "base_excess", "elix",        
                           "bicarb", "bili", "bun", "cl", "creat", "dbp", "fio2", "gcs", 
                           "gluc", "hr", "hgb", "inr", "lactate", "map", "paco2", "pao2",                 
                           "pf_ratio", "ph", "plt", "k", "rr", "na", "o2_sat","sbp", "temp",                 
                           "wbc")], 
                 method = 'pmm', 
                 m = 5, 
                 maxit = 15,
                 seed = 1,
                 printFlag = TRUE)

compl_fupmc_subset <- complete(fupmc_subset_mice, action = "long", include = TRUE)

summary(compl_fupmc_subset)


#diagnosing imputation problems: densityplot(imp_mice), stripplot(imp_mice)

sapply(fupmc, function(i) sum(is.na(i)))

sapply(upmc, function(i) sum(is.na(i)))



# Create 5 completed datasets
compl_upmc <- complete(imp_mice, action = "long", include = TRUE)


# View the results
summary(compl_upmc)

summary(fupmc)

# Example: Viewing the first completed dataset
head(compl_upmc[compl_upmc$.imp_mice == 2, ], )



###-------------

max(cupmc$ibw, na.rm=TRUE)
head(cupmc$max_prev_norepi_equiv)

#reshape
wupmc <- reshape(fupmc,
                 idvar = "hosp_id",
                 timevar = "time",
                 v.names = c("sofa_total", "sirs_total", "alb", "alt", "ast", "base_excess",        
                             "bicarb", "bili", "bun", "cl", "creat", "dbp", "elix", "fio2", "gcs", 
                             "gluc", "hr", "hgb", "inr", "lactate", "map", "mechvent", "paco2", "pao2",                 
                             "pf_ratio", "ph", "plt", "k", "rr", "shock_index", "na", "o2_sat","sbp", "temp",                 
                             "wbc", "fluid", "fluid_sum", "urine", "urine_sum", "fluid_balance", "norepi_equiv", 
                             "pulsepress", "fluid_cckg", "egfr", "icu", "fluid_cckg", "max_prev_norepi_equiv", "fluid_cckg_ibw"),
                 direction = "wide")


















###############
intersect(names(upmc), names(upmc2))


mupmc <- merge(upmc, upmc2, by = "hosp_id", all = TRUE)

?merge

length(unique(upmc2$hosp_id))

str(upmc)


str(upmc2)




# Using digest package for SHA-256 hashing
library(digest)
hashed_data <- sapply(upmc$hosp_id, function(x) digest(x, algo="sha256"))
hashed_factor <- as.factor(upmc$hosp_id)

hf_umpc <- merge(upmc, hashed_factor, all = TRUE)


?digest




height <- fread("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Heights.csv")

str(height)

length(unique(height$hosp_id))

humpc <- merge(upmc, height, all = TRUE)

str(mupmc)


mupmc <- merge(upmc2, humpc, all = TRUE)


mupmc[, hotntime := ]


#############################

upmc <- read.csv("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Emergency cohort pre-imputation carryforward V1.csv", 
                 colClasses = c(gender = "factor", icu = "factor", mechvent = "factor", race = "factor", dead = "factor",
                                surg = "factor",  pbc = "factor", pos_pbc = "factor", cx_type = "factor", 
                                cx_source = "factor", admit_year = "factor"))

upmc2 <- read.csv("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Emergency cohort original V1.csv", 
                     colClasses = c(gender = "factor", icu = "factor", mechvent = "factor", race = "factor", dead = "factor",
                                    surg = "factor",  pbc = "factor", pos_pbc = "factor", cx_type = "factor", 
                                    cx_source = "factor", admit_year = "factor"))


intersect(names(upmc), names(upmc2))

fupmc_subset <- fupmc[1:200,]


upmc_subset <- write.csv(C:/Users/vadim/OneDrive/Desktop/InSepTion)

fwrite(upmc_subset, "C:/Users/vadim/OneDrive/Desktop/InSepTion/upmc_subset.csv")


wupmc_subset <- reshape(upmc_subset,
                 idvar = "hosp_id",
                 timevar = "interval",
                 v.names = c("alb", "alt", "ast", "base_excess",        
                             "bicarb", "bili", "bun", "cl", "creat", "dbp", "elix", "fio2", "gcs", 
                             "gluc", "hr", "hgb", "inr", "lactate", "map", "mechvent", "paco2", "pao2",                 
                             "pf_ratio", "ph", "plt", "k", "rr", "shock_index", "na", "o2_sat","sbp", "temp",                 
                             "wbc", "fluid"),
                 direction = "wide")




upmc <- merge(upmc, upmc2, by = "hosp_id", all = TRUE)

mupmc <- upmc

mupmc <- mupmc %>%
  mutate(PulsePress = sbp - dbp)

mupmc <- mupmc %>%
  mutate(fluid_cckg = fluid / weight)

mupmc <- mupmc %>%
  mutate(fluid_sum_cckg = fluid_sum / weight)



#no egfr
wupmc <- reshape(mupmc,
                 idvar = "hosp_id",
                 timevar = "interval.x",
                 v.names = c("sofa_total", "sirs_total", "alb", "alt", "ast", "base_excess",        
                             "bicarb", "bili", "bun", "cl", "creat", "dbp", "elix", "fio2", "gcs", 
                             "gluc", "hr", "hgb", "inr", "lactate", "map", "mechvent", "paco2", "pao2",                 
                             "pf_ratio", "ph", "plt", "k", "rr", "shock_index", "na", "o2_sat","sbp", "temp",                 
                             "wbc", "fluid", "fluid_sum", "urine", "urine_sum", "fluid_balance", "norepi_equiv",
                             "max_prev_norepi_equiv", "PulsePress", "fluid_cckg", "fluid_sum_cckg", "icu"),
                 direction = "wide")

wupmc <- reshape(mupmc,
                 idvar = "hosp_id",
                 timevar = "interval.x",
                 v.names = c("sofa_total"),
                 direction = "wide")

View(wumpc)

warnings(wupmc)

colnames(mupmc)

str(upmc)

height <- read.csv("C:/Users/vadim/OneDrive/Desktop/UPMC/UPMC Heights.csv")

upmc <- merge(upmc, height, by = "hosp_id", all = TRUE)



upmc <- upmc |>
  mutate(pulsepress = sbp - dbp)

calculate_egfr <- function(creat, age, gender) {
  a <- ifelse(gender == "0", 101.9, 138.2)
  b <- ifelse(gender == "0", 0.80, 1.05)
  c <- ifelse(gender == "0", -0.742, -0.918)
  
  egfr <- a * ((creat / b) ^ c) * (0.993 ^ age)
  
  return(egfr)
}

upmc <- upmc |>
  mutate(egfr = calculate_egfr(creat, age, gender))

upmc <- upmc |>
  mutate(fluid_cckg = fluid / weight)

upmc <- upmc |>
  mutate(fluid_sum_cckg = fluid_sum / weight)
#Will adjust for height after adding that data



#################
# Load the dplyr package
library(dplyr)

# Sample dataset, replace this with your own
# hosp_id and interval are assumed to be in ascending order for each patient
df <- data.frame(
  hosp_id = c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5),
  interval = c(0, 1, 2, 0, 1, 0, 1, 2, 0, 1, 2, 0, 1, 2),
  sbp = c(110, 100, 105, 110, 115, 90, 95, 100, 167, 187, 198, 166, 166, 146),
  map = c(70, 60, 65, 80, 85, 50, 55, 60, 67, 87, 98, 66, 66, 46),
  norepi_equiv = c(0, 0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0)
)

# # Function to create the 'time' column for each group of hosp_id  # Find the first row that meets any of the criteria
create_time_column <- function(df) {
   first_row <- which.min(
     with(df, ifelse(sbp < 101 | map < 66 | norepi_equiv > 0, interval, Inf)))}
  
summary(create_time_column)

# Assuming 'df' is your dataframe and 'first_row' is the index of your identified row
n_rows <- nrow(df)
df$time <- seq(from = -(first_row - 1), to = n_rows - first_row)


create_time_column <- function(df) {
  first_row <- which.min(
    with(df, ifelse(sbp < 101 | map < 66 | norepi_equiv > 0, interval, Inf)))
  
  n_rows <- nrow(df)
  df$time <- seq(from = -(first_row - 1), to = n_rows - first_row)
  
  return(df)
}


library(dplyr)

##### THIS IS THE CODE THAT WORKS
create_time_column <- function(df) {
  df |>
    group_by(hosp_id) |>
    mutate(first_row = which.min(ifelse(sbp < 101 | map < 66 | norepi_equiv > 0, interval, Inf))) |>
    mutate(time = row_number() - first_row)|>
    ungroup() 
}

# Apply the function to your dataframe
df <- create_time_column(df)

####REMOVE PTS NOT HOTN OR IN SHOCK
df_filtered <- df |>
  group_by(hosp_id) |>                 # Group by individual
  filter(any(sbp < 101 | map < 66 | norepi_equiv > 0)) |>  # Keep if any record meets criteria
  ungroup()   

# Apply the function to your dataframe
df <- create_time_column(df)


which.min

  # If no row meets the criteria, return NULL to exclude this patient
  if (is.infinite(df$interval[first_row])) {
    return(NULL)
  }
  
q() 


  # Create the 'time' column
  df$time <- df$interval - df$interval[first_row]
  
  return(df)
}

# Apply the function to each group of hosp_id and filter out NULLs
new_data <- data %>%
  arrange(hosp_id, interval) %>%
  group_by(hosp_id) %>%
  group_modify(~ create_time_column(.)) %>%
  filter(!is.null(time))

# View the new dataset
print(new_data)
###############################

?slider
