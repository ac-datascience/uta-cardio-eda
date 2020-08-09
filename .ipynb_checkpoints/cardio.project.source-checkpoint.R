##########################################################################################################
# Project - Cardio Good Fitness source code
# Adrian Calvin
# Aug. 2020
##########################################################################################################

# Note: Code place in functions for easier re-use when creating notebook for report.

suppressMessages(library(lubridate))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(scales))

# Import data
cardio <- read.csv("CardioGoodFitness.csv")

########################################################################################################################
# Pre-process data for analysis/profile creation
########################################################################################################################
process_data <- function() {
    # Add group columns for age group and income group
    cardio <- mutate(
        cardio, 
        IncomeGroup    = cut(
                         Income, 
                         breaks = c(20000-1,30000-1,45000-1,60000-1,100000-1,Inf), 
                         labels=c("[20-30k)", "[30-45k)", "[45-60k)", "[60k-100k)", ">= 100k")),
        AgeGroup       = cut(Age, breaks = c(17, 19, 25, 29, 36, 59)),
        EducationGroup = cut(Education, breaks = c(0,12,15,16,99), labels=c("High School", "Some College", "Bachelors", "(Some)Grad School")))
        
    
    # Convert applicable columns to factors
    cardio$Gender = as.factor(cardio$Gender)
    cardio$Education = as.factor(cardio$Education)
    cardio$MaritalStatus = as.factor(cardio$Gender)
    cardio$Product = as.factor(cardio$Product)
    
    
    cardio
}

########################################################################################################################
# Generate and show Customer profile plots
########################################################################################################################
customer_profile_plots <- function() {
    par_backup = par()$mfrow
    par(mfrow=c(1, 3))
    
    factors <- c("EducationGroup", "AgeGroup", "IncomeGroup")
    for (f in factors) {
        print(ggplot(
            cardio, 
            aes(x = cardio[,f], fill = Product)
        ) + 
            labs(title = paste("Product by", f), x = f, y = "Product Count") + 
            geom_bar(position = "stack"))
        
    }
    
    suppressWarnings(par(mfrow=par_backup))
}
