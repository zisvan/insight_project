setwd("/Users/zisvan/insight_project/")

library(dplyr)
library(ggplot2)

# files <- list.files(path = "../college_data/", pattern = "*.csv")

# get one file for now from 11-12
data <- read.csv("../college_data/MERGED2011_12_PP.csv", na.strings = "NULL")

# variables to select (not all were used, make this leaner)
colNames <- c("OPEID", "OPEID6",
              "MD_EARN_WNE_P10", "MD_EARN_WNE_P6", "MN_EARN_WNE_P6", "MN_EARN_WNE_P10",
              "MD_FAMINC", "DEP_INC_AVG", "IND_INC_AVG",
              "MN_EARN_WNE_INDEP0_P10", "MN_EARN_WNE_INDEP1_P10")

colNums <- match(colNames, names(data))

four_year <- data %>%
  filter(ICLEVEL == 1) %>%
  select(colNums) %>%
  filter(complete.cases(.)) %>%
  distinct()


# convert factors to numeric
four_year[colNames] <- sapply(four_year[colNames], as.character)
four_year[colNames] <- sapply(four_year[colNames], as.numeric)

#remove newly introduced NAs by numericizing
four_year <- na.omit(four_year)

four_year <- mutate(four_year, fam_bracket = ifelse(DEP_INC_AVG < 30000, "Low",
                        ifelse(DEP_INC_AVG > 75000, "High", "Mid")))

four_year$fam_bracket_f <- factor(four_year$fam_bracket, levels = c("Low", "Mid", "High"))


p <- ggplot(four_year, aes(x = DEP_INC_AVG, y = MN_EARN_WNE_INDEP0_P10)) + 
  geom_point(alpha = 0.2, color = "blue") + 
  geom_smooth(method = "lm", formula = y ~ x, color = "red") + 
  xlab("Mean family income (2015 $s)") +
  ylab("Mean earning 10 years after enrollment (2015 $s)") +
  facet_grid(. ~ fam_bracket_f, scale = "free")

# Fit quadratic
# p2 <- ggplot(four_year, aes(x = DEP_INC_AVG, y = MN_EARN_WNE_INDEP0_P10)) + 
#   geom_point(alpha = 0.2, color = "blue") + 
#   geom_smooth(method = "lm", formula = y ~ poly(x,2), color = "red") + 
#   xlab("Mean family income (2015 $s)") +
#   ylab("Mean earning 10 years after enrollment (2015 $s)")


# Fit outside of ggplot to extract fit parameters
fitLo <- lm(formula = MN_EARN_WNE_INDEP0_P10 ~ DEP_INC_AVG, 
          data = four_year, fam_bracket_f == "Low")
fitMd <- lm(formula = MN_EARN_WNE_INDEP0_P10 ~ DEP_INC_AVG, 
            data = four_year, fam_bracket_f == "Mid")
fitHi <- lm(formula = MN_EARN_WNE_INDEP0_P10 ~ DEP_INC_AVG, 
            data = four_year, fam_bracket_f == "High")

# store slopes and intercepts in a data frame
fits <- data.frame(m = c(summary(fitLo)$coeff[2], 
                       summary(fitMd)$coeff[2],
                       summary(fitHi)$coeff[2]),
                   b = c(summary(fitLo)$coeff[1],
                         summary(fitMd)$coeff[1], 
                       summary(fitHi)$coeff[1]),
                   br = c("Low", "Mid", "High"))


# Simple function to determine if graduates of school 
# are considered good earners
good_earner <- function(y, x, bracket){
  slope = fits[fits$br == bracket,"m"]
  #print(slope)
  intercept = fits[fits$br == bracket,"b"]
  #print(intercept)
  
  if (y >= slope*x + intercept){
    #print("True")
    return(TRUE)
  } 
  else{
    #print("False")
    return(FALSE)
    }
}

# test <- good_earner(2000, 20000, "Low")

# add earning column
four_year_earn <- four_year %>%
  rowwise() %>%
  mutate(earnings = ifelse(good_earner(MN_EARN_WNE_INDEP0_P10,
                                            DEP_INC_AVG,
                                            fam_bracket),"Above", "Below")) 

# Get IDs of selection so far to bind to full dataset
earn_id <- four_year_earn %>%
  select(OPEID, OPEID6, fam_bracket_f, earnings)
                    
# Bind this with full dataset to keep only the selected schools but all variables
keep_schools <- merge(data, earn_id, by=c("OPEID","OPEID6"))
keep_schools <- subset(keep_schools, !duplicated(keep_schools[,"OPEID"]))


# Write this small(er) file
# It's still not too small - I'll select and remove NA's once analysis variables 
# are determined
write.csv(keep_schools, file = "C:/Users/zisvan/college_data/keep_schools.csv", row.names = FALSE)
