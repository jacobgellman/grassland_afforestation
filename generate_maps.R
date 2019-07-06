# Generate maps for:
# Birge, H.E., Bielski, C.H., Gellman, J.,
# Plantinga, A.J., Allen, C.R., Twidwell, D.,
# 2019. "Payments for carbon can lead to 
# unwanted costs from afforestation in the 
# U.S. Great Plains."

# libraries
library(tidyverse)
library(readxl)

# read in CPI data, series CUUR0000SA0 (used for CPI Inflation calculator)
CPI <- read_xlsx("raw_data/CPI_CUUR0000SA0.xlsx",range="A12:P23")

# inflation from 1997-2007
inflation_97_07 <- (CPI$Annual[length(CPI$Annual)] - CPI$Annual[1])/CPI$Annual[1]

# discount rate
discount <- 0.05

# social cost of carbon
carbon_cost <- 12

# read in 