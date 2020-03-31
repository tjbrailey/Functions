#' Cox Proximity Function
#' 
#' Takes a dataset of at least two types of election 
#' and calculates the electoral proximity of those 
#' elections, per the specification of Cox (1997).
#' 
#' This function requires the input of 
#' 
#' @param data the specified dataset. 

### Load test data 
burkina_faso <- readRDS(file = paste0(here::here(), "/ElectoralProximity/data/burkina_faso.rds"))

### The function
cox_prox <- function(data){
  
  ### Load required packages
  if(!require(tidyverse)){
    install.packages("tidyverse")
    library(tidyverse)
  }

  # Prepare columns
  dplyr::group_by(data, country_name, leg_elect_year)
  dplyr::mutate(data, next_election = dplyr::first(na.omit(pres_elect_year))) 
  dplyr::group_by(data, country_name) 
  tidyr::fill(data, next_election, .direction = "up")

  # Generate Cox's statistic  
  dplyr::mutate(data, cox_prox = ifelse(leg_elect_dum == 1,  
                                  2 * abs(
                                    (leg_elect_year - pres_elect_year_fill) /
                                      (next_election - pres_elect_year_fill) - (1/2)
                                    ), NA),
                cox_prox = ifelse(is.nan(cox_prox), 1, cox_prox))

}