install.packages("haven")
install.packages("sandwich")
library(haven)

data4web <- read_dta("data4web.dta")
years <- unique(data4web$year)


# gives us a list of the all the lms from 1948 - 1998
create_lms <- function(years) {
  lms <- list()
  for (year in years) {
    curr_lm <- lm(ltrade ~ ldist + lrgdp + lrgdppc + comlang + border + landl + 
                    island + lareap + comcol + curcol + colony + comctry + 
                    custrict + rta + bothin + onein + gsp,
                  data=data4web[data4web$year == year, ])
    lms[[as.character(year)]] <- curr_lm
    
  }
  return(lms)
  
}

# creates the table based on the linear models
# essentially extracts the needed estimates and ses
create_table2 <- function(lms) {
  year <- 1948
  for (lm in lms) {
    if (year %% 5 == 0) {
      curr_sum <- summary(lm, vcov = sandwich::vcovHC(lm, type="HC1"))
      bothin_est <- curr_sum$coefficients["bothin", "Estimate"]
      bothin_se <- curr_sum$coefficients["bothin", "Std. Error"]
      
      onein_est <- curr_sum$coefficients["onein", "Estimate"]
      onein_se <- curr_sum$coefficients["onein", "Std. Error"]
      
      if (year <= 1965){
        gsp_est <- "----"
        gsp_se <- "-"
      } else {
        gsp_est <- curr_sum$coefficients["gsp", "Estimate"]
        gsp_se <- curr_sum$coefficients["gsp", "Std. Error"]
      }
      output <- tryCatch({
        paste0("year: ", year, "     bothin: ", round(bothin_est, 2), 
               "  (", round(bothin_se, 2), ")  onein: ", 
               round(onein_est, 2), "(",  round(onein_se, 2) ,
               ")   gsp: ", round(gsp_est, 2), " (", round(gsp_se, 2),
               ")")
      }, error = function(err) {
        paste0("year: ", year, "     bothin: ", round(bothin_est, 2), 
               "  (", round(bothin_se, 2), ")  onein: ", 
               round(onein_est, 2), "(",  round(onein_se, 2) ,
               ")   gsp: ", gsp_est, " (", gsp_se,
               ")")
      })
      print(output)
    }
    year <- year + 1
  }
}


lms <- create_lms(years)
create_table2(lms)