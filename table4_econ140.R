# Version9

```r
install.packages("haven")
install.packages("lmtest")
library(haven)
library(lmtest)

data4web <- read_dta("ECON140Proj/data4web.dta")

regions <- c("default", "sasia1", "easia1", "ssafr1", "menaf1", "latca1", "highi1", "midin1", "lowin1", "least1")
# regions <- c("default", "sasia1", "easia1", "ssafr1")
create_lms4 <- function(regions) {
  lms <- list()
  for (r1 in regions) {
    # If region is "all_data", don't subset. Use the full dataset.
    if(r1 == "default") {
      curr_lm <- lm(ltrade ~bothin + onein + gsp + ldist + lrgdp + lrgdppc + rta +
                      custrict + comlang + border + landl + island + lareap + comcol
                    + curcol + colony + comctry + factor(year), data = data4web)
    } else {
      
      prefix <- substr(r1, 1, nchar(r1) - 1)
      suffix <- substr(r1, nchar(r1), nchar(r1))
      new_suffix <- as.numeric(suffix) + 1
      new_string <- paste0(prefix, new_suffix)
      r2 <-new_string
      
      region_data <- subset(data4web, data4web[[r1]] == 1 & data4web[[r2]] == 1)
      # View(region_data)
      curr_lm <- lm(ltrade ~ bothin + onein + gsp + ldist + lrgdp + lrgdppc + rta +
                      custrict + comlang + border + landl + island + lareap + comcol
                    + curcol + colony + comctry+ factor(year), data = region_data)
    }
    lms[[r1]] <- curr_lm
  }
  return(lms)
}

create_table4 <- function(lms) {
  # Header
  cat("Region\tbothin\t\t\tonein\t\t\tgsp\n")
  cat("---------------------------------------------------------------------\n")
  for (region in names(lms)) {
    lm <- lms[[region]]
    # Compute robust standard errors
    # robust_se <- vcovHC(lm, type = "HC1") # "HC1" corresponds to heteroskedasticity-consistent SE
    # ct <- coeftest(lm, robust_se)
    ct <- summary(lm)
    # Extract coefficients for bothin, onein, and gsp
    bothin_est <- ct$coefficients["bothin", "Estimate"]
    bothin_se <- ct$coefficients["bothin", "Std. Error"]
    onein_est <- ct$coefficients["onein", "Estimate"]
    onein_se <- ct$coefficients["onein", "Std. Error"]
    gsp_est <- ct$coefficients["gsp", "Estimate"]
    gsp_se <- ct$coefficients["gsp", "Std. Error"]
    # Print the results
    output <- paste0(region, "\t",
                     round(bothin_est, 2), "(", round(bothin_se, 2), ")\t",
                     round(onein_est, 2), "(", round(onein_se, 2), ")\t",
                     round(gsp_est, 2), "(", round(gsp_se, 2), ")")
    cat(output, "\n")
  }
}

create_table3 <- function(lms) {
  for (lm in lms) {
    curr_sum <- summary(lm, vcov = sandwich::vcovHC(lm, type="HC1"))
    bothin_est <- curr_sum$coefficients["bothin", "Estimate"]
    bothin_se <- curr_sum$coefficients["bothin", "Std. Error"]
    
    
    output <- tryCatch({
      paste0("bothin: ", round(bothin_est, 2), "  (", round(bothin_se, 2), 
             ")  onein: ",round(curr_sum$coefficients["onein", "Estimate"], 2), 
             "  (",  round(curr_sum$coefficients["onein", "Std. Error"], 2) , ")")
    }, error = function(err) {
      paste0("bothin: ", round(bothin_est, 2), "  (", round(bothin_se, 2), 
             ")  onein: NA (NA) -- (multicollinearity)")
    })
    
    
    print(output)
  }
}
# Run the code to create lms and display the table
lms <- create_lms4(regions)
rm(data4web)
create_table3(lms)
```
