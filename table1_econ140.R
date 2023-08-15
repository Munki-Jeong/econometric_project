install.packages("haven")
install.packages("sandwich")
install.packages("lmtest")
install.packages("multiwayvcov")
library(haven)
library(sandwich)
library(lmtest)
library(multiwayvcov)
data4web <- read_dta("data4web.dta")

#lm1.1, lm1.2, lm1.3 # which corresponds to each column 1, 2, 3, respectively
#add all the values from same i,j across years
lm1.1 <- lm(ltrade ~ bothin + onein + gsp + ldist + lrgdp + lrgdppc + regional +
              custrict + comlang + border + landl + island + lareap + comcol
            + curcol + colony + comctry + factor(year), data=data4web)
lm1.2 <- lm(ltrade ~ bothin + onein + gsp + ldist + lrgdp + lrgdppc + regional +
              custrict + comlang + border + landl + island + lareap + comcol
            + curcol + colony + comctry + factor(year),
            data = data4web[data4web$cty1 >= 200 & data4web$cty2 >= 200,])
lm1.3 <- lm(ltrade ~ bothin + onein + gsp + ldist + lrgdp + lrgdppc + regional +
              custrict + comlang + border + landl + island + lareap + comcol
            + curcol + colony + comctry + factor(year),
            data = data4web[data4web$year >= 1971,])
#lm1.4 does not work for most case, because of memory issue
# lm1.4 <- lm(ltrade ~ bothin + onein + gsp + ldist + lrgdp + lrgdppc + rta +
#               custrict + comlang + border + landl + island + lareap + comcol
#             + curcol + colony + comctry + country_pair_factor + factor(year),
#             data=data4web)

#to compute robust standard error clustered by countrypair
data4web$country_pair <- interaction(data4web$cty1, data4web$cty2)
clustered_se1 <- cluster.vcov(lm1.1, ~ country_pair)
clustered_se2 <- cluster.vcov(lm1.2, ~ country_pair)
clustered_se3 <- cluster.vcov(lm1.3, ~ country_pair)
#clustered_se4 <- cluster.vcov(lm1.4, ~ country_pair)
ct1 <- coeftest(lm1.1, clustered_se1)
ct2 <- coeftest(lm1.2, clustered_se2)
ct3 <- coeftest(lm1.3, clustered_se3)
#ct4 <- coeftest(lm1.4, clustered_se4)

#code for create table
create_table1 <- function(cts) {
  vars <- c("bothin", "onein", "gsp", "ldist", "lrgdp", "lrgdppc", "regional",
            "custrict", "comlang", "border", "landl", "island", "lareap",
            "comcol", "curcol", "colony", "comctry")
  labels <- c("Default", "no industrial countries", "post 1970")
  cat(paste("Regressor", paste(labels, collapse = "\t"), sep = "\t"), "\n")
  cat(rep("-", 100), "\n")

  for (v in vars) {
    results <- sapply(cts, function(ct) {
      if (v %in% rownames(ct)) {
        est <- round(ct[v, "Estimate"], 2)
        se <- round(ct[v, "Std. Error"], 2)
        return(paste0(est, " (", se, ")"))
      } else {
        return("NA (NA)")
      }
    })
    cat(paste(v, paste(results, collapse = "\t"), sep = "\t"), "\n")
  }
}

#cts <- list(ct1, ct2, ct3, ct4)
cts <- list(ct1, ct2, ct3)
create_table1(cts)