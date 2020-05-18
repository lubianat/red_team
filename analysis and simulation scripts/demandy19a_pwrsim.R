library("car")
library("dplyr")

# set-up 
rm(list = ls())
# allow for Type III SS
options(contrasts = c("contr.sum", "contr.poly"))

sim.DF <- data.frame()

for(i in 1:1000){
  n <- 100
  alpha <- .05
  # simulate data
  dem.neu <- cbind(cond = rep(x = "neu", times = n),
                   happ.diff = rnorm(n = n, mean = .25, sd = 1))
  #hist(as.numeric(dem.neu[,2]))
  
  dem.pos <- cbind(cond = rep(x = "pos", times = n),
                   happ.diff = rnorm(n = n, mean = .75, sd = 1))
  #hist(as.numeric(dem.pos[,2]))
  
  dem.neg <- cbind(cond = rep(x = "neg", times = n),
                   happ.diff = rnorm(n = n, mean = 0, sd = 1))
  #hist(as.numeric(dem.neg[,2]))
  
  DF <- data.frame(rbind(dem.neu, dem.pos, dem.neg))
  DF$happ.diff <- as.numeric(as.character(DF$happ.diff))
  #rm(dem.neg, dem.neu, dem.pos)
  
  # run analyses
    ## main effect of pose
    pose.p <- lm(happ.diff ~ 1, data = DF) %>% 
      Anova()
  
    ## condition interaction
    cond.p <- lm(happ.diff ~ cond, data = DF) %>% 
      Anova()
    
    ## subgroup analyses
    d.neu.p <- lm(happ.diff ~ 1, 
                  subset = cond == "neu", data = DF) %>%
      Anova()
    
    d.pos.p <- lm(happ.diff ~ 1, 
                  subset = cond == "pos", data = DF) %>%
      Anova()
    
    d.neg.p <- lm(happ.diff ~ 1, 
                  subset = cond == "neg", data = DF) %>%
      Anova()
    
  # save output
  sim.DF <- rbind(sim.DF,
                  cbind(n = n, 
                        pose.p = ifelse(pose.p$`Pr(>F)`[1] < alpha,
                                        1, 0),
                        cond.p = ifelse(cond.p$`Pr(>F)`[1] < alpha,
                                        1, 0),
                        d.neu.p = ifelse(d.neu.p$`Pr(>F)`[1] < alpha,
                                         1, 0),
                        d.pos.p = ifelse(d.pos.p$`Pr(>F)`[1] < alpha,
                                         1, 0),
                        d.neg.p = ifelse(d.neg.p$`Pr(>F)`[1] < alpha,
                                         1, 0)
                        )
                  )
}

# power for pose main effect
sum(sim.DF$pose.p) / nrow(sim.DF)

# power for condition interaction
sum(sim.DF$cond.p) / nrow(sim.DF)

# power for d.neu
sum(sim.DF$d.neu.p) / nrow(sim.DF)

# power for d.pos
sum(sim.DF$d.pos.p) / nrow(sim.DF)

# power for d.neg
sum(sim.DF$d.neg.p) / nrow(sim.DF)
