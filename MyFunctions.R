#### FUNCTIONS


# FUNCTION
#### CHECK MODEL ASSUMPTIONS ####
#(Generalized) Linear models make some strong assumptions concerning the data structure:

# Normality of Residuals (QQ plot, density plot of residuals)
# Correct specification of the variance structure; Evaluate homoscedasticity (plot residuals vs. fitted values)

fix.check <- function(mod){    #function to produce model-checking plots for the fixed effects of an lmer model
  par(mfrow = c(2,2))
  plot(fitted(mod),resid(mod))  #should have no pattern
  abline(h=0)
  print(anova(lm(fitted(mod)~resid(mod))))	#should be non-significant
  qqnorm(resid(mod), ylab="Residuals")		#should be approximately straight line
  qqline(resid(mod))
  plot(density(resid(mod)))					#should be roughly normally distributed
  rug(resid(mod))}


#There are 4 main assumptions to check for a basic linear model like this.
# Model check
ModelCheck <- function(fit){
  # Normality (of residuals)
  p1 <- ggplot(NULL) +
    aes(x = residuals(fit)) +
    geom_bar()
  
  # Constant Error Variance (of residuals)
  p2 <- ggplot(NULL) +
    aes(x = fitted(fit), y = residuals(fit)) +
    geom_hline(yintercept = 0, colour = "grey") +
    geom_point()
  
  # Independence (of residuals)
  p3 <- ggplot(NULL) +
    aes(y = residuals(fit), x = 1:length(residuals(fit)) ) +
    geom_point()
  check.plot <- plot_grid(p1, p2, p3, nrow = 2)
  check.plot
}


### Tidy results
TidyResults <- function(res, treatment){
  dat <- tidy(res, fit, effects = "fixed") %>% 
    mutate(estimate = (round(exp(estimate), 2)), 
           std.error = round(std.error, 2), 
           statistic = round(statistic, 2), 
           p.value = round(p.value, 3),
           term = gsub("Treatment", "", term),
           signif = ifelse(p.value < 0.05, 1, 0)) %>% 
    mutate(Comparison = treatment) %>% 
    rename(Treatment = term)
  return(dat)
}