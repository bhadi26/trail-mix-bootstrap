# trail mix 
library(ggplot2)
library(cowplot)

peanuts <- rpois(1, lambda = 10)
raisins <- rpois(1, lambda = 8)


# what do i have in my handful? 
# number of items drawn in handful
handful <- 4

# distribution of the number of peanuts in my handful 

gen_plot <- function(samples){
draws <- vector(mode='numeric', length=samples)
for (i in 1:samples) {
  draws[i] <- rhyper(1, peanuts, raisins, handful)
}

proportion_peanuts <- draws / (handful)
true_prop <- peanuts / (peanuts+raisins)

df <- data.frame(proportion_peanuts) 
colnames(df) <- c('proportion')
p <- ggplot(data=df, aes(x=proportion)) +
  geom_histogram(bins=10) + 
  geom_vline(xintercept=true_prop,col='red') + 
  geom_vline(xintercept=mean(df$proportion),col='blue') + 
  theme_minimal() + 
  ggtitle(paste(samples, "Samples of size", handful, "\nEst vs. True Proportion of Peanuts in Trail Mix\n",peanuts,"Peanuts and",raisins,"Raisins",round(true_prop,2),'%')) 

return (p)
}

# different number of samples
p1 <- gen_plot(samples=5)
p2 <- gen_plot(samples=10)
p3 <- gen_plot(samples=50)
p4 <- gen_plot(samples=100)
p5 <- gen_plot(samples=500)
p6 <- gen_plot(samples=1000)

# plot all together
plot_grid(p1, p2, p3, p4, p5, p6)



