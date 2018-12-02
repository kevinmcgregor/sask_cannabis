# Probability of getting 4 or more
# cannabis licenses in SK

c.data <- read.csv("~/Dropbox/website_projects/sask_cannabis/sk_cannabis_data.csv",
                   row.names = 1)
n <- NROW(c.data)

# Calculate probability of getting a permit in each city
permit.prob <- c.data$permits/c.data$submissions
names(permit.prob) <- rownames(c.data)

# Calculating probability of getting permits in 4 specific cities 
# (where Prairie Sky Cannabis were awarded permits):
cities <- c("Battleford", "Estevan", "Martensville", "Moosomin")
ps.prob <- prod(permit.prob[cities])

# Simulation to see probability of getting 4 or more permits in ANY city
# (Assuming you can only get one permit per city)
n.sim <- 10000
n.permit <- rep(0, n.sim)

for (i in 1:n.sim) {
  np.obtained <- rbinom(n, rep(1, n), permit.prob)
  n.permit[i] <- sum(np.obtained)
}

# Prob of getting 4 or more permits
mean(n.permit>=4)

# Distribution of number of permits
library(ggplot2)

p.df <- data.frame(Permit=n.permit)
ggplot(data=p.df, aes(Permit)) + geom_histogram() +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold")) +
    labs(x="Number of permits obtained")
ggsave(filename = "~/Dropbox/website_projects/sask_cannabis/plots/permit_hist.png", device="png")


# Calculating probability of 4 or more permits if you can obtain 
# multiple permits in each city
avail.permit <- sum(c.data$permits) # Total number of permits available
n.wo.permit <- sum(c.data$submissions) - avail.permit # Total number ending up without permits
# Prob of getting 4 or more permits
1-phyper(3, avail.permit, n.wo.permit, avail.permit)

# Would binomial have worked as well here?
1-pbinom(3, avail.permit, avail.permit/(avail.permit+n.wo.permit))



