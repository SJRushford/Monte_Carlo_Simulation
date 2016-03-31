# basic parameters
unit_cost <- 1.5 # per unit variable cost 
unit_price <- 4 # unit price per item
unit_disp <- 0.20 # cost for the disposal of unsold inventories

# simulate 3000 profits for 20K, 40K & 60K units each
profit <- rep(NA, 3000)

for (i in 1:3000){
  produced = rep(seq(20000,60000, by = 20000),1000) # generate 3000 variables
  prob[i] <- runif(1) # generate 3000 iteration of probabilities
  if(prob[i] <= 0.1){ demand[i] = 20000 } else if(prob[i] > 0.1 && prob[i] <= 0.45){demand[i] = 40000 } else {demand[i] = 60000 }
  if(produced[i] < demand[i]){ revenue[i] = produced[i]*unit_price } else { revenue[i] = demand[i]*unit_price }
  total_var_cost[i] <- produced[i]*unit_cost
  total_disp_cost[i] <- unit_disp*if(produced[i] > demand[i]){ produced[i] - demand[i] }else { 0 }
  profit[i] <- revenue[i]-(total_var_cost[i]+total_disp_cost[i])
}

# bind all parameters together
data <- cbind(produced, demand, revenue, total_var_cost, total_disp_cost, profit)
data <- data.frame(data, row.names = NULL)

# segregating data for production unit of 20000
first_slab <- data[data$produced==20000, ]
mu1 <- mean(first_slab$profit)
sd1 <- sd(first_slab$profit)
t1 <- sd1/mu1

# segregating data for production unit of 40000
second_slab <- data[data$produced==40000,]
mu2 <- mean(second_slab$profit)
sd2 <- sd(second_slab$profit)
t2 <- sd2/mu2

# segregating data for production unit of 60000
third_slab <- data[data$produced == 60000,]
mu3 <- mean(third_slab$profit)
sd3 <- sd(third_slab$profit)
t3 <- sd3/mu3

# creating vectors of the results
profit <- c(mu1, mu2, mu3)
risk <- c(sd1, sd2, sd3)
t_off <- c(t1, t2, t3)

# risk analysis
risk_return <- rbind(profit, risk, t_off)
risk_analysis <- data.frame(risk_return, row.names = NULL)
row.names(risk_analysis) <- c("Profit", "Risk", "Risk/Return Trade Off")
colnames(risk_analysis) <- c("20K", "40K", "60K")

# Print the risk analysis profile for all our production quantity
print(risk_analysis)

# Confidence Interval at 95% confidence level
upper_limit <- risk_analysis[1,2]+1.96*risk_analysis[2,2]/sqrt(1000)
lower_limit <- risk_analysis[1,2]-1.96*risk_analysis[2,2]/sqrt(1000)


# print the findings
print('At 95% Confidence Interval:')

print(paste("Upper Bound", sep = ": ", upper_limit)) 
print(paste("Lower Bound: ", sep = ": ", lower_limit))

