lambda_young <-  0.3
lambda_old <- 0.2

#Task 1
#Probability Matrix

young_p0 <- dpois(0,lambda_young)
young_p1 <- dpois(1,lambda_young)
young_p2 <- 1 - young_p0 - young_p1

old_p0 <- dpois(0,lambda_old)
old_p1 <- dpois(1,lambda_old)
old_p2 <- 1 - old_p0 - old_p1

#Matrix Table 1 Young
t1_row_0_young <- c(young_p0,young_p1,young_p2,0,0)
t1_row_1_young <- c(young_p0,0,young_p1,young_p2,0)
t1_row_2_young <- c(0,young_p0,0,young_p1,young_p2)
t1_row_3_young <- c(0,0,young_p0,0,young_p1+young_p2)
t1_row_4_young <- c(0,0,0,young_p0,young_p1+young_p2)
MATRIX_t1_young <- rbind(t1_row_0_young,t1_row_1_young,t1_row_2_young,t1_row_3_young,t1_row_4_young)
print(MATRIX_t1_young)

#Matrix Table 1 Old
t1_row_0_old <- c(old_p0,old_p1,old_p2,0,0)
t1_row_1_old <- c(old_p0,0,old_p1,old_p2,0)
t1_row_2_old <- c(0,old_p0,0,old_p1,old_p2)
t1_row_3_old <- c(0,0,old_p0,0,old_p1+old_p2)
t1_row_4_old <- c(0,0,0,old_p0,old_p1+old_p2)
MATRIX_t1_old <- rbind(t1_row_0_old,t1_row_1_old,t1_row_2_old,t1_row_3_old,t1_row_4_old)
print(MATRIX_t1_old)

#Matrix Table 2 Young
t2_row_0_young <- c(young_p0,young_p1,young_p2)
t2_row_1_young <- c(young_p0,0,young_p1+young_p2)
t2_row_2_young <- c(0,young_p0,young_p1+young_p2)
MATRIX_t2_young <- rbind(t2_row_0_young,t2_row_1_young,t2_row_2_young)
print(MATRIX_t2_young)

#Matrix Table 2 Old
t2_row_0_old <- c(old_p0,old_p1,old_p2)
t2_row_1_old <- c(old_p0,0,old_p1+old_p2)
t2_row_2_old <- c(0,old_p0,old_p1+old_p2)
MATRIX_t2_old <- rbind(t2_row_0_old,t2_row_1_old,t2_row_2_old)
print(MATRIX_t2_old)

#Matrix Table 3 Young
t3_row_0_young <- c(young_p0,young_p1,young_p2,0,0,0,0)
t3_row_1_young <- c(young_p0,0,young_p1,young_p2,0,0,0)
t3_row_2_young <- c(0,young_p0,0,young_p1,young_p2,0,0)
t3_row_3_young <- c(0,0,young_p0,0,young_p1,young_p2,0)
t3_row_4_young <- c(0,0,0,young_p0,0,young_p1,young_p2)
t3_row_5_young <- c(0,0,0,0,young_p0,0,young_p1+young_p2)
t3_row_6_young <- c(0,0,0,0,0,young_p0,young_p1+young_p2)
MATRIX_t3_young <- rbind(t3_row_0_young,t3_row_1_young,t3_row_2_young,t3_row_3_young,t3_row_4_young,t3_row_5_young,t3_row_6_young)
print(MATRIX_t3_young)

#Matrix Table 3 Old
t3_row_0_old <- c(old_p0,old_p1,old_p2,0,0,0,0)
t3_row_1_old <- c(old_p0,0,old_p1,old_p2,0,0,0)
t3_row_2_old <- c(0,old_p0,0,old_p1,old_p2,0,0)
t3_row_3_old <- c(0,0,old_p0,0,old_p1,old_p2,0)
t3_row_4_old <- c(0,0,0,old_p0,0,old_p1,old_p2)
t3_row_5_old <- c(0,0,0,0,old_p0,0,old_p1+old_p2)
t3_row_6_old <- c(0,0,0,0,0,old_p0,old_p1+old_p2)
MATRIX_t3_old <- rbind(t3_row_0_old,t3_row_1_old,t3_row_2_old,t3_row_3_old,t3_row_4_old,t3_row_5_old,t3_row_6_old)
print(MATRIX_t3_old)

#Task 1 Part 2
#2000 Simulations
policyholders <- read.csv("AgeLevelData.csv")
t1_young_premiums <- c(630, 720, 810, 900, 1170)
t1_old_premiums <- c(980, 1120, 1260, 1400, 1820)
premium_old <- 1400
expenses <- 3200
set.seed(100)
number_of_simulations <- 2000
number_of_young <- 5000
number_of_old <- 5000

young_start_levels <- policyholders$ncdlevel26[
  policyholders$age < 26
]
(young_start_levels)
calculate_next_levels <- function(start_levels, claims) {
  
  next_levels <- ifelse(
    claims == 0,
    pmax(start_levels - 1, 0),
    ifelse(
      claims == 1,
      pmin(start_levels + 1, 4),
      pmin(start_levels + 2, 4)
    )
  )
  
  return(next_levels)
}

t1_simulations_young_claims <- matrix(
  rpois(
    n = number_of_simulations * number_of_young,
    lambda = lambda_young
  ),
  nrow = number_of_simulations,
  ncol = number_of_young
)

t1_simulations_young_levels <- t(
  apply(
    t1_simulations_young_claims,
    MARGIN = 1,
    FUN = function(claim_row) {
      calculate_next_levels(
        start_levels = young_start_levels,
        claims = claim_row
      )
    }
  )
)

calculate_row_premium <- function(level_row, premiums) {
  
  level_counts <- tabulate(
    level_row + 1,
    nbins = length(premiums)
  )
  
  total_premium <- sum(level_counts * premiums)
  
  return(total_premium)
}

t1_young_total_premiums <- apply(
  t1_simulations_young_levels,
  MARGIN = 1,
  FUN = calculate_row_premium,
  premiums = t1_young_premiums
)

head(t1_young_total_premiums)

#Table 1 Old People
old_start_levels <- policyholders$ncdlevel26[
  policyholders$age < 26
]

t1_simulations_old_claims <- matrix(
  rpois(
    n = number_of_simulations * number_of_old,
    lambda = lambda_old
  ),
  nrow = number_of_simulations,
  ncol = number_of_old
)
t1_simulations_old_levels <- t(
  apply(
    t1_simulations_old_claims,
    MARGIN = 1,
    FUN = function(claim_row) {
      calculate_next_levels(
        start_levels = old_start_levels,
        claims = claim_row
      )
    }
  )
)
t1_old_total_premiums <- apply(
  t1_simulations_old_levels,
  MARGIN = 1,
  FUN = calculate_row_premium,
  premiums = t1_old_premiums
)

head(t1_old_total_premiums)

#Combining Old and Young
t1_total_premiums <-
  t1_young_total_premiums +
  t1_old_total_premiums

head(t1_total_premiums)
length(t1_total_premiums)

#Statisical Summary
premium_summary <- data.frame(
  Mean = mean(t1_total_premiums),
  Standard_Deviation = sd(t1_total_premiums),
  Minimum = min(t1_total_premiums),
  Lower_Quartile = quantile(t1_total_premiums, 0.25),
  Median = median(t1_total_premiums),
  Upper_Quartile = quantile(t1_total_premiums, 0.75),
  Maximum = max(t1_total_premiums)
)

round(premium_summary, 2)

hist(
  t1_total_premiums,
  breaks = 30,
  main = "Distribution of Simulated 2027 Premium Income",
  xlab = "Total premium income ($)",
  ylab = "Frequency"
)

abline(
  v = mean(t1_total_premiums),
  lwd = 2,
  lty = 2
)
text(
  x = mean(t1_total_premiums),
  y = par("usr")[4] * 0.9,
  labels = paste0("Mean = $", round(mean(t1_total_premiums), 0)),
  pos = 4,
  col = "red"
)

count_levels <- function(level_row) {
  
  tabulate(
    level_row + 1,
    nbins = 5
  )
}
t1_young_level_counts <- t(
  apply(
    t1_simulations_young_levels,
    MARGIN = 1,
    FUN = count_levels
  )
)

t1_old_level_counts <- t(
  apply(
    t1_simulations_old_levels,
    MARGIN = 1,
    FUN = count_levels
  )
)

t1_total_level_counts <-
  t1_young_level_counts +
  t1_old_level_counts

colnames(t1_total_level_counts) <- paste0("Level_", 0:4)

head(t1_total_level_counts)

level_summary <- data.frame(
  Level = 0:4,
  Mean = colMeans(t1_total_level_counts),
  Standard_Deviation = apply(t1_total_level_counts, 2, sd),
  Minimum = apply(t1_total_level_counts, 2, min),
  Median = apply(t1_total_level_counts, 2, median),
  Maximum = apply(t1_total_level_counts, 2, max)
)

level_summary[, -1] <- round(level_summary[, -1], 2)

level_summary

t1_profit <- mean(t1_total_premiums) - mean(t1_total_level_counts) *expenses
t1_profit


#Task 2
t2_young_premiums <- c(720, 810, 900)
t2_old_premiums <- c(1120, 1260, 1400)
t3_young_premiums <- c(540,630, 720, 810, 900, 1170,1440)
t3_old_premiums <- c(980, 1120, 1260, 1400, 1820)

build_matrix_table1 <- function(lambda) {
  p0 <- dpois(0, lambda)
  p1 <- dpois(1, lambda)
  p2 <- 1 - p0 - p1
  
  row_0 <- c(p0, p1, p2, 0, 0)
  row_1 <- c(p0, 0, p1, p2, 0)
  row_2 <- c(0, p0 ,0,p1, p2)
  row_3 <- c(0, 0, p0,0, p1+p2)
  row_4 <- c(0,0,0,p0, p1+p2)
  
  matrix_out <- rbind(row_0, row_1, row_2, row_3, row_4)
  return(matrix_out)
}

build_matrix_table2 <- function(lambda) {
  p0 <- dpois(0, lambda)
  p1 <- dpois(1, lambda)
  p2 <- 1 - p0 - p1
  
  row_0 <- c(p0, p1, p2)
  row_1 <- c(p0, 0, p1 +p2)
  row_2 <- c(0, p0 ,p1+ p2)
  
  matrix_out <- rbind(row_0, row_1, row_2)
  return(matrix_out)
}

build_matrix_table3 <- function(lambda) {
  p0 <- dpois(0, lambda)
  p1 <- dpois(1, lambda)
  p2 <- 1 - p0 - p1
  
  row_0 <- c(p0, p1, p2, 0, 0, 0, 0)
  row_1 <- c(p0, 0, p1, p2, 0, 0, 0)
  row_2 <- c(0, p0 ,0,p1, p2, 0, 0)
  row_3 <- c(0, 0, p0,0, p1,p2, 0)
  row_4 <- c(0,0,0,p0,0, p1,p2)
  row_5 <- c(0, 0,0,0, p0,0, p1+p2)
  row_6 <- c(0,0,0,0,0,p0, p1+p2)
  
  
  matrix_out <- rbind(row_0, row_1, row_2, row_3, row_4, row_5, row_6 )
  return(matrix_out)
}

st_distribution <- function(t_matrix) {
  
  number_of_levels <- nrow(t_matrix)
  
  A <- t(t_matrix) - diag(number_of_levels)
  
  # Replace the last equation with the condition that probabilities sum to 1
  A[number_of_levels, ] <- rep(1, number_of_levels)
  
  b <- c(
    rep(0, number_of_levels - 1),
    1
  )
  
  st_probabilities <- solve(A, b)
  
  return(as.numeric(st_probabilities))
}

P_Lambda <- function(lambda, t_function, premiums) {
  
  t_matrix <- t_function(lambda)
  
  stationary_probabilities <- st_distribution(
    t_matrix
  )
  
  mean_premium <- sum(
    stationary_probabilities * premiums
  )
  
  return(mean_premium)
}

delta_lambda <- 0.001

lambda_values <- seq(
  from = 0.01,
  to = 1,
  by = delta_lambda
)

lambda_values_extended <- c(
  lambda_values,
  max(lambda_values) + delta_lambda
)

t1_P_Lambda_young <- sapply(
  lambda_values_extended,
  function(lambda) {
    P_Lambda(
      lambda = lambda,
      t_function = build_matrix_table1,
      premiums = t1_young_premiums
    )
  }
)

t2_P_Lambda_young <- sapply(
  lambda_values_extended,
  function(lambda) {
    P_Lambda(
      lambda = lambda,
      t_function = build_matrix_table2,
      premiums = t2_young_premiums
    )
  }
)


t3_P_Lambda_young <- sapply(
  lambda_values_extended,
  function(lambda) {
    P_Lambda(
      lambda = lambda,
      t_function = build_matrix_table3,
      premiums = t3_young_premiums
    )
  }
)


calculate_loimaranta_efficiency <- function(lambda_values,
                                            mean_premiums,
                                            delta_lambda) {
  
  number_of_points <- length(lambda_values)
  
  p_lambda <- mean_premiums[
    1:number_of_points
  ]
  
  p_lambda_plus_delta <- mean_premiums[
    2:(number_of_points + 1)
  ]
  
  premium_derivative <- (
    p_lambda_plus_delta - p_lambda
  ) / delta_lambda
  
  efficiency <- (
    lambda_values / p_lambda
  ) * premium_derivative
  
  return(efficiency)
}

t1_efficiency_young <- calculate_loimaranta_efficiency(
  lambda_values = lambda_values,
  mean_premiums = t1_P_Lambda_young,
  delta_lambda = delta_lambda
)

t2_efficiency_young <- calculate_loimaranta_efficiency(
  lambda_values = lambda_values,
  mean_premiums = t2_P_Lambda_young,
  delta_lambda = delta_lambda
)

t3_efficiency_young <- calculate_loimaranta_efficiency(
  lambda_values = lambda_values,
  mean_premiums = t3_P_Lambda_young,
  delta_lambda = delta_lambda
)

efficiency_range <- range(
  t1_efficiency_young,
  t2_efficiency_young,
  t3_efficiency_young,
  1,
  na.rm = TRUE
)

plot(
  lambda_values,
  t1_efficiency_young,
  type = "l",
  lwd = 2,
  lty = 1,
  col = "black",
  ylim = efficiency_range,
  xlab = expression(lambda),
  ylab = expression(eta(lambda)),
  main = "Loimaranta Efficiency of the Three BMS Schemes"
)

lines(
  lambda_values,
  t2_efficiency_young,
  lwd = 2,
  lty = 2,
  col = "blue"
)

lines(
  lambda_values,
  t3_efficiency_young,
  lwd = 2,
  lty = 3,
  col = "red"
)

abline(
  h = 1,
  lty = 4,
  col = "grey40"
)

legend(
  "topright",
  legend = c(
    "Table 1 scheme",
    "Table 2 scheme",
    "Table 3 scheme",
    "Ideal efficiency"
  ),
  col = c(
    "black",
    "blue",
    "red",
    "grey40"
  ),
  lty = c(1, 2, 3, 4),
  lwd = c(2, 2, 2, 1),
  bty = "n"
)




#Task 2 Part 2
P_young_option0 <- P_Lambda(lambda = lambda_young, t_function = build_matrix_table1, premiums = t1_young_premiums)
P_old_option0   <- P_Lambda(lambda = lambda_old, t_function = build_matrix_table1, premiums = t1_old_premiums)

profit_option0 <- 5000 * (P_young_option0 - lambda_young * expenses) +
  5000 * (P_old_option0   - lambda_old   * expenses)


P_young_option1 <- P_Lambda(lambda = lambda_young, t_function = build_matrix_table2, premiums = t2_young_premiums)
P_old_option1   <- P_Lambda(lambda = lambda_old, t_function = build_matrix_table2, premiums = t2_old_premiums)

profit_option1 <- 5000 * (P_young_option1 - lambda_young * expenses) +
  5000 * (P_old_option1   - lambda_old   * expenses)

P_young_option2 <- P_Lambda(lambda = lambda_young, t_function = build_matrix_table3, premiums = t3_young_premiums)
P_old_option2   <- P_Lambda(lambda = lambda_old, t_function = build_matrix_table1, premiums = t3_old_premiums)

profit_option2 <- 5000 * (P_young_option2 - lambda_young * expenses) +
  5000 * (P_old_option2   - lambda_old   * expenses)

print(paste0("Profit for Option 0: $", round(profit_option0, 2)))
print(paste0("Profit for Option 1: $", round(profit_option1, 2)))
print(paste0("Profit for Option 2: $", round(profit_option2, 2)))


#Lambdas increase to 0.6 for both young and old
lambda_test <- 0.6
P_young_option0 <- P_Lambda(lambda = lambda_test, t_function = build_matrix_table1, premiums = t1_young_premiums)
P_old_option0   <- P_Lambda(lambda = lambda_test, t_function = build_matrix_table1, premiums = t1_old_premiums)

profit_option0 <- 5000 * (P_young_option0 - lambda_test * expenses) +
  5000 * (P_old_option0   - lambda_test   * expenses)


P_young_option1 <- P_Lambda(lambda = lambda_test, t_function = build_matrix_table2, premiums = t2_young_premiums)
P_old_option1   <- P_Lambda(lambda = lambda_test, t_function = build_matrix_table2, premiums = t2_old_premiums)

profit_option1 <- 5000 * (P_young_option1 - lambda_test * expenses) +
  5000 * (P_old_option1   - lambda_test   * expenses)

P_young_option2 <- P_Lambda(lambda = lambda_test, t_function = build_matrix_table3, premiums = t3_young_premiums)
P_old_option2   <- P_Lambda(lambda = lambda_test, t_function = build_matrix_table1, premiums = t3_old_premiums)

profit_option2 <- 5000 * (P_young_option2 - lambda_test * expenses) +
  5000 * (P_old_option2   - lambda_test   * expenses)

print(paste0("Profit for Option 0: $", round(profit_option0, 2)))
print(paste0("Profit for Option 1: $", round(profit_option1, 2)))
print(paste0("Profit for Option 2: $", round(profit_option2, 2)))