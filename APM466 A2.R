# risk neutral probability & basic information
rf = 0
u = 1.1
d = 1/1.1
p = (1+rf-d)/(u-d)
s0 = 1
X = 1
N = 52

# underlying pricing tree
underlying_tree <- data.frame (matrix(nrow=N+1,ncol=N+1))

for (j in 1:(N+1)) {
  for (i in 1:(j)) {
    underlying_tree[i,j] = u^(j-i) * d^(i-1) * s0
  }}

# option payoff if exercise right now
exercise_payoff <- function(St, X, option) {
  if (option == "call") { payoff <- max(0, St - X) } 
  if (option == "put") { payoff <- max(0, X - St) }
  return(payoff)
}

########## up-swing option price #########

# 1-up-swing option tree
up_swing_tree_1 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  up_swing_tree_1[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "call")
}
for (j in c(N:1)) {
  for (i in c(1:j)) {
    exercise_now = exercise_payoff(underlying_tree[i,j], X, "call")
    wait = p * up_swing_tree_1[i,j+1] + (1-p) * up_swing_tree_1[i+1, j+1]
    up_swing_tree_1[i,j] = max(exercise_now, wait)
  }}

# 2-up-swing option tree
up_swing_tree_2 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  up_swing_tree_2[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "call")
}
for (j in c(N:1)) {
  for (i in c(1:j)) {
    wait = p * up_swing_tree_2[i,j+1] + (1-p) * up_swing_tree_2[i+1, j+1]
    exercise_now = exercise_payoff(underlying_tree[i,j], X, "call") + up_swing_tree_1[i,j]
    up_swing_tree_2[i,j] = max(exercise_now, wait)
  }}


# 3-up-swing option tree
up_swing_tree_3 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  up_swing_tree_3[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "call")
}

for (j in c(N:1)) {
  for (i in c(1:j)) {
    wait = p * up_swing_tree_3[i,j+1] + (1-p) * up_swing_tree_3[i+1, j+1]
    if (j == N) {exercise_now = exercise_payoff(underlying_tree[i,j], X, "call") + up_swing_tree_1[i,j]}
    else {exercise_now = exercise_payoff(underlying_tree[i,j], X, "call") + up_swing_tree_2[i,j]}
    up_swing_tree_3[i,j] = max(exercise_now, wait)
  }}

# create optimal node table to record node at which option is exercised
optimal_node_up <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (j in (N+1):1) {
  for (i in c(1:j)) { optimal_node_up[i,j]=0 }
}

# 4-up-swing option tree
up_swing_tree_4 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  up_swing_tree_4[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "call")
}

for (j in N:1) {
  for (i in 1:j) {
    wait = p * up_swing_tree_4[i,j+1] + (1-p) * up_swing_tree_4[i+1, j+1]
    if (j == N) {exercise_now = exercise_payoff(underlying_tree[i,j], X, "call") + up_swing_tree_1[i,j]}
    else if (j == (N-1)) {exercise_now = exercise_payoff(underlying_tree[i,j], X, "call") + up_swing_tree_2[i,j]}
    else {exercise_now = exercise_payoff(underlying_tree[i,j], X, "call") + up_swing_tree_3[i,j]}
    up_swing_tree_4[i,j] = max(exercise_now, wait)
    
    # option payoff from exercising right now is greater than option price, then this node is regarded as optimal node
    # all the optimal nodes are marked as "1" in the optimal node table
    if (exercise_now > wait) {optimal_node_up[i,j] = 1}
  }}

four_up_swing_option_price <- up_swing_tree_4[1,1] *50


########## down-swing option price ##########

# 1-down-swing option tree
down_swing_tree_1 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  down_swing_tree_1[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "put")
}
for (j in c(N:1)) {
  for (i in c(1:j)) {
    exercise_now = exercise_payoff(underlying_tree[i,j], X, "put")
    wait = p * down_swing_tree_1[i,j+1] + (1-p) * down_swing_tree_1[i+1, j+1]
    down_swing_tree_1[i,j] = max(exercise_now, wait)
  }}

# 2-down-swing option tree
down_swing_tree_2 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  down_swing_tree_2[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "put")
}
for (j in c(N:1)) {
  for (i in c(1:j)) {
    wait = p * down_swing_tree_2[i,j+1] + (1-p) * down_swing_tree_2[i+1, j+1]
    exercise_now = exercise_payoff(underlying_tree[i,j], X, "put") + down_swing_tree_1[i,j]
    down_swing_tree_2[i,j] = max(exercise_now, wait)
  }}

# 3-down-swing option tree
down_swing_tree_3 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  down_swing_tree_3[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "put")
}

for (j in c(N:1)) {
  for (i in c(1:j)) {
    wait = p * down_swing_tree_3[i,j+1] + (1-p) * down_swing_tree_3[i+1, j+1]
    if (j == N) {exercise_now = exercise_payoff(underlying_tree[i,j], X, "put") + down_swing_tree_1[i,j]}
    else {exercise_now = exercise_payoff(underlying_tree[i,j], X, "put") + down_swing_tree_2[i,j]}
    down_swing_tree_3[i,j] = max(exercise_now, wait)
  }}

# create optimal node table to record node at which option is exercised
optimal_node_down <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (j in (N+1):1) {
  for (i in c(1:j)) { optimal_node_down[i,j]=0 }
}

# 4-down-swing option tree
down_swing_tree_4 <- data.frame (matrix(nrow=N+1,ncol=N+1))
for (i in 1:(N+1)) {
  down_swing_tree_4[i,N+1] <- exercise_payoff(underlying_tree[i,N+1], X, "put")
}

for (j in c(N:1)) {
  for (i in c(1:j)) {
    wait = p * down_swing_tree_4[i,j+1] + (1-p) * down_swing_tree_4[i+1, j+1]
    if (j == N) {exercise_now = exercise_payoff(underlying_tree[i,j], X, "put") + down_swing_tree_1[i,j]}
    else if (j == (N-1)) {exercise_now = exercise_payoff(underlying_tree[i,j], X, "put") + down_swing_tree_2[i,j]}
    else {exercise_now = exercise_payoff(underlying_tree[i,j], X, "put") + down_swing_tree_3[i,j]}
    down_swing_tree_4[i,j] = max(exercise_now, wait)
    
    # option payoff from exercising right now is greater than option price, then this node is regarded as optimal node
    # all the optimal nodes are marked as "1" in the optimal node table
    if (exercise_now > wait) {optimal_node_down[i,j] = -1}
  }}

four_up_swing_option_price <- up_swing_tree_4[1,1] *50

four_down_swing_option_price <- down_swing_tree_4[1,1] *50000

cat("Price of 4-up swing option is: ", four_up_swing_option_price, 
    "\nPrice of 4-down swing option is: ", four_down_swing_option_price)
