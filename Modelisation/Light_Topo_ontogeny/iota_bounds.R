-10 -> beta2_p # -10 
beta1 <- 7*2*-exp(beta2_p) # 0

O <- -beta1/(2*(-exp(beta2_p)))
O = -7
O = 0

(-(O)-7)/log(100)
-(O)/log(100)

# VB vs GD
(-(-beta1/2*(-exp(beta2_p)))-7)/log(100) == ((beta1/(-2*exp(beta2_p))) - 7)/log(100)
-(-beta1/2*(-exp(beta2_p)))/log(100) == - beta1 / (2 * exp(beta2_p) * log(100))


(-(-beta1/(2*(-exp(beta2_p))))-7)/log(100) == ((beta1/(-2*exp(beta2_p))) - 7)/log(100)
-(-beta1/(2*(-exp(beta2_p))))/log(100) == - beta1 / (2 * exp(beta2_p) * log(100))

(-(-beta1/(2*(-exp(beta2_p))))-7)/log(100) # 0
-(-beta1/(2*(-exp(beta2_p))))/log(100)  # 1.5
