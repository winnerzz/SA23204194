## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
library(knitr)
kable(head(women), caption = "First few rows of the women dataset.")

## -----------------------------------------------------------------------------
summary(women)

## -----------------------------------------------------------------------------
library(ggplot2)
library(DAAG)
library(boot)
library(bootstrap)
library(coda)
library(dplyr)

wo_plot<-ggplot(women, aes(x=weight, y=height)) + geom_point() + ggtitle("Scatter Plot of weight vs height")
wo_plot+xlab("weight (in)")+ ylab("height (lb)")

## -----------------------------------------------------------------------------
model <- lm(weight~ height, data=women)
summary(model)

## -----------------------------------------------------------------------------
plot(model, which=1)

## -----------------------------------------------------------------------------
mean_height <- mean(women$height)
median_height <- median(women$height)
sd_height <- sd(women$height)
c(mean=mean_height, median=median_height, sd=sd_height)

## -----------------------------------------------------------------------------
hist(women$height, main="Histogram of height", xlab="height(in)")

## -----------------------------------------------------------------------------
hist(women$weight, main="Histogram of Weight", xlab="Weight (lb)")

## -----------------------------------------------------------------------------
boxplot(women$weight, main="Boxplot of Weight", ylab="Weight (lb)")

## -----------------------------------------------------------------------------
set.seed(0)
n <- 1000
u <- runif(n) # Generate u from U(0,1)
x <- rep(0,n) # define x with length n
for(i in 1:n){
   if(u[i]<=1/2) # deliver x when u <=1/2
    x[i]<- log(2*u[i])
   else # deliver x when u >1/2
    x[i]<- -log(2*(1-u[i]))
} 

hist(x, prob = TRUE,main= expression("1/2e^(−|x|)"),ylim = c(0,.5)) # plot the hist of x
t<- seq(-100, 100, .001) 
y<- 1/2*exp(-abs(t))
lines(t, y) # plot the line of density function 1/2*e^(-|x|)

## -----------------------------------------------------------------------------
num <- 10000
a <- 2
b <- 2
x <- seq(0,1,0.01)
Prob <- dbeta(x,shape1=a,shape2=b)
plot(x,Prob,type='l')


## -----------------------------------------------------------------------------
num <- 10000
a2 <- 0.5
b2 <- 0.5
x2 <- seq(0,1,0.01)
Prob <- dbeta(x2,shape1=a2,shape2=b2)
plot(x2,Prob,type='l')

## -----------------------------------------------------------------------------
n <- 1e3;j<-k<-0;y <- numeric(n)
while (k < n) {
u <- runif(1)
j <- j + 1
x <- runif(1) #random variate from g(.)
if (x^2 * (1-x) > u) {
#we accept x
k <- k + 1
y[k] <- x
}
}
j

## -----------------------------------------------------------------------------
hist(y, prob = TRUE, main = expression(f(x)==12*x^2*(1-x)),ylim=c(0,2))
t <- seq(0, 1, .01)
lines(t, 12*t^2*(1-t))

## -----------------------------------------------------------------------------
q <- qbeta(ppoints(n), 3, 2)
qqplot(q, y, cex=0.25, xlab="Beta(3, 2)", ylab="Sample")
abline(0, 1)

## -----------------------------------------------------------------------------
my.function39<-function(n){
u1 <- runif(n,-1,1)
u2 <- runif(n,-1,1)
u3 <- runif(n,-1,1) #generate U1,U2,U3
  z<-rep(0,n)
for(i in 1:n)
if(abs(u3[i])>=abs(u2[i])&&abs(u3[i])>=abs(u1[i])) # The algorithm given in the question
  z[i]<- u2[i]
else
  z[i]<- u3[i]
return(z)
}

## -----------------------------------------------------------------------------
n<- 1e4
z<-my.function39(n)
hist(z,prob = TRUE,main = expression(f(x)==3/4*(1-x^2))) # 10000个数据
s <- seq(-1,1,.0001)
lines(s,(3/4)*(1-s*s))

## -----------------------------------------------------------------------------
my.sample<- function(x,size,replace = TRUE,prob = NULL){
l<- length(x)
 if(is.null(prob)) # when prob is null
  p <- rep(1/l,l) # Default equal probability distribution
 else
   p<-prob # Specified probability distribution weights
cp <- cumsum(p); 
m <- size; U = runif(size)
r <- x[findInterval(U,cp)+1] # Find i in the second step of the algorithm
return(r)
}


## -----------------------------------------------------------------------------
set.seed(0)
sample.1<-my.sample(1:3, size = 10000, replace = TRUE)
prob1 = c(1/3, 1/3 ,1/3)
ct1 <- as.vector(table(sample.1)); ct1/sum(ct1)/prob1

## -----------------------------------------------------------------------------
set.seed(0)
sample.2<-my.sample(1:3, size = 10000, replace = TRUE,prob = c(.2, .3, .5))
prob2 = c(.2, .3, .5)
ct2 <- as.vector(table(sample.2)); ct2/sum(ct2)/prob2

## -----------------------------------------------------------------------------

data1<-sample.1
data2<-sample.2
library(ggplot2)

combined_data <- data.frame(
  Group = factor(rep(c("equal prob", "given prob"), each = 10000)),
  Value = c(data1, data2)
)

# Create side-by-side histograms
histogram <- ggplot(combined_data, aes(x = Value, fill = Group)) +
  geom_histogram(binwidth = 0.2, position = "dodge") +
  labs(title = "Histogram of equal prob vs given prob", x = "Value", y = "Frequency") +
  scale_fill_manual(values = c("equal prob" = "blue", "given prob" = "red"))

# Show histogram
print(histogram)


## -----------------------------------------------------------------------------
c1<- my.sample(1:10, size = 10000, replace = TRUE,prob = c(.3, .2, .1, .1, .05, .05, .05, .05, .05, .05))
c2 <- sample(1:10, size = 10000, replace = TRUE,prob = c(.3, .2, .1, .1, .05, .05, .05, .05, .05, .05))

## -----------------------------------------------------------------------------

data1<-c1
data2<-c2
library(ggplot2)

combined_data <- data.frame(
  Group = factor(rep(c("Group 1", "Group 2"), each = 10000)),
  Value = c(data1, data2)
)

# Create side-by-side histograms
histogram <- ggplot(combined_data, aes(x = Value, fill = Group)) +
  geom_histogram(binwidth = 0.2, position = "dodge") +
  labs(title = "Histogram of my.sample vs sample", x = "Value", y = "Frequency") +
  scale_fill_manual(values = c("Group 1" = "blue", "Group 2" = "red"))

# Show histogram
print(histogram)


## -----------------------------------------------------------------------------
K <- 100
pihat1 <- rep(0,K)
rate <- c(0.4,0.7,1)
 for (i in 1:100){
    set.seed(i)
    d <- 1
    l <- d*0.4
    n <- 1e6
    
    X <- runif(n,0,d/2)
    Y <- runif(n,0,pi/2)
    pihat1[i] <- 2*l/d/mean(l/2*sin(Y)>X)}
pihat1

## -----------------------------------------------------------------------------
pihat2 <- rep(0,K)
 for (i in 1:100){
    set.seed(i)
    d <- 1
    l <- d*0.7
    n <- 1e6
    X <- runif(n,0,d/2)
    Y <- runif(n,0,pi/2)
    pihat2[i] <- 2*l/d/mean(l/2*sin(Y)>X)}
pihat2


## -----------------------------------------------------------------------------
pihat3 <- rep(0,K)
rate <- c(0.4,0.7,1)
 for (i in 1:100){
    set.seed(i)
    d <- 1
    l <- d*1
    n <- 1e6
    
    X <- runif(n,0,d/2)
    Y <- runif(n,0,pi/2)
    pihat3[i] <- 2*l/d/mean(l/2*sin(Y)>X)}
pihat3

## -----------------------------------------------------------------------------
sum((pihat1-pi)^2)
sum((pihat2-pi)^2)
sum((pihat3-pi)^2)

## -----------------------------------------------------------------------------
e<-exp(1)
e-(e-1)^2


## -----------------------------------------------------------------------------
e*e-1+2*e-4*(e-1)*(e-1)


## -----------------------------------------------------------------------------

 1-(0.5*(e*e-1)+e-2*(e-1)*(e-1))/(0.5*(e*e-1)-(e-1)*(e-1))

## -----------------------------------------------------------------------------
n <- 1e6
set.seed(1)
U <- runif(n)
U1 <- exp(U) 
U2 <- exp(1-U)
T1 <- (U1+U2)/2 # antithetic estimator of hat{theta}

## -----------------------------------------------------------------------------
mean(U1)


## -----------------------------------------------------------------------------
mean(T1)

## -----------------------------------------------------------------------------
(var(U1)-var(T1))/var(U1)

## -----------------------------------------------------------------------------
0.0162/0.9676

## -----------------------------------------------------------------------------
x <- seq(1.01, 10, .01)
g <- function(x) {x*x*exp(-x*x/2)/(2*pi)^0.5*(x>=1)}
plot(g,type = "l",xlim=c(1,5) , ylim = c(0, 0.4))

## -----------------------------------------------------------------------------
plot(g,xlim=c(1,5),type = "l", ylim = c(0, 0.7))
f1<-function(x){2*dnorm(x,mean=1,sd=1)*(x>=1)}
f2<-function(x){exp(1-x)*(x>=1)}
lines(x,f1(x),xlim=c(1,5),lty = 10,col=2)
lines(x,f2(x),xlim=c(1,5),lty = 6,col=3)
legend("topright", inset = 0.02, legend = c("g(x)", "f1","f2"), lty = c(1,6,1))


## -----------------------------------------------------------------------------
gg <- 10000*rep(1,length(x))
fg1 <- g(x)/f1(x)
fg2 <- g(x)/f2(x)

plot(x,gg,ylim = c(0, 0.9))
lines(x,fg1,lty = 1)
lines(x,fg2,lty = 2)
legend("topright", inset = 0.02, legend = c("g/f1","g/f2"), lty = c(1,2))

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
x <- abs(rnorm(m))+1
fg1 <- g(x) / f1(x)
x <- rexp(m,1)+1
fg2 <- g(x) / f2(x)
theta.hat = c(mean(fg1), mean(fg2))
var.hat = c(var(fg1), var(fg2)) / m
rbind(theta.hat, var.hat)

## -----------------------------------------------------------------------------
integrate(g, 1, Inf)$value

## -----------------------------------------------------------------------------
set.seed(0)
m <- 1e4
x <- abs(rnorm(m))+1
fg1 <- g(x) / f1(x)
x <- rexp(m,1)+1
fg2 <- g(x) / f2(x)
theta.hat = c(mean(fg1), mean(fg2))
var.hat = c(var(fg1), var(fg2)) / m
rbind(theta.hat, var.hat)

## -----------------------------------------------------------------------------
real<-integrate(g, 1, Inf)$value

## -----------------------------------------------------------------------------
mean(fg1)-real
mean(fg2)-real

## -----------------------------------------------------------------------------
set.seed(1)
M <- 10000
k <- 5
m <- M/k
theta.hat <- var.hat <- numeric(k)
g <- function(x) {exp(-x) / (1 + x*x) * (x>0) * (x<1)}
f <- function(x) {k * exp(-x) / (1 - exp(-1)) * (x>0) * (x<1)}
for (i in 1:k) {
  u <- runif(m, (i-1)/k, i/k)
  x <- -log(1 - u * (1 - exp(-1)))
  gf <- g(x) / f(x)
  theta.hat[i] <- mean(gf)
  var.hat[i] <- var(gf)
}
var1 <- sqrt(mean(var.hat))
c(sum(theta.hat), var1)

## -----------------------------------------------------------------------------
set.seed(1)
m <- 10000
g <- function(x) {exp(-x) / (1 + x*x) * (x>0) * (x<1)}
f <- function(x) {exp(-x) / (1 - exp(-1)) * (x>0) * (x<1)}
u <- runif(m)
x <- - log(1 - u*(1-exp(-1))) 
gf <- g(x) / f(x)
var2 <- sd(gf)
c(mean(gf), var2)

## -----------------------------------------------------------------------------
(var2-var1)/var2

## -----------------------------------------------------------------------------
n <- 20
t <- qt(c(0.025, 0.975), df = n - 1)

## -----------------------------------------------------------------------------
CI <- replicate(10000, expr = {
x <- rchisq(n, df = 2)
ci <- mean(x) + t * sd(x)/sqrt(n)# low confidence limit and upper confidence limit
})
LCL <- CI[1, ]
UCL <- CI[2, ]
sum(LCL < 2 & UCL > 2)
mean(LCL < 2 & UCL > 2)

## -----------------------------------------------------------------------------

alpha <- 0.05
n_simulations <- 10000
sample_size <- 10
population_mean <- 1  # True population mean for χ²(1)

simulate_t_test <- function() {
data <- rchisq(sample_size, df = 1)
t_result <- t.test(data, mu = population_mean)
# Return TRUE if null hypothesis is rejected (p-value < alpha), FALSE otherwise
  return(t_result$p.value < alpha)
}

set.seed(1)  
results <- replicate(n_simulations, simulate_t_test())
empirical_error_rate <- mean(results)

cat("Empirical Type I error rate:", empirical_error_rate, "\n")
cat("Significance level (alpha):", alpha, "\n")


## -----------------------------------------------------------------------------

alpha <- 0.05
n_simulations <- 10000
sample_size <- 10
population_mean <- 1  # True population mean for U(0,2)

simulate_t_test <- function() {
data <- runif(sample_size, min=0,max=2)
t_result <- t.test(data, mu = population_mean)
# Return TRUE if null hypothesis is rejected (p-value < alpha), FALSE otherwise
  return(t_result$p.value < alpha)
}

set.seed(1)  
results <- replicate(n_simulations, simulate_t_test())
empirical_error_rate <- mean(results)

cat("Empirical Type I error rate:", empirical_error_rate, "\n")
cat("Significance level (alpha):", alpha, "\n")


## -----------------------------------------------------------------------------
alpha <- 0.05
n_simulations <- 10000
sample_size <- 10
population_mean <- 1  # True population mean for Exponential(rate=1)

simulate_t_test <- function() {
data <- rexp(sample_size, rate=1)
t_result <- t.test(data, mu = population_mean)
# Return TRUE if null hypothesis is rejected (p-value < alpha), FALSE otherwise
  return(t_result$p.value < alpha)
}

set.seed(1)  
results <- replicate(n_simulations, simulate_t_test())
empirical_error_rate <- mean(results)

cat("Empirical Type I error rate:", empirical_error_rate, "\n")
cat("Significance level (alpha):", alpha, "\n")


## -----------------------------------------------------------------------------
set.seed(2)
library(dplyr)
# 初始化参数
M <- 1000
m <- 1000
alpha <- 0.1
n_h0 <- 0.95*m  # 原假设数量
n_h1 <- 0.05*m  # 对立假设数量

# 初始化结果存储矩阵
results <- matrix(0, nrow = M, ncol = 6)
colnames(results) <- c("TPR_Bonf", "FWER_Bonf", "FDR_Bonf", "TPR_BH", "FWER_BH", "FDR_BH")

for (i in 1:M) {
  # 生成p值
  p_values <- c(runif(n_h0, 0, 1), rbeta(n_h1, 0.1, 1))
  
  # 应用Bonferroni和BH校正
  p_adj_bonferroni <- p.adjust(p_values, method = "bonferroni")
  p_adj_bh <- p.adjust(p_values, method = "BH")
  
  # 计算统计指标
  reject_bonf <- p_adj_bonferroni < alpha
  reject_bh <- p_adj_bh < alpha
  
  TPR_Bonf <- sum(reject_bonf[(n_h0+1):m]) / n_h1
  FWER_Bonf <- sum(reject_bonf[1:n_h0]) > 0
  FDR_Bonf <- sum(reject_bonf[1:n_h0]) / max(sum(reject_bonf), 1)  # 防止除以0
  
  TPR_BH <- sum(reject_bh[(n_h0+1):m]) / n_h1
  FWER_BH <- sum(reject_bh[1:n_h0]) > 0
  FDR_BH <- sum(reject_bh[1:n_h0]) / max(sum(reject_bh), 1)  # 防止除以0
  
  # 保存结果
  results[i, ] <- c(TPR_Bonf, FWER_Bonf, FDR_Bonf, TPR_BH, FWER_BH, FDR_BH)
}

# 计算平均值
results_df <- as.data.frame(results)
mean_results <- colMeans(results_df, na.rm = TRUE)

# 输出结果
results <- tibble(Method = c("Bonferroni", "B-H"),
                   FWER = rep(0, 2),
                   FDR = rep(0, 2),
                   TPR = rep(0, 2))

results$FWER[1]<-mean_results[2]
results$FWER[2]<-mean_results[5]
results$FDR[1]<-mean_results[3]
results$FDR[2]<-mean_results[6]
results$TPR[1]<-mean_results[1]
results$TPR[2]<-mean_results[4]

print(results)



## -----------------------------------------------------------------------------
# Set the parameters
# 设置参数
true_lambda <- 2
sample_sizes <- c(5, 10, 20)
num_simulations <- 1000
num_bootstrap_replicates <- 1000

# 初始化变量来存储结果
bootstrap_biases <- matrix(0, nrow = length(sample_sizes), ncol = num_simulations)
bootstrap_standard_errors <- matrix(0, nrow = length(sample_sizes), ncol = num_simulations)
theoretical_biases <- numeric(length(sample_sizes))
theoretical_standard_errors <- numeric(length(sample_sizes))

# 对每个样本大小执行模拟
for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]
  
  for (j in 1:num_simulations) {
    # 从指数分布生成随机样本
    sample_data <- rexp(n, rate = true_lambda)
    
    # 计算lambda的最大似然估计（MLE）
    mle_lambda <- 1 / mean(sample_data)
    
    # 计算自助法重复样本
    bootstrap_replicates <- replicate(num_bootstrap_replicates, {
      resampled_data <- sample(sample_data, replace = TRUE)
      mle_resampled <- 1 / mean(resampled_data)
      mle_resampled
    })
    
    # 计算自助法偏差和标准误差
    bootstrap_biases[i, j] <- mean(bootstrap_replicates) - mle_lambda
    bootstrap_standard_errors[i, j] <- sqrt(mean((bootstrap_replicates - mean(bootstrap_replicates))^2))
    
    # 计算理论偏差和标准误差
    theoretical_biases[i] <- true_lambda / (n - 1)
    theoretical_standard_errors[i] <- (true_lambda * n) / ((n - 1) * sqrt(n - 2))
  }
}

# 计算自助法偏差和标准误差的均值和标准差
mean_bootstrap_biases <- rowMeans(bootstrap_biases)
mean_bootstrap_standard_errors <- rowMeans(bootstrap_standard_errors)

# 打印结果
results <- data.frame(Sample_Size = sample_sizes,
                      Mean_Bootstrap_Bias = mean_bootstrap_biases,
                      Theoretical_Bias = theoretical_biases,
                      Mean_Bootstrap_Standard_Error = mean_bootstrap_standard_errors,
                      Theoretical_Standard_Error = theoretical_standard_errors)
print(results)


## -----------------------------------------------------------------------------

# Load the required library for bootstrapping
library(boot)
library(bootstrap)
# Define a function to compute the correlation statistic
cor.stat <- function(x, i = 1:NROW(x)) {
 cor(x[i, 1], x[i, 2])
 }
cor.stat2 <- function(x, i = 1:NROW(x)) {
 o <- boot(x[i, ], cor.stat, R = 25)
 n <- length(i)
 c(o$t0, var(o$t) * (n - 1)/n^2)
 }
# Set the number of bootstrap resamples
num_bootstrap_samples <- 1000

# Perform bootstrap resampling
set.seed(1)  # Set a seed for reproducibility
boot_results <- boot(law, statistic = cor.stat2, R = 1000)

# Calculate the bootstrap t confidence interval
boot_ci <- boot.ci(boot_results, type = "stud")  # Bias-corrected and accelerated CI

# Print the bootstrap t confidence interval
print(boot_ci)


## -----------------------------------------------------------------------------
b <- boot(law, statistic = cor.stat2, R = 1000)
boot.ci(b, type = "stud")$stud

## -----------------------------------------------------------------------------

b <- boot(law, statistic = cor.stat2, R = 1000)
boot.ci(b, type = "stud")$stud


## -----------------------------------------------------------------------------
b <- boot(law, statistic = cor.stat2, R = 1000)
boot.ci(b, type = "stud")$stud


## -----------------------------------------------------------------------------

library(boot)

# 从 'aircondit' 数据集中采样数据的函数
# 'x' 应为用于索引的整数
getSample <- function(index) {
  
  selected_sample <- aircondit[index]
  selected_sample
}

# 计算采样数据平均值的函数 'data' 代表数据，'indices' 代表自助法样本指数
calculateMean <- function(data, indices) {
  sample_mean <- mean(as.matrix(data[indices, ]))
  sample_mean
}

# 执行自助法分析并生成结果的函数 'sample_data' 是采样数据，'stat_function' 是统计函数，'replications' 是自助法重复次数
performBootstrap <- function(sample_data, stat_function, replications) {
  bootstrap_result <- boot(sample_data, statistic = stat_function, R = replications)

  # 使用不同方法计算置信区间
  confidence_intervals <- boot.ci(bootstrap_result, type = c("norm", "perc", "basic", "bca"))

  # 自助法结果
  print(bootstrap_result)

  # 置信区间
  print(confidence_intervals)

  # 的直方图
  hist(bootstrap_result$t, prob = TRUE, main = " ")

  # 在直方图上标出原始统计量
  points(bootstrap_result$t0, 0, cex = 2, pch = 16)

  # 返回自助法对象以进行进一步分析
  return(bootstrap_result)
}

set.seed(1)

# 使用 'getSample' 函数采样数据
sample <- getSample(1)  # 这里假设 '1' 是 'aircondit' 的有效索引

# 执行 'performBootstrap' 函数进行自助法分析
resu <- performBootstrap(sample,calculateMean,2000)

## -----------------------------------------------------------------------------
library(bootstrap)
library(boot)

set.seed(0)

prepare_sample <- function(data) {
  matrix_data <- as.matrix(data)  # 将数据转换为矩阵格式
  return(matrix_data)
}

# 分析函数，主成分分析并计算偏差和标准误
analyze_data <- function(matrix_data) {
  sample_size <- nrow(matrix_data)
  jackknife_estimates <- numeric(sample_size)
  
  # 计算总体统计量
  covariance_matrix <- cov(matrix_data)
  eigenvalues <- eigen(covariance_matrix)$values
  overall_estimate <- max(eigenvalues / sum(eigenvalues))
  
  # 计算留一法估计量
  for (index in 1:sample_size) {
    sample_subset <- matrix_data[-index, ]
    subset_covariance <- cov(sample_subset)
    subset_eigenvalues <- eigen(subset_covariance)$values
    jackknife_estimates[index] <- max(subset_eigenvalues / sum(subset_eigenvalues))
  }
  
  # 计算偏差和标准误
  jackknife_bias <- (sample_size - 1) * (mean(jackknife_estimates) - overall_estimate)
  jackknife_se <- sqrt((sample_size - 1) / sample_size * sum((jackknife_estimates - mean(jackknife_estimates))^2))
  
  results <- c(overall_estimate, jackknife_bias, jackknife_se)
  
  return(results)
}
# ，将结果放入数据框中
organize_results <- function(analysis_results) {
  organized_results <- data.frame(analysis_results[1], analysis_results[2], analysis_results[3])
  names(organized_results) <- c('est', 'bias', 'se')  # 命名列名
  return(organized_results)
}


matrix_data <- prepare_sample(scor)


analysis_results <- analyze_data(matrix_data)


final_results <- organize_results(analysis_results)

# 结果表格
knitr::kable(final_results, align = "c")

## -----------------------------------------------------------------------------

set.seed(1)


bootstrap_statistic <- function(data, indices) {
  sample_data <- as.matrix(data[indices, ])
  covariance_sample <- cov(sample_data)
  eigen_sample <- eigen(covariance_sample)
  lambda_sample <- eigen_sample$values
  max(lambda_sample / sum(lambda_sample))
}


bootstrap_analysis <- function(data) {
  boot(data, statistic = bootstrap_statistic, R = 2000)
}

# 打印结果
bootstrap_results <- bootstrap_analysis(scor)
print(bootstrap_results)


## ----warning=FALSE------------------------------------------------------------

library(DAAG, warn.conflict = FALSE)
attach(ironslag)
set.seed(1)
Analysis2 <- function(magnetic){
  n <- length(magnetic)
  N <- choose(n, 2)
  e1 <-numeric(N);e2 <- numeric(N);e3 <- numeric(N);e4 <- numeric(N);e5 <- numeric(N)
  ij <- 1
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    k <- c(i, j); y <- magnetic[-k]; x <- chemical[-k]
     # Model1 experiment
    J1 <- lm(y ~ x)
    yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
    e1[ij] <- sum((magnetic[k] - yhat1)^2)
     # Model2 experiment
    J2 <- lm(y ~ x + I(x^2))
    yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] +
      J2$coef[3] * chemical[k]^2
    e2[ij] <- sum((magnetic[k] - yhat2)^2)
     # Model3 experiment
    J3 <- lm(log(y) ~ x)
    logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k]
    yhat3 <- exp(logyhat3)
    e3[ij] <- sum((magnetic[k] - yhat3)^2)
     # Model4 experiment
    J4 <- lm(log(y) ~ log(x))
    logyhat4 <- J4$coef[1] + J4$coef[2] * log(chemical[k])
    yhat4 <- exp(logyhat4)
    e4[ij] <- sum((magnetic[k] - yhat4)^2)
    c2 <- x^2;c3 <- x^3
     # Model5 experiment
    J5 <- lm(y ~ x + c2 + c3)
    yhat5 <- J5$coef[1] + J5$coef[2] * chemical[k] +
      J5$coef[3] * chemical[k]^2 + J5$coef[4] * chemical[k]^3
    e5[ij] <- sum((magnetic[k] - yhat5)^2)
    ij <- ij + 1
  }
  Result <- c(sum(e1), sum(e2), sum(e3), sum(e4), sum(e5))/N
  return(Result)
}

Res <- Analysis2(magnetic)
Res

## -----------------------------------------------------------------------------
cvm_test <- function(x, y, R = 999) {
  n <- length(x)
  m <- length(y)
  z <- c(x, y)
  N <- n + m
  
  # Compute empirical CDFs
  Fn <- sapply(z, function(zi) mean(zi <= x))
  Gm <- sapply(z, function(zi) mean(zi <= y))
  
  cvm0 <- ((n * m) / N) * sum((Fn - Gm)^2)
  
  # Permutation test
  cvm <- replicate(R, {
    Z <- sample(z)
    X <- Z[1:n]
    Y <- Z[(n + 1):N]
    
    Fn_perm <- sapply(Z, function(zi) mean(zi <= X))
    Gm_perm <- sapply(Z, function(zi) mean(zi <= Y))
    
    ((n * m) / N) * sum((Fn_perm - Gm_perm)^2)
  })
  
  p_value <- mean(c(cvm, cvm0) >= cvm0)
  
  return(list(statistic = cvm0, p.value = p_value))
}


x1 <- chickwts$weight[chickwts$feed == "soybean"]
x2 <- chickwts$weight[chickwts$feed == "sunflower"]
x3 <- chickwts$weight[chickwts$feed == "linseed"]

# show the result
cvm_test_result <- cvm_test(x1, x3)
print(cvm_test_result)


## -----------------------------------------------------------------------------
cvm_test_result2 <- cvm_test(x2, x3)
print(cvm_test_result2)


## -----------------------------------------------------------------------------

# sample data
x <- rnorm(50, mean = 0, sd = 5)
y <- rnorm(60, mean = 0, sd = 5)

count5_test <- function(x, y, R = 10000) {
  # Helper function to compute the number of extreme points
  extremepoints_count <- function(sample, combined_sample) {
    sorted_combined <- sort(combined_sample)
    # Define the extreme values as the 5 smallest and 5 largest values from the combined sample
    extremes <- c(head(sorted_combined, 5), tail(sorted_combined, 5))
    return(sum(sample %in% extremes))
  }
  
  # Combine the samples
  combined <- c(x, y)
  
  # Compute observed test statistic as the difference in number of extreme points
  T_obs <- abs(extremepoints_count(x, combined) - extremepoints_count(y, combined))
  
  # Permutation test
  T_permuted <- numeric(R)
  for (i in 1:R) {
    permuted <- sample(combined)
    x_perm <- permuted[1:length(x)]
    y_perm <- permuted[(length(x) + 1):length(combined)]
    T_permuted[i] <- abs(extremepoints_count(x_perm, combined) - extremepoints_count(y_perm, combined))
  }
  
  # Compute p-value
  p_value <- mean(T_permuted >= T_obs)
  
  return(list(statistic = T_obs, p.value = p_value))
}



# Running the test
test_result <- count5_test(x, y)
print(test_result)



## -----------------------------------------------------------------------------
x2 <- rnorm(20, mean = 0, sd = 3)
y2 <- rnorm(30, mean = 0, sd = 3)
test_result2 <- count5_test(x2, y2)
print(test_result2)

## -----------------------------------------------------------------------------
library(stats)
library(ggplot2)

out_a <- function(b1, b2, b3, f0) {
   equation <- function(a) {
    f0 - exp(a + b1 * 1 + b2 * 1 + b3 * (0.5)) / (1 + exp(a + b1 * 1 + b2 * 1 + b3 * (0.5)))
  }
  a <- uniroot(equation, lower = -10, upper = 10)$root
  return(a)
}

## -----------------------------------------------------------------------------

N <- 10^6
b1 <- 0
b2 <- 1
b3 <- -1
f0 <- c(0.1, 0.01, 0.001, 0.0001)


a_values <- sapply(f0, out_a, b1=b1, b2=b2, b3=b3)


plot_data <- data.frame(a = a_values, log_f0 = -log(f0))

## -----------------------------------------------------------------------------

ggplot(plot_data, aes(x = a, y = log_f0)) +
  geom_point() +
  geom_line() +
  labs(title = "-log(f0) vs a", x = "a", y = "-log(f0)") +
  theme_minimal()


## -----------------------------------------------------------------------------
set.seed(194) 
laplace_pdf <- function(x) {
  return(0.5 * exp(-abs(x)))
}

# Random walk Metropolis sampler
rwm_sampler <- function(n, sigma) {
  x <- numeric(n)  
  x[1] <- 0        # Start at 0
  accept <- 0      
  
  for (i in 2:n) {
    current_x <- x[i - 1]
    proposed_x <- current_x + rnorm(1, mean = 0, sd = sigma)
    
    # Calculate acceptance ratio
    acceptance_ratio <- laplace_pdf(proposed_x) / laplace_pdf(current_x)
    
    # Accept or reject the proposal
    if (runif(1) < acceptance_ratio) {
      x[i] <- proposed_x
      accept <- accept + 1
    } else {
      x[i] <- current_x
    }
  }
  
  list(samples = x, acceptance_rate = accept / (n - 1))
}


n_samples <- 10000

sigmas <- c(0.5, 1, 2,5)

results <- lapply(sigmas, function(sigma) rwm_sampler(n_samples, sigma))

# Output the acceptance rates for each sigma
acceptance_rates <- sapply(results, function(result) result$acceptance_rate)
chains <- lapply(results, function(result) result$samples)

# Plotting the chains for each variance
#par(mfrow = c(length(sigmas), 1))  # Set up the plot area
#for (i in seq_along(sigmas)) {
#  plot(results[[i]]$samples, type = 'l', main = paste("Chain with sigma =", sigmas[i]),
#       xlab = "Iteration", ylab = "Value")

## -----------------------------------------------------------------------------
# Plot chains and histograms
par(mfrow = c(2, length(sigmas)))

## -----------------------------------------------------------------------------
chain <- chains[[1]]
  sigma_value <- sigmas[1]
  # Chain plot
  plot(chain, type = "l", main = paste("Chain with sd =", sigma_value))
  # Histogram
  hist(chain, probability = TRUE, breaks = 40, main = paste("Histogram with sigma =", sigma_value))
  curve(laplace_pdf, col = "red", add = TRUE)

## -----------------------------------------------------------------------------
chain <- chains[[2]]
  sigma_value <- sigmas[2]
  # Chain plot
  plot(chain, type = "l", main = paste("Chain with sd =", sigma_value))
  # Histogram
  hist(chain, probability = TRUE, breaks = 40, main = paste("Histogram with sigma =", sigma_value))
  curve(laplace_pdf, col = "red", add = TRUE)

## -----------------------------------------------------------------------------
chain <- chains[[3]]
  sigma_value <- sigmas[3]
  # Chain plot
  plot(chain, type = "l", main = paste("Chain with sd =", sigma_value))
  # Histogram
  hist(chain, probability = TRUE, breaks = 40, main = paste("Histogram with sigma =", sigma_value))
  curve(laplace_pdf, col = "red", add = TRUE)

## -----------------------------------------------------------------------------
chain <- chains[[4]]
  sigma_value <- sigmas[4]
  # Chain plot
  plot(chain, type = "l", main = paste("Chain with sd =", sigma_value))
  # Histogram
  hist(chain, probability = TRUE, breaks = 40, main = paste("Histogram with sigma =", sigma_value))
  curve(laplace_pdf, col = "red", add = TRUE)

## -----------------------------------------------------------------------------
acceptance_rates

## -----------------------------------------------------------------------------
set.seed(123) # For reproducibility

# Parameters for the bivariate normal distribution
rho <- 0.9
variance <- 1 - rho^2

# Gibbs sampler function
gibbs_sampler <- function(n, burn_in) {
 
  x <- y <- numeric(n + burn_in)
  x[1] <- y[1] <- 0
  
  # Gibbs sampling
  for (i in 2:(n + burn_in)) {
    x[i] <- rnorm(1, mean = rho * y[i - 1], sd = sqrt(variance))
    y[i] <- rnorm(1, mean = rho * x[i], sd = sqrt(variance))
  }
  
  # Return the chain without the burn-in period
  data.frame(x = x[-(1:burn_in)], y = y[-(1:burn_in)])
}


n_samples <- 10000
burn_in <- 1000


samples <- gibbs_sampler(n_samples, burn_in)

# Plot the samples
plot(samples$x, samples$y, xlab = "X", ylab = "Y", main = "Bivariate Normal Samples", pch = 20)

# Fit a simple linear regression model Y = β0 + β1X
model <- lm(y ~ x, data = samples)





## -----------------------------------------------------------------------------
model$coefficients

## -----------------------------------------------------------------------------
# Check the residuals of the model for normality and constant variance
#par(mfrow = c(2, 2))
plot(model)  

## -----------------------------------------------------------------------------
library(coda)
set.seed(1)
# Rayleigh PDF
rayleigh_pdf <- function(x, sigma = 1) {
  if (x > 0) {
    return((x / sigma^2) * exp(- (x^2) / (2 * sigma^2)))
  } 
  else {
    return(0)
  }
}

# Metropolis-Hastings sampler
MH <- function(n, init, sigma = 1) {
  sample <- numeric(n)
  sample[1] <- init
  
  for (i in 2:n) {
    current <- sample[i - 1]
    proposed <- abs(rnorm(1, mean = current, sd = sigma))  # Proposal from a normal distribution
    
    # Acceptance probability
    alpha <- min(1, rayleigh_pdf(proposed, sigma) / rayleigh_pdf(current, sigma))
    
    if (runif(1) < alpha) {
      sample[i] <- proposed
    } else {
      sample[i] <- current
    }
  }
  
  return(sample)
}


n_samples <- 10000
n_chains <- 3

# Initialize 
init_values <- c(0.1, 1, 2)  # Different starting points for each chain
chains <- lapply(init_values, function(init) MH(n_samples, init))


## -----------------------------------------------------------------------------
# compute variances of each chain
W <- sapply(chains, var)

# compute the variance between chains
chain_means <- sapply(chains, mean)
mean_of_chain_means <- mean(chain_means)
B_over_n <- sum((chain_means - mean_of_chain_means)^2) / (n_chains - 1)

# var est
sigma_hat_squared <- ((n_samples - 1) / n_samples) * mean(W) + B_over_n

# GR diagnostic index
R_hat <- sqrt(sigma_hat_squared / mean(W))

R_hat

## -----------------------------------------------------------------------------

# Convert to mcmc objects and combine into a mcmc.list
mcmc_samples <- mcmc.list(lapply(chains, as.mcmc))

# Run the GR diagnostic
gr_diag <- gelman.diag(mcmc_samples)

# Check if the GR statistic is less than 1.2
converg <- all(gr_diag$psrf[1, ] < 1.2)

# Plot GR diagnostic
gelman.plot(mcmc_samples)



## -----------------------------------------------------------------------------

# Output the results
list(convergence_diagnostic = gr_diag, converged = converg)

## -----------------------------------------------------------------------------
gr_diag$psrf

## -----------------------------------------------------------------------------
U <- c(11,8,27,13,16,0,23,10,24,2)
V <- c(12,9,28,14,17,1,24,11,25,3)

fun <- function(x) {
  sapply(x, function(lambda) {
    sum((V * exp(-lambda * V) - U * exp(-lambda * U)) / (exp(-lambda * V) - exp(-lambda * U)))
  })
}

curve(fun, from = 0, to = 1)
abline(h = 0, lty = 3)


## -----------------------------------------------------------------------------
mle1<- uniroot(fun, lower = 0.001, upper = 0.2)
mle1


## -----------------------------------------------------------------------------
rm(list = ls())
U <- c(11,8,27,13,16,0,23,10,24,2)
V <- c(12,9,28,14,17,1,24,11,25,3)

EM <- function(U,V,max.it=10000,eps=1e-8){
  n <- length(U)
  i <- 1
  theta1 <- 0.01
  theta2 <- 0.02
  while( abs(theta1 - theta2) >= eps){
    theta1 <- theta2
   fun <- function(x) {
  sapply(x, function(lambda) {
    sum((V * exp(-lambda * V) - U * exp(-lambda * U)) / (exp(-lambda * V) - exp(-lambda * U)))
  })
}
    su <- fun(theta1)
    theta2 <- n/(n/theta1 + su)
    print(round(c(theta2),9))
    if(i == max.it) break
    i <- i + 1    
  }
  return(theta2)
 #return(i)
}

mle2 <- EM(U,V,max.it=10000,eps=1e-5)



## -----------------------------------------------------------------------------
calculate_game_solution <- function(input_matrix) {
    # Normalize the input matrix
    min_val <- min(input_matrix)
    norm_matrix <- input_matrix - min_val
    max_val <- max(norm_matrix)
    norm_matrix <- norm_matrix / max_val

    # Set up parameters for simplex method
    rows <- nrow(norm_matrix)
    cols <- ncol(norm_matrix)
    iterations <- cols^3

    # Set up for player X
    objective_x <- c(rep(0, rows), 1)
    constraint_x <- -cbind(t(norm_matrix), rep(-1, cols))
    boundary_x <- rep(0, cols)
    equality_x <- t(as.matrix(c(rep(1, rows), 0)))
    eq_boundary_x <- 1

    # Solve for player X
    solution_x <- simplex(a = objective_x, A1 = constraint_x, b1 = boundary_x, A3 = equality_x, b3 = eq_boundary_x,
                          maxi = TRUE, n.iter = iterations)

    # Set up for player Y
    objective_y <- c(rep(0, cols), 1)
    constraint_y <- cbind(norm_matrix, rep(-1, rows))
    boundary_y <- rep(0, rows)
    equality_y <- t(as.matrix(c(rep(1, cols), 0)))
    eq_boundary_y <- 1

    # Solve for player Y
    solution_y <- simplex(a = objective_y, A1 = constraint_y, b1 = boundary_y, A3 = equality_y, b3 = eq_boundary_y,
                          maxi = FALSE, n.iter = iterations)

    # Construct the final solution
    final_solution <- list(
        NormalizedMatrix = norm_matrix * max_val + min_val,
        StrategyX = solution_x$soln[1:rows],
        StrategyY = solution_y$soln[1:cols],
        GameValue = solution_x$soln[rows + 1] * max_val + min_val
    )

    return(final_solution)
}

# Sample data and library call
game_matrix <- matrix(c(0, -2, -2, 3, 0, 0, 4, 0, 0, 2, 0, 0, 0,
                        -3, -3, 4, 0, 0, 2, 0, 0, 3, 0, 0, 0, -4, -4, -3,
                        0, -3, 0, 4, 0, 0, 5, 0, 0, 3, 0, -4, 0, -4, 0, 5,
                        0, 0, 3, 0, 0, 4, 0, -5, 0, -5, -4, -4, 0, 0, 0,
                        5, 0, 0, 6, 0, 0, 4, -5, -5, 0, 0, 0, 6, 0, 0, 4,
                        0, 0, 5, -6, -6, 0), 9, 9)

library(boot)
adjusted_matrix <- game_matrix + 2
solution <- calculate_game_solution(adjusted_matrix)
print(solution$GameValue)


## -----------------------------------------------------------------------------
print(round(cbind(solution$StrategyX, solution$StrategyY), 7))

## -----------------------------------------------------------------------------
print(round(solution$StrategyX * 61, 7))




## -----------------------------------------------------------------------------
require(graphics)
# create a list
pts <- list(x = cars[,1], y = cars[,2])

## -----------------------------------------------------------------------------
vec_pts1 <- unlist(pts)
is(vec_pts1)

## -----------------------------------------------------------------------------
vec_pts2 <- as.vector(pts)
is(vec_pts2)

## -----------------------------------------------------------------------------
x <- c(a = 1, b = 2)
is.vector(x)
dim(x)

## -----------------------------------------------------------------------------
m <- as.matrix(1:10)
is.matrix(m)

## -----------------------------------------------------------------------------
is.array(m)

## -----------------------------------------------------------------------------
mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("row1", "row2"),
                               c("C.1", "C.2", "C.3")))

## -----------------------------------------------------------------------------
is.matrix(mdat)
is.array(mdat)

## -----------------------------------------------------------------------------
L3 <- LETTERS[1:3]
char <- sample(L3, 10, replace = TRUE)
d <- data.frame(x = 1, y = 1:10, char = char)
d
d_asmtrix <- as.matrix(d)

## -----------------------------------------------------------------------------
df <- data.frame(
  Numeric = c(1, 2, 3, 4, 5),
  Character = as.character(c('a', 'b', 'c', 'd', 'e')),
  Boolean = c(TRUE, FALSE, TRUE, FALSE, TRUE),
  Factor = factor(c('apple', 'banana', 'orange', 'apple', 'banana'))
)
df
df_asmtrix <- as.matrix(df)

## -----------------------------------------------------------------------------
class(d[,2])
class(df[,3])
class(d_asmtrix[,2])
class(df_asmtrix[,3])

## -----------------------------------------------------------------------------
d2 <- data.frame(x = 1, y = 1)
d2

## -----------------------------------------------------------------------------
d2 <- d2[-1,]
d2

## -----------------------------------------------------------------------------
scale01 <- function(x) {
rng <- range(x, na.rm = TRUE)
(x - rng[1]) / (rng[2] - rng[1])
}

## -----------------------------------------------------------------------------
df<-data.frame(x=c(1,2,3,4,5),y=c(7,3,9,2,6),z=as.character(c('a', 'b', 'c', 'd', 'e')))
#df_scaled <- data.frame(lapply(df, scale01))
#df_scaled

## -----------------------------------------------------------------------------
numeric_cols <- sapply(df, is.numeric)
df[, numeric_cols] <- lapply(df[, numeric_cols], scale01)
df

## -----------------------------------------------------------------------------
df <- data.frame(x=c(1,2,3,4),y=c(5,6,7,8))
vapply(df, sd, numeric(1))

## -----------------------------------------------------------------------------
df2<- data.frame(x = c(1,6,5,2,10),y = c(4,3,8,7,9),z = as.character(c('a', 'b', 'c', 'd', 'e')))
num_cols <- vapply(df2,is.numeric,logical(1))
vapply(df2[num_cols], sd, numeric(1))

## -----------------------------------------------------------------------------
gibbs_sampler_R <- function(n, a, b, num_iterations) {
  x <- numeric(num_iterations)
  y <- numeric(num_iterations)
  y[1] <- runif(1)  # Initial value for y

  for (i in 2:num_iterations) {
    x[i] <- rbinom(1, n, y[i-1])
    y[i] <- rbeta(1, x[i] + a, n - x[i] + b)
  }
  return(data.frame(x, y))
}


## -----------------------------------------------------------------------------
library(Rcpp)
library(microbenchmark)
cppFunction('
  DataFrame gibbs_sampler_Rcpp(int n, double a, double b, int num_iterations) {
    NumericVector x(num_iterations);
    NumericVector y(num_iterations);
    y[0] = R::runif(0, 1);  // Initial value for y

    for(int i = 1; i < num_iterations; i++) {
      x[i] = R::rbinom(n, y[i-1]);
      y[i] = R::rbeta(x[i] + a, n - x[i] + b);
    }
    return DataFrame::create(Named("x")=x, Named("y")=y);
  }
')

## -----------------------------------------------------------------------------
# Define parameters
n <- 100  # Set your desired values for n, a, b, and num_iterations
a <- 2
b <- 3
num_iterations <- 10000

ts <- microbenchmark(R=gibbs_sampler_R(n, a, b, num_iterations),Rcpp=gibbs_sampler_Rcpp(n, a, b, num_iterations),times=10)
summary(ts)
                     

## -----------------------------------------------------------------------------
# all packages used
# [1] "library(knitr)"                      
#[2] "library(ggplot2)"                    
#[3] "library(DAAG)"                       
#[4] "library(boot)"                       
#[5] "library(bootstrap)"                  
#[6] "library(coda)"                       
#[7] "library(dplyr)"                      
#[8] "library(stats)"                      
#[9] "require(graphics)"                   
#[10] "library(Rcpp)"                       
#[11] "library(microbenchmark)" 

