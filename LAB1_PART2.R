library(ggplot2)
update.packages(ggplot2)
##5.a
big.exp.draws.1 <- rexp(1100000)
mean(big.exp.draws.1)
sd(big.exp.draws.1)
##5.b
hist(big.exp.draws.1, probability = T) 
afun <- function(x) 1-exp(-x)
curve(afun, add = T)
# or use ggplot2
tibble(expdraw = big.exp.draws.1) %>% ggplot()+
  geom_histogram(aes(x = big.exp.draws.1, y = (..count..)/sum(..count..)), binwidth = 0.5)+
  ylab('frequency')+
  stat_function(fun = afun, col = 'blue')
##5.c
big2 <- big.exp.draws.1 > 1
mean(big.exp.draws.1[big2])
# or use dplyr after chapter 4
tibble(draw = big.exp.draws.1) %>% filter(draw > 1) %>% summarize(mean = mean(draw))
##5.d
big.exp.draws.1.mat <- matrix(big.exp.draws.1, nrow = 1100)
bighist <- hist(big.exp.draws.1.mat)
##5.e
mean(big.exp.draws.1.mat[,371])
##5.f
bigmeans <- colMeans(big.exp.draws.1.mat)
hist(bigmeans)
##5.g
bigsqrt <- sqrt(big.exp.draws.1)
mean(bigsqrt)