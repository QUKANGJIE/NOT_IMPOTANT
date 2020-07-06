library(ggplot2)
update.packages(ggplot2)

exp.draws.1 <- rexp(n=200)
inf.1<-c(mean(exp.draws.1),sd(exp.draws.1,na.rm = FALSE))
exp.draws.0.1 <- rexp(n=200,rate = 0.1)
inf.0.1<-c(mean(exp.draws.0.1),sd(exp.draws.0.1,na.rm = FALSE))
exp.draws.0.5 <- rexp(n=200,rate = 0.5)
inf.0.5<-c(mean(exp.draws.0.5),sd(exp.draws.0.5,na.rm = FALSE))
exp.draws.5 <- rexp(n=200,rate = 5)
inf.5<-c(mean(exp.draws.5),sd(exp.draws.5,na.rm = FALSE))
exp.draws.10 <- rexp(n=200,rate = 10)
inf.10<-c(mean(exp.draws.10),sd(exp.draws.10,na.rm = FALSE))
##3.a-problem
hist(exp.draws.1)
tibble(expdraw = exp.draws.1) %>% ggplot(aes(x = exp.draws.1))+
  geom_histogram(bins = 15)
##3.b-problem
plot(sort(exp.draws.1)) # or use ggplot2
tibble(expdraw = exp.draws.1) %>% arrange(expdraw) %>% 
  ggplot(aes(x = 1:length(expdraw), y = expdraw))+
  geom_point()
##3.c-problem
plot(exp.draws.1, exp.draws.5) # or use ggplot2
tibble(x = exp.draws.1, y = exp.draws.5) %>% 
  ggplot(aes(x = x, y = y))+
  geom_point()
##4.a-problem
exprn <- tibble(exp.1 = exp.draws.1, exp.0.1 = exp.draws.0.1, exp.0.5 = exp.draws.0.5, exp.5 = exp.draws.5, exp.10 = exp.draws.10)
emeans <- tibble(rates = c(1, 0.1, 0.5, 5, 10), means = colMeans(exprn), stds = apply(exprn, 2, sd))
rord <- order(emeans$rates)
emeans <- emeans[rord,]
# after chapter 4
emeans1 <- exprn %>% gather(key = 'draws', value = 'value') %>% 
  group_by(draws) %>% summarize(means = mean(value), stds = sd(value)) %>% arrange(desc(means)) %>% ungroup() %>% mutate(rates = parse_double(substr(draws, start = 5, stop = nchar(draws))))

plot(means ~ rates, data = emeans)
# or using ggplot2
emeans1 %>% ggplot(aes(x = rates, y = means))+
  geom_point()
##4.b-problem
plot(stds ~ rates, data = emeans)
# or using ggplot2
emeans1 %>% ggplot(aes(x = rates, y = stds))+
  geom_point()
##4.c-problem
plot(means ~ stds, data = emeans)
# or using ggplot2
emeans1 %>% ggplot(aes(x = stds, y = means))+
  geom_point()