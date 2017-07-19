library(ggplot2)

# Load the data set
load('beautyeval.Rda')

# Visualize it
ggplot(beautydata,
       mapping=aes(x=beauty, y=eval, col=sex)) + 
  geom_point() +
  geom_smooth(method='lm') +
  xlab('Lecturer attractiveness') +
  ylab('Teaching evaluation score') +
  ggtitle('Do good looking lecturers get better teaching evaluation score?')


# Classical linear regression
M.lm <- lm(eval ~ beauty*sex, data=beautydata)


# Set up data for use with Jags
model.data = list('beauty'=beautydata$beauty,
                  'sex'=as.numeric(beautydata$sex)-1,
                  'eval'=beautydata$eval,
                  'N'=dim(beautydata)[1])

# Define the Jags model by sending the Jags model description
# and the data to the Jags program, via rjags.
M <- jags.model('linear_regression.jags',
                data=model.data,
                n.chains = 3)

# Iterate the mcmc 
update(M, 10000)

# Draw samples 
S <- coda.samples(M, 
                  variable.names = c('alpha', 
                                     'beta.sex',
                                     'beta.beauty',
                                     'beta.interaction'),
                  10000)

# Check convergence
gelman.diag(S)

# Plot samples
plot(S[,2], trace=F)

# Summarize samples
summary(S)

# High posterior density interval
HPDinterval(S)
