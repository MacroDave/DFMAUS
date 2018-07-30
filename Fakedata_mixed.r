library(ggthemes); library(stringr);library(reshape2);library(rstan); library(dplyr); library(ggplot2)

library(broom)

#Create Data
# Set up sample size
N <- 500

#Setup Int for Qtly Variables
temp <- c(1,1,3)
QTLY <- rep(temp, N)
QTLY <- QTLY[1:N]

# Set up states
theta <- 0.9
sigma <- c(0.5, 0.7)
phi <- 0.7
beta <- 0.3
gamma <- c(0.5, -0.3, 0.7, .4)
lambda <- 0.6

x <- rep(NA,N)
p <- rep(NA,N)
x[1] <- rnorm(1, 0, 0.5)
p[1] <- rnorm(1, 0, 0.3)
for(i in 2:N) {
  x[i] <- theta*x[i-1] + rnorm(1, 0, sigma[1])
  p[i] <- phi*p[i-1] + beta*x[i] + rnorm(1, 0, sigma[2])
}

plot.ts(p)
plot.ts(x)

# Setup Signal Variables (Function of underlying state variable)
z1 <- rep(NA,N) #nominal monthly variable
z2 <- rep(NA,N) #real monthly variable
z3 <- rep(NA,N) #nominal qtly variable
z4 <- rep(NA,N) #real qtly variable
z5 <- rep(NA,N) #qtly price variable

for(i in 3:N) {
  z1[i] = x[i] + p[i] + rnorm(1, 0, 1)
  z2[i] = gamma[2]*x[i] + rnorm(1, 0, 1)
  if (QTLY[i] == 3) {
    z3[i] = (1/3)*(x[i]+x[i-1]+x[i-2])*gamma[3] + (1/3)*(p[i]+p[i-1]+p[i-2]) + rnorm(1, 0, 1)
    #z3[i] = (1/3)*(x[i]+x[i-1]+x[i-2])*gamma[3] + (1/3)*(p[i]+p[i-1]+p[i-2])*0.5 + rnorm(1, 0, 1)
    z4[i] = (1/3)*(x[i]+x[i-1]+x[i-2])*gamma[4] + rnorm(1, 0, 1)
    z5[i] = (1/3)*(p[i]+p[i-1]+p[i-2])*lambda + rnorm(1, 0, 1)
  } else {
    z3[i] = -999
    z4[i] = -999
    z5[i] = -999
  }
}

# Setup Data for Model
data_dfm <- data.frame(z1,z2,z3,z4,z5)
data_dfm <- data_dfm[3:N,]
data_all <- data.frame(z1,z2,z3,z4,z5,x,p)

# Have a look at the data to make sure you know what's happening
plot.ts(data_dfm[,1:2])

# Run the model
compiled_model <- stan_model("simpledata.stan")

dfm_basic <- sampling(compiled_model,
                      data = list(T = nrow(data_dfm),
                                  P = 5,
                                  Y = data_dfm), control = list(adapt_delta = 0.99),
                      chains=4, cores = 4, iter = 2000, init = 0)

print(dfm_basic, pars = c("theta", "phi", "beta", "gamma", "sigma", "lambda"))

# shinystan::launch_shinystan(dfm_basic)
# Check to see if params have been recaptured
estimates <- tidy(dfm_basic, pars = c("theta", "phi", "beta", "gamma", "sigma", "lambda", "sigma_measurement"), conf.int = T)

estimates %>%
  mutate(actuals = c(theta, phi, beta, gamma, sigma, lambda, rep(1, 5))) %>%
  ggplot(aes(x = term, ymin = conf.low, ymax = conf.high, y = estimate)) +
  geom_linerange(alpha = 0.4, colour = "orange") +
  geom_point(size = 2, colour = "red") +
  geom_point(aes(y = actuals)) +
  theme_hc() +
  coord_flip()


# Extract the estimates of the state
xhat <- extract(dfm_basic, pars = "xhat", permuted = T)[[1]] %>%
  as.data.frame

phat <- extract(dfm_basic, pars = "phat", permuted = T)[[1]] %>%
  as.data.frame

#Compare Extracted State Variable from Stan to Artificial Data
xhat_ts <- xhat %>% melt %>%
  mutate(obs = str_extract(variable, "[0-9]{1,4}") %>% as.numeric) %>%
  group_by(obs) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>%
  mutate(actuals = data_all[3:N,6])

phat_ts <- phat %>% melt %>%
  mutate(obs = str_extract(variable, "[0-9]{1,4}") %>% as.numeric) %>%
  group_by(obs) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.025),
            upper = quantile(value, 0.975)) %>%
  mutate(actuals = data_all[3:N,7])

xhat_ts %>%
  ggplot(aes(x = obs)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "orange") +
  geom_line(aes(y = actuals)) +
  theme_economist() +
  ggtitle("Eco State estimates (orange) and actual (black)")

xhat_ts %>%
  ggplot(aes(x = median, y = actuals)) +
  geom_point(alpha = 0.7) +
  theme_economist() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_smooth(method = "lm") +
  ggtitle("Actual and predicted eco state")

phat_ts %>%
  ggplot(aes(x = obs)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "orange") +
  geom_line(aes(y = actuals)) +
  theme_economist() +
  ggtitle("Price State estimates (orange) and actual (black)")

phat_ts %>%
  ggplot(aes(x = median, y = actuals)) +
  geom_point(alpha = 0.7) +
  theme_economist() +
  geom_abline(aes(intercept = 0, slope = 1)) +
  geom_smooth(method = "lm") +
  ggtitle("Actual and predicted price state")

#Pull predictions from estimated model
preds <- dfm_basic%>% as.data.frame %>% dplyr::select(contains("Y_predict")) %>% as.matrix
preds1 <- preds[,1:1998]
preds2 <- preds[,1999:3996]

#Compare Fake Data and Predicted Data Densities
bayesplot::ppc_dens_overlay(data_dfm[,1], preds[1:100,1:1998])
bayesplot::ppc_dens_overlay(data_dfm[,2], preds[1:100,1999:3996])

#Compare Extracted Prediction from Stan to Artificial Data for Nominal Monthyl Variable

preds1_ts <- preds1[1,1:1998] %>% as.data.frame
names(preds1_ts) <- c("median")
preds1_ts <- preds1_ts %>%
  mutate(obs = seq(1:1998) %>% as.numeric) %>%
#preds1_ts <- preds1 %>% melt %>%
#  mutate(obs = rep(1:1998, times=1, each=1000) %>% as.numeric) %>%
#  group_by(obs) %>%
#  summarise(median = median(value),
#            lower = quantile(value, 0.025),
#            upper = quantile(value, 0.975)) %>%
  mutate(actuals = data_dfm[,1])

preds1_ts %>%
  ggplot(aes(x = obs)) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "orange") +
  geom_line(aes(y = actuals)) +
  theme_economist() +
  ggtitle("Monthly Nominal Variable Predictions (orange) and actual (black)")

preds2_ts <- preds2[1,1:1998] %>% as.data.frame
names(preds2_ts) <- c("median")
preds2_ts <- preds2_ts %>%
  mutate(obs = seq(1:1998) %>% as.numeric) %>%
  #preds1_ts <- preds1 %>% melt %>%
  #  mutate(obs = rep(1:1998, times=1, each=1000) %>% as.numeric) %>%
  #  group_by(obs) %>%
  #  summarise(median = median(value),
  #            lower = quantile(value, 0.025),
  #            upper = quantile(value, 0.975)) %>%
  mutate(actuals = data_dfm[,2])

preds2_ts %>%
  ggplot(aes(x = obs)) +
  #geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +
  geom_line(aes(y = median), colour = "orange") +
  geom_line(aes(y = actuals)) +
  theme_economist() +
  ggtitle("Monthly Real Variable Predictions (orange) and actual (black)")

# Check to see if params have been recaptured
estimates <- tidy(dfm_basic, pars = c("theta", "phi", "beta", "gamma", "sigma", "lambda"), conf.int = T)

estimates %>%
  mutate(actuals = c(theta, phi, beta, gamma, sigma, lambda)) %>%
  ggplot(aes(x = term, ymin = conf.low, ymax = conf.high, y = estimate)) +
  geom_linerange(alpha = 0.4, colour = "orange") +
  geom_point(size = 2, colour = "red") +
  geom_point(aes(y = actuals)) +
  theme_hc()







