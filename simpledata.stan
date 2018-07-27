data {
  int T; // number of obs
  int P; // number of observed variables
  matrix[T,P] Y; //dataset of generated series
}

parameters {

  vector[T] xhat; // state variable
  vector[T] phat; // state price variable

  real<lower = 0> gamma_1; // First factor loading--restricted to positive for identificaton
  vector[P-2] gamma_rest; // The rest of the factor loadings

  real<lower=0> theta; // AR(1) Coef on state Series
  real<lower=0> phi;   // AR(1) coef

  real<lower = 0> beta;  // coef of eco state in p state eqn

  real lambda;   // Loading on Price Factor for Price Variables

  vector<lower = 0>[2] sigma; // The scale of innovations to the state variables
  vector<lower = 0>[5] sigma_measurement;
}

transformed parameters {
  vector[P-1] gamma;
  // Need to create a complete factor loading vector
  gamma[1] = gamma_1;
  gamma[2:(P-1)] = gamma_rest;
}

model {
  //priors
  xhat[1] ~ normal(0,1);
  phat[1] ~ normal(0,1);
  gamma_1 ~ normal(0,1);
  gamma_rest ~ normal(0, 1);
  theta ~ normal(0,1);
  phi ~ normal(0,1);
  beta ~ normal(0,1);
  lambda ~ normal(0,1);
  sigma ~ inv_gamma(1.3, 1);
  sigma_measurement ~ lognormal(0, 1);

  // State Equation
  for(t in 2:T) {
    xhat[t] ~ normal(xhat[t-1]*theta, sigma[1]);
    phat[t] ~ normal(phat[t-1]*phi + xhat[t]*beta, sigma[2]);
  }

  // Measurement Equations
  for(t in 3:T) {
    Y[t,1] ~ normal(xhat[t] + phat[t], sigma_measurement[1]); //nominal monthly variable

    Y[t,2] ~ normal(xhat[t]*gamma[2], sigma_measurement[2]); //real monthly variable

    if(Y[t,3] != -999) {
      Y[t,3] ~ normal( (3.0^-1)*sum(xhat[(t-2):t])*gamma[3] + (3.0^-1)*sum(phat[(t-2):t]), sigma_measurement[3]); //qtly nom variable
    }

    if(Y[t,4] != -999) {
      Y[t,4] ~ normal( (3.0^-1)*sum(xhat[(t-2):t])*gamma[4], sigma_measurement[4]); //qtly real variable
    }

    if(Y[t,5] != -999) {
      Y[t,5] ~ normal( (3^-1)*sum(phat[(t-2):t])*lambda, sigma_measurement[5]); //qtly price variable
    }
  }
}

generated quantities {
  matrix[T,2] Y_predict;
  vector[T] xhat_predict;
  vector[T] phat_predict;

  xhat_predict[1] = xhat[1];
  phat_predict[1] = phat[1];
  Y_predict[1,1] = Y[1,1];
  Y_predict[1,2] = Y[1,2];

  for(t in 2:T) {
    xhat_predict[t] = normal_rng(xhat_predict[t-1]*theta, sigma[1]);
    phat_predict[t] = normal_rng(phat_predict[t-1]*phi + xhat_predict[t]*beta, sigma[2]);

    Y_predict[t,1] = normal_rng(xhat_predict[t]*gamma[1] + phat_predict[t], 1); //nominal monthly variable
    Y_predict[t,2] = normal_rng(xhat_predict[t]*gamma[2], 1); //real monthly variable
  }
}
