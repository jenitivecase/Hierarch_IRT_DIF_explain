stancode <- "
data {
  int<lower=0> n_people;
  int<lower=0> n_items;
  int<lower=0> n_ref;
  int<lower=0> n_ref_1;
  int dataset[n_people, n_items];
  int<lower=0, upper=1> group[n_people];
  real DIFpredict[n_items];
}

parameters {
  real<lower=0> a[n_items];
  real b[n_items];
  real theta[n_people];
  real D[n_items];
  real beta0;
  real beta1;
  real<lower=0> sigma2;
  real foc_mean;
//  real<lower=0> foc_var;
}

transformed parameters {
  real mu[n_items];
  real ss_err[n_items];
  real ss_reg[n_items];
  real SSE;
  real SSR;
  real R2;
  
  for (j in 1:n_items) {
    mu[j] = beta0 + beta1*DIFpredict[j];
    ss_err[j] = pow((D[j]-mu[j]),2);
    ss_reg[j] = pow((mu[j]-mean(D[])),2);
  }
  
  SSE = sum(ss_err[]);
  SSR = sum(ss_reg[]);
  R2 = SSR/(SSR+SSE);
}

model {
  for (i in 1:n_people) {
    for (j in 1:n_items) {
      dataset[i,j] ~ bernoulli_logit(a[j]*(theta[i] - (b[j] + D[j]*group[i])));
    }
  }	
  
  a ~ lognormal(0,1);
  b ~ normal(0,1);

// specify N(0,1) for reference group, then estimate mean, var for reference group...
  for(i in 1:n_ref){
    theta ~ normal(0,1);
  }
  for(i in n_ref_1:n_people){
    theta ~ normal(foc_mean, 1);
  }
  
  D ~ normal(mu, sigma2);
  foc_mean ~ uniform(-10, 10);
  foc_var ~ uniform(0, 100);
  beta0 ~ normal(0,1);
  beta1 ~ normal(0,1);
  sigma2 ~ uniform(0,100);
}
"



stancode_long <- "
data {
  int<lower=0> n_people;
  int<lower=0> n_items;
  int<lower=0> n_observations;
  int<lower=0, upper=n_people> respondentid[n_observations];
  int<lower=0, upper=n_items> itemid[n_observations];
  int<lower=0, upper=1> response[n_observations];
  int<lower=0, upper=1> group[n_observations];
  real DIFpredict[n_items];
}

parameters {
  real<lower=0> a[n_items];
  real b[n_items];
  real theta[n_people];
  real D[n_items];
  real beta0;
  real beta1;
  real<lower=0> sigma2;
}

transformed parameters {
  real mu[n_items];
  real ss_err[n_items];
  real ss_reg[n_items];
  real SSE;
  real SSR;
  real R2;
  
  for (j in 1:n_items) {
    mu[j] = beta0 + beta1*DIFpredict[j];
    ss_err[j] = pow((D[j]-mu[j]),2);
    ss_reg[j] = pow((mu[j]-mean(D[])),2);
  }
  
  SSE = sum(ss_err[]);
  SSR = sum(ss_reg[]);
  R2 = SSR/(SSR+SSE);
}

model {
  
  vector[n_observations] eta;
  
  for(i in 1:n_observations){
    eta[i] = a[itemid[i]]*(theta[respondentid[i]] - b[itemid[i]] + D[itemid[i]] * group[i]);

  response ~ bernoulli_logit(eta);
  
  a ~ lognormal(0,1);
  b ~ normal(0,1);
  theta ~ normal(0,1);
  D ~ normal(mu, sigma2);
  
  beta0 ~ normal(0,1);
  beta1 ~ normal(0,1);
  sigma2 ~ uniform(0,100);
}
"



stancode_2PL <- "
data {
  int<lower=0> n_people;
  int<lower=0> n_items;
  int dataset[n_people, n_items];
}

parameters {
  real<lower=0> a[n_items];
  real b[n_items];
  real theta[n_people];
}
model {
  for (i in 1:n_people) {
    for (j in 1:n_items) {
      dataset[i,j] ~ bernoulli_logit(a[j]*(theta[i]-b[j]));
    }
  }	
    
    a ~ lognormal(0,1);
    b ~ normal(0,1);
  theta ~ normal(0,1);
}
"