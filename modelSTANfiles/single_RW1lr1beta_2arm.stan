  data {
    int<lower=1> T;                       // number of trials
    int<lower=-1, upper=2> choice[T];  // The choices made; 1 & 2 coding
    matrix[T,2] outcome;                   // The outcome; I modified it...
  }
  
  transformed data {
    vector[2] initV;
    initV = rep_vector(0.5, 2); //start at 0.5
  }
  
  // Declare all parameters as vectors for vectorizing
  parameters {
    // Subject-specific, raw parameters
    real alpha_pr; // learning rate
    real beta_pr;  // inverse temperature
  }
  
  transformed parameters {
    // Transform into the bounds
    real<lower=0, upper=1> alpha;
    real<lower=0, upper=10> beta; //need to think: should we impose this bound??
  
    alpha = inv_logit(alpha_pr); //not inv_logit? may be more efficient?
    beta  = inv_logit(beta_pr)*10; //can this cope with the bound we gave for beta??
  }
  
  model {
    // Define values
    vector[2] ev;    // expected value
    vector[2] prob;  // probability
    real prob_1_;

    real PE;     // prediction error
    
    // Individual parameters: I had to move these BELOW the vector[] and real; how did the hBayesDM code work???
    alpha_pr  ~ normal(0, 1); //is this weakly informative??
    beta_pr   ~ normal(0, 1); 

    // Initialize values
    ev = initV; // initial ev values

    for (t in 1:T) {
      // Compute action probabilities
      prob[1] = 1 / (1 + exp(-(beta * (ev[1] - ev[2])))); //could there be a missing minus sign??
      prob_1_ = prob[1];
      prob[2] = 1 - prob_1_;
      choice[t] ~ categorical(prob); //categorical takes in vector? and output 1 or 2

      // Prediction error
      PE   =  outcome[t,choice[t]] - ev[choice[t]];

      // Value updating (learning)
      ev[choice[t]]   += alpha * PE;
    }
  }
