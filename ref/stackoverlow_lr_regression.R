http://stackoverflow.com/questions/15478327/implementation-of-logistic-regression-formula-in-r


From a mathematical perspective, an unconstrained magnitude on the weight vector does not yield a unique solution. When I added these two lines to the classifier function, it converged in two steps:

# Normalize
weights <- weights/norm(weights)
...

# Update weights
weights <- weights_old - learn_rate*gradient
weights <- weights / norm(weights)
I couldn't make @SimonO101's work, and I'm not using this code for real work (there are builtins like glm), so it's enough to do loops that I understand. The whole function is as follows:

# Logistic regression
# Takes training example vector, output vector, learn rate scalar, and convergence delta limit scalar
my_logr <- function(training_examples,training_outputs,learn_rate,conv_lim) {
  # Initialize gradient vector
  gradient <- as.vector(rep(0,NCOL(training_examples)))
  # Difference between weights
  del_weights <- as.matrix(1)
  # Weights
  weights <- as.matrix(runif(NCOL(training_examples)))
  weights_old <- as.matrix(rep(0,NCOL(training_examples)))

  # Normalize
  weights <- weights/norm(weights)

  # Compute gradient
  while(norm(del_weights) > conv_lim) {

    for (k in 1:NCOL(training_examples)) {
      gradient <- gradient - 1/NROW(training_examples)*
        ((t(training_outputs[k]*training_examples[k,]
            /(1+exp(training_outputs[k]*t(weights)%*%as.numeric(training_examples[k,]))))))
    }
#     gradient <- -1/NROW(training_examples) * sum(training_outputs * training_examples / (1 + exp(training_outputs * weights%*%training_outputs) ) )

    # Update weights
    weights <- weights_old - learn_rate*gradient
    weights <- weights / norm(weights)
    del_weights <- as.matrix(weights_old - weights)
    weights_old <- weights

    print(weights)
  }
    return(weights)
}
shareimprove this answer
answered Mar 19 '13 at 8:22

Trevor Alexander
8231829
  	 	
+1. Looks good. I didn't fully understand the algorithm process. I'm setting about that this evening. –  Simon O'Hanlon Mar 19 '13 at 13:30
  	 	
What do you mean "weights"? And how you calculate standard error and p values? Could you give some references of this algorithm also? Thanks! –  qed Mar 30 '14 at 19:00
  	 	
I am also trying to implement logistic regression in c++, but using the IRLS algorithm, there is matrix inversion involved, quite a headache sometimes. –  qed Mar 30 '14 at 19:01