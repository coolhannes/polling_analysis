# Set up your environment
rm(list = ls())
library(tidyverse)

# Number of Simulated Observations/Voters
number_of_voters <- 100000

# Set a seed to make reproducible
set.seed(435)

# Set up population ideology

## N observations with the following distribution
## This is the distribution of ideology, on a 0-1 scale, (mean of 0) in the pop
ideology <- rnorm(number_of_voters, mean = 0, sd = 1)
ideology_df <- as.data.frame(ideology)
colnames(ideology_df) <- c("ideology")

## Plot the Ideology
ideology_df %>%
    ggplot(aes(x = ideology)) +
    geom_density() +
    ggtitle("Population Ideological Distribution")


# Build the response probability with a logistic sigmoid function

# By changing the mean to -5, we are capturing the left most tail of the sigmoid function (probability of responding)
# which tends towards 0 response rate. At 0, we have a 50% response rate, and towards the right we capture the 
# 100% response rates, and towards the left, we approach 0% response rates so our -5 mean sample is capturing these
# people that are very unlikely to respond

sig_neg_5 = ideology^2 + rnorm(number_of_voters, mean = -5, sd = 1) 

## Exponentiate the response scores
resp_prob = exp(sig_neg_5)/(exp(sig_neg_5) + 1)
resp_prob_df <- as.data.frame(resp_prob)
colnames(resp_prob_df) <- c("resp_prob")

resp_prob_df <- resp_prob_df %>%
    mutate(label = "response_bias")

## Plot the Response Prob
resp_prob_df %>%
    ggplot(aes(x = resp_prob)) +
    geom_density() +
    ggtitle("Population Response Probability") 

# We need to sample the population ideology given the response rates we modeled
# This means we have to apply the response probability to each voter ideology
# Pnorm takes probabilities, mean and sd and spits out a sample a sample

# normal_pop_sample <- sample(x, size, replace = FALSE, prob = NULL)
ideology_sample <- sample(x = ideology, size = number_of_voters/100, replace = TRUE)
p = resp_prob/sum(resp_prob)
response_bias_sample <- sample(x = ideology, size = number_of_voters/100, replace = TRUE, prob = p)
# sample(x = rnorm(number_of_voters, mean = -5, sd = 1), size = number_of_voters, replace = FALSE, prob = resp_prob)
response_bias_sample_df <- as.data.frame(response_bias_sample)

response_bias_sample_df <- response_bias_sample_df %>%
    rename("ideology" = "response_bias_sample") %>%
    mutate(label = "response_bias")

ideology_sample_df <- as.data.frame(ideology_sample)

ideology_sample_df <- ideology_sample_df %>%
    rename("ideology" = "ideology_sample") %>%
    mutate(label = "random_sample")


both_samples <- ideology_sample_df %>%
    rbind(response_bias_sample_df) %>%
    mutate(
        label = ifelse(label == "random_sample", "Random Sampling", "Biased Sampling"),
        label = as.factor(label)
    )

# The graph shows us what our respondent "worlds" look like. 
# With a random sample, we should get respondents representative of the population
# while a sample subject to response biases is more polarized

both_samples %>%
    ggplot(aes(x = ideology, group = label, fill = label)) +
    geom_density(adjust=1.5, alpha=.4) +
    labs(title = "Ideological Distribution of Respondents by Sampling", x = "Ideology", y = "Density") +
    guides(fill = guide_legend(title=NULL)) +
    theme_minimal()


# Now we simulate what it looks like to pull 1000 surveys from each of these two worlds, 
# and we map the distribution of the resulting means
rand_samp_means <- c()
respons_bias_means <- c()
for (x in 1:1000) {
    ideology_mean <- mean(sample(x = ideology, size = 1000, replace = TRUE))
    rand_samp_means <- append(rand_samp_means, ideology_mean)
    
    bias_smpl_mean <- mean(sample(x = ideology, size = 1000, replace = TRUE, prob = p))
    respons_bias_means <- append(respons_bias_means, bias_smpl_mean)
}

moderate_simulation <- as.data.frame(rand_samp_means)
extreme_simulation <- as.data.frame(respons_bias_means)

extreme_simulation <- extreme_simulation %>%
    rename("av_ideology" = "respons_bias_means") %>%
    mutate(label = "response_bias")

both_simulations <- moderate_simulation %>%
    rename("av_ideology" = "rand_samp_means") %>%
    mutate(label = "random_sample") %>%
    rbind(extreme_simulation)

both_simulations$label <- as.factor(both_simulations$label)
both_simulations %>%
    ggplot(aes(x = av_ideology, group = label, fill = label)) +
    geom_density(adjust=1.5, alpha=.4) +
    ggtitle("Distribution of Average Ideology",
            subtitle = "Simulation of 1000 Surveys Conducted on Each Population") + 
    labs(x = "Average Ideology",
         y = "Density") +
    guides(fill=guide_legend(title=NULL)) +
    theme_minimal()
