# Polling Analysis
Analysis of Public Polling Data. 

### polling_ideology_nonresponse_sim.R
This is a simulation of how distribution of ideology as measured by surveys changes, while the mean doesn't, if ideology and response propensity are related. Specifically, if more ideologically motivated respondents are driven to respond, how does that affect the distribution of measured values in a simulation of thousands of surveys? 

The result is that we end up with a similar mean to simple random sampling, but that the distribution is much wider given how often the ideological biases in response will bleed into the measurement. This also assumes you cannot (which we can't, really) account for ideology in weighting.