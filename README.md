# Faulkner Comment
Code and data for Storch et al. (*2021*); "Comment: Associations among fish length, dam passage history, and survival to adulthood in two at-risk species of Pacific salmon" 

## R Code
The files listed below (active links) contains code for power simulations presented in Storch et al. (*in review*).  Code and data for all other analyses are available upon request to the corresponding author.

* [FaulknerCmntPwr_chinook.R](https://github.com/FishPC/FaulknerReview/blob/main/FaulknerCmntPwr_chinook.R):  this version of the code requires manual adjustment of certain parameters (i.e., "ss.multiplier" and "sims").  At small simulation sizes, output may vary slightly among runs at a given sample size multiplier ("ss.multiplier").

* [FaulknerCmntPwr_chinookParallel.R](https://github.com/FishPC/FaulknerReview/blob/main/FaulknerCmntPwr_chinookParallel.R):  this version consolidates "FaulknerCmntPwr_chinook.r" and allows for parallel processing. At small simulation sizes, output may vary slightly among runs at a given sample size multiplier ("ss.multiplier").

* [FaulknerCmntPwr_steelhead.R](https://github.com/FishPC/FaulknerReview/blob/main/FaulknerCmntPwr_steelhead.R):  this version of the code requires manual adjustment of certain parameters (i.e., "ss.multiplier" and "sims").  At small simulation sizes, output may vary slightly among runs at a given sample size multiplier ("ss.multiplier").

* [FaulknerCmntPwr_steelheadParallel.R](https://github.com/FishPC/FaulknerReview/blob/main/FaulknerCmntPwr_steelheadParallel.R):  this version consolidates "FaulknerCmntPwr_chinook.r" and allows for parallel processing. At small simulation sizes, output may vary slightly among runs at a given sample size multiplier ("ss.multiplier").
