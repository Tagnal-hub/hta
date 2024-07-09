# Sickle Cell

####**** Parameters ****####  

# First, assign the appropriate values to the parameters below.  

## Probabilities 
p.die.no.treat <- 0.03 # Probability of dying if not treated
p.die.hu <- 0 # Probability of dying if on hydroxurea
p.sev.hu <- 0.27 # Probability of severe SCD if living on hydroxyurea
p.die.ctx <- 0.011 # Probability of dying if CTX
p.sev.chel.ctx <- 0.15 # Probability of severe SCD if on iron chelation if on CTX
p.sev.no.chel.ctx <- 0.15 # Probability of severe SCD if not on iron chelation
p.chel.ctx <- 0.95 # Probability of iron chelating if CTX
p.die.sct <- 0.06 # Probability of dying if STC
p.graft.sct <- 0.1 # Probability of graft failure if STC
p.gvhd.no.graft.sct <- 0.038 # Probability of chronic GVHD if no graft failure if STC


## Costs

cost.no.treat <- 0 # Cost if no treatment
cost.hu <- 698 # cost of hydroxurea
cost.ctx <- 1330 # cost of chronic transfusion
cost.chel <- 5000 # cost of iron chelation
cost.sct <- 25000 # cost of stem cell transplant

## Utility.
util.death <- 0
util.no.treat.sev <- 0.7
util.sev.hu <- 0.65
util.no.sev.hu <- 0.85
util.sev.chel.ctx <- 0.55
util.no.sev.chel.ctx <- 0.75
util.sev.no.chel.ctx <- 0.6
util.no.sev.no.chel.ctx <- 0.8
util.graft.sct <- 0.55
util.gvhd.no.graft.sct <- 0.65
util.no.gvhd.no.graft.sct <- 0.95


####**** Decision tree ****####


## No treatment ## ----  

# Path 1:
# Dies
no.treat.death.path <- p.die.no.treat
no.treat.death.cost <- cost.no.treat
no.treat.death.util <- util.death

# Path 2:
# Lives and Severe SCD
no.treat.live.sev.path <- 1-p.die.no.treat
no.treat.live.sev.cost <- cost.no.treat
no.treat.live.sev.util <- util.no.treat.sev

# Testing arm probabilities and results 

# Now create a vector containing all the pathway probabilities. Then do the same for 
# the costs and utilities.

no.treat.probs.vec <- c(no.treat.live.sev.path, no.treat.death.path)
no.treat.costs.vec <- c(no.treat.live.sev.cost, no.treat.death.cost)
no.treat.utils.vec <- c(no.treat.live.sev.util, no.treat.death.util)

## Next, multiply the appropriate vectors to get each of the pathway costs
no.treat.costs <- no.treat.probs.vec * no.treat.costs.vec    ## expected value of costs for each pathway (hint: use vector multiplication)

no.treat.utils <-  no.treat.probs.vec * no.treat.utils.vec    ## expected value of cases for each pathways (hint: use vector multiplication)

## For the total costs and cases of the whole intervention arm (i.e. all 5 pathways)
# take the sum of the vectors 
no.treat.costs.total <- sum(no.treat.costs)
no.treat.utils.total <- sum(no.treat.utils)

# Here we can show and label the results 
no.treat.results <- c(costs = no.treat.costs.total, utils = no.treat.utils.total)

## Hydroxyurea ## ----  

# Path 1:
# Dies
hu.death.path <- p.die.hu
hu.death.cost <- cost.hu
hu.death.util <- util.death

# Path 2:
# Lives and Severe SCD
hu.sev.path <- p.sev.hu * (1 - p.die.hu)
hu.sev.cost <- cost.hu
hu.sev.util <- util.sev.hu

# Path 3:
# Lives and no Severe SCD
hu.no.sev.path <- (1 - p.sev.hu) * (1 - p.die.hu)
hu.no.sev.cost <- cost.hu
hu.no.sev.util <- util.no.sev.hu

# Testing arm probabilities and results 

# Now create a vector containing all the pathway probabilities. Then do the same for 
# the costs and utilities.

hu.probs.vec <- c(hu.death.path, hu.sev.path, hu.no.sev.path)
hu.costs.vec <- c(hu.death.cost, hu.sev.cost, hu.no.sev.cost)
hu.utils.vec <- c(hu.death.util, hu.sev.util, hu.no.sev.util)


## Next, multiply the appropriate vectors to get each of the pathway costs
hu.costs  <- hu.probs.vec * hu.costs.vec

hu.utils <- hu.probs.vec * hu.utils.vec

## For the total costs and cases of the whole intervention arm (i.e. all 5 pathways)
# take the sum of the vectors 
hu.costs.total <- sum(hu.costs)
hu.utils.total <- sum(hu.utils)

# Here we can show andhu.costs.vec# Here we can show and label the results 
hu.results <- c(costs = hu.costs.total, utils = hu.utils.total)

## Chronic Transfusion ## ----  

# Path 1:
# Dies
ctx.death.path <- p.die.ctx
ctx.death.cost <- cost.ctx
ctx.death.util <- util.death

# Path 2:
# Lives and Iron chelation and Severe SCD
ctx.chel.sev.path <- (1 - p.die.ctx) * p.chel.ctx * p.sev.chel.ctx
ctx.chel.sev.cost <- cost.ctx + cost.chel
ctx.chel.sev.util <- util.sev.chel.ctx

# Path 3:
# Lives and Iron chelation and no Severe SCD
ctx.chel.no.sev.path <- (1 - p.die.ctx) * p.chel.ctx * (1 - p.sev.chel.ctx)
ctx.chel.no.sev.cost <- cost.ctx + cost.chel
ctx.chel.no.sev.util <- util.no.sev.chel.ctx

# Path 4:
# Lives and no Iron chelation and Severe SCD
ctx.no.chel.sev.path <- (1 - p.die.ctx) * (1 - p.chel.ctx) * p.sev.no.chel.ctx
ctx.no.chel.sev.cost <- cost.ctx
ctx.no.chel.sev.util <- util.sev.no.chel.ctx

# Path 5:
# Lives and no Iron Chelation and No severe SCD
ctx.no.chel.no.sev.path <- (1 - p.die.ctx) * (1 - p.chel.ctx) * (1 - p.sev.no.chel.ctx)
ctx.no.chel.no.sev.cost <- cost.ctx
ctx.no.chel.no.sev.util <- util.no.sev.no.chel.ctx

# Testing arm probabilities and results 

# Now create a vector containing all the pathway probabilities. Then do the same for 
# the costs and utilities.
ctx.probs.vec <- c(ctx.death.path, ctx.chel.sev.path, ctx.chel.no.sev.path, ctx.no.chel.sev.path, ctx.no.chel.no.sev.path)
ctx.costs.vec <- c(ctx.death.cost, ctx.chel.sev.cost, ctx.chel.no.sev.cost, ctx.no.chel.sev.cost, ctx.no.chel.no.sev.cost)
ctx.utils.vec <- c(ctx.death.util, ctx.chel.sev.util, ctx.chel.no.sev.util, ctx.no.chel.sev.util, ctx.no.chel.no.sev.util)

## Next, multiply the appropriate vectors to get each of the pathway costs
ctx.costs <- ctx.probs.vec * ctx.costs.vec 

ctx.utils <- ctx.probs.vec * ctx.utils.vec

## For the total costs and cases of the whole intervention arm (i.e. all 5 pathways)
# take the sum of the vectors 
ctx.costs.total <- sum(ctx.costs)
ctx.utils.total <- sum(ctx.utils)

# Here we can show andhu.costs.vec# Here we can show and label the results 
ctx.results <- c(costs = ctx.costs.total, utils = ctx.utils.total)

## Stem Cell Transplant ## ----  

# Path 1:
# Dies
sct.death.path <- p.die.sct
sct.death.cost <- cost.sct
sct.death.util <- util.death

# Path 2:
# Lives and Graft failure
sct.graft.path <- (1 - p.die.sct) * p.graft.sct
sct.graft.cost <- cost.sct
sct.graft.util <- util.graft.sct

# Path 3:
# Lives and no graft failure and chronic gvhd
sct.no.graft.gvhd.path <- (1 - p.die.sct) * (1 - p.graft.sct) * p.gvhd.no.graft.sct
sct.no.graft.gvhd.cost <- cost.sct
sct.no.graft.gvhd.util <- util.gvhd.no.graft.sct

# Path 4:
# Lives and no graft failure and no chronic gvhd
sct.no.graft.no.gvhd.path <- (1 - p.die.sct) * (1 - p.graft.sct) * (1 - p.gvhd.no.graft.sct)
sct.no.graft.no.gvhd.cost <- cost.sct
sct.no.graft.no.gvhd.util <- util.no.gvhd.no.graft.sct

# Testing arm probabilities and results 

# Now create a vector containing all the pathway probabilities. Then do the same for 
# the costs and utilities.
sct.probs.vec <- c(sct.death.path, sct.graft.path, sct.no.graft.gvhd.path, sct.no.graft.no.gvhd.path)
sct.costs.vec <- c(sct.death.cost, sct.graft.cost, sct.no.graft.gvhd.cost, sct.no.graft.no.gvhd.cost)
sct.utils.vec <- c(sct.death.util, sct.graft.util, sct.no.graft.gvhd.util, sct.no.graft.no.gvhd.util)

## Next, multiply the appropriate vectors to get each of the pathway costs
sct.costs <- sct.probs.vec * sct.costs.vec

sct.utils <- sct.probs.vec * sct.utils.vec

## For the total costs and cases of the whole intervention arm (i.e. all 5 pathways)
# take the sum of the vectors 
sct.costs.total <- sum(sct.costs)
sct.utils.total <- sum(sct.utils)

# Here we can show and label the results 
sct.results <- c(costs = sct.costs.total, utils = sct.utils.total)





####**** Analysis: Incremental results ****#### 

# Create a named list of the results
results_list <- list(
  no.treat = no.treat.results,
  hu = hu.results,
  ctx = ctx.results,
  sct = sct.results
)

# Combine the list into a dataframe
results_df <- do.call(rbind, results_list)

# Convert row names to a column
results_df <- data.frame(Treatment = rownames(results_df), results_df)
rownames(results_df) <- NULL

# ICER ----
base_cost <- results_df$costs[1]
base_util <- results_df$utils[1]

results_df <- results_df |>
  dplyr::mutate(
    inc_cost = costs - base_cost,
    inc_util = utils - base_util,
    icer = inc_cost/inc_util
  )


# Net Monetary Benefit & Net Health Benefit ----

threshold <- 30000

results_df <- results_df |>
  dplyr::mutate(
    nmb = (inc_util * threshold) - inc_cost,
    nhb = inc_util - (inc_cost/threshold)
  ) |>
  dplyr::select(-c("inc_cost", "inc_util"))


#### Cost Effectiveness Acceptability Curve ####

results_list <- list(
  no.treat = no.treat.results,
  hu = hu.results,
  ctx = ctx.results,
  sct = sct.results
)

# Extract costs and outcomes into separate vectors
costs <- sapply(results_list, function(x) x['costs'])
outcomes <- sapply(results_list, function(x) x['utils'])

# Combine into a data frame for convenience
psa_data <- data.frame(treatment = names(results_list), costs = costs, outcomes = outcomes)


calculate_incrementals <- function(base, comparator) {
  incr_costs <- comparator['costs'] - base['costs']
  incr_effects <- comparator['utils'] - base['utils']
  data.frame(incr_costs = incr_costs, incr_effects = incr_effects)
}

# Baseline results (no treatment)
base <- results_list$no.treat

# Incremental costs and effects for each treatment compared to no treatment
incr_hu_vs_notreat <- calculate_incrementals(base, results_list$hu)
incr_ctx_vs_notreat <- calculate_incrementals(base, results_list$ctx)
incr_sct_vs_notreat <- calculate_incrementals(base, results_list$sct)

# Combine into a single list for convenience
psa_incr_data <- list(
  hu_vs_notreat = incr_hu_vs_notreat,
  ctx_vs_notreat = incr_ctx_vs_notreat,
  sct_vs_notreat = incr_sct_vs_notreat
)

wtp_thresholds <- seq(0, 200000, by = 500)

calculate_ceac <- function(incr_data, wtp_thresholds) {
  ceac <- sapply(wtp_thresholds, function(wtp) {
    mean(incr_data$incr_effects > 0 & incr_data$incr_costs / incr_data$incr_effects <= wtp)
  })
  ceac
}

# Calculate CEACs for each comparison
ceac_hu_vs_notreat <- calculate_ceac(incr_hu_vs_notreat, wtp_thresholds)
ceac_ctx_vs_notreat <- calculate_ceac(incr_ctx_vs_notreat, wtp_thresholds)
ceac_sct_vs_notreat <- calculate_ceac(incr_sct_vs_notreat, wtp_thresholds)

# Combine into a data frame for plotting
ceac_data <- data.frame(
  wtp = wtp_thresholds,
  hu_vs_notreat = ceac_hu_vs_notreat,
  ctx_vs_notreat = ceac_ctx_vs_notreat,
  sct_vs_notreat = ceac_sct_vs_notreat
)

library(ggplot2)

# Melt data for plotting
ceac_data_long <- tidyr::pivot_longer(ceac_data, cols = -"wtp")

# Plot
ggplot(ceac_data_long, aes(x = wtp, y = value, color = name)) +
  geom_line(size = 1) +
  labs(title = "Cost-Effectiveness Acceptability Curve",
       x = "Willingness-to-Pay Threshold (WTP) per QALY",
       y = "Probability Cost-Effective",
       color = "Comparison") +
  theme_minimal()







