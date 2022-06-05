################################# Dynamics of Cooperative Networks Associated With Gender Among South Indian Tamils
################################# Replication Code: Data Analysis




library(purrr) 
library(dplyr) 

library(network) 
library(sna) 
library(igraph) 
library(intergraph)

library(kinship2) 

library(ggplot2) 
library(ggstance) ## Used for geom_pointrangeh 
library(viridis) 
library(jtools)
library(ggthemes)
library(scales) ## used for alpha() # https://www.r-bloggers.com/2018/08/using-the-scales-package-to-change-alpha-in-base-r-plots/
library(ggh4x) ## Used for facetted_pos_scales()

library(scatterplot3d) # http://www.sthda.com/english/wiki/scatterplot3d-3d-graphics-r-software-and-data-visualization
library(svglite) ## Used to save ggplot2 figures as .svg files

library(reshape2) 

library(Matrix)

library(pastecs) 
library(stargazer) 

library(RSiena) ## You may also need to install the package ‘tcltk’ which RSiena uses to display model progress when estimation is not set to silent.

library(parallel) ## Not on CRAN, automatically installed with Base R.


set.seed(20180709)
options(scipen = 8)
options(digits = 5)
options(max.print = 50000000)



#################################### SET NUMBER OF AVAILABLE COMPUTING CORES ####################################  
## Unfortunately, RSiena ignores the "seed" argument for the purposes of random number generation
## when estimating SAOMs using multiple CPU cores. In order for the "seed" argument below in sienaAlgorithmCreate()
## to not be ignored, siena07() — i.e., the function used to estimate SAOMs — must be run using a single core/not in 
## parallel. This slows estimation considerably. However, adherence to the random seed (here, 20180709) is necessary 
## to exactly reproduce results. 

cores <- 10



#################################### LOAD NETWORK AND ATTRIBUTE DATA FOR ALAKAPURAM AND TENPATTI ####################################  
source("Gender_Cooperative_Networks_DataPrep_Tamil_Nadu.R")



######################################################################## STOCHASTIC ACTOR-ORIENTED MODELS (SAOMs) ########################################################################
############################################## DEFINE FUNCTION FOR THE ITERATIVE RUNNING OF siena07() ############################################## 
### The following function will repeatedly execute siena07() until a SAOM has converged in line with the convergence criteria below.
### Here, prevAns (i.e., an earlier existing "on track" estimation result) is repeatedly used to determine the initial values for estimation at each successive iteration of the algorithm.
### Because each run of siena07() can take many hours, each iteration is saved as an .RData file so no progress is lost.
siena07RunToConvergence <- function(alg, dat, eff, ans0, modelName, ...){
  
  numr <- 0
  
  ans <- siena07(alg, data = dat, effects = eff, prevAns = ans0, returnDeps = TRUE, ...)
  
  repeat{
    numr <- numr + 1  ## Count the number of repeated runs
    tconv.max <- ans$tconv.max  ## Extract the overall maximum convergence ratio
    tratio.max <- max( abs( ans$tconv ) ) ## Identify the maximum absolute value of the t-ratios for convergence for each of the parameters
    
    if (tconv.max > 100) {  ## Divergence without much hope of returning to good parameter values
      print(ans)
      cat("WARNING: Extreme Divergence. Terminating run.\n")
      return("WARNING: Extreme Divergence. Terminating run")
    }
    ## These are the convergence criteria used for the study. Convergence is excellent when the overall maximum convergence ratio (tconv.max) 
    ## is less than 0.12, and, for all the individual parameters, the t-ratios for convergence are all (tratio.max) less than 0.10 in absolute value
    else if (tconv.max < 0.12 & tratio.max < 0.10) {
      print(ans)
      cat(paste0("Maximum Absolute Value Amongst Convergence t-Ratios: ", tratio.max, "\n"))
      cat(paste0("Model Has Converged After ", numr, " iterations. \n"))
      
      save(ans, file =  paste0(modelName,"_SIENA_Iteration_Number_", numr,"_CONVERGED.RData") )
      
      return(ans)
      
    }
    else {
      print(ans)
      cat("WARNING: Convergence Inadequate.\n")
      cat(paste0("Overall Maximum Convergence Ratio: ", tconv.max, "\n"))
      cat(paste0("Iteration Number: ", numr), "\n") ## Report how far along we are
      
      save(ans, file =  paste0(modelName,"_SIENA_Iteration_Number_", numr,"_NOT_CONVERGED.RData") )
      
      ans <- siena07(alg, data = dat, effects = eff, prevAns = ans, returnDeps = TRUE, ...)
      
    }
  }
}


################################ DEFINE SIENA OBJECTS FOR SAOM ESTIMATION #################################

villagers <- rownames(socialsupportTN.13)
villagers.size <- length(villagers)
villagers <- sienaNodeSet(villagers.size, nodeSetName = "villagers", names = villagers)


#################################### THE DEPENDENT NETWORK
### To fit the SAOMs, we need a 3D array of network snapshots/waves.
### However, before we do that, we need to add the structurally-determined values to the second wave in order to tell SIENA which villagers have moved away/died.
### N.B. Structural Code 10 indicates ties that are impossible.
## FIRST, create new matrix object we can modify without destroying the originals.
wave.13 <- socialsupportTN.13
wave.17 <- socialsupportTN.17


######################################### Network Construction: Handling Network Leavers
## Villagers who were present in 2013 and who did not take the survey in 2017 could be NOMINATED by those villagers present in 2013 who did take the survey in 2017?
## As mentioned in the paper, we restrict our analysis to only those ties between residents who are present.
table(list( "Did Survey" = individuals.TN.13$DidSurvey17, ## 190 residents present in 2013 are absent in 2017
            "Alive" = individuals.TN.13$Living_2017, ## 51 residents alive in 2013 have passed away by 2017
            "In Village" = individuals.TN.13$Residing_2017) ## 122 residents present in 2013 have moved away form the village by 2017; 17 residents remain in the village in 2017 but do not participate in the study
)


rowSums(wave.17[which(individuals.TN.13$DidSurvey17 == FALSE), ]) == 0 ## Sanity Check. 190 actors exit the network in 2017 and they should all have zero outgoing ties.
table(rowSums(wave.17[which(individuals.TN.13$DidSurvey17 == FALSE), ]) == 0) 

sum(wave.17[, which(individuals.TN.13$DidSurvey17 == FALSE)]) ## Number of ties from the 782 people who were present in 2013 to the 190 people who were absent in 2017; RUN: dim(wave.17[, which(individuals.TN.13$DidSurvey17 == FALSE)])
length(which(colSums(wave.17[, which(individuals.TN.13$DidSurvey17 == FALSE)]) > 0)) ## 102 people present in 2013 but absent in 2017 receive nominations in 2017.

## Of the 102 people present in 2013 but absent in 2017 receive nominations in 2017, how many were in/outside of the village and alive/dead?
table(list( "Did Survey" = individuals.TN.13[names(which(colSums(wave.17[, which(individuals.TN.13$DidSurvey17 == FALSE)]) > 0)), "DidSurvey17"], 
            "Alive" = individuals.TN.13[names(which(colSums(wave.17[, which(individuals.TN.13$DidSurvey17 == FALSE)]) > 0)), "Living_2017"], 
            "In Village" = individuals.TN.13[names(which(colSums(wave.17[, which(individuals.TN.13$DidSurvey17 == FALSE)]) > 0)), "Residing_2017"])
)


## SECOND, add the Structural Zeros/Structural Code 10 to the wave-two sociomatrix.
wave.17[which(individuals.TN.13$DidSurvey17 == FALSE), ] <- 10 ## Making this change tells SIENA that ties from actors who do not participate in the survey in 2017 are impossible
wave.17[, which(individuals.TN.13$DidSurvey17 == FALSE)] <- 10 ## Making this change tells SIENA that ties to actors who do not participate in the survey in 2017 are impossible/this ensures that we only model ties between people within each village who participate in the survey in 2017.


## THIRD, create our 3D array using the modified sociomatrices.
support_array <- array(data = c(wave.13, wave.17), dim = c(villagers.size, villagers.size, 2))


## FOURTH, formally create the SIENA dependent network object.
support_net <- sienaNet(support_array, type = "oneMode", nodeSet = "villagers", allowOnly = FALSE)


####################################  MONADIC COVARIATES — CENTRED AT THEIR WITHIN-VILLAGE MEANS
## During the simulation, the value of the changing covariate for wave m is treated as valid/fixed for the whole period from wave m to wave m+1. 
## If the data set has M waves, this means that the values, if any, for wave M will not be used. Therefore, the number of columns can 
## be M or M-1; if the former, the values in the last column will not be used.
## Accordingly, as my analysis is for two period-specific networks, one only needs the covariate for the first wave.
## Note that coCovar( ) is used instead of varCovar( ) as each period-specific network gets its own covariate objects!

gender <- coCovar(individuals.TN.13$Gender,
                  centered = FALSE, nodeSet = "villagers")


age <- coCovar(scale(2013 - individuals.TN.13$BirthYear, center = TRUE, scale = TRUE)[,1],
               centered = FALSE, nodeSet = "villagers")

age_squared <- coCovar(scale( (2013 - individuals.TN.13$BirthYear)^2, center = TRUE, scale = TRUE)[,1],
                       centered = FALSE, nodeSet = "villagers")

education <- coCovar(scale(individuals.TN.13$EduYears_2013, center = TRUE, scale = TRUE)[,1],
                     centered = FALSE, nodeSet = "villagers")

HH_wealth <- coCovar(scale(log(individuals.TN.13$HouseholdWealth_2013), center = TRUE, scale = TRUE)[,1],
                     centered = FALSE, nodeSet = "villagers")

caste <- coCovar(as.numeric(factor(individuals.TN.13$Caste)),
                 centered = FALSE, nodeSet = "villagers")

reservation.status <- coCovar(individuals.TN.13$Reservation_Status,
                              centered = FALSE, nodeSet = "villagers")

immigrant.status <- coCovar(individuals.TN.13$non.natal.village,
                            centered = FALSE, nodeSet = "villagers")

partnership.status <- coCovar(ifelse(individuals.TN.13$Status_2013 == "Not Married", 1, 0),
                              centered = FALSE, nodeSet = "villagers")

cor(
  cbind(Generous_2013 = colSums(TN.Nets[["Generous_2013"]])[individuals.TN.13$IndivID], 
        Influence_2013 = colSums(TN.Nets[["Influence_2013"]])[individuals.TN.13$IndivID],
        Character_2013 = colSums(TN.Nets[["Character_2013"]])[individuals.TN.13$IndivID],
        Strength_2013 = colSums(TN.Nets[["Strength_2013"]])[individuals.TN.13$IndivID]
  ),
  method = "spearman" ## https://www.guru99.com/r-pearson-spearman-correlation.html#2 
)

general_reputation <- colSums(TN.Nets[["Generous_2013"]] + 
                                TN.Nets[["Influence_2013"]] + 
                                TN.Nets[["Character_2013"]] + 
                                TN.Nets[["Strength_2013"]]
)
general_reputation <- general_reputation[individuals.TN.13$IndivID] ## Ensure things are in the right order
general_reputation <- sqrt(general_reputation)
general_reputation <- coCovar(scale(general_reputation, center = TRUE, scale = TRUE)[,1],
                              centered = FALSE, nodeSet = "villagers")


HH_ID <- coCovar(as.numeric(factor(individuals.TN.13$GPS_2013)), 
                 centered = FALSE, nodeSet = "villagers")


village <- coCovar(ifelse(individuals.TN.13$Location_2013 == "Tenpatti", 1, 0), 
                   centered = FALSE, nodeSet = "villagers")



####################################  DYADIC COVARIATES
# consanguineal.relatedness <- relatednessTN.13
# diag(consanguineal.relatedness) <- 0 ## 1's along the diagonal (One's relatedness to themselves. Not needed for the SAOMs)
# consanguineal.relatedness <- coDyadCovar( consanguineal.relatedness, ## coefficient of relatedness between i and j
#                                           centered = FALSE, nodeSets = c("villagers", "villagers"),
#                                           type = "oneMode")
# 
# affinal.relatedness <- relatedness.affinalTN.13
# affinal.relatedness <- coDyadCovar( affinal.relatedness, ## Coefficient of relatedness between i and j via marriage
#                                     centered = FALSE, nodeSets = c("villagers", "villagers"),
#                                     type = "oneMode")


consanguineal.kin <- relatednessTN.13
diag(consanguineal.kin) <- 0 ## 1's along the diagonal (One's relatedness to themselves. Not needed for the SAOMs)
consanguineal.kin[consanguineal.kin > 0] <- 1

affinal.kin <- relatedness.affinalTN.13
affinal.kin[affinal.kin > 0] <- 1

kin.dyad <- consanguineal.kin + affinal.kin
kin.dyad[kin.dyad > 0] <- 1
kin.dyad <- coDyadCovar( kin.dyad, ## Coefficient of relatedness between i and j via marriage
                         centered = FALSE, nodeSets = c("villagers", "villagers"),
                         type = "oneMode")

friendship.dyad <- coDyadCovar( friendshipTN.13,
                                centered = FALSE, nodeSets = c("villagers", "villagers"),
                                type = "oneMode")

## log of the geographic distance following: Preciado, P., Snijders, T.A., Burk, W.J., Stattin, H., Kerr, M., 2012. Does proximity matter? Distance dependence of adolescent friendships. Social Networks, 34(1):18-31. https://doi.org/10.1016/j.socnet.2011.01.002
geodist.dyad <- log(interhousehold.dist.TN.13 + 1)
geodist.dyad.mean <- stat.desc(geodist.dyad[upper.tri(geodist.dyad, diag = F)])["mean"] ## Distance is symmetric! Accordingly, we only want to calculate the mean across unique dyads.
geodist.dyad.sd <- stat.desc(geodist.dyad[upper.tri(geodist.dyad, diag = F)])["std.dev"] ## Distance is symmetric! Accordingly, we only want to calculate the standard deviation across unique dyads.

geodist.dyad <- (geodist.dyad - geodist.dyad.mean)/geodist.dyad.sd # Rescale for model fitting
geodist.dyad <- coDyadCovar(geodist.dyad, ## Inter-household distance in meters
                             centered = FALSE, nodeSets = c("villagers", "villagers"),
                             type = "oneMode")


####################################  CREATE THE RSIENA DATA OBJECT FOR MODEL FITTING
multidata <- sienaDataCreate(support_net,
                             # consanguineal.relatedness,
                             # affinal.relatedness,
                             kin.dyad,
                             friendship.dyad,
                             geodist.dyad,
                             gender,
                             age,
                             age_squared,
                             education,
                             HH_wealth,
                             reservation.status,
                             caste,
                             immigrant.status,
                             partnership.status,
                             general_reputation,
                             HH_ID,
                             village,
                             nodeSets = list(villagers)
) 


villages.TN.sienaData <- multidata


rm(villagers, villagers.size, support_net, support_array, wave.13, wave.17,
   # consanguineal.relatedness,
   # affinal.relatedness,
   consanguineal.kin,
   affinal.kin,
   kin.dyad,
   friendship.dyad,
   geodist.dyad, geodist.dyad.mean, geodist.dyad.sd,
   gender,
   age,
   age_squared,
   education,
   HH_wealth,
   reservation.status,
   caste,
   immigrant.status,
   partnership.status,
   general_reputation,
   HH_ID,
   village,
   multidata
) 





#### RSIENA-provided summary statistics about the dependent network and covariates
print01Report(villages.TN.sienaData, modelname = "Gender_Cooperation_Tamil_Nadu_2022_sienaDataSummary", getDocumentation = FALSE)



############################################# DEFINE SIENA ALGORITHM ############################################# 
modelparams <- sienaAlgorithmCreate(projname = "Gender_Cooperation_Tamil_Nadu_2022_Estimation_History" 
                                    
                                    # Boolean. Only relevant for Method of Moments simulation/estimation. 
                                    # If TRUE, use conditional simulation; if FALSE, unconditional simulation. 
                                    , cond = TRUE
                                    
                                    # Boolean. Whether to use maximum likelihood method or Method of Moments estimation.
                                    , maxlike = FALSE
                                    
                                    # Number of subphases in phase 2.
                                    , nsub = 4
                                    
                                    # Number of iterations in phase 3. For regular use with the Method of Moments, n3 = 1000
                                    # mostly suffices. For use in publications and for Maximum Likelihood, at least n3 = 3000
                                    # is advised. Sometimes much higher values are required for stable estimation of standard errors.
                                    , n3 = 10000
                                    
                                    # This determines the step sizes in the estimation algorithm. If the algorithm is unstable 
                                    # (e.g., oscillating between wild parameter estimates and convergence from run to run),
                                    # use a smaller value (but greater than 0). The default value is 0.2. Sometimes
                                    # for difficult data-model combinations, the algorithm diverges very quickly, and this
                                    # may be countered by smaller values of firstg, e.g., 0.01 or 0.05.
                                    , firstg = 0.30 
                                    
                                    # Number between 0 and 1 (bounds included), values outside this interval will be truncated; 
                                    # for diagonalize = 0 the complete estimated derivative matrix will be used for updates in the Robbins-Monro procedure; 
                                    # for diagonalize = 1 only the diagonal entries will be used; for values between 0 and 1, the weighted average will be 
                                    # used with weight diagonalize for the diagonalized matrix. Has no effect for Maximum Likelihood estimation.
                                    # Higher values are more stable, lower values potentially more efficient. 
                                    # Default for Method of Moments estimation is diagonalize = 0.2.
                                    , diagonalize = 0.2
                                    
                                    # The random seed will NOT be adhered to if one runs siena07RunToConvergence()/siena07() with multiple cores
                                    , seed = 20180709
)



############################################# ESTIMATE LONGITUDINAL SAOMs ############################################# 

## Model 1: Baseline Model (Typically Converges After Two Iterations of siena07RunToConvergence)
fit.1.modeffects <- getEffects(villages.TN.sienaData, nintn = 20)

fit.1.modeffects <- includeEffects(fit.1.modeffects, recip, name = "support_net", type = "eval", fix = FALSE, verbose = FALSE, include = TRUE) ## Contingent giving; N.B. Direct reciprocity is automatically included in SAOMs. Make it Explicit.

# fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "consanguineal.relatedness", type = "eval", fix = FALSE, verbose = FALSE)
# fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "affinal.relatedness", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "kin.dyad", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "friendship.dyad", type = "eval", fix = FALSE, verbose = FALSE) 
fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "geodist.dyad", type = "eval", fix = FALSE, verbose = FALSE)

fit.1.modeffects <- includeEffects(fit.1.modeffects, sameX, name = "support_net", interaction1 = "gender", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, absDiffX, name = "support_net", interaction1 = "age", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, sameX, name = "support_net", interaction1 = "caste", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, absDiffX, name = "support_net", interaction1 = "general_reputation", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, sameX, name = "support_net", interaction1 = "HH_ID", type = "eval", fix = FALSE, verbose = FALSE)

fit.1.modeffects <- includeEffects(fit.1.modeffects, absDiffX, name = "support_net", interaction1 = "village", type = "eval", fix = FALSE, verbose = FALSE)

fit.1.modeffects <- includeEffects(fit.1.modeffects, inPopSqrt, name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, inActSqrt, name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, outActSqrt, name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.1.modeffects <- includeEffects(fit.1.modeffects, transTrip, name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, transRecTrip, name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, cycle3, name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)


fit.1.ans <- siena07RunToConvergence(alg = modelparams, dat = villages.TN.sienaData, eff = fit.1.modeffects, ans0 = NULL,
                                     modelName = "fit.1.ans_TN",
                                     batch = TRUE, verbose = FALSE, silent = FALSE,
                                     nbrNodes = cores, useCluster = TRUE)




## Model 2: Fully Interacted Model (Typically Converges After Two Iterations of siena07RunToConvergence)
fit.2.modeffects <- includeEffects(fit.1.modeffects, egoX, name = "support_net", interaction1 = "gender", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, recip, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

# fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "consanguineal.relatedness"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
# fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "affinal.relatedness"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "kin.dyad"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "friendship.dyad"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "geodist.dyad"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, sameX, interaction1 = c("gender", "gender"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, absDiffX, interaction1 = c("gender", "age"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, sameX, interaction1 = c("gender", "caste"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, absDiffX, interaction1 = c("gender", "general_reputation"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, sameX, interaction1 = c("gender", "HH_ID"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, absDiffX, interaction1 = c("gender", "village"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, inPopSqrt, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, inActSqrt, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, outActSqrt, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, transTrip, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, transRecTrip, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, cycle3, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)


fit.2.ans <- siena07RunToConvergence(alg = modelparams, dat = villages.TN.sienaData, eff = fit.2.modeffects, ans0 = NULL,
                                     modelName = "fit.2.ans_TN",
                                     batch = TRUE, verbose = FALSE, silent = FALSE,
                                     nbrNodes = cores, useCluster = TRUE)



## Model 3: Social Constraints Adjusted Model (Typically Converges After Two Iterations of siena07RunToConvergence)
fit.3.modeffects <- includeEffects(fit.2.modeffects, egoX, name = "support_net", interaction1 = "age", type = "eval", fix = FALSE, verbose = FALSE)
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoX, name = "support_net", interaction1 = "age_squared", type = "eval", fix = FALSE, verbose = FALSE)
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoX, name = "support_net", interaction1 = "education", type = "eval", fix = FALSE, verbose = FALSE)
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoX, name = "support_net", interaction1 = "HH_wealth", type = "eval", fix = FALSE, verbose = FALSE)
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoX, name = "support_net", interaction1 = "reservation.status", type = "eval", fix = FALSE, verbose = FALSE)
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoX, name = "support_net", interaction1 = "immigrant.status", type = "eval", fix = FALSE, verbose = FALSE)
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoX, name = "support_net", interaction1 = "partnership.status", type = "eval", fix = FALSE, verbose = FALSE)
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoX, name = "support_net", interaction1 = "general_reputation", type = "eval", fix = FALSE, verbose = FALSE)


fit.3.ans <- siena07RunToConvergence(alg = modelparams, dat = villages.TN.sienaData, eff = fit.3.modeffects, ans0 = NULL,
                                     modelName = "fit.3.ans_TN",
                                     batch = TRUE, verbose = FALSE, silent = FALSE,
                                     nbrNodes = cores, useCluster = TRUE)




################################# COMBINE All SAOM FIT OBJECTS FOR POST-PROCESSING ################################# 
villages.TN.sienaFits <- list("Model_1" = fit.1.ans, 
                              "Model_2" = fit.2.ans,
                              "Model_3" = fit.3.ans
                              )
closeAllConnections() 




################################# DISTRIBUTIONAL GOODNESS OF FIT ASSESSMENT ################################# 
cl <- makeCluster(cores)

# ## The following lines ensures that sienaGOF/GeodesicDistribution can access the "network" package and the "sna" package via groundhog on each CPU core
# clusterEvalQ(cl, library("groundhog")) ## https://stackoverflow.com/questions/40749916/parallel-in-r-function-not-found-how-to-export-to-cluster-functions-from-ext
# clusterEvalQ(cl, groundhog.library("network", "2021-10-10", quiet.install = FALSE)) ## clusterEvalQ evaluates a literal expression on each cluster node. It is a parallel version of evalq, and is a convenience function invoking clusterCall.
# clusterEvalQ(cl, groundhog.library("sna", "2021-10-10", quiet.install = FALSE))

villages.TN.sienaGOFs.indegree <- list()
villages.TN.sienaGOFs.outdegree <- list()
villages.TN.sienaGOFs.geodist <- list()
villages.TN.sienaGOFs.triadcensus <- list()
villages.TN.sienaGOFs.genderstats <- list()

for(i in names(villages.TN.sienaFits)){ # model <- "Model_3"
  
  model <- i 
  
  cat("\n",paste("Model Number:", which(c("Model_1", "Model_2", "Model_3") == model)),"\n")
  
  
  ## See ?RSiena::sienaGOF
  GeodesicDistribution <- function (i, data, sims, period, groupName,
                                    varName, levls = c(1:5, Inf), cumulative = FALSE, ...) {
    x <- RSiena::networkExtraction(i, data, sims, period, groupName, varName)
    require(network)
    require(sna)
    # a <- sna::geodist(symmetrize(x))$gdist ## http://faculty.ucr.edu/~hanneman/nettext/C7_Connection.html#geodesic
    a <- sna::geodist(x)$gdist ## These are the geodesic distances for directed paths
    if (cumulative)
    {
      gdi <- sapply(levls, function(i){ sum(a <= i) })
    }
    else
    {
      gdi <- sapply(levls, function(i){ sum(a == i) })
    }
    names(gdi) <- as.character(levls)
    gdi
  }
  
  
  ## Reputation Nominations for GenderedEgoStatsDistribution
  general_reputation <- colSums(TN.Nets[["Generous_2013"]] +
                                  TN.Nets[["Influence_2013"]] +
                                  TN.Nets[["Character_2013"]] +
                                  TN.Nets[["Strength_2013"]]
  )
  general_reputation <- general_reputation[individuals.TN.13$IndivID] ## Ensure things are in the right order
  general_reputation <- sqrt(general_reputation) ## Use the square-root transformation to match the fitted models.
  
  ## Binary Kinship for GenderedEgoStatsDistribution
  consanguineal.kin <- relatednessTN.13
  diag(consanguineal.kin) <- 0 ## 1's along the diagonal (One's relatedness to themselves. Not needed for the SAOMs)
  consanguineal.kin[consanguineal.kin > 0] <- 1
  
  affinal.kin <- relatedness.affinalTN.13
  affinal.kin[affinal.kin > 0] <- 1
  
  kin.dyad <- consanguineal.kin + affinal.kin
  kin.dyad[kin.dyad > 0] <- 1
  
  
  same.gender <- data.frame()
  for(i in 1:nrow(individuals.TN.13)){
    gender <- individuals.TN.13$Gender[i] ## What is i's gender? ## 1 == Female; Male == 0
    gender.match <- individuals.TN.13$Gender == gender ## Match i's gender against the vector of all residents' genders across both villages
    gender.match <- as.numeric(gender.match) ## Convert the TRUE/FALSE vectors into zeros (FALSE) and ones (TRUE)
    
    same.gender <- rbind(same.gender, gender.match) 
    #print(gender.match)
    rm(i, gender, gender.match)
  }
  same.gender <- as.matrix(same.gender)
  colnames(same.gender) <- individuals.TN.13$IndivID
  rownames(same.gender) <- individuals.TN.13$IndivID
  diag(same.gender) <- 0 
  
  
  
  
  ## See ?RSiena::sienaGOF
  GenderedEgoStatsDistribution <- function(i, obsData, sims, period, groupName, varName, 
                                           vertex_IDs, gender, reputation_nominations, binary_friendship, binary_kinship, binary_same_gender,...) {
    
    require(sna)
    require(Matrix)
    
    # target.network <- RSiena::networkExtraction(1, fit.1.ans$f, fit.1.ans$sims, period = 1, varName = "support_net", groupName = "Data1")
    
    target.network <- RSiena::networkExtraction(i, obsData, sims, period, groupName, varName)
    
    target.network %v% "vertex.names" <- vertex_IDs
    target.network %v% "gender" <- ifelse(gender == 1, "Female", "Male")
    target.network %v% "reputation_nominations" <- reputation_nominations
    
    
    ## H1: Out-degree
    ego.supportive.ties <- sna::degree(target.network, gmode = "digraph", cmode = "outdegree")
    names(ego.supportive.ties) <- vertex_IDs
    
    ego.supportive.ties.mean.diff <- tapply(X = ego.supportive.ties, INDEX = target.network %v% "gender", FUN = mean)
    ego.supportive.ties.mean.diff <- ego.supportive.ties.mean.diff[["Female"]] - ego.supportive.ties.mean.diff[["Male"]]
    
    
    
    ## H2: Reciprocity
    ego.supportive.mutuals <- sna::degree(symmetrize(target.network, rule = "strong"), gmode = "graph", cmode = "freeman")
    
    ego.supportive.mutuals.mean.diff <- tapply(X = ego.supportive.mutuals, INDEX = target.network %v% "gender", FUN = mean)
    ego.supportive.mutuals.mean.diff <- ego.supportive.mutuals.mean.diff[["Female"]] - ego.supportive.mutuals.mean.diff[["Male"]]
    
    
    
    ## H3: Friendship
    ego.supportive.friends <- rowSums(target.network[,]*binary_friendship)
    
    ego.supportive.friends.mean.diff <- tapply(X = ego.supportive.friends, INDEX = target.network %v% "gender", FUN = mean)
    ego.supportive.friends.mean.diff <- ego.supportive.friends.mean.diff[["Female"]] - ego.supportive.friends.mean.diff[["Male"]]
    
    
    
    ## H4: Kinship
    ego.supportive.kin <- rowSums(target.network[,]*binary_kinship)
    
    ego.supportive.kin.mean.diff <- tapply(X = ego.supportive.kin, INDEX = target.network %v% "gender", FUN = mean)
    ego.supportive.kin.mean.diff <- ego.supportive.kin.mean.diff[["Female"]] - ego.supportive.kin.mean.diff[["Male"]]
    
    
    
    ## H5a: Transitive Triplets
    ## To count the number of transitive triads and the number of cyclic triads in each actor's ego-nets we simply count the number of outbound and inbound two paths that ego's outgoing ties close.
    ## To do this, we use the function kpath.census()
    ## dyadic.tabulation == the type of dyadic path count information to be tabulated. 
    ## dyadic.tabulation == "sum" returns a vertex by vertex matrix of source/destination path counts.
    target.network.two.paths <- kpath.census(target.network, mode = "digraph", maxlen = 2, tabulate.by.vertex = FALSE, path.comembership = "none", dyadic.tabulation = "sum")
    
    
    ## This returns the sum of the number of outbound two-paths between ego and each of their alters.
    ego.supportive.transitive.triads <- rowSums(target.network.two.paths$paths.bydyad*target.network[,])
    ego.supportive.transitive.triads <- ego.supportive.transitive.triads[vertex_IDs] ## Ensure the vector is arranged in the correct order
    
    ego.supportive.transitive.triads.mean.diff <- tapply(X = ego.supportive.transitive.triads, INDEX = target.network %v% "gender", FUN = mean)
    ego.supportive.transitive.triads.mean.diff <- ego.supportive.transitive.triads.mean.diff[["Female"]] - ego.supportive.transitive.triads.mean.diff[["Male"]]


    
    ## H5b: Three Cycles
    ## This returns the sum of the number of inbound two-paths between ego and each of their alters.
    ego.supportive.three.cycles <- rowSums(t(target.network.two.paths$paths.bydyad)*target.network[,]) ## Note the transposing of "target.network.two.paths$paths.bydyad"!
    ego.supportive.three.cycles <- ego.supportive.three.cycles[vertex_IDs] ## Ensure the vector is arranged in the correct order
    
    ego.supportive.three.cycles.mean.diff <- tapply(X = ego.supportive.three.cycles, INDEX = target.network %v% "gender", FUN = mean)
    ego.supportive.three.cycles.mean.diff <- ego.supportive.three.cycles.mean.diff[["Female"]] - ego.supportive.three.cycles.mean.diff[["Male"]]
    
    
    
    ## H6: Absolute Difference: General Reputation
    general_reputation.absdiff <- matrix(data = rep( target.network %v% "reputation_nominations" , times = network.size(target.network)), nrow = network.size(target.network), ncol = network.size(target.network)) 
    general_reputation.absdiff <- abs( general_reputation.absdiff - t(general_reputation.absdiff) )
    colnames(general_reputation.absdiff) <- vertex_IDs
    rownames(general_reputation.absdiff) <- vertex_IDs
    diag(general_reputation.absdiff) <- 0
    
    ego.supporitive.ties.reputationAbsDiff <- rowSums(target.network[,]*general_reputation.absdiff)
    
    ego.supporitive.ties.reputationAbsDiff.mean.diff <- tapply(X = ego.supporitive.ties.reputationAbsDiff, INDEX = target.network %v% "gender", FUN = mean)
    ego.supporitive.ties.reputationAbsDiff.mean.diff <- ego.supporitive.ties.reputationAbsDiff.mean.diff[["Female"]] - ego.supporitive.ties.reputationAbsDiff.mean.diff[["Male"]]
    
    
    ## H7: Same Gender
    ego.same.gender.patrons <- rowSums(target.network[,]*binary_same_gender)
    
    ego.same.gender.patrons.mean.diff <- tapply(X = ego.same.gender.patrons, INDEX = target.network %v% "gender", FUN = mean)
    ego.same.gender.patrons.mean.diff <- ego.same.gender.patrons.mean.diff[["Female"]] - ego.same.gender.patrons.mean.diff[["Male"]]
    
    
    
    ## Bind all of the gender-based differences together in a named vector
    gendered_ego_stats <- c(
      "Out-degree" = ego.supportive.ties.mean.diff,
      "Reciprocal Out-degree" = ego.supportive.mutuals.mean.diff,
      "Supportive Friends" = ego.supportive.friends.mean.diff,
      "Supportive Kin" = ego.supportive.kin.mean.diff,
      "Transitive Triads" = ego.supportive.transitive.triads.mean.diff,
      "Three Cycles" = ego.supportive.three.cycles.mean.diff,
      "General Reputation" = ego.supporitive.ties.reputationAbsDiff.mean.diff,
      "Same Gender Patrons" = ego.same.gender.patrons.mean.diff
    )
    
    ## names are transferred automatically
    gendered_ego_stats
    
  }
  
  # ## Use this bit of code to test GenderedEgoStatsDistribution()/ensure the function gives one what they want.
  # GenderedEgoStatsDistribution(i = 1, fit.1.ans$f, fit.1.ans$sims, period = 1, varName = "support_net",
  #                              groupName = "Data1", vertex_IDs = individuals.TN.13$IndivID, gender = individuals.TN.13$Gender, reputation_nominations = general_reputation,
  #                              binary_friendship = friendshipTN.13, binary_kinship = kin.dyad, binary_same_gender = same.gender)

  
  maxInDegree <- max(sna::degree(socialsupportTN.17, gmode = "digraph", cmode = "indegree"))
  
  maxOutDegree <- max(sna::degree(socialsupportTN.17, gmode = "digraph", cmode = "outdegree"))
  
  maxGeodist <- max(sna::geodist(socialsupportTN.17, inf.replace = -99)$gdist) ## Replace infinite geodesics with -99 to easily retrieve max
  gc()
  
  
  
  temp.gof.indegree <- sienaGOF(sienaFitObject = villages.TN.sienaFits[[model]], 
                                auxiliaryFunction = IndegreeDistribution, 
                                varName = "support_net", 
                                cumulative = FALSE, 
                                levls = 0:maxInDegree, 
                                cluster = cl
  )
  print(temp.gof.indegree)
  gc()
  
  
  temp.gof.outdegree <- sienaGOF(sienaFitObject = villages.TN.sienaFits[[model]], 
                                 auxiliaryFunction = OutdegreeDistribution, 
                                 varName = "support_net", 
                                 cumulative = FALSE, 
                                 levls = 0:maxOutDegree, 
                                 cluster = cl
  )
  print(temp.gof.outdegree)
  gc()
  
  
  temp.gof.geodist <- sienaGOF(sienaFitObject = villages.TN.sienaFits[[model]], 
                               auxiliaryFunction = GeodesicDistribution,
                               varName = "support_net", 
                               cumulative = FALSE, 
                               levls = c(1:maxGeodist,Inf), 
                               cluster = cl
  )
  print(temp.gof.geodist)
  gc()
  
  
  temp.gof.triadcensus <- sienaGOF(sienaFitObject = villages.TN.sienaFits[[model]], 
                                   auxiliaryFunction = TriadCensus,
                                   varName = "support_net", 
                                   cluster = cl
  )
  print(temp.gof.triadcensus)
  gc()
  
  
  temp.gof.genderstats <- sienaGOF(sienaFitObject = villages.TN.sienaFits[[model]], 
                                   auxiliaryFunction = GenderedEgoStatsDistribution,
                                   vertex_IDs = individuals.TN.13$IndivID,
                                   gender = individuals.TN.13$Gender,
                                   reputation_nominations = general_reputation,
                                   binary_friendship = Matrix(friendshipTN.13, sparse = TRUE), ## Use sparse matricies to help ease computational burdern
                                   binary_kinship = Matrix(kin.dyad, sparse = TRUE),
                                   binary_same_gender = Matrix(same.gender, sparse = TRUE),
                                   varName = "support_net", cluster = cl)
  print(temp.gof.genderstats)
  gc()
  
  
  villages.TN.sienaGOFs.indegree[[model]] <- temp.gof.indegree
  villages.TN.sienaGOFs.outdegree[[model]] <- temp.gof.outdegree
  villages.TN.sienaGOFs.geodist[[model]] <- temp.gof.geodist
  villages.TN.sienaGOFs.triadcensus[[model]] <- temp.gof.triadcensus
  villages.TN.sienaGOFs.genderstats[[model]] <- temp.gof.genderstats
  
}
rm(model, GeodesicDistribution,
   temp.gof.indegree, temp.gof.outdegree, temp.gof.geodist, temp.gof.triadcensus, temp.gof.genderstats,
   maxInDegree, maxOutDegree, maxGeodist,
   general_reputation, consanguineal.kin, affinal.kin, kin.dyad
)

stopCluster(cl)
rm(cl)


 
## Plot the distributions to visually compare fit.
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.indegree[[1]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.indegree[[2]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.indegree[[3]], center = FALSE, scale = FALSE, violin = FALSE)

RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.outdegree[[1]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.outdegree[[2]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.outdegree[[3]], center = FALSE, scale = FALSE, violin = FALSE)

RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.geodist[[1]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.geodist[[2]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.geodist[[3]], center = FALSE, scale = FALSE, violin = FALSE)

RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.triadcensus[[1]], center = TRUE, scale = TRUE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.triadcensus[[2]], center = TRUE, scale = TRUE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.triadcensus[[3]], center = TRUE, scale = TRUE, violin = FALSE)

RSiena:::plot.sienaGOF( villages.TN.sienaGOFs.genderstats[[1]], center = TRUE, scale = FALSE, violin = FALSE) 
RSiena:::plot.sienaGOF( villages.TN.sienaGOFs.genderstats[[2]], center = TRUE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF( villages.TN.sienaGOFs.genderstats[[3]], center = TRUE, scale = FALSE, violin = FALSE)

 
 
################################# TABLE 1 (PART 1): MULTI-PARAMETER WALD TESTS #################################
## RUN: ?Multipar.RSiena
fit.2.ans.Walt.test <- Multipar.RSiena(ans = fit.2.ans, c(12, 19:34)) ## Positive integers specify the tested effects (as numbered in "print(ans)" )
print(fit.2.ans.Walt.test)

fit.3.ans.Walt.test <- Multipar.RSiena(ans = fit.3.ans, c(14, 16:19, 21:23)) #c(14, 16:18, 20:23)
print(fit.3.ans.Walt.test)



################################# TABLE 1 (PART 2): DISTRIBUTIONAL GOODNESS-OF-FIT #################################
## Extract information from the sienaGOF objects and combine in one data frame
siena.GOFs <- rbind.data.frame(
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.indegree,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) ),
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.outdegree,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) ),
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.geodist,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) ),
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.triadcensus,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) ),
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.genderstats,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) )
  , stringsAsFactors = FALSE)


## Round all results to the thousandths place for tabular presentation
siena.GOFs <- apply(siena.GOFs , MARGIN = 2, FUN = function(x){sprintf("%.3f", x)})


## Basic column names
rownames(siena.GOFs) <- c("In-degree Distribution", "Out-degree Distribution", "Distribution of Geodesic Distances", "Triad Census", "Ego-Network Statistics x Gender")


print(siena.GOFs) ## See how it all looks.


#### Use Microsoft Word's convert text to table option (tab delimited)
write.table(siena.GOFs[,c("Model_1.MHD", "Model_1.p_value",
                          "Model_2.MHD", "Model_2.p_value",
                          "Model_3.MHD", "Model_3.p_value")],
            file = "T1_ModelGOFs.txt", sep = "\t", quote = FALSE, row.names = TRUE) ## Main Models




 
########################### FIGURE 4: COEFFICIENT PLOT ###########################
## First, create a list of data frames that contain the results from each fitted SIENA model object
siena.coefs <- lapply(X = rev(villages.TN.sienaFits), ## Reverse the order of the fitted SIENA model objects in villages.TN.sienaFits to left join with the output from the fully-specified model
                      FUN = function(x){ cbind.data.frame(effect = c("Rate parameter", x$effects$effectName), ## Name of each effect
                                                          estimate = c(x$rate, x$theta), ## Rate parameters + parameter estimates
                                                          std.error = c(x$vrate, x$se),  ## Standard errors of the Rate parameters + standard error of each parameter estimate
                                                          statistic = c(x$rate, x$theta)/c(x$vrate, x$se), ## Test statistic
                                                          p.value = 2*pnorm( abs( c(x$rate, x$theta)/c(x$vrate, x$se) ), lower.tail = FALSE), ## Two-sided p-value associated with each parameter estimate
                                                          conf.high = c(x$rate, x$theta) + (1.96 * c(x$vrate, x$se)),
                                                          conf.low = c(x$rate, x$theta) - (1.96 * c(x$vrate, x$se))
                                                          ,
                                                          stringsAsFactors = FALSE)
                      }
)



## Second, add to each data frame the pretty name for each effect in each model.
## These names should be given in the printed order of the effects in each SIENA model object.
## Note that lapply returns the data frames in reverse order.
siena.coefs[[1]] <- cbind.data.frame(model = "Model 3",
                                     term = c("Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
                                              "Out-degree",
                                              "Reciprocity",
                                              "Transitive Triplets",
                                              "Transitive Reciprocated Triplets",
                                              "Three Cycles",
                                              "In-degree Popularity",
                                              "In-degree Activity",
                                              "Out-degree Activity",
                                              "Kinship",
                                              "Friendship",
                                              "Geographic Distance",
                                              "Woman (Ego)",
                                              "Same Gender",
                                              "Age (Ego)",
                                              "Absolute Difference: Age",
                                              "Age^2 (Ego)",
                                              "Years of Education (Ego)",
                                              "Household Wealth (Ego)",
                                              "Reservation Status: Scheduled Caste (Ego)",
                                              "Same Caste",
                                              "Immigrant Status: Non-Natal Village Resident (Ego)",
                                              "Partnership Status: Not Married (Ego)",
                                              "General Reputation (Ego)",
                                              "Absolute Difference: General Reputation",
                                              "Same Household",
                                              "Absolute Difference: Village",
                                              "Woman (Ego) x Reciprocity",
                                              "Woman (Ego) x Kinship",
                                              "Woman (Ego) x Friendship",
                                              "Woman (Ego) x Geographic Distance",
                                              "Woman (Ego) x Same Gender",
                                              "Woman (Ego) x Absolute Difference: Age",
                                              "Woman (Ego) x Same Caste",
                                              "Woman (Ego) x Absolute Difference: General Reputation",
                                              "Woman (Ego) x Same Household",
                                              "Woman (Ego) x Absolute Difference: Village",
                                              "Woman (Ego) x In-degree Popularity",
                                              "Woman (Ego) x In-degree Activity",
                                              "Woman (Ego) x Out-degree Activity",
                                              "Woman (Ego) x Transitive Triplets",
                                              "Woman (Ego) x Transitive Reciprocated Triplets",
                                              "Woman (Ego) x Three Cycles"
                                     ),
                                     siena.coefs[[1]], stringsAsFactors = FALSE, row.names = NULL) 



siena.coefs[[2]] <- cbind.data.frame(model = "Model 2",
                                     term = c("Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
                                              "Out-degree",
                                              "Reciprocity",
                                              "Transitive Triplets",
                                              "Transitive Reciprocated Triplets",
                                              "Three Cycles",
                                              "In-degree Popularity",
                                              "In-degree Activity",
                                              "Out-degree Activity",
                                              "Kinship",
                                              "Friendship",
                                              "Geographic Distance",
                                              "Woman (Ego)",
                                              "Same Gender",
                                              # "Age (Ego)",
                                              "Absolute Difference: Age",
                                              # "Age^2 (Ego)",
                                              # "Years of Education (Ego)",
                                              # "Household Wealth (Ego)",
                                              # "Reservation Status: Scheduled Caste (Ego)",
                                              "Same Caste",
                                              # "Immigrant Status: Non-Natal Village Resident (Ego)",
                                              # "Partnership Status: Not Married (Ego)",
                                              # "General Reputation (Ego)",
                                              "Absolute Difference: General Reputation",
                                              "Same Household",
                                              "Absolute Difference: Village",
                                              "Woman (Ego) x Reciprocity",
                                              "Woman (Ego) x Kinship",
                                              "Woman (Ego) x Friendship",
                                              "Woman (Ego) x Geographic Distance",
                                              "Woman (Ego) x Same Gender",
                                              "Woman (Ego) x Absolute Difference: Age",
                                              "Woman (Ego) x Same Caste",
                                              "Woman (Ego) x Absolute Difference: General Reputation",
                                              "Woman (Ego) x Same Household",
                                              "Woman (Ego) x Absolute Difference: Village",
                                              "Woman (Ego) x In-degree Popularity",
                                              "Woman (Ego) x In-degree Activity",
                                              "Woman (Ego) x Out-degree Activity",
                                              "Woman (Ego) x Transitive Triplets",
                                              "Woman (Ego) x Transitive Reciprocated Triplets",
                                              "Woman (Ego) x Three Cycles"
                                     ),
                                     siena.coefs[[2]], stringsAsFactors = FALSE, row.names = NULL) 


siena.coefs[[3]] <- cbind.data.frame(model = "Model 1",
                                     term = c("Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
                                              "Out-degree",
                                              "Reciprocity",
                                              "Transitive Triplets",
                                              "Transitive Reciprocated Triplets",
                                              "Three Cycles",
                                              "In-degree Popularity",
                                              "In-degree Activity",
                                              "Out-degree Activity",
                                              "Kinship",
                                              "Friendship",
                                              "Geographic Distance",
                                              # "Woman (Ego)",
                                              "Same Gender",
                                              # "Age (Ego)",
                                              "Absolute Difference: Age",
                                              # "Age^2 (Ego)",
                                              # "Years of Education (Ego)",
                                              # "Household Wealth (Ego)",
                                              # "Reservation Status: Scheduled Caste (Ego)",
                                              "Same Caste",
                                              # "Immigrant Status: Non-Natal Village Resident (Ego)",
                                              # "Partnership Status: Not Married (Ego)",
                                              # "General Reputation (Ego)",
                                              "Absolute Difference: General Reputation",
                                              "Same Household",
                                              "Absolute Difference: Village" #,
                                              # "Woman (Ego) x Reciprocity",
                                              # "Woman (Ego) x Kinship",
                                              # "Woman (Ego) x Friendship",
                                              # "Woman (Ego) x Geographic Distance",
                                              # "Woman (Ego) x Same Gender",
                                              # "Woman (Ego) x Absolute Difference: Age",
                                              # "Woman (Ego) x Same Caste",
                                              # "Woman (Ego) x Absolute Difference: General Reputation",
                                              # "Woman (Ego) x Same Household",
                                              # "Woman (Ego) x Absolute Difference: Village",
                                              # "Woman (Ego) x In-degree Popularity",
                                              # "Woman (Ego) x In-degree Activity",
                                              # "Woman (Ego) x Out-degree Activity",
                                              # "Woman (Ego) x Transitive Triplets",
                                              # "Woman (Ego) x Transitive Reciprocated Triplets",
                                              # "Woman (Ego) x Three Cycles"
                                     ),
                                     siena.coefs[[3]], stringsAsFactors = FALSE, row.names = NULL) 


siena.coefs.long <- do.call(rbind.data.frame, siena.coefs)
rownames(siena.coefs.long) <- NULL



## Third, define a vector of effect names to use to arrange the results in the preferred order for presentation in the paper
reorder.all.pretty.effects.of.interest <- c(
  "Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
  
  "Out-degree",
  "Reciprocity",
  "Kinship",
  "Friendship",
  "Transitive Triplets",
  "Three Cycles",
  "Absolute Difference: General Reputation",
  
  
  "Transitive Reciprocated Triplets",
  
  "In-degree Popularity",
  "In-degree Activity",
  "Out-degree Activity",
  
  "Absolute Difference: Age",
  "Same Gender",
  "Same Caste",
  
  "Absolute Difference: Village",
  "Same Household",
  "Geographic Distance",
  
  
  "Woman (Ego)",
  "Woman (Ego) x Reciprocity",
  "Woman (Ego) x Kinship",
  "Woman (Ego) x Friendship",
  "Woman (Ego) x Transitive Triplets",
  "Woman (Ego) x Three Cycles",
  "Woman (Ego) x Absolute Difference: General Reputation",
  
  
  "Woman (Ego) x Transitive Reciprocated Triplets",
  
  "Woman (Ego) x In-degree Popularity",
  "Woman (Ego) x In-degree Activity",
  "Woman (Ego) x Out-degree Activity",
  
  "Woman (Ego) x Absolute Difference: Age",
  "Woman (Ego) x Same Gender",
  "Woman (Ego) x Same Caste",
  
  "Woman (Ego) x Absolute Difference: Village",
  "Woman (Ego) x Same Household",
  "Woman (Ego) x Geographic Distance",
  
  
  "Age (Ego)",
  "Age^2 (Ego)",
  "Years of Education (Ego)",
  "Household Wealth (Ego)",
  "Reservation Status: Scheduled Caste (Ego)",
  "Immigrant Status: Non-Natal Village Resident (Ego)",
  "Partnership Status: Not Married (Ego)",
  "General Reputation (Ego)"
)



## Fourth, categorise each effect in the models.
all.pretty.effects.of.interest.class <- list(
  "Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)" = "Rate Effects",
  
  "Out-degree" = "H1",
  "Reciprocity" = "H2",
  "Kinship" = "H3",
  "Friendship" = "H4",
  "Transitive Triplets" = "H5a",
  "Three Cycles" = "H5b",
  "Absolute Difference: General Reputation" = "H6",
  "Same Gender" = "H7",
  
  
  "Transitive Reciprocated Triplets" = "Controls (Constitutive)",
  
  "In-degree Popularity" = "Controls (Constitutive)",
  "In-degree Activity" = "Controls (Constitutive)",
  "Out-degree Activity" = "Controls (Constitutive)",
  
  "Absolute Difference: Age" = "Controls (Constitutive)",
  "Same Caste" = "Controls (Constitutive)",
  
  "Absolute Difference: Village" = "Controls (Constitutive)",
  "Same Household" = "Controls (Constitutive)",
  "Geographic Distance" = "Controls (Constitutive)",
  
  
  
  "Woman (Ego)" = "H1",
  "Woman (Ego) x Reciprocity" = "H2",
  "Woman (Ego) x Kinship" = "H3",
  "Woman (Ego) x Friendship" = "H4",
  "Woman (Ego) x Transitive Triplets" = "H5a",
  "Woman (Ego) x Three Cycles" = "H5b",
  "Woman (Ego) x Absolute Difference: General Reputation" = "H6",
  "Woman (Ego) x Same Gender" = "H7",
  
  
  "Woman (Ego) x Transitive Reciprocated Triplets" = "Controls (Interactions)",
  
  "Woman (Ego) x In-degree Popularity" = "Controls (Interactions)",
  "Woman (Ego) x In-degree Activity" = "Controls (Interactions)",
  "Woman (Ego) x Out-degree Activity" = "Controls (Interactions)",
  
  "Woman (Ego) x Absolute Difference: Age" = "Controls (Interactions)",
  "Woman (Ego) x Same Caste" = "Controls (Interactions)",
  
  "Woman (Ego) x Absolute Difference: Village" = "Controls (Interactions)",
  "Woman (Ego) x Same Household" = "Controls (Interactions)",
  "Woman (Ego) x Geographic Distance" = "Controls (Interactions)",
  
  
  "Age (Ego)" = "Socio-Demographics",
  "Age^2 (Ego)" = "Socio-Demographics",
  "Years of Education (Ego)" = "Socio-Demographics",
  "Household Wealth (Ego)" = "Socio-Demographics",
  "Reservation Status: Scheduled Caste (Ego)" = "Socio-Demographics",
  "Immigrant Status: Non-Natal Village Resident (Ego)" = "Socio-Demographics",
  "Partnership Status: Not Married (Ego)" = "Socio-Demographics",
  "General Reputation (Ego)" = "Socio-Demographics"
)



## Fifth, add categories of each covariate to the data frame of results 
siena.coefs.long$term.class <- unlist(all.pretty.effects.of.interest.class[siena.coefs.long$term]) ## Query the list using the name of each effect/term to retrieve its class
siena.coefs.long$term.class <- factor(siena.coefs.long$term.class, levels = c("H1", "H2", "H3", "H4", "H5a", "H5b", "H6", "H7", "Controls (Constitutive)", "Controls (Interactions)", "Socio-Demographics"))



## Sixth, reorder everything in the preferred order
siena.coefs.long$term <- factor(siena.coefs.long$term, levels = rev(reorder.all.pretty.effects.of.interest) )
siena.coefs.long$model <- factor(siena.coefs.long$model, levels = c("Model 1", "Model 2", "Model 3"), labels = c( "Baseline", "Fully Interacted", "Social Constraints"))



## Seventh, build the coefficient plot!
## Much of the code to create the custom coefficient plot is adapted from the excellent package jtools 
## which sadly is not designed to work with SAOM model objects
# https://github.com/jacob-long/jtools/blob/HEAD/R/plot_coefs.R
# http://www.sthda.com/english/wiki/ggplot2-point-shapes

dh <- 0.45 ## How big should the horizontal dodge/spacing between the confidence intervals be?
point.size <- 3 ## How big should the points/shapes for the parameter estimates be?
exp = FALSE ## Exponentiate the vertical line indicating "no association"?

siena.coefs.long.no.rate <- subset(siena.coefs.long, siena.coefs.long$effect != "Rate parameter")
siena.coefs.long.only.rate <- subset(siena.coefs.long, siena.coefs.long$effect == "Rate parameter")

coefficient.plot <- (ggplot(data = siena.coefs.long.no.rate, 
                            aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high)) 
                     + geom_pointrangeh( aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high, colour = model, shape = model),
                                         position = position_dodgev(height = dh),
                                         fill = "white", 
                                         fatten = point.size, size = 0.7,
                                         show.legend = length(siena.coefs) > 1) # omit legend if just a single model
                     + geom_vline(xintercept = 1 - !exp, linetype = 2, size = .25) 
                     + labs(title = NULL,
                            y = NULL,
                            x = expression(Log~Odds~Ratio~of~italic(x)[ij]~" = "~1~" = "~Alter~is~Source~of~Social~Support~"for"~Ego~" (i = Ego, j = Alter)"~plain("+ 95% Confidence Interval")), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
                            # tag = "a",
                            ## This sections uses the paste0 function to combine various bits of information about the rate parameter to create the caption at the bottom of Figure 1.
                            caption = paste0("\n", 
                                             "Rate (Avg. Tie Changes) Wave 1 (2013) → Wave 2 (2017): ",
                                             "Model 1 (",
                                             "Est. = ",  sprintf("%.3f", siena.coefs.long.only.rate$estimate[ which(siena.coefs.long.only.rate$model == "Baseline") ]),
                                             "; S.E. = ",  sprintf("%.3f", siena.coefs.long.only.rate$std.error[ which(siena.coefs.long.only.rate$model == "Baseline") ]),
                                             "); ",
                                             
                                             "Model 2 (",
                                             "Est. = ",  sprintf("%.3f", siena.coefs.long.only.rate$estimate[ which(siena.coefs.long.only.rate$model == "Fully Interacted") ]),
                                             "; S.E. = ",  sprintf("%.3f", siena.coefs.long.only.rate$std.error[ which(siena.coefs.long.only.rate$model == "Fully Interacted") ]),
                                             "); ",
                                             
                                             "Model 3 (",
                                             "Est. = ",  sprintf("%.3f", siena.coefs.long.only.rate$estimate[ which(siena.coefs.long.only.rate$model == "Social Constraints") ]),
                                             "; S.E. = ",  sprintf("%.3f", siena.coefs.long.only.rate$std.error[ which(siena.coefs.long.only.rate$model == "Social Constraints") ]),
                                             ")",
                                             "\n", 
                                             "\n", 
                                             "SAOMs fit using Z-scores for age, wealth (Natural Log Indian Rupees), general-reputation nominations (Square Root), and geographic distance (Natural Log Metres). 
                                             Age [Mean = 44.01; S.D. = 14.70]; Log(Wealth) [Mean = 12.62; S.D. = 0.94]; Sqrt(General Reputation) [Mean = 2.28; S.D. = 1.97]; Log(Geographic Distance + 1) [Mean = 5.69; S.D. = 1.36].
                                             Log/square-root transformations taken prior to standardisation. S.D. = Standard Deviation."
                            )
                     ) 
                     + theme_nice(style = "black") 
                     + theme(axis.line = element_line(color = "black"),
                             legend.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend bg
                             legend.box.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend panel bg
                             legend.key = element_rect(fill = "transparent", colour = "transparent", color = NA),
                             plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot,
                             panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
                             panel.grid.major.y = element_blank(),
                             panel.grid.minor.y = element_blank(),
                             panel.grid.major.x = element_line(size = 0.15, linetype = 1, colour = "#E2E2E2"),
                             panel.grid.minor.x = element_blank(),
                             panel.border = element_blank(),
                             panel.spacing = unit(2, "lines"),
                             axis.text = element_text(size = 11, colour = "#767676"),
                             axis.text.y = element_text(hjust = 1),
                             axis.title = element_text(size = 11, colour = "#767676"),
                             plot.title = element_text(size = 11, colour = "#767676"),
                             plot.caption = element_text(size = 11, colour = "#767676"),
                             legend.position = "top",
                             legend.title = element_text(size = 10, colour = "#767676"),
                             legend.text = element_text(size = 10, colour = "#767676"),
                             plot.tag = element_text(size = 13, vjust = -4, face = "bold", colour = "#767676")
                     ) 
                     + scale_x_continuous(limits = c(-3, 3), breaks = c(-2, -1, 0, 1, 2) ) 
                     + scale_colour_manual(values = c("#33BBEE", "#EE7733", "#CC3311"), 
                                           limits = rev(levels(siena.coefs.long.no.rate$model)), 
                                           name = "Stochastic Actor-Oriented Model:") 
                     + scale_shape_manual(values = c(24, 21, 22), 
                                          limits = rev(levels(siena.coefs.long.no.rate$model)),
                                          name = "Stochastic Actor-Oriented Model:") # "#EE7733", "#009988", "#CC3311", "#33BBEE" 
                     
                     + facet_grid(term.class ~ ., scales = "free_y", space = "free_y", switch = "x")
)


# plot(coefficient.plot)
ggsave(plot = coefficient.plot,
       filename = "F4_Gender_Cooperative_Networks.svg", device = "svg", dpi = 900,
       scale = 3.2, width = 4.35, height = 5, units = "in", bg = "transparent")





################################# FIGURE 5: LINEAR COMBINATIONS OF PARAMETER ESTIMATES (MODEL 3) #################################
## This procedure comes from Chapter 8, p. 95-87, of Ripley, R. M., Snijders, T. A. B., Boda, Z., Vörös, A., & Preciado, P. (2022). Manual for RSiena (v. 1.3.6) [R]. http://www.stats.ox.ac.uk/~snijders/siena/RSiena_Manual.pdf

## Define custom functions that one can pass a fitted SAOM to to generate the linear combinations for each hypothesis.
H1.linear.combination <- function(gender = NULL, 
                                  fitted.SAOM = NULL,
                                  outdegree_i = NULL,
                                  indegree_i = NULL,
                                  indegree_j = NULL,
                                  ...){
  
  ## H1: Out-degree/Trait-based (Gender) Activity
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matrix from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta
  
  
  ## Third, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fourth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.H1 <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Fifth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.H1[which(effectName %in% c("gender ego"))] <- female
  
  mat.H1[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.H1[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.H1[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.H1[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.H1[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.H1[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Sixth, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.H1*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.H1 %*% covtheta %*% t(mat.H1)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.H1*theta)/sqrt(mat.H1 %*% covtheta %*% t(mat.H1)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Seventh, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  
  ## Names are transferred automatically
  linear.combination.test
  
}



H2.linear.combination <- function(gender = NULL, 
                                  fitted.SAOM = NULL,
                                  outdegree_i = NULL,
                                  indegree_i = NULL, 
                                  indegree_j = NULL,
                                  ...){
  
  ## H2: Reciprocity
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matrix from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta
  
  
  ## Third, because of the incoming tie being reciprocated by x_ij — i.e., the tie x_ji — i's in-degree is increased by one under reciprocity!
  ## As we fix the in-degree of i to 6 (i.e., the 2013 median) and only consider one new incoming tie, we need not make an adjustment for the maximum observed in-degree for 2013 (64).
  indegree_i <- indegree_i + 1
  
  
  ## Fourth, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fifth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.H2 <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Sixth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.H2[which(effectName %in% c("outdegree (density)"))] <- 1 
  
  mat.H2[which(effectName %in% c("gender ego"))] <- female 
  
  mat.H2[which(effectName %in% c("reciprocity"))] <- 1 
  mat.H2[which(effectName %in% c("gender ego x reciprocity"))] <- female * 1 
  
  mat.H2[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.H2[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.H2[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.H2[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.H2[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.H2[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Seventh, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.H2*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.H2 %*% covtheta %*% t(mat.H2)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.H2*theta)/sqrt(mat.H2 %*% covtheta %*% t(mat.H2)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Eight, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  
  ## Names are transferred automatically
  linear.combination.test
  
}



H3.linear.combination <- function(gender = NULL, 
                                  fitted.SAOM = NULL,
                                  outdegree_i = NULL,
                                  indegree_i = NULL,
                                  indegree_j = NULL,
                                  ...){
  
  ## H3: Kinship
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matrix from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta
  
  
  ## Third, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fourth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.H3 <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Fifth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.H3[which(effectName %in% c("outdegree (density)"))] <- 1 
  
  mat.H3[which(effectName %in% c("gender ego"))] <- female 
  
  mat.H3[which(effectName %in% c("kin.dyad"))] <- 1 
  mat.H3[which(effectName %in% c("gender ego x kin.dyad"))] <- female * 1 
  
  mat.H3[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.H3[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.H3[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.H3[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.H3[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.H3[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Sixth, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.H3*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.H3 %*% covtheta %*% t(mat.H3)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.H3*theta)/sqrt(mat.H3 %*% covtheta %*% t(mat.H3)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Seventh, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  
  ## Names are transferred automatically
  linear.combination.test
  
}



H4.linear.combination <- function(gender = NULL, 
                                  fitted.SAOM = NULL,
                                  outdegree_i = NULL,
                                  indegree_i = NULL,
                                  indegree_j = NULL,
                                  ...){
  
  ## H4: Friendship
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matrix from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta
  
  
  ## Third, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fourth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.H4 <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Fifth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.H4[which(effectName %in% c("outdegree (density)"))] <- 1 
  
  mat.H4[which(effectName %in% c("gender ego"))] <- female 
  
  mat.H4[which(effectName %in% c("friendship.dyad"))] <- 1 
  mat.H4[which(effectName %in% c("gender ego x friendship.dyad"))] <- female * 1 
  
  mat.H4[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.H4[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.H4[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.H4[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.H4[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.H4[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Sixth, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.H4*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.H4 %*% covtheta %*% t(mat.H4)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.H4*theta)/sqrt(mat.H4 %*% covtheta %*% t(mat.H4)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Seventh, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  ## Names are transferred automatically
  linear.combination.test
  
}



H5a.linear.combination <- function(gender = NULL, 
                                   fitted.SAOM = NULL,
                                   outdegree_i = NULL,
                                   indegree_i = NULL, 
                                   indegree_j = NULL,
                                   transitive_triads = 12,
                                   ...){
  
  ## H5a: Transitive Triplets
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matrix from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta

  
  ## Third, modify the out-degree of i and the in-degree of j to reflect the number of transitive triads we want the linear combination for.
  ## To clarify, we focus on the tie x_ij closing one ore more transitive triads h of the general form [i → h → j ← i].
  ## Given h two-paths running from actor i to actor j via h distinct third parties, we add h to the out-degree of i and we add h to the in-degree of j.
  ## However, were one to just add h to the out-degree of i, it would result in out-degrees outside of the observed range in our data (i.e., 0 to 32 for 2013)!
  ## Accordingly, we first use the if() statement below to ensure that actor i's total out-degree is always less than or equal to 32 (i.e., the observed max for 2013).
  ## Specifically, we set h to 12 (i.e., the 2013 median number of two-paths closed by ego).
  ## And as 32 = 12 + 20, out-degrees over 20 must capped to keep ego's total out-degree within the observed range.
  ## A similar adjustment is made to the in-degree of j. The observed range of in-degrees is 0 to 64 in 2013, where 64 = 12 + 52
  if(outdegree_i > (32 - transitive_triads) ){
    outdegree_i <- (32 - transitive_triads)
  }

  if(indegree_j > (64 - transitive_triads)){
    indegree_j <- (64 - transitive_triads)
  }

  outdegree_i <- outdegree_i + transitive_triads
  indegree_j <- indegree_j + transitive_triads
  
  
  ## Fourth, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fifth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.H5a <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Sixth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.H5a[which(effectName %in% c("outdegree (density)"))] <- 1 
  
  mat.H5a[which(effectName %in% c("gender ego"))] <- female 
  
  mat.H5a[which(effectName %in% c("transitive triplets"))] <- transitive_triads
  mat.H5a[which(effectName %in% c("gender ego x transitive triplets"))] <- female * transitive_triads 
  
  mat.H5a[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.H5a[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.H5a[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.H5a[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.H5a[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.H5a[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Seventh, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.H5a*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.H5a %*% covtheta %*% t(mat.H5a)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.H5a*theta)/sqrt(mat.H5a %*% covtheta %*% t(mat.H5a)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Seventh, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  ## Names are transferred automatically
  linear.combination.test
  
}



H5b.linear.combination <- function(gender = NULL, 
                                   fitted.SAOM = NULL,
                                   outdegree_i = NULL,
                                   indegree_i = NULL, 
                                   indegree_j = NULL,
                                   cyclic_triads = 5,
                                   ...){
  
  ## H5b: Three Cycles
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matrix from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta
  
  
  ## Third, modify the in-degree of i to reflect the number of cyclic triads we want the linear combination for.
  ## To clarify, we focus on the tie x_ij closing one ore more cyclic triads h of the general form [i → j → h → i].
  ## Given h two-paths running from actor j to actor i via h distinct third parties, we add we add h to the in-degree of i.
  ## As we fix the in-degree of i to 6 (i.e., the 2013 median) and only consider h = 5 (i.e., the 2013 median number of three cycles in one-degree ego-nets), 
  ## we need not make an adjustment for the maximum observed in-degree for 2013 (64).
  ## As the out-degree of j does not feature in any of the effects in our linear combination, it is ignored — although the two-paths would add h to the out-degree of j. 
  indegree_i <- indegree_i + cyclic_triads
 
  
  ## Fourth, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fifth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.H5b <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Sixth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.H5b[which(effectName %in% c("outdegree (density)"))] <- 1 
  
  mat.H5b[which(effectName %in% c("gender ego"))] <- female 
  
  mat.H5b[which(effectName %in% c("3-cycles"))] <- cyclic_triads
  mat.H5b[which(effectName %in% c("gender ego x 3-cycles"))] <- female * cyclic_triads 
  
  mat.H5b[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.H5b[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.H5b[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.H5b[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.H5b[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.H5b[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Seventh, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.H5b*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.H5b %*% covtheta %*% t(mat.H5b)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.H5b*theta)/sqrt(mat.H5b %*% covtheta %*% t(mat.H5b)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Eighth, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  ## Names are transferred automatically
  linear.combination.test
  
}



H6.linear.combination <- function(gender = NULL, 
                                  fitted.SAOM = NULL,
                                  outdegree_i = NULL,
                                  indegree_i = NULL, 
                                  indegree_j = NULL,
                                  abs_diff_general_rep = 1,
                                  ...){
  
  ## H6: Absolute Difference: General Reputation
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matrix from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta
  
  
  ## Third, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fourth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.H6 <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Fifth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.H6[which(effectName %in% c("outdegree (density)"))] <- 1 
  
  mat.H6[which(effectName %in% c("gender ego"))] <- female 
  
  mat.H6[which(effectName %in% c("general_reputation abs. difference"))] <- abs_diff_general_rep
  mat.H6[which(effectName %in% c("gender ego x general_reputation abs. difference"))] <- female * abs_diff_general_rep 
  
  mat.H6[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.H6[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.H6[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.H6[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.H6[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.H6[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Sixth, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.H6*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.H6 %*% covtheta %*% t(mat.H6)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.H6*theta)/sqrt(mat.H6 %*% covtheta %*% t(mat.H6)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Seventh, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  ## Names are transferred automatically
  linear.combination.test
  
}



H7.linear.combination <- function(gender = NULL,
                                  fitted.SAOM = NULL,
                                  outdegree_i = NULL,
                                  indegree_i = NULL, 
                                  indegree_j = NULL,
                                                ...){
  
  ## Bonus: Same Gender
  
  ## First, should the linear combination be for women (female == 1) or men (male == 0)
  female <- as.numeric(gender == "female")
  
  
  ## Second, extract the character vector of effects names, the numeric vector of parameter estimates, and the variance-covariance matric from the fitted SAOM object
  effectName <- fitted.SAOM$effects$effectName
  theta <- fitted.SAOM$theta
  covtheta <- fitted.SAOM$covtheta
  
  
  ## Third, define the values of i's out-degree and j's in-degree given the creation/maintenance of the tie x_ij for a ministep
  outdegree_i.toggle <- outdegree_i + 1
  indegree_j.toggle <- indegree_j + 1
  
  
  ## Fourth, define the hypothesis-specific one-row matrix to use to construct the linear combination and its standard error using fitted.SAOM
  mat.homophily.gender <- matrix(data = 0, nrow = 1, ncol = fitted.SAOM$pp) # fitted.SAOM$pp == the number of estimated parameters (non-rate function only) in the fitted SAOM
  
  
  ## Fifth, take the empty one-row matrix (i.e., mat.H1) and include the change statistic/covariate-value for each effect involved in the linear combination.
  ## In so doing, identify the effects for the linear combination using the the character vector of actual effect names which comes from the fitted SAOM object.
  ## These change statistics are induced by the creation/maintenance of a single tie x_ij .
  ## The calculation of the change-statistic/covariate value uses the formulae for the SAOM network effects in Ripley et al. (2022).
  ## It is assumed that, when excluding x_ij, actor i has an out-degree = outdegree_i and an in-degree = indegree_i and that actor j has an out-degree = 0 and an in-degree = indegree_j.
  ## As the out-degree of j does not feature in any of the effects in our models, it is ignored in the linear combination.
  ## "outdegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's out-degree.
  ## "indegree - activity (sqrt)" = i's out-degree multiplied by the square-root of i's in-degree.
  ## "indegree - popularity (sqrt)" = the sum of the square-root of the in-degree of each network member to whom i sends a tie.
  ## This all must be kept in mind when calculating the total effects. 
  
  mat.homophily.gender[which(effectName %in% c("outdegree (density)"))] <- 1 
  
  mat.homophily.gender[which(effectName %in% c("gender ego"))] <- female 
  
  mat.homophily.gender[which(effectName %in% c("same gender"))] <- 1
  mat.homophily.gender[which(effectName %in% c("gender ego x same gender"))] <- female * 1 
  
  mat.homophily.gender[which(effectName %in% c("outdegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) 
  mat.homophily.gender[which(effectName %in% c("indegree - activity (sqrt)"))] <- outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) 
  mat.homophily.gender[which(effectName %in% c("indegree - popularity (sqrt)"))] <- sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) ## This assumes that all those to whom i is tied have the same in-degree
  
  mat.homophily.gender[which(effectName %in% c("gender ego x outdegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(outdegree_i.toggle) - outdegree_i*sqrt(outdegree_i) ) 
  mat.homophily.gender[which(effectName %in% c("gender ego x indegree - activity (sqrt)"))] <- female * ( outdegree_i.toggle*sqrt(indegree_i) - outdegree_i*sqrt(indegree_i) ) 
  mat.homophily.gender[which(effectName %in% c("gender ego x indegree - popularity (sqrt)"))] <- female * ( sum(sqrt(indegree_j.toggle), rep(indegree_j, outdegree_i)) - sum(rep(indegree_j, outdegree_i)) )
  
  
  ## Sixth, construct the linear combination of interest and its standard error following the steps in Chapter 8, p. 95-87, of Ripley et al. (2022).
  ## The variances of the SAOM parameter estimates are on the prime diagonal of the covariance matrix included within a fitted SAOM object. 
  ## Additionally, the standard errors are the square roots of these diagonal elements.
  ## Note that dividing the linear combination by the standard error yields the Z-test-statistic from Wald.RSiena()
  
  lincomb <- sum(mat.homophily.gender*theta) ## Actual linear combination obtained by summing the elements of the one-row matrix product
  lincomb_se <- sqrt(mat.homophily.gender %*% covtheta %*% t(mat.homophily.gender)) ## Standard error for the linear combination
  lincomb_test_stat <- sum(mat.homophily.gender*theta)/sqrt(mat.homophily.gender %*% covtheta %*% t(mat.homophily.gender)) ## Test statistic for linear combination
  lincomb_p_value <- 2*pnorm( abs( lincomb_test_stat ), lower.tail = FALSE) ## Two-sided p-value associated with the linear combination ;   ## Tom's Wald.RSiena() reports a p-value for the chi-squared test stat with one degree of freedom...
  lincomb_conf_high <- lincomb + (1.96 * lincomb_se)
  lincomb_conf_low <- lincomb - (1.96 * lincomb_se)
  
  
  ## Seventh, bind all of the results together in a named vector
  linear.combination.test <- c(
    "lincomb" = lincomb,
    "lincomb_se" = lincomb_se,
    "lincomb_test_stat" = lincomb_test_stat,
    "lincomb_p_value" = lincomb_p_value,
    "lincomb_conf_high" = lincomb_conf_high,
    "lincomb_conf_low" = lincomb_conf_low
  )
  
  ## Names are transferred automatically
  linear.combination.test
  
}


## Note that the custom functions return the estimated total effect (i.e., "lincomb") plus the upper and lower bounds of their 95% confidence intervals.
# https://stackoverflow.com/questions/56366535/using-sapply-inside-sapply
# Below, we pass lapply() a vector of numbers (0:32) representing the range of the observed out-degrees of the network members in 2013.
# The custom function passed to lapply() is a call to sapply() which constructs the linear combination of effects given i's out-degree (0:32, handled by lapply) and given j's indegree (0:64, handled by sapply()) whilst using a hypothesis-specific custom function (e.g., "H1.linear.combination")
# For each hypotheses, this lapply(sapply()) call returns a list of 33 matrices (i.e., one for each value in the range of observed out-degrees). 
# Each of the matrices has 65 rows (i.e., one for each value in the range of observed in-degrees). 
# Each of the 65 rows in each of the 33 matricies is for a linear combination given i's out-degree and j's in-degree. These rows are the outputs from a hypothesis-specific custom function (e.g., "H1.linear.combination").
# Note that, seq_along(H1) simply returns a vector of integer ranging from 1 to the number of elements in H1. Accordingly, outdegree_i = x-1!
# Furthermore, all linear combinations ignore j's out-degree as none of the effects in our models incorporate this information.

 
## How many people should seek support from i for the linear combinations? That is, what is i's in-degree/level of popularity?
incomming_ties <- 6

 
## H1: Out-degree/Trait-based (Gender) Activity
H1.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H1.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H1.female <- lapply(X = seq_along(H1.female), FUN = function(x){ cbind.data.frame(hypothesis = "H1", gender = "female", effect = "H1: Woman (Ego) x Activity", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H1.female[[x]]) }) # https://stackoverflow.com/a/56094806
H1.female <- do.call(rbind.data.frame, H1.female)

H1.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H1.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H1.male <- lapply(X = seq_along(H1.male), FUN = function(x){ cbind.data.frame(hypothesis = "H1", gender = "male", effect = "H1: Woman (Ego) x Activity", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H1.male[[x]]) }) # https://stackoverflow.com/a/56094806
H1.male <- do.call(rbind.data.frame, H1.male)

H1 <- rbind.data.frame(H1.female, H1.male)
rm(H1.female, H1.male)


## H2: Reciprocity
H2.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H2.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H2.female <- lapply(X = seq_along(H2.female), FUN = function(x){ cbind.data.frame(hypothesis = "H2", gender = "female", effect = "H2: Reciprocity", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H2.female[[x]]) }) # https://stackoverflow.com/a/56094806
H2.female <- do.call(rbind.data.frame, H2.female)

H2.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H2.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H2.male <- lapply(X = seq_along(H2.male), FUN = function(x){ cbind.data.frame(hypothesis = "H2", gender = "male", effect = "H2: Reciprocity", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H2.male[[x]]) }) # https://stackoverflow.com/a/56094806
H2.male <- do.call(rbind.data.frame, H2.male)

H2 <- rbind.data.frame(H2.female, H2.male)
rm(H2.female, H2.male)


## H3: Kinship
H3.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H3.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H3.female <- lapply(X = seq_along(H3.female), FUN = function(x){ cbind.data.frame(hypothesis = "H3", gender = "female", effect = "H3: Kinship", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H3.female[[x]]) }) # https://stackoverflow.com/a/56094806
H3.female <- do.call(rbind.data.frame, H3.female)

H3.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H3.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H3.male <- lapply(X = seq_along(H3.male), FUN = function(x){ cbind.data.frame(hypothesis = "H3", gender = "male", effect = "H3: Kinship", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H3.male[[x]]) }) # https://stackoverflow.com/a/56094806
H3.male <- do.call(rbind.data.frame, H3.male)

H3 <- rbind.data.frame(H3.female, H3.male)
rm(H3.female, H3.male)


## H4: Friendshp
H4.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H4.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H4.female <- lapply(X = seq_along(H4.female), FUN = function(x){ cbind.data.frame(hypothesis = "H4", gender = "female", effect = "H4: Friendshp", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H4.female[[x]]) }) # https://stackoverflow.com/a/56094806
H4.female <- do.call(rbind.data.frame, H4.female)

H4.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H4.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H4.male <- lapply(X = seq_along(H4.male), FUN = function(x){ cbind.data.frame(hypothesis = "H4", gender = "male", effect = "H4: Friendshp", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H4.male[[x]]) }) # https://stackoverflow.com/a/56094806
H4.male <- do.call(rbind.data.frame, H4.male)

H4 <- rbind.data.frame(H4.female, H4.male)
rm(H4.female, H4.male)


## H5a: Transitive Triplets
H5a.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H5a.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H5a.female <- lapply(X = seq_along(H5a.female), FUN = function(x){ cbind.data.frame(hypothesis = "H5a", gender = "female", effect = "H5a: Transitive Triplets", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H5a.female[[x]]) }) # https://stackoverflow.com/a/56094806
H5a.female <- do.call(rbind.data.frame, H5a.female)

H5a.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H5a.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H5a.male <- lapply(X = seq_along(H5a.male), FUN = function(x){ cbind.data.frame(hypothesis = "H5a", gender = "male", effect = "H5a: Transitive Triplets", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H5a.male[[x]]) }) # https://stackoverflow.com/a/56094806
H5a.male <- do.call(rbind.data.frame, H5a.male)

H5a <- rbind.data.frame(H5a.female, H5a.male)
rm(H5a.female, H5a.male)


## H5b: Three Cycles
H5b.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H5b.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H5b.female <- lapply(X = seq_along(H5b.female), FUN = function(x){ cbind.data.frame(hypothesis = "H5b", gender = "female", effect = "H5b: Three Cycles", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H5b.female[[x]]) }) # https://stackoverflow.com/a/56094806
H5b.female <- do.call(rbind.data.frame, H5b.female)

H5b.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H5b.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H5b.male <- lapply(X = seq_along(H5b.male), FUN = function(x){ cbind.data.frame(hypothesis = "H5b", gender = "male", effect = "H5b: Three Cycles", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H5b.male[[x]]) }) # https://stackoverflow.com/a/56094806
H5b.male <- do.call(rbind.data.frame, H5b.male)

H5b <- rbind.data.frame(H5b.female, H5b.male)
rm(H5b.female, H5b.male)


## H6: Absolute Difference: General Reputation
H6.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H6.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H6.female <- lapply(X = seq_along(H6.female), FUN = function(x){ cbind.data.frame(hypothesis = "H6", gender = "female", effect = "H6: Abs. Diff. General Reputation", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H6.female[[x]]) }) # https://stackoverflow.com/a/56094806
H6.female <- do.call(rbind.data.frame, H6.female)

H6.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H6.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H6.male <- lapply(X = seq_along(H6.male), FUN = function(x){ cbind.data.frame(hypothesis = "H6", gender = "male", effect = "H6: Abs. Diff. General Reputation", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H6.male[[x]]) }) # https://stackoverflow.com/a/56094806
H6.male <- do.call(rbind.data.frame, H6.male)

H6 <- rbind.data.frame(H6.female, H6.male)
rm(H6.female, H6.male)


## H7: Gender Homophily
H7.female <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H7.linear.combination(gender = "female", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H7.female <- lapply(X = seq_along(H7.female), FUN = function(x){ cbind.data.frame(hypothesis = "H7", gender = "female", effect = "H7: Same Gender", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H7.female[[x]]) }) # https://stackoverflow.com/a/56094806
H7.female <- do.call(rbind.data.frame, H7.female)

H7.male <- lapply(X = 0:32, FUN = function(x){ t( sapply(X = 0:64, FUN = function(y){H7.linear.combination(gender = "male", fitted.SAOM = fit.3.ans, outdegree_i = x, indegree_i = incomming_ties, indegree_j = y)}) ) } )
H7.male <- lapply(X = seq_along(H7.male), FUN = function(x){ cbind.data.frame(hypothesis = NA, gender = "male", effect = "H7: Same Gender", outdegree_i = x-1, indegree_i = incomming_ties, indegree_j = 0:64, H7.male[[x]]) }) # https://stackoverflow.com/a/56094806
H7.male <- do.call(rbind.data.frame, H7.male)

H7 <- rbind.data.frame(H7.female, H7.male)
rm(H7.female, H7.male)



## Create the 3d Scatterplot  
colors <-  alpha(c("#0077BB", "#EE7733"), 1.00) # "#0077BB" == Blue (Women); "#EE7733" == Orange
bullet.points <- c(16, 4) ## Bullet points for "statistical signifiance" at the 95% level
bullet.size <- c(1, 0.05)

svg("F5_Gender_Cooperative_Networks.svg",
    width = 18, height = 9, bg = "transparent") 

par(mfrow = c(2, 4), # 8 figures arranged in 2 rows and 4 columns
    oma = c(1, 1, 1, 1), # Outer margin area; https://derekogle.com/IFAR/supplements/plotting/CommonAxisLabels.html; https://stackoverflow.com/a/71486472
    cex.main = 1.75, cex.axis = 1.25, cex.lab = 1.15 # https://stackoverflow.com/questions/4241798/how-to-increase-font-size-in-a-plot-in-r
)
 
scatterplot3d(x = H1$outdegree_i, y = H1$indegree_j, z = H1$lincomb,
              xlab = "", #expression("Out-degree"[i]),
              ylab = "", #expression("In-degree"[j]),
              zlab = expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H1: Woman (Ego) x Activity",
              zlim = c(-6, 1),
              color = colors[ifelse(H1$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H1$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H1$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

scatterplot3d(x = H2$outdegree_i, y = H2$indegree_j, z = H2$lincomb,
              xlab = "", #expression("Out-degree"[i]),
              ylab = "", #expression("In-degree"[j]),
              zlab = "", #expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H2: Reciprocity",
              zlim = c(-6, 1),
              color = colors[ifelse(H2$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H2$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H2$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

scatterplot3d(x = H3$outdegree_i, y = H3$indegree_j, z = H3$lincomb,
              xlab = "", #expression("Out-degree"[i]),
              ylab = "", #expression("In-degree"[j]),
              zlab = "", #expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H3: Kinship",
              zlim = c(-6, 1),
              color = colors[ifelse(H3$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H3$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H3$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

scatterplot3d(x = H4$outdegree_i, y = H4$indegree_j, z = H4$lincomb,
              xlab = "", #expression("Out-degree"[i]),
              ylab = expression("In-degree"[j]),
              zlab = "", #expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H4: Friendship",
              zlim = c(-6, 1),
              color = colors[ifelse(H4$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H4$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H4$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

scatterplot3d(x = H5a$outdegree, y = H5a$indegree_j, z = H5a$lincomb,
              x.ticklabs = c(0, 5, 10, 15, 20, 20, 20, 20) + 12, # This is to reflect the adjustment to out-degrees to keep them <= 32 given the closure of 12 two-aths
              y.ticklabs = c(0, 10, 20, 30, 40, 50, 52, 52) + 12, # This is to reflect the adjustment to in-degrees to keep them <= 64 given the closure of 12 two-aths
              xlab = expression("Out-degree"[i]),
              ylab = "", #expression("In-degree"[j]),
              zlab = expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H5a: Transitive Triplets",
              zlim = c(-1, 6),
              color = colors[ifelse(H5a$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H5a$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H5a$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

scatterplot3d(x = H5b$outdegree_i, y = H5b$indegree_j, z = H5b$lincomb,
              xlab = expression("Out-degree"[i]),
              ylab = "", #expression("In-degree"[j]),
              zlab = "", #expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H5b: Three Cycles",
              zlim = c(-6, 1),
              color = colors[ifelse(H5b$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H5b$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H5b$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

scatterplot3d(x = H6$outdegree_i, y = H6$indegree_j, z = H6$lincomb,
              xlab = expression("Out-degree"[i]),
              ylab = "", #expression("In-degree"[j]),
              zlab = "", #expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H6: Absolute Diff.: General Reputation",
              zlim = c(-6, 1),
              color = colors[ifelse(H6$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H6$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H6$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

scatterplot3d(x = H7$outdegree_i, y = H7$indegree_j, z = H7$lincomb,
              xlab = expression("Out-degree"[i]),
              ylab = expression("In-degree"[j]),
              zlab = "", #expression(hat(beta)[Linear~Combination]~"for"~Log~Odds~of~italic(x)[ij]~" = "~1), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
              main = "H7: Same Gender",
              zlim = c(-6, 1),
              color = colors[ifelse(H7$gender == "female", 1, 2)],
              type = "p", # type = "p" == "points"
              pch = bullet.points[ifelse(H7$lincomb_p_value < 0.001, 1, 2)], 
              cex.symbols = bullet.size[ifelse(H7$lincomb_p_value < 0.001, 1, 2)],
              grid = TRUE, box = FALSE,
              mar = c(5, 3, 1, 3), #  A numerical vector of the form c(bottom, left, top, right) which gives the lines of margin to be specified on the four sides of the plot.
              y.margin.add = 0.5, # add additional space between tick mark labels and axis label of the y axis
              col.axis = "#767676", col.grid = "#767676", col.lab = "#767676", col.main = "#767676")

legend(x = -21, y = -7.5, legend = c("Women", "Men"), col = colors, pch = 16, cex = 2, text.col = "#767676", horiz = TRUE, bty = "n", xpd = NA)

dev.off()   



################################# FIGURE 6: DISTRIBUTIONAL GOODNESS-OF-FIT — EGO-NET STRUCTURE x GENDER #################################
villages.TN.sienaGOFs.genderstats.simulations <- data.frame()

for(i in names(villages.TN.sienaFits)){ # i <- "Model_3"
  
  model <- which(c("Model_1", "Model_2", "Model_3") == i)
  
  statistics <- rownames(t(descriptives.sienaGOF( villages.TN.sienaGOFs.genderstats[[model]] )))
  
  observed.statistics <- as.vector(villages.TN.sienaGOFs.genderstats[[model]]$Joint$Observations)
  names(observed.statistics) <- statistics
  
  simulated.statistics <- villages.TN.sienaGOFs.genderstats[[model]]$Joint$Simulations
  colnames(simulated.statistics) <- statistics
  # simulated.statistics <- apply(X = simulated.statistics, MARGIN = 2, FUN = scale, center = TRUE, scale = TRUE)
  
  simulated.statistics <- melt(simulated.statistics)
  colnames(simulated.statistics) <- c("simulation_num", "statistic", "simulated_stat")
  simulated.statistics$statistic <- factor(simulated.statistics$statistic, levels = statistics)
  simulated.statistics$observed_stat <- observed.statistics[as.character(simulated.statistics$statistic)]
  simulated.statistics <- cbind.data.frame(model = paste0("Model ", model), simulated.statistics)
  
  villages.TN.sienaGOFs.genderstats.simulations <- rbind.data.frame(villages.TN.sienaGOFs.genderstats.simulations, simulated.statistics, stringsAsFactors = FALSE)
  
}
rm(i, statistics, observed.statistics, simulated.statistics)
villages.TN.sienaGOFs.genderstats.simulations$model <- factor(villages.TN.sienaGOFs.genderstats.simulations$model, levels = c("Model 1", "Model 2", "Model 3"), labels = c( "Baseline", "Fully Interacted", "Social Constraints"))




gof.box.plot <- (ggplot(data = villages.TN.sienaGOFs.genderstats.simulations, aes(y = simulated_stat, x = statistic, color = model, fill = model)) 
                 + geom_hline(aes(yintercept = observed_stat), linetype = 2, size = .15, alpha = 0.75, colour = "#767676")
                 
                 # https://jrnold.github.io/ggthemes/reference/geom_tufteboxplot.html
                 # A point indicating the median, a gap indicating the interquartile range, and lines for whiskers.
                 + geom_tufteboxplot(median.type = "point", whisker.type = "line", size = 0.80)
                 + labs(title = NULL,
                        y = expression(Statistic~bar(y)[Women]~-~Statistic~bar(y)[Men]), # https://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/plotmath.html
                        x = "\nEgocentric Network Statistic\n"#, 
                        # # tag = "a",
                        # caption = "The mean of an ego-net statistic for men is subtracted from the mean of that statistic for women. 
                        # Positive values indicate that the ego-nets of women have, on average, more of a statistic relative to the ego-nets of men.
                        # The observed-difference between the gender-specific mean value of a statistic is given by the dashed grey line."
                 )
                 + theme_nice(style = "black") 
                 + theme(axis.line = element_line(color = "black"),
                         legend.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend bg
                         legend.box.background = element_rect(fill = "transparent", colour = "transparent"), # get rid of legend panel bg
                         legend.key = element_rect(fill = "transparent", colour = "transparent"),
                         plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot,
                         panel.background = element_rect(fill = "transparent"), # bg of the panel
                         panel.grid.major.y = element_blank(),
                         panel.grid.minor.y = element_blank(),
                         panel.grid.major.x = element_blank(), # element_line(size = 0.15, linetype = 1, colour = "#E2E2E2"),
                         panel.grid.minor.x = element_blank(),
                         panel.border = element_blank(),
                         panel.spacing = unit(2, "lines"),
                         axis.text = element_text(size = 11, colour = "#767676"),
                         axis.text.y = element_text(hjust = 1),
                         axis.title = element_text(size = 11, colour = "#767676"),
                         plot.title = element_text(size = 11, colour = "#767676"),
                         plot.caption = element_text(size = 11, colour = "#767676"),
                         legend.position = "top",
                         legend.title = element_text(size = 10, colour = "#767676"),
                         legend.text = element_text(size = 10, colour = "#767676"),
                         plot.tag = element_text(size = 13, vjust = -4, face = "bold", colour = "#767676"),
                         strip.text = element_blank()
                 )
                
                 + facet_wrap(statistic ~ ., nrow = 1, scales = "free")
                 
                 + facetted_pos_scales(y = list(statistic == "Out-degree" ~ scale_y_continuous(limits = c(-0.8, 1.6), breaks = seq(-0.8, 1.6, 0.6)), # https://teunbrand.github.io/ggh4x/reference/facetted_pos_scales.html
                                                statistic == "Reciprocal Out-degree" ~ scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)),
                                                statistic == "Supportive Friends" ~ scale_y_continuous(limits = c(-0.05, 0.35), breaks = seq(-0.05, 0.35, 0.1)),
                                                statistic == "Supportive Kin" ~ scale_y_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.15)),
                                                statistic == "Transitive Triads" ~ scale_y_continuous(limits = c(-2, 6), breaks = seq(-2, 6, 2)),
                                                statistic == "Three Cycles" ~ scale_y_continuous(limits = c(-2, 6), breaks = seq(-2, 6, 2)),
                                                statistic == "General Reputation" ~ scale_y_continuous(limits = c(2, -6), breaks = seq(2, -6, -2)),
                                                statistic == "Same Gender Patrons" ~ scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5))
                 )
                 )
                 + scale_fill_manual(values = c("#33BBEE", "#EE7733", "#CC3311"),
                                     limits = rev(levels(villages.TN.sienaGOFs.genderstats.simulations$model)),
                                     name = "Stochastic Actor-Oriented Model:")
                 + scale_color_manual(values = c("#33BBEE", "#EE7733", "#CC3311"),
                                      limits = rev(levels(villages.TN.sienaGOFs.genderstats.simulations$model)),
                                      name = "Stochastic Actor-Oriented Model:")
)


# plot(gof.box.plot)
ggsave(plot = gof.box.plot,
       filename = "F6_Gender_Cooperative_Networks.svg", device = "svg", dpi = 900,
       scale = 3.5, width = 5, height = 1.50, units = "in", bg = "transparent")

 


################################# SI TABLE 2 + SI TABLE 3: NETWORK DESCRIPTIVE STATISTICS (BY WAVE) #################################
network.snapshots <- list(
  socialsupportTN.13 = socialsupportTN.13,
  socialsupportTN.17 = socialsupportTN.17
)

lapply(network.snapshots, sum) ## Number of composite relationships
lapply(network.snapshots, function(x){sum(x*coresidence)}) ## Number of of intra-village composite relationships
lapply(network.snapshots, function(x){sum(x)-sum(x*coresidence)}) ## Number of of inter-village composite relationships


gden(network.snapshots, mode = "digraph") ## Network Density


grecip(network.snapshots, measure = "edgewise") ## Tie Reciprocity (i.e., proportion of edges/arcs which are reciprocated)
gtrans(dat = network.snapshots, mode = "digraph", measure = "weak", use.adjacency	= FALSE) ## Graph-Level Transitivity


sna::dyad.census(network.snapshots) 
sna::triad.census(network.snapshots)


quantile(sna::degree(network.snapshots[["socialsupportTN.13"]], gmode = "digraph", cmode = "outdegree"), probs = seq(0,1,0.05))
quantile(sna::degree(network.snapshots[["socialsupportTN.17"]], gmode = "digraph", cmode = "outdegree"), probs = seq(0,1,0.05))

quantile(sna::degree(network.snapshots[["socialsupportTN.13"]], gmode = "digraph", cmode = "indegree"), probs = seq(0,1,0.05))
quantile(sna::degree(network.snapshots[["socialsupportTN.17"]], gmode = "digraph", cmode = "indegree"), probs = seq(0,1,0.05))


table(sna::degree(network.snapshots[["socialsupportTN.13"]], gmode = "digraph", cmode = "outdegree"))
table(sna::degree(network.snapshots[["socialsupportTN.17"]], gmode = "digraph", cmode = "outdegree"))


table(sna::degree(network.snapshots[["socialsupportTN.13"]], gmode = "digraph", cmode = "indegree"))
table(sna::degree(network.snapshots[["socialsupportTN.17"]], gmode = "digraph", cmode = "indegree"))


stat.desc(sna::degree(network.snapshots[["socialsupportTN.13"]], gmode = "digraph", cmode = "outdegree"))
stat.desc(sna::degree(network.snapshots[["socialsupportTN.17"]], gmode = "digraph", cmode = "outdegree"))


stat.desc(sna::degree(network.snapshots[["socialsupportTN.13"]], gmode = "digraph", cmode = "indegree"))
stat.desc(sna::degree(network.snapshots[["socialsupportTN.17"]], gmode = "digraph", cmode = "indegree"))


table(sna::geodist(network.snapshots[["socialsupportTN.13"]])$gdist)
table(sna::geodist(network.snapshots[["socialsupportTN.17"]])$gdist)
 

## To count the number of transitive triads and the number of cyclic triads in each actor's ego-nets we simply count the number of outbound and inbound two paths that ego's outgoing ties close.
## To do this, we use the function kpath.census()
## dyadic.tabulation == the type of dyadic path count information to be tabulated. 
## dyadic.tabulation == "sum" returns a vertex by vertex matrix of source/destination path counts.
socialsupportTN.13.two.paths <- kpath.census(network.snapshots[["socialsupportTN.13"]], mode = "digraph", maxlen = 2, tabulate.by.vertex = FALSE, path.comembership = "none", dyadic.tabulation = "sum")

## Descriptive stats for the number of transitive triads in network members' one-degree ego-nets
## This returns the sum of the number of outbound two-paths between ego and an alter.
stat.desc(rowSums(socialsupportTN.13.two.paths$paths.bydyad*network.snapshots[["socialsupportTN.13"]]))


## Descriptive stats for the number of three cycles in network members' one-degree ego-nets
## This returns the sum of the number of inbound two-paths between ego and an alter.
stat.desc(rowSums(t(socialsupportTN.13.two.paths$paths.bydyad)*network.snapshots[["socialsupportTN.13"]]))



# ggplot(data = melt(socialsupportTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(socialsupportTN.17), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(friendshipTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(relatednessTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(relatedness.affinalTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(interhousehold.dist.TN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(interhousehold.dist.TN.13*(coresidence == 0)), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(coresidence), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 



################################# SI TABLE 4 + SI TABLE 5: MONADIC/DYADIC DESCRIPTIVE STATISTICS (2013) #################################
## Monadic Covariates
table(individuals.TN.13$Gender, useNA = "always") ## 1 == Female; Male == 0
table(individuals.TN.13$Reservation_Status, useNA = "always")  ## 1 = Scheduled Caste; 0 = Backward Caste
table(individuals.TN.13$Caste, individuals.TN.13$Location_2013, useNA = "always") 
table(individuals.TN.13$non.natal.village, useNA = "always")  ## 1 = Village Immigrant/Non-Natal Village; 0 = Village Native/Natal Village
table(ifelse(individuals.TN.13$Status_2013 == "Not Married", 1, 0), useNA = "always") ## 1 == Not Married; 0 == Married
length(table(individuals.TN.13$GPS_2013)) ## How many households?
table(ifelse(individuals.TN.13$Location_2013 == "Tenpatti", 1, 0), useNA = "always") ## 1 == Tenpatti; 0 == Alakapuram


## Reputation Nominations
general_reputation <- colSums(TN.Nets[["Generous_2013"]] +
                                TN.Nets[["Influence_2013"]] +
                                TN.Nets[["Character_2013"]] +
                                TN.Nets[["Strength_2013"]]
)
general_reputation <- general_reputation[individuals.TN.13$IndivID] ## Ensure things are in the right order


stargazer( data.frame( Age = 2013-individuals.TN.13$BirthYear,
                       `Years of Education` = individuals.TN.13$EduYears_2013,
                       `Household Wealth` = individuals.TN.13$HouseholdWealth_2013,
                       `Household Wealth (log)` = log(individuals.TN.13$HouseholdWealth_2013),
                       `General Reputation` = general_reputation,
                       `General Reputation (Sqrt)` = sqrt(general_reputation)
), 
summary = T, summary.logical = T, digits = 2,
summary.stat = c("n", "mean", "sd", "median", "min", "max"),
type = "text")


## Dyadic Covariates 
## Consanguineal Relatedness; symmetric dyadic covariate
stat.desc(relatednessTN.13[upper.tri(relatednessTN.13, diag = F)]) 


## Affinal Relatedness; symmetric dyadic covariate
stat.desc(relatedness.affinalTN.13[upper.tri(relatedness.affinalTN.13, diag = F)]) 


## Binary Kinship; symmetric dyadic covariate
consanguineal.kin <- relatednessTN.13
diag(consanguineal.kin) <- 0 ## 1's along the diagonal (One's relatedness to themselves. Not needed for the SAOMs)
consanguineal.kin[consanguineal.kin > 0] <- 1

affinal.kin <- relatedness.affinalTN.13
affinal.kin[affinal.kin > 0] <- 1

kin.dyad <- consanguineal.kin + affinal.kin
kin.dyad[kin.dyad > 0] <- 1
rm(consanguineal.kin, affinal.kin)

table(kin.dyad[upper.tri(kin.dyad, diag = F)], useNA = "always") 


## Binary Friendship/Perceived Social Closeness; asymmetric dyadic covariate
table(friendshipTN.13, useNA = "always")


## Geographic (Inter-household) Distance; symmetric dyadic covariate
stat.desc(interhousehold.dist.TN.13[upper.tri(interhousehold.dist.TN.13, diag = F)]) 




################################# SI TABLE 7: PARAMETER ESTIMATES #################################
all.pretty.effects.of.interest.table <- c(
  "Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
  "Out-degree",
  "Reciprocity",
  "Transitive Triplets",
  "Transitive Reciprocated Triplets",
  "Three Cycles",
  "In-degree Popularity",
  "In-degree Activity",
  "Out-degree Activity",
  "Kinship",
  "Friendship",
  "Geographic Distance",
  "Woman (Ego)",
  "Same Gender",
  "Age (Ego)",
  "Absolute Difference: Age",
  "Age^2 (Ego)",
  "Years of Education (Ego)",
  "Household Wealth (Ego)",
  "Reservation Status: Scheduled Caste (Ego)",
  "Same Caste",
  "Immigrant Status: Non-Natal Village Resident (Ego)",
  "Partnership Status: Not Married (Ego)",
  "General Reputation (Ego)",
  "Absolute Difference: General Reputation",
  "Same Household",
  "Absolute Difference: Village",
  "Woman (Ego) x Reciprocity",
  "Woman (Ego) x Kinship",
  "Woman (Ego) x Friendship",
  "Woman (Ego) x Geographic Distance",
  "Woman (Ego) x Same Gender",
  "Woman (Ego) x Absolute Difference: Age",
  "Woman (Ego) x Same Caste",
  "Woman (Ego) x Absolute Difference: General Reputation",
  "Woman (Ego) x Same Household",
  "Woman (Ego) x Absolute Difference: Village",
  "Woman (Ego) x In-degree Popularity",
  "Woman (Ego) x In-degree Activity",
  "Woman (Ego) x Out-degree Activity",
  "Woman (Ego) x Transitive Triplets",
  "Woman (Ego) x Transitive Reciprocated Triplets",
  "Woman (Ego) x Three Cycles"
)

reorder.all.pretty.effects.of.interest.table <- c(
  "Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
  
  "Out-degree",
  "Reciprocity",
  "Kinship",
  "Friendship",
  "Transitive Triplets",
  "Transitive Reciprocated Triplets",
  "Three Cycles",
  "Absolute Difference: General Reputation",
  
  "In-degree Popularity",
  "In-degree Activity",
  "Out-degree Activity",
  "Absolute Difference: Age",
  "Same Gender",
  "Same Caste",
  "Absolute Difference: Village",
  "Same Household",
  "Geographic Distance",
  
  "Woman (Ego)",
  "Woman (Ego) x Reciprocity",
  "Woman (Ego) x Kinship",
  "Woman (Ego) x Friendship",
  "Woman (Ego) x Transitive Triplets",
  "Woman (Ego) x Transitive Reciprocated Triplets",
  "Woman (Ego) x Three Cycles",
  "Woman (Ego) x Absolute Difference: General Reputation",
  
  "Woman (Ego) x In-degree Popularity",
  "Woman (Ego) x In-degree Activity",
  "Woman (Ego) x Out-degree Activity",
  "Woman (Ego) x Absolute Difference: Age",
  "Woman (Ego) x Same Gender",
  "Woman (Ego) x Same Caste",
  "Woman (Ego) x Absolute Difference: Village",
  "Woman (Ego) x Same Household",
  "Woman (Ego) x Geographic Distance",
  
  "Age (Ego)",
  "Age^2 (Ego)",
  "Years of Education (Ego)",
  "Household Wealth (Ego)",
  "Reservation Status: Scheduled Caste (Ego)",
  "Immigrant Status: Non-Natal Village Resident (Ego)",
  "Partnership Status: Not Married (Ego)",
  "General Reputation (Ego)"
)


## Create a list of data frames that contain the results from each fitted SIENA model object
siena.coefs.tables <- lapply(X = rev(villages.TN.sienaFits), ## Reverse the order of the fitted SIENA model objects in villages.TN.sienaFits to left join with the output from the fully-specified model
                      FUN = function(x){ cbind.data.frame(effect = c("Rate parameter", x$effects$effectName), ## Name of each effect
                                                          beta_hat = c(x$rate, x$theta), ## Rate parameters + parameter estimates
                                                          se_beta = c(x$vrate, x$se),  ## Standard errors of the Rate parameters + standard error of each parameter estimate
                                                          p_value = 2*pnorm( abs( c(x$rate, x$theta)/c(x$vrate, x$se) ), lower.tail = FALSE) ## Two-sided p-value associated with each parameter estimate
                                                          ,
                                                          stringsAsFactors = FALSE)
                      }
)

siena.coefs.tables <- reduce(.x = siena.coefs.tables, .f = left_join, by = "effect") ## Left join; https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list


## Round all results to the thousandths place for tabular presentation; The first column contains the effect names (character class), hence [,-1]
# siena.coefs.tables[,-1] <- apply(siena.coefs.tables[,-1], MARGIN = 2, FUN = function(x){sprintf("%.3f", x)})
siena.coefs.tables[,c(4, 7, 10)] <- apply(siena.coefs.tables[,c(4, 7, 10)], MARGIN = 2, FUN = function(x){ ifelse(x < 0.001, scientific(x, digits = 1), sprintf("%.3f", x)) })
siena.coefs.tables[,-c(1, 4, 7, 10)] <- apply(siena.coefs.tables[,-c(1, 4, 7, 10)], MARGIN = 2, FUN = function(x){sprintf("%.3f", x)})

siena.coefs.tables[is.na(siena.coefs.tables)] <- ""
siena.coefs.tables[siena.coefs.tables == "NA"] <- ""


## Basic column names
colnames(siena.coefs.tables) <- c("effect",
                           "beta_hat_M3", "se_beta_M3", "p_value_M3",
                           "beta_hat_M2", "se_beta_M2", "p_value_M2",
                           "beta_hat_M1", "se_beta_M1", "p_value_M1"
)


## Reorder the columns of the data frame siena.coefs.tables for presentation
siena.coefs.tables <- siena.coefs.tables[c("effect",
                             "beta_hat_M1", "se_beta_M1", "p_value_M1",
                             "beta_hat_M2", "se_beta_M2", "p_value_M2",
                             "beta_hat_M3", "se_beta_M3", "p_value_M3"
)]


rownames(siena.coefs.tables) <- all.pretty.effects.of.interest.table ## Make the effect names in the first column the official row names
siena.coefs.tables <- siena.coefs.tables[reorder.all.pretty.effects.of.interest.table, ] ## Indexing by row names, reorder the rows of siena.coefs.tables for presentation
siena.coefs.tables$effect <- NULL ## Remove the first column
siena.coefs.tables[siena.coefs.tables == "NA"] <- "" ## The cells associated with results for effects only in the second/sixth, third/seventh, and forth/eigth model specifications are NA for the other model specifications. Replace with nothing for pretty tabular presentation.


print(siena.coefs.tables) ## See how it all looks.


#### Use Microsoft Word's convert text to table option (tab delimited)
write.table(siena.coefs.tables[, c("beta_hat_M1", "se_beta_M1", "p_value_M1",
                            "beta_hat_M2", "se_beta_M2", "p_value_M2",
                            "beta_hat_M3", "se_beta_M3", "p_value_M3")],
            file = "ST7_ModelEstimates.txt", sep = "\t", quote = FALSE, row.names = TRUE) ## Main Models


