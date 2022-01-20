################################# Dynamics of Cooperative Networks Partially Attributable to Gender Amongst South Indian Tamils
################################# Replication Code: Data Analysis



## To help ensure the reproducibility of findings from this project, I have decided to use the R package "groundhog"
library(groundhog)


## The groundhog.library() command requires two values. Like library(), you indicate 
## which package you want to load. Additionally, you must enter a date — any date (formatted as "yyyy-mm-dd"). 
## Groundhog will load the most recent version of the named package from CRAN from the entered 
## date. It will also load all dependencies of that package current on the entered date.
## Put simply, and borrowing from the package creator (see: https://groundhogr.com):

## 1) "groundhog" Makes R scripts reproducible by "replacing library("pkg")" with "groundhog.library("pkg", "date")"
## 2) groundhog.library() loads a package and its dependencies, as available on the chosen date.
## 3) Packages and their dependencies get automatically installed on their initial loading via groundhog if needed 
## 4) You will need to create a dedicated folder for groundhog package installs during the initial executing of "library(groundhog)").
## 4) Installation keeps, rather than replaces, existing other versions of a package of interest (e.g., versions from other dates).
## 5) If you want to stop using groundhog at any time, simply: replace "groundhog.library("pkg", "date")" with library("pkg") when loading a package of interest

## Note that versions of RSiena are inconsistently pushed to CRAN (which groundhog pulls from). 
## Accordingly, we have included the source code for the version of RSiena used for this project
## with all of the other replication materials. You will have to install RSiena manually.
## Similarly, we have included the source code for the version of groundhog used for this
## project with the replication materials and it should also be installed manually first.

## Finally, note that groundhog may prompt you to restart R immediately after loading packages due to version clashes and then reload the packages with groundhog again.
## This is a bit annoying, but please be sure to follow the groundhog prompts.

groundhog.library("tidyverse", "2021-10-10", quiet.install = FALSE) ## You may also need to install GFortran which is used to install the Matrix package
groundhog.library("purrr", "2021-10-10", quiet.install = FALSE)

groundhog.library("network", "2021-10-10", quiet.install = FALSE)
groundhog.library("sna", "2021-10-10", quiet.install = FALSE)
groundhog.library("igraph", "2021-10-10", quiet.install = FALSE)

groundhog.library("kinship2", "2021-10-10", quiet.install = FALSE)

groundhog.library("stringr", "2021-10-10", quiet.install = FALSE)


groundhog.library("ggplot2", "2021-10-10", quiet.install = FALSE)
groundhog.library("reshape2", "2021-10-10", quiet.install = FALSE)
groundhog.library("viridis", "2021-10-10", quiet.install = FALSE)
# groundhog.library("unikn", "2021-10-10", quiet.install = FALSE)

groundhog.library("pastecs", "2021-10-10", quiet.install = FALSE)
groundhog.library("stargazer", "2021-10-10", quiet.install = FALSE)


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



####################################  LOAD NETWORK AND ATTRIBUTE DATA FOR ALAKAPURAM AND TENPATTI  ####################################  
source("Gender_Cooperative_Networks_DataPrep_Tamil_Nadu.R")



################################# Variable Construction: Village Coresidence #################################
coresidence <- data.frame()
for(i in 1:nrow(individuals.TN.13)){
  village <- individuals.TN.13$Location_2013[i] ## In which village does i live?
  village.match <- individuals.TN.13$Location_2013 == village ## Match i's village against the vector of all residents locations across both villages
  village.match <- as.numeric(village.match) ## Convert the TRUE/FALSE vectors into zeros (FALSE) and ones (TRUE)
  
  coresidence <- rbind(coresidence, village.match) 
  #print(village.match)
  rm(i, village, village.match)
}
coresidence <- as.matrix(coresidence)
colnames(coresidence) <- individuals.TN.13$IndivID
rownames(coresidence) <- individuals.TN.13$IndivID
diag(coresidence) <- 0 ## Zeroing-out the diagonal ensures that the 108 individual residents (i.e., the diagonal cells) are excluded from Figure 3



################################# NETWORK DESCRIPTIVE STATISTICS (By Wave) #################################
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


# ggplot(data = melt(socialsupportTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(socialsupportTN.17), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(friendshipTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(relatednessTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(relatedness.affinalTN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(interhousehold.dist.TN.13), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(interhousehold.dist.TN.13*(coresidence == 0)), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 
# ggplot(data = melt(coresidence), aes(x = Var1, y = Var2, fill = value)) + geom_tile() + theme(axis.text = element_blank()) + scale_fill_viridis(option = "plasma", direction = 1) 



################################# MONADIC/DYADIC DESCRIPTIVE STATISTICS: 2013 #################################
## Monadic Covariates
table(individuals.TN.13$Gender) ## 1 == Female; Male == 0
table(individuals.TN.13$Reservation_Status)  ## 1 = Scheduled Caste; 0 = Backward Caste
table(individuals.TN.13$Caste, individuals.TN.13$Location_2013, useNA = "always") 
table(individuals.TN.13$non.natal.village)  ## 1 = Village Immigrant/Non-Natal Village; 0 = Village Native/Natal Village
table(ifelse(individuals.TN.13$Status_2013 == "Not Married", 1, 0)) ## 1 == Not Married; 0 == Married
table(ifelse(individuals.TN.13$Location_2013 == "Tenpatti", 1, 0)) ## 1 == Tenpatti; 0 == Alakapuram


stargazer( data.frame( Age = 2013-individuals.TN.13$BirthYear,
                       `Years of Education` = individuals.TN.13$EduYears_2013,
                       `Household Wealth` = individuals.TN.13$HouseholdWealth_2013,
                       `General Reputation` = colSums(TN.Nets[["Generous_2013"]] + 
                                                          TN.Nets[["Influence_2013"]] + 
                                                          TN.Nets[["Character_2013"]] + 
                                                          TN.Nets[["Strength_2013"]]
                                                      )
                       ), 
           summary = T, summary.logical = T, digits = 2,
           summary.stat = c("n", "mean", "sd", "median", "min", "max"),
           type = "text")


## Dyadic Covariates 
stat.desc(relatednessTN.13[upper.tri(relatednessTN.13, diag = F)]) ## Consanguineal Relatedness; symmetric dyadic covariate
stat.desc(relatedness.affinalTN.13[upper.tri(relatedness.affinalTN.13, diag = F)]) ## Affinal Relatedness; symmetric dyadic covariate

stat.desc(interhousehold.dist.TN.13[upper.tri(interhousehold.dist.TN.13, diag = F)]) ## Geographic (Inter-household) Distance; symmetric dyadic covariate

table(friendshipTN.13)  ## Friendship/Perceived Social Closeness; asymmetric dyadic covariate




######################################################################## STOCHASTIC ACTOR-ORIENTED MODELS (SAOMs) ########################################################################
############################################## FUNCTION FOR THE ITERATIVE RUNNING OF siena07() ############################################## 
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
    ## is less than 0.10, and, for all the individual parameters, the t-ratios for convergence are all (tratio.max) less than 0.1 in absolute value
    else if (tconv.max < 0.10 & tratio.max < 0.10) {
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



################################ Define SIENA Objects for SAOM Estimation #################################

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
support_net <- sienaNet(support_array, type = "oneMode", nodeSet = "villagers"
                        , allowOnly = FALSE)


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
general_reputation <- log(general_reputation + 1)
general_reputation <- coCovar(scale(general_reputation, center = TRUE, scale = TRUE)[,1],
                              centered = FALSE, nodeSet = "villagers")


HH_ID <- coCovar(as.numeric(factor(individuals.TN.13$GPS_2013)), ## FamID_2013 ## Should this be the physical structure (GPS) for jumping households or the groups who pool resources (FamID_2013), multiple of which may be in the same physical structure (GPS)?
                 centered = FALSE, nodeSet = "villagers")


village <- coCovar(ifelse(individuals.TN.13$Location_2013 == "Tenpatti", 1, 0), 
                   centered = FALSE, nodeSet = "villagers")



####################################  DYADIC COVARIATES
consanguineal.relatedness <- relatednessTN.13
diag(consanguineal.relatedness) <- 0 ## 1's along the diagonal (One's relatedness to themselves. Not needed for the SAOMs)
consanguineal.relatedness <- coDyadCovar( consanguineal.relatedness, ## coefficient of relatedness between i and j
                                          centered = FALSE, nodeSets = c("villagers", "villagers"),
                                          type = "oneMode")


affinal.relatedness <- coDyadCovar( relatedness.affinalTN.13, ## Coefficient of relatedness between i's spouse s and some other person j
                                          centered = FALSE, nodeSets = c("villagers", "villagers"),
                                          type = "oneMode")


friendship.dyad <- coDyadCovar( friendshipTN.13,
                                centered = FALSE, nodeSets = c("villagers", "villagers"),
                                type = "oneMode")


## log of the geographic distance following: Preciado, P., Snijders, T.A., Burk, W.J., Stattin, H., Kerr, M., 2012. Does proximity matter? Distance dependence of adolescent friendships. Social Networks, 34(1):18-31. https://doi.org/10.1016/j.socnet.2011.01.002
geodist.dyad <- coDyadCovar( log(interhousehold.dist.TN.13 + 1), ## Inter-household distance in meters
                             centered = FALSE, nodeSets = c("villagers", "villagers"),
                             type = "oneMode")


####################################  CREATE THE RSIENA DATA OBJECT FOR MODEL FITTING
multidata <- sienaDataCreate(support_net,
                             consanguineal.relatedness,
                             affinal.relatedness,
                             friendship.dyad,
                             geodist.dyad,
                             gender,
                             age,
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


rm(villagers, villagers.size, support_net,
   consanguineal.relatedness,
   affinal.relatedness,
   friendship.dyad,
   geodist.dyad,
   gender,
   age,
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
print01Report(villages.TN.sienaData, modelname = "Gender_Cooperation_Tamil_Nadu_2021_sienaDataSummary", getDocumentation = FALSE)



############################################# Define SIENA Algorithm ############################################# 
modelparams <- sienaAlgorithmCreate(projname = "Gender_Cooperation_Tamil_Nadu_2021_Estimation_History" 
                                    
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
                                    , n3 = 30000
                                    
                                    # This determines the step sizes in the estimation algorithm. If the algorithm is unstable 
                                    # (e.g., oscillating between wild parameter estimates and convergence from run to run),
                                    # use a smaller value (but greater than 0). The default value is 0.2. Sometimes
                                    # for difficult data-model combinations, the algorithm diverges very quickly, and this
                                    # may be countered by smaller values of firstg, e.g., 0.01 or 0.05.
                                    , firstg = 0.1
                                    
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

fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "consanguineal.relatedness", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "affinal.relatedness", type = "eval", fix = FALSE, verbose = FALSE)
fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "friendship.dyad", type = "eval", fix = FALSE, verbose = FALSE) 
fit.1.modeffects <- includeEffects(fit.1.modeffects, X, name = "support_net", interaction1 = "geodist.dyad", type = "eval", fix = FALSE, verbose = FALSE)

fit.1.modeffects <- includeEffects(fit.1.modeffects, sameX, name = "support_net", interaction1 = "gender", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, simX, name = "support_net", interaction1 = "age", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, simX, name = "support_net", interaction1 = "education", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, sameX, name = "support_net", interaction1 = "caste", type = "eval", fix = FALSE, verbose = FALSE) 

fit.1.modeffects <- includeEffects(fit.1.modeffects, simX, name = "support_net", interaction1 = "general_reputation", type = "eval", fix = FALSE, verbose = FALSE) 

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



## Model 2: Fully Interacted Model (Typically Converges After Four Iterations of siena07RunToConvergence)
fit.2.modeffects <- includeEffects(fit.1.modeffects, egoX, name = "support_net", interaction1 = "gender", type = "eval", fix = FALSE, verbose = FALSE) 

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, recip, interaction1 = c("gender", ""), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "consanguineal.relatedness"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "affinal.relatedness"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "friendship.dyad"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)
fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, X, interaction1 = c("gender", "geodist.dyad"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, sameX, interaction1 = c("gender", "gender"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, simX, interaction1 = c("gender", "age"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, simX, interaction1 = c("gender", "education"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, sameX, interaction1 = c("gender", "caste"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

fit.2.modeffects <- includeInteraction(fit.2.modeffects, egoX, simX, interaction1 = c("gender", "general_reputation"), name = "support_net", type = "eval", fix = FALSE, verbose = FALSE)

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



## Model 3: Social Constraints Adjusted Model (Typically Converges After Six Iterations of siena07RunToConvergence)
fit.3.modeffects <- includeEffects(fit.2.modeffects, egoX, name = "support_net", interaction1 = "age", type = "eval", fix = FALSE, verbose = FALSE) 
fit.3.modeffects <- includeEffects(fit.3.modeffects, egoSqX, name = "support_net", interaction1 = "age", type = "eval", fix = FALSE, verbose = FALSE) 
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



################################# TABLE 1 (PART 2): MULTI-PARAMETER WALD TESTS #################################
## RUN: ?Multipar.RSiena
fit.2.ans.Walt.test <- Multipar.RSiena(ans = fit.2.ans, c(13, 21:38)) ## Positive integers specify the tested effects (as numbered in "print(ans)" )
print(fit.2.ans.Walt.test)

fit.3.ans.Walt.test <- Multipar.RSiena(ans = fit.3.ans, c(15:16, 18, 20:22, 24:26))
print(fit.3.ans.Walt.test)




################################# COMBINE All SAOM FIT OBJECTS FOR POST-PROCESSING ################################# 
villages.TN.sienaFits <- list(fit.1.ans, fit.2.ans, fit.3.ans)

names(villages.TN.sienaFits) <- c("Model_1", "Model_2", "Model_3")

closeAllConnections() 



################################# DISTRIBUTIONAL GOODNESS OF FIT ASSESSMENT ################################# 
cl <- makeCluster(cores)

## The following lines ensures that sienaGOF/GeodesicDistribution can access the "network" package and the "sna" package via groundhog on each CPU core
clusterEvalQ(cl, library("groundhog")) ## https://stackoverflow.com/questions/40749916/parallel-in-r-function-not-found-how-to-export-to-cluster-functions-from-ext
clusterEvalQ(cl, groundhog.library("network", "2021-10-10", quiet.install = FALSE)) ## clusterEvalQ evaluates a literal expression on each cluster node. It is a parallel version of evalq, and is a convenience function invoking clusterCall.
clusterEvalQ(cl, groundhog.library("sna", "2021-10-10", quiet.install = FALSE))

villages.TN.sienaGOFs.attribute.indegree <- list()
villages.TN.sienaGOFs.attribute.outdegree <- list()
villages.TN.sienaGOFs.attribute.geodist <- list()
villages.TN.sienaGOFs.attribute.triadcensus <- list()

for(i in names(villages.TN.sienaFits)){ # model <- "Model_1"
  
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
  
  
  maxInDegree <- max(sna::degree(socialsupportTN.13, gmode = "digraph", cmode = "indegree"),
                     sna::degree(socialsupportTN.17, gmode = "digraph", cmode = "indegree")
                     )
  
  maxOutDegree <- max(sna::degree(socialsupportTN.13, gmode = "digraph", cmode = "outdegree"),
                      sna::degree(socialsupportTN.17, gmode = "digraph", cmode = "outdegree")
                      )
  
  maxGeodist <- max(sna::geodist(socialsupportTN.13, inf.replace = -99)$gdist, ## Replace infinite geodesics with -99 to easily retrieve max
                    sna::geodist(socialsupportTN.17, inf.replace = -99)$gdist
                    ) 
  gc()
  
  temp.gof.indegree <- sienaGOF(villages.TN.sienaFits[[model]], IndegreeDistribution,
                                varName = "support_net", cumulative = FALSE, levls = 0:maxInDegree, cluster = cl)
  print(temp.gof.indegree)
  gc()
  
  temp.gof.outdegree <- sienaGOF(villages.TN.sienaFits[[model]], OutdegreeDistribution,
                                 varName = "support_net", cumulative = FALSE, levls = 0:maxOutDegree, cluster = cl)
  print(temp.gof.outdegree)
  gc()
  
  temp.gof.geodist <- sienaGOF(villages.TN.sienaFits[[model]], GeodesicDistribution,
                               varName = "support_net", cumulative = FALSE, levls = c(1:maxGeodist,Inf), cluster = cl) 
  print(temp.gof.geodist)
  gc()
  
  temp.gof.triadcensus <- sienaGOF(villages.TN.sienaFits[[model]], TriadCensus,
                                   varName = "support_net", cluster = cl)
  print(temp.gof.triadcensus)
  gc()
  
  
  villages.TN.sienaGOFs.attribute.indegree[[model]] <- temp.gof.indegree 
  villages.TN.sienaGOFs.attribute.outdegree[[model]] <- temp.gof.outdegree
  villages.TN.sienaGOFs.attribute.geodist[[model]] <- temp.gof.geodist
  villages.TN.sienaGOFs.attribute.triadcensus[[model]] <- temp.gof.triadcensus

}
rm(i, model, GeodesicDistribution,
   temp.gof.indegree, temp.gof.outdegree, temp.gof.geodist, temp.gof.triadcensus,
   maxInDegree, maxOutDegree, maxGeodist
)

stopCluster(cl)
rm(cl)



## Plot the distributions to visually compare fit (Main Models).
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.indegree[[1]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.indegree[[2]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.indegree[[3]], center = FALSE, scale = FALSE, violin = FALSE)

RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.outdegree[[1]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.outdegree[[2]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.outdegree[[3]], center = FALSE, scale = FALSE, violin = FALSE)

RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.geodist[[1]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.geodist[[2]], center = FALSE, scale = FALSE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.geodist[[3]], center = FALSE, scale = FALSE, violin = FALSE)

RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.triadcensus[[1]], center = TRUE, scale = TRUE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.triadcensus[[2]], center = TRUE, scale = TRUE, violin = FALSE)
RSiena:::plot.sienaGOF(villages.TN.sienaGOFs.attribute.triadcensus[[3]], center = TRUE, scale = TRUE, violin = FALSE)

  

################################# TABLE 1 (PART 1): PARAMETER ESTIMATES #################################
all.pretty.effects.of.interest <- c( ## Arranged based on the RSiena internal ordering of effects as they appear in the estimated SAOMs
  "Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
  "Out-degree",
  "Reciprocity",
  "Transitive Triplets",
  "Transitive Reciprocated Triplets",
  "Three Cycles",
  "In-degree Popularity",
  "In-degree Activity",
  "Out-degree Activity",
  "Consanguineal Relatedness",
  "Affinal Relatedness",
  "Friendship",
  "Geographic Distance",
  "Gender: Woman (Ego)",
  "Same Gender",
  "Age (Ego)",
  "Age^2 (Ego)",
  "Age Similarity",
  "Years of Education (Ego)",
  "Years of Education Similarity",
  "Household Wealth (Ego)",
  "Reservation Status: Scheduled Caste (Ego)",
  "Same Caste",
  "Immigrant Status: Non-Natal Village Resident (Ego)",
  "Partnership Status: Not Married (Ego)",
  "General Reputation (Ego)",
  "General Reputation Similarity",
  "Same Household",
  "Absolute Difference: Village",
  "Gender: Woman (Ego) x Reciprocity",
  "Gender: Woman (Ego) x Consanguineal Relatedness",
  "Gender: Woman (Ego) x Affinal Relatedness",
  "Gender: Woman (Ego) x Friendship",
  "Gender: Woman (Ego) x Geographic Distance",
  "Gender: Woman (Ego) x Same Gender",
  "Gender: Woman (Ego) x Age Similarity",
  "Gender: Woman (Ego) x Years of Education Similarity",
  "Gender: Woman (Ego) x Same Caste",
  "Gender: Woman (Ego) x General Reputation Similarity",
  "Gender: Woman (Ego) x Same Household",
  "Gender: Woman (Ego) x Absolute Difference: Village",
  "Gender: Woman (Ego) x In-degree Popularity",
  "Gender: Woman (Ego) x In-degree Activity",
  "Gender: Woman (Ego) x Out-degree Activity",
  "Gender: Woman (Ego) x Transitive Triplets",
  "Gender: Woman (Ego) x Transitive Reciprocated Triplets",
  "Gender: Woman (Ego) x Three Cycles"
)


reorder.all.pretty.effects.of.interest <- c( ## Arranged in the preferred order for tabular presentation in the paper
  "Rate (Avg. Tie Changes)  m_1 (2013) → m_2 (2017)",
  
  "Out-degree",
  
  "Absolute Difference: Village",
  "Same Household",
  "Geographic Distance",
  
  "Reciprocity",
  "Consanguineal Relatedness",
  "Affinal Relatedness",
  "Friendship",
  
  "Same Gender",
  "Age Similarity",
  "Years of Education Similarity",
  "Same Caste",
  "General Reputation Similarity",
  
  "In-degree Popularity",
  "In-degree Activity",
  "Out-degree Activity",
  "Transitive Triplets",
  "Transitive Reciprocated Triplets",
  "Three Cycles",
  
  "Gender: Woman (Ego)",
  
  "Gender: Woman (Ego) x Absolute Difference: Village",
  "Gender: Woman (Ego) x Same Household",
  "Gender: Woman (Ego) x Geographic Distance",
  
  "Gender: Woman (Ego) x Reciprocity",
  "Gender: Woman (Ego) x Consanguineal Relatedness",
  "Gender: Woman (Ego) x Affinal Relatedness",
  "Gender: Woman (Ego) x Friendship",
  
  "Gender: Woman (Ego) x Same Gender",
  "Gender: Woman (Ego) x Age Similarity",
  "Gender: Woman (Ego) x Years of Education Similarity",
  "Gender: Woman (Ego) x Same Caste",
  "Gender: Woman (Ego) x General Reputation Similarity",
  
  "Gender: Woman (Ego) x In-degree Popularity",
  "Gender: Woman (Ego) x In-degree Activity",
  "Gender: Woman (Ego) x Out-degree Activity",
  "Gender: Woman (Ego) x Transitive Triplets",
  "Gender: Woman (Ego) x Transitive Reciprocated Triplets",
  "Gender: Woman (Ego) x Three Cycles",
  
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
siena.coefs <- lapply(X = rev(villages.TN.sienaFits), ## Reverse the order of the fitted SIENA model objects in villages.TN.sienaFits to left join with the output from the fully-specified model
                      FUN = function(x){ cbind.data.frame(effect = c("Rate parameter", x$effects$effectName), ## Name of each effect
                                                          beta_hat = c(x$rate, x$theta), ## Rate parameters + parameter estimates
                                                          se_beta = c(x$vrate, x$se),  ## Standard errors of the Rate parameters + standard error of each parameter estimate
                                                          p_value = 2*pnorm( abs( c(x$rate, x$theta)/c(x$vrate, x$se) ), lower.tail = FALSE) ## Two-sided p-value associated with each parameter estimate
                                                          ,
                                                          stringsAsFactors = FALSE)
                      }
)

siena.coefs <- reduce(.x = siena.coefs, .f = left_join, by = "effect") ## Left join; https://stackoverflow.com/questions/8091303/simultaneously-merge-multiple-data-frames-in-a-list


## Round all results to the thousandths place for tabular presentation; The first column contains the effect names (character class), hence [,-1]
siena.coefs[,-1] <- apply(siena.coefs[,-1], MARGIN = 2, FUN = function(x){sprintf("%.3f", x)})


## Basic column names
colnames(siena.coefs) <- c("effect",
                           "beta_hat_M3", "se_beta_M3", "p_value_M3",
                           "beta_hat_M2", "se_beta_M2", "p_value_M2",
                           "beta_hat_M1", "se_beta_M1", "p_value_M1"
)


## Reorder the columns of the data frame siena.coefs for presentation
siena.coefs <- siena.coefs[c("effect",
                             "beta_hat_M1", "se_beta_M1", "p_value_M1",
                             "beta_hat_M2", "se_beta_M2", "p_value_M2",
                             "beta_hat_M3", "se_beta_M3", "p_value_M3"
)]


rownames(siena.coefs) <- all.pretty.effects.of.interest ## Make the effect names in the first column the official row names
siena.coefs <- siena.coefs[reorder.all.pretty.effects.of.interest, ] ## Indexing by row names, reorder the rows of siena.coefs for presentation
siena.coefs$effect <- NULL ## Remove the first column
siena.coefs[siena.coefs == "NA"] <- "" ## The cells associated with results for effects only in the second/sixth, third/seventh, and forth/eigth model specifications are NA for the other model specifications. Replace with nothing for pretty tabular presentation.


print(siena.coefs) ## See how it all looks.


#### Use Microsoft Word's convert text to table option (tab delimited)
write.table(siena.coefs[, c("beta_hat_M1", "se_beta_M1", "p_value_M1",
                            "beta_hat_M2", "se_beta_M2", "p_value_M2",
                            "beta_hat_M3", "se_beta_M3", "p_value_M3")],
            file = "T1_PT1_ModelEstimates.txt", sep = "\t", quote = FALSE, row.names = TRUE) ## Main Models



################################# TABLE 2: Goodness-of-Fit #################################
## Extract information from the sienaGOF objects and combine in one data frame
siena.GOFs <- rbind.data.frame(
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.attribute.indegree,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) ),
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.attribute.outdegree,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) ),
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.attribute.geodist,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) ),
  do.call(cbind, lapply(X = villages.TN.sienaGOFs.attribute.triadcensus,
                        FUN = function(x){ cbind.data.frame(MHD = x$Joint$ObservedTestStat, p_value = x$Joint$p, stringsAsFactors = FALSE)  }
  ) )
  , stringsAsFactors = FALSE)


## Round all results to the thousandths place for tabular presentation
siena.GOFs <- apply(siena.GOFs , MARGIN = 2, FUN = function(x){sprintf("%.3f", x)})


## Basic column names
rownames(siena.GOFs) <- c("In-degree Distribution", "Out-degree Distribution", "Distribution of Geodesic Distances", "Triad Census")


print(siena.GOFs) ## See how it all looks.


#### Use Microsoft Word's convert text to table option (tab delimited)
write.table(siena.GOFs[,c("Model_1.MHD", "Model_1.p_value",
                          "Model_2.MHD", "Model_2.p_value",
                          "Model_3.MHD", "Model_3.p_value")],
            file = "T2_ModelGOFs.txt", sep = "\t", quote = FALSE, row.names = TRUE) ## Main Models



