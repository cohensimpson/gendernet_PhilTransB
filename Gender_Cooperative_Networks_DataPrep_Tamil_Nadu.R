################################# Dynamics of Cooperative Networks Partially Attributable to Gender Amongst South Indian Tamils
################################# Replication Code: Data Preparation
 


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




#################################### Load Data: Tenpatti & Alakapuram ####################################
## Read in the files that include details of each individual, including age, gender, caste & religion, years of education, household wealth, religious participation, etc.
village.names <- c("Alakapuram", "Tenpatti")
individuals <- read.csv("TN_Indiv.csv", header = TRUE, stringsAsFactors = FALSE)
families <- read.csv("TN_Fam.csv", header = TRUE, stringsAsFactors = FALSE)
parents <- read.csv("TN_Kinship.csv", header = TRUE, stringsAsFactors = FALSE)
romantic.partnerships <- read.csv("TN_partnerships.csv", header = TRUE, stringsAsFactors = FALSE)
interhousehold.distance <- read.csv("TN_Dist.csv", header = TRUE, stringsAsFactors = FALSE)

elAla.13.17 <- read.csv("Ala_1317.csv", header = TRUE, stringsAsFactors = FALSE)
elTen.13.17 <- read.csv("Ten_1317.csv", header = TRUE, stringsAsFactors = FALSE)

## Code to generate a new "union" of the male (M) and female (F) questions about borrowing items ("TasksF_2017" and "TasksM_2017") and informal ("TalkF_2017" and "TalkM_2017") talk using only the answers that are gender-aligned.
## In their original forms, "TasksF_2017", "TasksM_2017", "TalkF_2017", and "TalkM_2017" include answers from both male and female interviewers.
## However, the union versions ("TasksU_2017" and "TalkU_2017") ensure that we only analyse women's responses about themselves and the other women they live with, mutatis mutandis for men.

# 5. For you and/or the other women in your household, who happily helps you with tasks [question in Tamil implies physical assistance] [TasksF_2017]
# 6. For you and/or the other men in your household, who happily helps you with tasks [question in Tamil implies physical assistance] [TasksM_2017]
gender.temp.Ala <- individuals$Gender[match(elAla.13.17$Ego, individuals$IndivID)] ## match returns a vector of the positions of the (first) matches of each entry in the the first object (elAla.13.17$Ego) in the second (individuals$IndivID).
elAla.13.17$TasksU_2017 <- elAla.13.17$TasksF_2017*as.numeric(gender.temp.Ala == "Female") + elAla.13.17$TasksM_2017*as.numeric(gender.temp.Ala == "Male") ## Retain only the nominations of egos who took the survey (i.e., those in "individuals") and who have the appropriate gender (N.B., those who did not take the survey are given NA by match() )
elAla.13.17$TalkU_2017 <- elAla.13.17$TalkF_2017*as.numeric(gender.temp.Ala == "Female") + elAla.13.17$TalkM_2017*as.numeric(gender.temp.Ala == "Male")
rm(gender.temp.Ala)

# 7. For you and/or the other women in your household, who do you happily and casually have conversations with? [TalkF_2017]
# 8. For you and/or the other men in your household, who do you happily and casually have conversations with? [TalkM_2017]
gender.temp.Ten <- individuals$Gender[match(elTen.13.17$Ego, individuals$IndivID)] 
elTen.13.17$TasksU_2017 <- elTen.13.17$TasksF_2017*as.numeric(gender.temp.Ten == "Female") + elTen.13.17$TasksM_2017*as.numeric(gender.temp.Ten == "Male") 
elTen.13.17$TalkU_2017 <- elTen.13.17$TalkF_2017*as.numeric(gender.temp.Ten == "Female") + elTen.13.17$TalkM_2017*as.numeric(gender.temp.Ten == "Male")
rm(gender.temp.Ten)


elAla.13 <- elAla.13.17[, c(1, 2, 3:22)]
elAla.17 <- elAla.13.17[, c(1, 2, 23:39)]

elTen.13 <- elTen.13.17[, c(1, 2, 3:22)]
elTen.17 <- elTen.13.17[, c(1, 2, 23:39)]

elAla.13 <- subset(elAla.13, rowSums(elAla.13[, 3:22]) > 0) ## N.B., All participants nominate at least one person/no isolates.
elAla.17 <- subset(elAla.17, rowSums(elAla.17[, 3:17]) > 0)

elTen.13 <- subset(elTen.13, rowSums(elTen.13[, 3:22]) > 0) 
elTen.17 <- subset(elTen.17, rowSums(elTen.17[, 3:17]) > 0) 

length(unique(elAla.13$Ego))
length(unique(elTen.13$Ego))

length(unique(elAla.17$Ego))
length(unique(elTen.17$Ego))

individuals$DidSurvey13 <- individuals$IndivID %in% unique(c(elAla.13$Ego, elTen.13$Ego))
individuals$DidSurvey17 <- individuals$IndivID %in% unique(c(elAla.17$Ego, elTen.17$Ego))

table(individuals$DidSurvey13,individuals$DidSurvey17)


##### Analytic Sample for Analysis 
# N.B., Recall that SAOMs model network change between one or mare pairs of consecutive network waves. Consequently, individuals who
# join during the final wave (here, 2017) play no role in SAOM simulations and should be excluded. Practically speaking, this means
# we model network change from 2013 and to 2017, where individuals new to Alakapuram and Tenpatti in 2017 are excluded.

## N.B.,Female Resident TN17103 appears in the 2013 Alakapuram edgelist AND the 2013 and 2017 Tenpatti edgelists
## In line with TN_Indiv.csv, TN17103 lived in Tenpatti during the two survey waves (although Alakapuram is his/her natal village) 
## Accordingly, remove her from the Alakapuram edgelist
individuals.Ala.13 <- subset(individuals, individuals$IndivID %in% unique(elAla.13$Ego)) 
individuals.Ten.13 <- subset(individuals, individuals$IndivID %in% unique(elTen.13$Ego))

rownames(individuals.Ala.13) <- individuals.Ala.13$IndivID
rownames(individuals.Ten.13) <- individuals.Ten.13$IndivID

# View(rbind.data.frame(individuals.Ala.13["TN17103",], individuals.Ten.13["TN17103",]))
individuals.Ala.13 <- individuals.Ala.13[-which(individuals.Ala.13$IndivID == "TN17103"),]

print(nrow(individuals.Ala.13))
print(nrow(individuals.Ten.13))


FamID.GPS_2013.lookup <- families$GPS_2013
names(FamID.GPS_2013.lookup) <- families$FamID

# N.B. Multiple families may reside in the same house (i.e., GPS_2013 == House ID)
individuals.Ala.13$GPS_2013 <- FamID.GPS_2013.lookup[individuals.Ala.13$FamID_2013] 
individuals.Ten.13$GPS_2013 <- FamID.GPS_2013.lookup[individuals.Ten.13$FamID_2013]

# Households (i.e., GPS_2013) can contain multiple families (i.e., groups of related people that pool resources). What is the breakdown of the number of families per house?
table(tapply(X = families$FamID, INDEX = families$GPS_2013, FUN = function(x){length(x)})) ## N.B. 72 ==  Families who aren't actually resident in the village

# Combine the village-specific data frames for the unified analysis.
individuals.TN.13 <- rbind.data.frame(individuals.Ala.13, individuals.Ten.13, stringsAsFactors = FALSE)



#################################### Variable Construction: Gender #################################### 
# 0 = Male (n = 336); 1 = Female (n = 446)
individuals.TN.13$Gender <- ifelse(individuals.TN.13$Gender == "Female", 1, 0)   
table(individuals.TN.13$Gender)
## NB: 64602 and 64603 were incorrectly recorded as male in the NHB files given to Cohen; they are female, as properly recorded in these files.



#################################### Variable Construction: Household Wealth of Each Family (Indian Rupees, INR) ####################################
# Power, E. A. (2017). Social Support Networks and Religiosity in Rural South India. Nature Human Behaviour, 1(3), 0057. https://doi.org/10.1038/s41562-017-0057
# Similarly to Power (2017) who states in her supplementary information, "the measure of household wealth is based on property holdings,
# recorded during the household survey. This measure approximates the monetary value (in 1000 Indian rupee units) of all
# of the property of the household (house and facilities, land holdings, vehicles, livestock)."

families$HouseValue_2013 <- (          ((families$Roof_2013 == "Thatch-roofed")*10000) ## Roof/House Structure Type
                                     + ((families$Roof_2013 == "Cement")*200000) ## Roof/House Structure Type
                                     + ((families$Roof_2013 == "Tile-roofed")*120000) ## Roof/House Structure Type
                                     + ((families$Roof_2013 == "Centering house")*200000) ## Roof/House Structure Type (Centering Homes are ???)
                                     + ((families$Roof_2013 == "Corrugated")*10000) ## Roof/House Structure Type
                                     + ((families$Roof_2013 == "Sand house")*10000) ## Roof/House Structure Type
                                     + ((families$Roof_2013 == "Tin house")*10000) ## Roof/House Structure Type
                                     + (ifelse(families$Toilet_2013 == "Yes", 1, 0)*4000) ## Latrine
                                     + (ifelse(families$Pipe_2013 == "Yes", 1, 0)*2000) ## Water Supply
                                     + (ifelse(families$UPSBattery_2013 == 1, 1, 0)*15000) ## Uninterruptible Power Supply
)

families$VehicleValue_2013 <- (          ((families$Transport_2013 == "Auto")*200000) ## Auto Rickshaw
                                       + ((families$Transport_2013 == "Cart")*15000) ## Cart
                                       + ((families$Transport_2013 == "Cart\vMotorcycle")*(15000 + 40000)) ## Cart & Motorcycle
                                       + ((families$Transport_2013 == "Cycle")*4000) ## Moped
                                       + ((families$Transport_2013 == "Cycle\vMotorcycle")*(4000 + 40000)) ## Moped & Motorcycle
                                       + ((families$Transport_2013 == "Cycle\vWhite Van")*(4000 + 300000)) ## Moped & White Van
                                       + ((families$Transport_2013 == "farming small tractor\vMotorcycle")*(300000 + 40000)) ## Small Tractor & Motorcycle (Should Tractor be 400,000 per NHB?)
                                       + ((families$Transport_2013 == "Lorry")*400000) ## Lorry Truck
                                       + ((families$Transport_2013 == "Motorcycle")*40000) ## Motorcycle
                                       + ((families$Transport_2013 == "Motorcycle\vCycle")*(40000 + 4000)) ## Moped & Motorcycle 
                                       + ((families$Transport_2013 == "Motorcycle\vTractor")*(40000 + 400000)) ## Large Tractor & Motorcycle (Should Tractor be 400,000 per NHB?)
                                       + ((families$Transport_2013 == "Motorcycle\vWhite Van")*(40000 + 300000)) ## White Van & Motorcycle 
                                       + ((families$Transport_2013 == "motorcycle-cart")*(40000 + 15000)) ## Cart & Motorcycle
                                       + ((families$Transport_2013 == "Scooter")*25000) ## Scooter
                                       + ((families$Transport_2013 == "Scooter\vMotorcycle")*(25000 + 40000)) ## Scooter & Motorcycle
                                       + ((families$Transport_2013 == "small tractor")*300000) ## Small Tractor
                                       + ((families$Transport_2013 == "White Van")*300000) ## White Van
                                       + ((families$Transport_2013 == "White Van\vMotorcycle\vMotorcycle")*(300000 + 40000 + 40000)) ## White Van & 2 x Motorcycles
                                       + ((families$Transport_2013 == "White Van\vTractor")*(300000 + 400000)) ## White Van & Large Tractor
)

families$LandValue_2013 <- (        (families$OwnLandAgri_2013*250000) ## Agricultural Fields (1 acre = 250,000 INR)
                                  + (families$OwnLandForest_2013*100000) ##  Forest Lands (1 acre = 100,000 INR)
                                  + (ifelse(families$PumpSet_2013 == "Yes", 1, 0)*(15000 + 15000)) ## Bore Well (Yes = 15,000 INR) + Pump Set (Yes = 15,000 INR)
)

families$AnimalValue_2013 <- (        (families$Cow_2013*20000) ## Cows (1 = 20,000 INR)
                                    + (families$Goat_2013*5000) ## Goats (1 = 5000 INR) 
                                    + (families$Chicken_2013*500) ## Chickens (1 = 500 INR)
)



## Baseline Wealth = Universal wealth floor plus a per-person floor is also added. 
## These floors assumes that even the poorest households have: 5 Steel containers; 5 small plastic containers; 2 big plastic containers; 1 dosai pan; 
## 3 regular pans; 3 pots; 1 small pot (for milk); 1 idli steamer; 2 ladels; 2 flat serving spoons; 4 lids; 1 bucket; and 1 pourer — all collectively valued as 3500 Indian Rupees
## Then, for each person in the household, they are assumed to have: 1 plate; 1 bowl (kinnam); 1 tumbler; 1 tiffin box; and 2 water jugs — all collectively valued at 400 INR per person
families$BaseValue_2013 <- 3500 + (400*families$FamCountAllRes_2013) ## CountAllRes == Count of all family members (adults plus adolescents) #families$FamCountAllRes_2013; families$FamCountAllRes_2013; families$FamCountAdRes_2013


## Sum for Household Wealth
families$CashValue_2013 <- (    families$HouseValue_2013 
                                + families$VehicleValue_2013 
                                + families$LandValue_2013 
                                + families$AnimalValue_2013
                                + families$BaseValue_2013
)


HouseholdWealth_2013.lookup <- families$CashValue_2013
names(HouseholdWealth_2013.lookup) <- families$FamID

individuals.TN.13$HouseholdWealth_2013 <- HouseholdWealth_2013.lookup[individuals.TN.13$FamID_2013] 





#################################### Variable Construction: Reservation Status #################################### 
## See the UN in India for Info on Scheduled Caste and Scheduled Tribes
## http://in.one.un.org/task-teams/scheduled-castes-and-scheduled-tribes/
## Other Backward Caste and the Creamy Layer: https://en.wikipedia.org/wiki/Creamy_layer
## https://en.wikipedia.org/wiki/Forward_caste

individuals.TN.13$Reservation_Status <- individuals.TN.13$Caste
## 1 = Scheduled Caste (n = 392)
## 0 = Backward Caste (n = 390)
individuals.TN.13$Reservation_Status <- ifelse(individuals.TN.13$Reservation_Status %in% c("Arundhathiyar", "Pallar", "Paraiyar"), 1, 0)
individuals.TN.13$Tevar <- ifelse(individuals.TN.13$Caste %in% c("Agamudaiyaan", "Maravar", "Kallar"), 1, 0)
table(individuals.TN.13$Reservation_Status)



#################################### Variable Construction: Natal Village/Immigrant Status #################################### 
individuals.TN.13$non.natal.village <- ifelse(individuals.TN.13$NativePlace == individuals.TN.13$Location_2013, 0, 1) # (i.e., natal resident or not),




######################################### Variable Construction: Coefficient of Genetic Relatedness #########################################
parents$Sex <- parents$Gender
parents$Sex[parents$Sex == "M"] <- 1
parents$Sex[parents$Sex == "F"] <- 2
parents$Sex[is.na(parents$Sex)] <- 3
parents$Sex[parents$Sex == "NA"] <- 3 ## Sex for the following individuals are coded as "999"/missing due to being stillborn (TN14509, TN14510, TN51411, TN63306, TN80327, TN80328, TN80329). Based on the "individuals" data frame, all have passed away.
parents$Sex[parents$Sex == "999"] <- 3
parents$Sex <- as.numeric(parents$Sex)  


## Construct the pedigree structure for the residents of both villages (N.B. TN_Kinship.csv is more comprehensive than Ala_Kenship.csv and Ten_Kenship.csv!)
TN.pedigree <- pedigree(id = parents$Ego, ## Identification variable for focal individual
                        dadid = parents$Father, ## Identification variable for father 
                        momid = parents$Mother, ## Identification variable for mother 
                        sex = parents$Sex, ## Gender of individual noted in ‘id’ (1 = "male", 2 = "female", 3 = "unknown", 4 = "terminated")
                        missid = 999)

## Use the pedigree structure to construct pairwise kinship coefficients.
# The kinship coefficient between two subjects is the probability that a randomly 
# selected allele from a locus will be IBD between them. It is obviously 0 between 
# unrelated individuals. For an autosomal site and no inbreeding it will be 0.5 for an 
# individual with themselves, .25 between mother and child, .125 between an uncle and neice, etc.
TN.relatedness <- kinship(TN.pedigree) 


## Multiply by 2 because kinship2 is allelic.
TN.relatedness <- 2*TN.relatedness 

table(TN.relatedness == t(TN.relatedness)) ## Sanity Check
table(diag(TN.relatedness)) ## There are a few values above 1, which is due to close kin marriages. 

diag(TN.relatedness) <- 1 ## 1's along the diagonal (One's relatedness to themselves). 


## Construct village-specific relatedness matrices by filtering the master relatedness matrix, retaining only the 2013 study participants
setdiff(individuals.TN.13$IndivID, colnames(TN.relatedness))

relatednessTN.13 <- TN.relatedness[individuals.TN.13$IndivID, individuals.TN.13$IndivID]



######################################### Variable Construction: Partnership Status #########################################
# romantic.partnerships$NotDivorced_13 <- ifelse(romantic.partnerships$Status_2013 %in% c("Married"), 1, 0)
# romantic.partnerships$NotDivorced_17 <- ifelse(romantic.partnerships$Status_2017 %in% c("Married"), 1, 0)
romantic.partnerships$NotDivorced_13 <- ifelse(romantic.partnerships$Status_2013 %in% c("Married", "Widowed"), 1, 0)
romantic.partnerships$NotDivorced_17 <- ifelse(romantic.partnerships$Status_2017 %in% c("Married", "Widowed"), 1, 0)


romantic.partnerships.statuses.2013.lookup <- c(romantic.partnerships$Status_2013, romantic.partnerships$Status_2013)
names(romantic.partnerships.statuses.2013.lookup) <- c(romantic.partnerships$Husband, romantic.partnerships$Wife)

romantic.partnerships.statuses.2017.lookup <- c(romantic.partnerships$Status_2017, romantic.partnerships$Status_2017)
names(romantic.partnerships.statuses.2017.lookup) <- c(romantic.partnerships$Husband, romantic.partnerships$Wife)

romantic.partnerships.spouse.lookup <- c(romantic.partnerships$Wife, romantic.partnerships$Husband)
names(romantic.partnerships.spouse.lookup) <- c(romantic.partnerships$Husband, romantic.partnerships$Wife) ## This works as there are no same-sex couplings to our knowledge


individuals.TN.13$Status_2013 <- romantic.partnerships.statuses.2013.lookup[individuals.TN.13$IndivID]
individuals.TN.13$Status_2017 <- romantic.partnerships.statuses.2017.lookup[individuals.TN.13$IndivID]
individuals.TN.13$Spouse <- romantic.partnerships.spouse.lookup[individuals.TN.13$IndivID]


individuals.TN.13$Status_2013[is.na(individuals.TN.13$Status_2013)] <- "Not Married" 
individuals.TN.13$Status_2013[individuals.TN.13$Status_2013 == "Divorced"] <- "Not Married"
# individuals.TN.13$Status_2013[individuals.TN.13$Status_2013 == "Married"] <- "Married"
individuals.TN.13$Status_2013[individuals.TN.13$Status_2013 == "None"] <- "Not Married" 
individuals.TN.13$Status_2013[individuals.TN.13$Status_2013 == "Separated"] <- "Not Married"
individuals.TN.13$Status_2013[individuals.TN.13$Status_2013 == "Widowed"] <- "Not Married"

individuals.TN.13$Status_2017[is.na(individuals.TN.13$Status_2017)] <- "Not Married" 
individuals.TN.13$Status_2017[individuals.TN.13$Status_2017 == "Divorced"] <- "Not Married"
# individuals.TN.13$Status_2017[individuals.TN.13$Status_2017 == "Married"] <- "Married"
individuals.TN.13$Status_2017[individuals.TN.13$Status_2017 == "Separated"] <- "Not Married"
individuals.TN.13$Status_2017[individuals.TN.13$Status_2017 == "Widowed"] <- "Not Married"



######################################### Variable Construction: Affinal Relatedness #########################################
## To construct the matrix for affinal relatedness — i.e., the genetic relatedness between i's and some other person j via i's spouse s,
## we rely on the calculation of shortest paths (i.e., the number of "hops" or "steps" between two individuals i and j) in a "network" 
## composed of immediate biological kin (i.e., consanguineal relatedness == 0.5) AND their spouses (i.e., those who are NOT divorced/separated).
## The shortest paths in this "network" then counts the number of steps (i.e., the length of the shortest path) between 
## two people VIA THEIR GENETIC KIN. Accordingly, INCLUSION OF SPOUSAL DYADS ALLOWS SHORTEST PATHS TO RUN THROUGH PEOPLE WHO ARE MARRIED.
## If spousal dyads were removed, paths could only run through genetic kin. 
## Below we will need to remove these genetic-kin-only paths to retain those that only run through marriage.
## Last, to actually derive the measure of affinal relatedness we divide one by two raised to the power of the number of steps "g" between two people in the immediate kin "network" (i.e., (1/(2^g)) ).
## Thus, like consanguineal relatedness, our measure of affinal relatedness decays by half for every immediate relative between two people.
## Note that by constructing affinal relatedness in this way, we necessarily give give spouses an affinal relatedness equal to 0.5.
## Similarly, our approach allows affinal paths to run between ex-spouses and ex-spouse's family members via ex-spouse's children. 


## FIRST, construct a "network" of immediate kin (parents and their children + full siblings)
TN.relatedness.immediate <- (TN.relatedness == 0.5)*1 ## Multiplying the matrix of logicals/booleans by one produces a binary matrix of zeros and ones for whether or not two people are immediate kin.


## SECOND, add in a number of ancestral partnerships that create affinal relatedness, but who are generally long-deceased, and so about whose marriage we know little
## We identify these relationships through their presence in the parents data frame and their absence from the romantic.partnerships data frame, and add the missing cases
## In all cases, the relevant parties are dead, so we list them as "Widowed"
parents.temp <- parents
parents.temp$InPartnerships <- paste0(parents$Father,"_",parents$Mother) %in% paste0(romantic.partnerships$Husband,"_",romantic.partnerships$Wife)
parents.temp$ToAdd <- parents.temp$InPartnerships == FALSE & parents.temp$Father != "999" & parents.temp$Mother != "999"
to.add <- data.frame("Husband" = parents.temp$Father, "Wife" = parents.temp$Mother, "Status_2013" = "Widowed", "Status_2017"= "Widowed", "NotDivorced_13"= 1, "NotDivorced_17"= 1)
to.add <- subset(to.add, parents.temp$ToAdd == TRUE)

romantic.partnerships <- rbind(romantic.partnerships, to.add)


## THIRD, identify those who are married in 2013, excluding people who are divorced/separated — BUT NOT PEOPLE WHO ARE WIDOWED (see above)! 
## Excluding divorcees removes the *direct* connection between the former spouses. Thus, paths will not run via those who are divorced, severing affinal links.
## However, if former spouses have had children, they will be *indirectly* connected via their offspring. 
## Accordingly, affinal relatedness for the family members of the ex-spouses will still exist. 
## However, affinal relatedness in the event of divorce will be half the value of what it otherwise would have been had the ex-spouses stayed married.
## This is because affinal relatness will run through the longer path via their child.
## For example, consider an ex-husband, his ex-wife, their son, and the ex-wife's brother. 
## In this scenario, affinal relatedness between the ex-husband and the ex-brother-in-law is 0.125.
## This is because the affinal path runs from the ex-husband to his son (one step), from the son to the ex-wife (one step), and then from the ex-wife to the ex-brother-in-law (one step).
## But, if they had stayed married, affinal relatedness between the ex-husband and the ex-brother-in-law would have been 0.25.
## This is because the affinal path would have run from the ex-husband to the ex-wife and then from the ex-wife to the ex-brother-in-law (two steps versus three step)
romantic.partnerships.married.13 <- subset(romantic.partnerships, NotDivorced_13 == 1) 



## FOURTH, construct a data frame of spousal dyads for 2013 (i.e., Husband_ID + Wife_ID)
## Recall from above that "NotDivorced_13" is simply a binary vector for people who are Married/Widowed (== 1) or Divorced (== 0).
romantic.partnerships.married.13 <- romantic.partnerships.married.13[, c("Husband", "Wife", "NotDivorced_13")] 


## FIFTH, create a symmetric matrix for 2013 spousal relationships, populated with values from romantic.partnerships.married.13.
## Note that we use ALL individuals in the pedigree for the two villages — both dead and alive — hence the pegging of spouse.matrix to TN.relatedness
spouse.matrix <- matrix(data = 0, nrow = nrow(TN.relatedness.immediate), ncol = ncol(TN.relatedness.immediate))
rownames(spouse.matrix) <- rownames(TN.relatedness.immediate)
colnames(spouse.matrix) <- colnames(TN.relatedness.immediate)

for(i in 1:nrow(romantic.partnerships.married.13)){
  
  ## Marriage is of course symmetric. Thuse, we must add a value of one to the matrix for the spousal dyad (Husband_ID, Wife_ID) AND for the spousal dyad (Wife_ID, Husband_ID)!
  spouse.matrix[which(rownames(spouse.matrix) == romantic.partnerships.married.13$Husband[i]), which(colnames(spouse.matrix) == romantic.partnerships.married.13$Wife[i])] <- romantic.partnerships.married.13$NotDivorced_13[i] 
  spouse.matrix[which(rownames(spouse.matrix) == romantic.partnerships.married.13$Wife[i]), which(colnames(spouse.matrix) == romantic.partnerships.married.13$Husband[i])] <- romantic.partnerships.married.13$NotDivorced_13[i]
}
rm(i)
table(spouse.matrix == t(spouse.matrix)) ## Sanity check. Should all be TRUE.


## Checks for missing entries.
setdiff(romantic.partnerships.married.13$Husband, rownames(TN.relatedness.immediate))
setdiff(romantic.partnerships.married.13$Wife, rownames(TN.relatedness.immediate))


## SIXTH, create a composite binary matrix for immediate kin dyads and for spousal dyads which will be used to calculate the affinal path lengths.
immediate.family <- TN.relatedness.immediate + spouse.matrix


## SEVENTH, convert immediate.family into a proper network object in order to calculate the length of the shortest path between all members of the pedigree for the two villages using distances().
## Note that distances() will produce a symmetric matrix. Also, the shortest path length from a vertex to itself is always zero. For unreachable vertices Inf is included.
immediate.family.net <- igraph::graph_from_adjacency_matrix(immediate.family, mode = "undirected", weighted = NULL)
affinal.path.length <- igraph::distances(immediate.family.net, mode = "all", algorithm = "unweighted", weights = NULL)


## EIGHTH, derive affinal relatedness by dividing one by two raised to the power of the length of the shortest path "g" between two people in the immediate kin "network" (i.e., (1/(2^g)) )
## This function says: "For every row (i.e., MARGIN == 1) of the matrix X, raise the integer 2 to the inverse power of each element of the row g".
## Note that by using a negative exponent, one is simply taking the inverse/reciprocal of the base of the expression to the gth power. 
## For example, for g == 2, (2^-2) is equal to (1/(2^2)) and, for g == 4, (2^-4) is equal to (1/(2^4)). 
## Also, note that an exponent of one (i.e., immediate kin and spouses) resolves to the base of the expression. Thus (2^-1) is equal to (1/(2^1)) or (1/(2)).
TN.relatedness.affinal <- apply(X = affinal.path.length, MARGIN = 1, FUN = function(g){2^-g}) 
TN.relatedness.affinal <- TN.relatedness.affinal - TN.relatedness

## NINTH, remove values below 0.125
## note that we get some NEGATIVE values here. These reflect cases of affinal relatedness that is set to 0 in the romantic.partnerships.married.13 file. 
## For example: TN00201 is shown as having a negative affinal relatedness value of -0.375 with TN00208, his granddaughter. 
## This is because TN00208 was born in 2017, and their parents were not married in 2013, so their spousal connection is not considered here. 
## We also are less sure about our coverage at lower values, so will censor below 0.125 (which represents, for example, ego's child's spouse's parent; ego's spouse's grandparent, etc.)
table(TN.relatedness.affinal)
TN.relatedness.affinal[TN.relatedness.affinal<0.125] <- 0


## TENTH/FINALLY, construct village-specific affinal relatedness matrices by filtering the master affinal relatedness matrix to retain only the 2013 study participants.
relatedness.affinalTN.13 <- TN.relatedness.affinal[individuals.TN.13$IndivID, individuals.TN.13$IndivID]
table(relatedness.affinalTN.13)



######################################### Network Construction: 2013 and 2017 Sociometric Questions #########################################
######################################### 2013 Sociometric Questions
# 1. If you want to talk about important matters, who do you talk with? [ImpIss_2013]
# 2. If you want daily work [implying daily wage labor] or a new job [implying more permanent employment], who do you approach? [Work_2013]
# 3. Who will amicably help you with physical tasks [meaning, running errands and other chores]? [Errand_2013]
# 4. Who do you borrow household items from? [Borrow_2013]
# 5. If you suddenly need a small amount of money for something, whom would you ask for it from? [Cash_2013]
# 6. If you need a lot of money, whom would you ask for it from [meaning, a loan]? [Loan_2013]
# 7. If you have to go to work and need someone to watch your child, who would you give them to? [Babysit_2013]
# 8. If you had to spend a lot of time talking with someone, who would you like to talk with? [Talk_2013]
# 9. If any problem happens, who are the people who will help you? [Defend_2013]
# 10. Who do you know well in a "high position" [e.g., government officials, police, lawyers, teachers, etc.] [Position_2013]
# 11. Who are your very close friends or relatives? [Close_2013]
# 12. Who are the people who give you advice? [Advice_2013]
# 13. In this village, who is the very industrious person doing work at all times? [Hardwork_2013]
# 14. In this village, who is the person with a generous disposition? [Generous_2013]
# 15. In this village, who do you think gives good advice to anyone who asks? [AllAdvice_2013]
# 16. In this village, if some problem happens, who is the person to resolve it? Who has the influence and authority? [Influence_2013]
# 17. In this village, who is the person with good character? [Character_2013]
# 18. In this village, who do you think is the person with a lot of devotion (bhakti)? [Devout_2013]
# 19. In this village, who do you think is really strong? [Strength_2013]
# 20. In this village, who do you think knows how to conduct all rituals? [RitKnow_2013]


######################################### 2017 Sociometric Questions
# 1. If you had an unexpected emergency expense, such as a hospital medical treatment, from whom could you get a loan of 2000 Rs [a weeks wages] or more? [LoanAsk_2017]
# 2. In the event of an unexpected emergency expense, such as a hospital medical treatment, who would come ask you for a loan of 2000 Rs or more? [LoanGive_2017]
# 3. For your basic essentials such as rice, sugar, oil and other groceries and household needs, who could you immediately ask and get? [ItemBorrow_2017]
# 4. For their basic essentials such as rice, sugar, oil and other groceries and household needs, who could immediately ask and get from you? [ItemGive_2017]
# 5. For you and/or the other women in your household, who happily helps you with tasks [question in Tamil implies physical assistance] [TasksF_2017]
# 6. For you and/or the other men in your household, who happily helps you with tasks [question in Tamil implies physical assistance] [TasksM_2017]
# 7. For you and/or the other women in your household, who do you happily and casually have conversations with? [TalkF_2017]
# 8. For you and/or the other men in your household, who do you happily and casually have conversations with? [TalkM_2017]
# 9. Among those you know very well, who are those of high office, important people, government or NGO employees of high position? For example those in the police department, in politics, or working as a lawyer? [Position_2017]
# 10. Who are the people in Madurai, Chennai, or other outside or overseas places who could get things done for you? [Outside_2017]
# 11. If you wanted to discuss important and confidential matters, who would you talk to? [ImpIss_2017]
# 12. If you needed more or new wage work or a salaried job, who could you ask for help finding it? [Work_2017]
# 13. In this village, who is the person with a generous disposition? [Generous_2017]
# 14. In this village, if some problem happens, who is the person to resolve it? Who has the influence and authority? [Influence_2017]
# 15. In this village, who do you think is the person with a lot of devotion (bhakti)? [Devout_2017]

# 16. [TasksU_2017]
# 17. [TalkU_2017]



######################################### List of Networks
networks.of.study <- c("ImpIss_2013", "Work_2013", "Errand_2013", 
                       "Borrow_2013", "Cash_2013", "Loan_2013", 
                       "Babysit_2013", "Talk_2013", "Defend_2013",
                       "Position_2013", "Close_2013", "Advice_2013",
                       "Hardwork_2013", "Generous_2013", "AllAdvice_2013",
                       "Influence_2013", "Character_2013", "Devout_2013", 
                       "Strength_2013", "RitKnow_2013",
                       
                       "LoanAsk_2017", "LoanGive_2017", "ItemBorrow_2017", "ItemGive_2017", ## 2017 Sociometric Questions
                       "TasksF_2017", "TasksM_2017", "TalkF_2017", "TalkM_2017", 
                       "Position_2017", "Outside_2017", "ImpIss_2017", "Work_2017", 
                       "Generous_2017", "Influence_2017", "Devout_2017", "TasksU_2017", "TalkU_2017")



######################################### Network Construction: Draw Networks #########################################
TN.Nets <- list()

el.TN.13.17 <- rbind.data.frame(elAla.13.17, elTen.13.17, stringsAsFactors = FALSE)

# el.TN.13.17.porcupines <- subset(el.TN.13.17, el.TN.13.17$Ego %in% individuals.TN.13$IndivID)
# el.TN.13.17.porcupines <- el.TN.13.17.porcupines[, c("Ego", "Alter",
#                                                      "ImpIss_2013", "Work_2013", "Errand_2013", "Borrow_2013", "Cash_2013", "Loan_2013", "Talk_2013",
#                                                      "ImpIss_2017", "Work_2017", "TasksU_2017", "ItemBorrow_2017", "LoanAsk_2017", "TalkU_2017")]
# length(unique(el.TN.13.17.porcupines$Alter))
# length(setdiff(unique(el.TN.13.17.porcupines$Alter), individuals.TN.13$IndivID)) ## Over 5,000 porcupines!

for(i in networks.of.study){

  villagers <- individuals.TN.13$IndivID

  temp <- el.TN.13.17[, c("Ego", "Alter", i)] ## Pull the edgelist for the relation of interest. This yields a three column data frame where the third column indicates whether or not the tie of type i exists
  temp <- temp[temp[i] == 1, ] ## Pull column i (binary indicator of tie or no tie), logically match to 1 (i.e., yes there is a tie; TRUE), and then subset the edgelist by row to get the tie sender and receiver
  temp <- subset(temp, temp$Ego %in% villagers & temp$Alter %in% villagers) ## Retain only intra-village ties for the analysis

  temp.matrix <- matrix(data = 0, nrow = length(villagers), ncol = length(villagers))
  rownames(temp.matrix) <- as.character(villagers)
  colnames(temp.matrix) <- as.character(villagers)

  for(j in 1:nrow(temp)){

    surveyed.individual <- temp[j, "Ego"] ## Run through the rows of the edgelist (temp) and get the tie sender/source/nominator (first column)
    target <- temp[j, "Alter"] ## Run through the rows of the edgelist (temp) and get the tie receiver/target/nominee (second column)
    temp.matrix[which(rownames(temp.matrix) == surveyed.individual), which(colnames(temp.matrix) == target)] <- 1 ## Index the empty matrix by row/send and column/receiver to add the tie
  }

  # sum(temp.matrix) == nrow(temp) ## Sanity Check

  TN.Nets[[as.character(i)]] <- temp.matrix


  rm(i, j, villagers, temp, temp.matrix, surveyed.individual, target)
}




#################################### Network Construction: Composite Social Support Network (2013) #################################### 
socialsupportTN.13 <- (TN.Nets[["ImpIss_2013"]] # 1. If you want to talk about important matters, who do you talk with? [ImpIss_2013]
                        + TN.Nets[["Work_2013"]] # 2. If you want daily work [implying daily wage labor] or a new job [implying more permanent employment], who do you approach? [Work_2013]
                        + TN.Nets[["Errand_2013"]] # 3. Who will amicably help you with physical tasks [meaning, running errands and other chores]? [Errand_2013]
                        + TN.Nets[["Borrow_2013"]] # 4. Who do you borrow household items from? [Borrow_2013]
                        + TN.Nets[["Cash_2013"]] # 5. If you suddenly need a small amount of money for something, whom would you ask for it from? [Cash_2013]
                        + TN.Nets[["Loan_2013"]] # 6. If you need a lot of money, whom would you ask for it from [meaning, a loan]? [Loan_2013]
                        + TN.Nets[["Talk_2013"]] # 8. If you had to spend a lot of time talking with someone, who would you like to talk with? [Talk_2013]
                        )
socialsupportTN.13[socialsupportTN.13 >= 1] <- 1




#################################### Network Construction: Composite Social Support Network (2017) #################################### 
socialsupportTN.17 <- (TN.Nets[["ImpIss_2017"]] # 11. If you wanted to discuss important and confidential matters, who would you talk to? [ImpIss_2017]
                        + TN.Nets[["Work_2017"]] # 12. If you needed more or new wage work or a salaried job, who could you ask for help finding it? [Work_2017]
                        + TN.Nets[["TasksU_2017"]] # 16. Union of ["TasksF_2017"] and ["TasksM_2017"]; See above.
                        + TN.Nets[["ItemBorrow_2017"]] # 3. For your basic essentials such as rice, sugar, oil and other groceries and household needs, who could you immediately ask and get? [ItemBorrow_2017]
                        + TN.Nets[["LoanAsk_2017"]] # 1. If you had an unexpected emergency expense, such as a hospital medical treatment, from whom could you get a loan of 2000 Rs [a weeks wages] or more? [LoanAsk_2017]
                        + TN.Nets[["TalkU_2017"]] # 17. Union of ["TalkF_2017"] and ["TalkM_2017"]; See above.
) 

socialsupportTN.17[socialsupportTN.17 >= 1] <- 1

print(table(socialsupportTN.13, socialsupportTN.17))


#################################### Network Construction: Friendship #################################### 
friendshipTN.13 <- TN.Nets[["Close_2013"]] # 11. Who are your very close friends or relatives? [Close_2013]



######################################### Variable Construction: Inter-household Geographic Distance (Meters) #########################################
interhousehold.dist.TN.13 <- matrix(data = 0, nrow = length(individuals.TN.13$IndivID), ncol = length(individuals.TN.13$IndivID))
rownames(interhousehold.dist.TN.13) <- as.character(individuals.TN.13$GPS_2013) ### N.B. This creates an appropriate matrix for the dyadic covariate only when all adjacency matrices are in the same order as individuals.Ala.13
colnames(interhousehold.dist.TN.13) <- as.character(individuals.TN.13$GPS_2013)

for(i in 1:nrow(interhousehold.distance)){

  HH1 <- interhousehold.distance[i, 1] ## GPS1
  HH2 <- interhousehold.distance[i, 2] ## GPS2
  
  interhousehold.dist.TN.13[which(rownames(interhousehold.dist.TN.13) == HH1), which(colnames(interhousehold.dist.TN.13) == HH2)] <- interhousehold.distance[i, 3]
  
}
rm(i, HH1, HH2)

rownames(interhousehold.dist.TN.13) <- as.character(individuals.TN.13$IndivID)
colnames(interhousehold.dist.TN.13) <- as.character(individuals.TN.13$IndivID)

print(table(interhousehold.dist.TN.13 == t(interhousehold.dist.TN.13))) ## Sanity Check





