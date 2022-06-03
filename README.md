# Dynamics of Cooperative Networks Associated With Gender Among South Indian Tamils (Simpson & Power, Revise & Resubmit, Philos. Trans. R. Soc. Lond., B, Biol. Sci.)

## Abstract
Helping behaviour is thought to play a major role in the genetic and cultural evolution of group-living animals. Yet, it is unclear to what extent human males and human females secure support via the same mechanisms of cooperation. Accordingly, we investigate help seeking vis-à-vis gender using data spanning five years from virtually all adults in two Tamil villages (N = 782). Simulations of network dynamics (i.e., Stochastic Actor-Oriented Models) calibrated to these data indicate that women are more inclined to create and maintain supportive bonds in relation to multiple cooperative mechanisms (e.g., reciprocity, kin bias, friend bias, generalised exchange). However, gender-related differences in the simulated micro-level dynamics of help seeking are modest and do not appear to translate to marked divergence in the structure of the egocentric networks of men and women. Ultimately, findings are consistent with the notion that human males and human females are similarly social. 

<br>

![](https://github.com/cohensimpson/gendernet_PhilTransB/blob/main/F1_Gender_Cooperative_Networks.svg) 

<br> 

**Figure 1.** Stylisation of sex-homogenous one-degree (i.e., one-step) egocentric networks based on evolutionary theorising of male and female sociality as, respectively, “dyadic” and “group-based”. Arcs (i.e., directed connections) indicate hypothetical aid relationships — i.e., to whom does one turn to for help? — and are coloured based on relationship type. Dark-blue arcs emanate from kin, light-blue arcs emanate from friends, and yellow arcs emanate from in-group strangers who are neither kin nor friend. Red vertices (i.e., nodes) indicate ego. Vertices for ego’s alters are coloured to reflect status relative to ego — where darker-coloured vertices are more high-status than ego (i.e., the focal actor), white vertices are more low-status, and grey vertices are of a similar status. **Panel a.** Theorised female egocentric network characterised by low absolute size, low interconnectivity, a large degree of status homogeneity, and no supportive bonds with non-kin and non-friends. **Panel b.** Theorised male egocentric network characterised by large absolute size, high interconnectivity, a large degree of status heterogeneity, and multiple supportive bonds between kin, friends, non-kin, and non-friends. Lengths of arcs, placement of vertices, and spacing between arcs/vertices are purely aesthetic.

<br>
<br> 

![](https://github.com/cohensimpson/gendernet_PhilTransB/blob/main/F5_Gender_Cooperative_Networks.svg) 

<br> 

**Figure 5.** Modest gender-based differences in the simulated micro-level dynamics of help seeking. Here, $x\_{i,j} = 1 = i$ relies on $j$ for help (e.g., food, money, childcare, physical assistance). The hypothesis-specific linear combinations $\hat{\beta}\_{\text{Linear Combination}}$ denote the total contribution to a [utility function](https://doi.org/10.1016/j.socnet.2009.02.004) representing the overall “attractiveness” of creating and maintaining a single outgoing tie $x\_{i,j}$ by a focal actor $i$ (ego) who is either a woman (blue bullets) or man (orange bullets) with identical characteristics (e.g., average age, average wealth, average social status) and identical network positions. Each bullet is a linear combination of parameters estimates implicated in interactions between gender and a hypothesised mechanism of network formation (e.g., Gender, Reciprocity, and Gender $\times$ Reciprocity). Linear combinations are constructed by varying $i$’s out-degree from zero to 32 whilst varying the in-degree of $i$’s potential alter $j$ from zero to 64 (i.e., the observed ranges in our data from 2013). There are 2,145 (33×65) linear combinations for the average man and the average woman for each cooperative mechanism (H1-H7). Tiny bullets are for linear combinations that have a p-value ≥ 0.001. For information on why we test our hypotheses in this manner, see [Brambor et al. (2006)](https://doi.org/10.1093/pan/mpi014) and [Snijders et al. (2010)](https://doi.org/10.1016/j.socnet.2009.02.004).


## Overview of Files in Replication Pack
Here, you will find two R Scripts — one master/main script entitled "Gender_Cooperative_Networks_Analysis_Tamil_Nadu.R" in addition to a companion script "Gender_Cooperative_Networks_DataPrep_Tamil_Nadu.R". The two R scripts are written to be used in conjunction with seven ".csv" files encoding the monadic, dyadic, and network data used to carry out the analyses presented in our paper. 

Because they contain sensitive information (e.g., geographic location) for a large number of adults, we have not posted the ".csv" data files here in our GitHub repository alongside our R code. To access these files, please contact the second author of our paper and the data controller Eleanor A. Power (e.a.power@lse.ac.uk) to sign an ethics and data sharing agreement, cc'ing the first author Cohen R. Simpson (c.r.simpson@lse.ac.uk). After signing this agreement, we are happy to promptly share the data files as well as our complete R workspace which contains objects for the transformed data, objects for the fitted models, and plottable objects for goodness-of-fit tests.


## R Code
We have treated both scripts a bit like running "notebooks". Accordingly, throughout the two scripts — particularly the main script — you will find code to carry out the analyses reported in the paper alongside commands used to produce useful print out (e.g., descriptive statistics, small tables, plots, etc.). Furthermore, we have left extensive comments throughout both scripts that (hopefully) give you insight into the thinking behind the decisions we take. 
 
**_After_** you have placed the seven ".csv" data files and the two R scripts in the same R working directory, installed the necessary packages, and set the number of available computing cores for your machine (see circa Line 54 of the main R script), you should be able to simply hit the "source" button in RStudio or run "source("Gender_Cooperative_Networks_Analysis_Tamil_Nadu.R")" to redo our analyses. This will generate Figure 4, Figure 5, and Figure 6 and also print the numbers used to produce Table 1, SI Table 2, SI Table 3, SI Table 4, SI Table 5, and SI Table 7. Code to generate the values in SI Tables 2, 3, 4, and 5 appear in the main R script circa Lines 2314-2451 — although note that some information reported in SI Table 3 is pulled from reports generated by the R library we use to fit our models (i.e., "RSiena)" when setting up model estimation. Figure 1, Figure 2, Figure 3, SI Table 1, SI Table 6, SI Table and SI Table 8 are all created by hand.


## Executables & Packages
In addition to the R scripts, we have also included in the repository the installation files for the version of the R packages integral to our analysis — i.e, the package "RSiena" (https://github.com/snlab-nl/rsiena/wiki). Note that you may need to first install GCC (https://formulae.brew.sh/formula/gcc) — i.e., the GNU Compiler Collection — before attempting to install RSiena from source. Also, please see the first 50-ish lines of the R scripts for the list of of packages used for our analysis that you will need to install in addition to short notes on dependencies that you may need to address.

Note well that, when re-running our analyses, some numerical results may differ slightly from those reported in the paper. This is due to stochastic perturbations. We have used the same random seed (20180709) to ensure exact reproducibility wherever possible. However, this is not always an option depending on the function. And, unfortunately, when one uses multiple CPU cores, the estimation procedure for Stochastic Actor-Oriented Models ignores one chosen random seed and thus prevents exact reproducibility (see the RSiena source code).


## Summary of Files in Repository

 1) Gender_Cooperative_Networks_Analysis_Tamil_Nadu.R (R script for data preparation and transformation)
 
 2) Gender_Cooperative_Networks_DataPrep_Tamil_Nadu.R (R script for data analyses and plotting)

 3) TN_Indiv.csv (Data on Individual Residents in Village 1 and Village 2 [Restricted Access; Data Sharing/Ethics Agreement Required])
 4) TN_Fam.csv (Data on Family Units/Households in Village 1 and Village 2 [Restricted Access; Data Sharing/Ethics Agreement Required])
 5) TN_Partnerships.csv (Data on Residents' Marital Partnerships [Restricted Access; Data Sharing/Ethics Agreement Required])
 6) TN_Kinship.csv (Data on Residents' Genetic Kin [Restricted Access; Data Sharing/Ethics Agreement Required])
 7) TN_Dist.csv (Data on Geographic Proximity Between Residents' Homes [Restricted Access; Data Sharing/Ethics Agreement Required])
 8) Ten_1317.csv (Network Data for Village 1 [Restricted Access; Data Sharing/Ethics Agreement Required])
 9) Ala_1317.csv (Network Data for Village 2 [Restricted Access; Data Sharing/Ethics Agreement Required])

 10) groundhog-1.5.0.tar.gz ("groundhog" source code used for version control for R libraries)

 11) Ripley et al. - 2021 - Manual for RSiena (v. 1.3.5).pdf [4]

 12) rsiena-1.3.5.tar.gz ("RSiena" source code used to fit Stochastic Actor-Oriented Models)


## Key Citations for Replicators
[1] Simpson, C.R. & Power, E.A. “Dynamics of Cooperative Networks Associated With Gender Amongst South Indian Tamils.” Working Paper.

[2] Power, E.A. (2017). Social Support Networks and Religiosity in Rural South India. Nature Human Behaviour, 1(3), 0057. https://doi.org/10.1038/s41562-017-0057

[3] Power, E.A. & Ready, E. (2019). Cooperation Beyond Consanguinity: Post-Marital Residence, Delineations of Kin and Social Support among South Indian Tamils. Philosophical Transactions of the Royal Society B: Biological Sciences, 374(1780), 20180070. https://doi.org/10.1098/rstb.2018.0070

[4] Ripley, R.M., Snijders, T.A.B., Boda, Z., Vörös, A. & Preciado, P. (2021). Manual for RSiena (v. 1.3.5) [R]. http://www.stats.ox.ac.uk/~snijders/siena/RSiena_Manual.pdf


## Notes
1) Thank you for your interest in our work! Please do let us know if something goes wrong. We are more than happy to help and you can always email us.

