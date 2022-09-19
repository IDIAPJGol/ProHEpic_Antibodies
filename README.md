# ProHEpic_Antibodies
Code for the analysis of antibody levels from the ProHEpic study. The results of this study can be found in 
Violán, C., Torán-Monserrat, P., Quirant, B. et al. Kinetics of humoral immune response over 17 months of COVID-19 pandemic in a large cohort of healthcare workers in Spain: the ProHEpiC-19 study. BMC Infect Dis 22, 721 (2022). https://doi.org/10.1186/s12879-022-07696-6

## Use of synthetic data
Synthetic data have been generated using the R package synthpop to be able to run the scripts. These data are similar to the original when aggregated, but the temporal relationship of the samples of the same individuals does not hold. For this reasons, scripts that generate Figures 2 and 3 do not work, as the hyperparameters of the NLME models have been left as in the published models.

## Summary of tables and figures
Please, find more details about the calculation of each table and figure in their corresponding caption.

* Table 1: Demographic description and PCR testing for the study participants according to their clinical condition.
* Table 2: Description (N, %) of the results of the antibodies tests through the follow-up period.
* Table S1: Number of available samples of each SARS-CoV-2 antibody per assessment timepoint. 
* Table S2: Description (N, %) of the main symptoms in participants according to disease severity and sex assigned at birth.
* Table S3: Parameter estimation for SARS-CoV-2 antibodies (IgM(N), IgG(N), IgG(S)) NLME models. 

* Figure 1: Description of the IgM (N), IgG (N) ang IgG(S) levels, by days from diagnosis.
* Figure 2: Decay of IgM(N), IgG(N) and IgG(S) levels since diagnosis in total sample and by clinical condition.
* Figure 3: Decay of IgM(N), IgG(N) and IgG(S) levels ince diagnosis, both aggregated and stratified by sex.
* Figure S1: Flow chart of the participant sample of the ProHEpic study. There is no script for that as it was created using PowerPoint.
* Figure S2: Q-Q plots of the random effects of the NLME models.

## Notes
There is no script of its own to generate Table S3 because the necessary information to create it is generated when generating figures 2 and 3.
