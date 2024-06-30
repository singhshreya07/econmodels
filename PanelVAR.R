#INSTALLING AND LOADING THE PACKAGE
install.packages("panelvar")
library(panelvar)
citation("panelvar")

#IMPORTING THE DATASET
df <- read.csv(file.choose())
df

#RUNNING A PANEL VAR
#gmm - method of moments for endogenicity 
varone <-pvargmm(
  dependent_vars = c("LWAGE","LOGWKS","EXP"),
  lags = 1,
  exog_vars = c("INDB"),
  transformation = "fd", #first difference transformation
  data = df,
  panel_identifier = c("ID", "YEAR"),  #what sort of variables determine the cross-sectional dimension and the time series dimension of dataset
  steps = c("twostep"), #two step gmm
  system_instruments = TRUE,
  max_instr_dependent_vars = 99,
  min_instr_dependent_vars = 2L,
  collapse = FALSE
)
summary(varone)


#RUNNING DIAGNOSTICS
Andrews_Lu_MMSC(varone)  #generates multivariate Aikaike/BIC/MIC test
stab_varone <- stability(varone)
print(stab_varone)
#PVAR satisfies the stability conditionas all eigenvalues lie inside the unit circle
plot(stab_varone)


#GENERATING IRFs

varone_oirf <- oirf(varone, n.ahead = 4) #generating projections for 4 years ahead
varone_girf <- girf(varone, n.ahead = 4, ma_approx_steps = 4)

varone_bs <- bootstrap_irf(varone, typeof_irf = c("GIRF"), n.ahead = 4, nof_Nstar_draws = 10, confidence.band = 0.95, mc.cores = 1)
#to bootstrap to essentially show the confidence interval in my particular graph
#people usually include this just for robustness
plot(varone_girf, varone_bs)

#interpretation: wages do not cause each other generally, so the results are somewhat reasonable
#