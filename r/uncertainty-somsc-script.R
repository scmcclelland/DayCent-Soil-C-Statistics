# file name:    uncertainty-somsc-script.R
# created:      16 November 2024
# last updated: 20 January 2026

# description: This file contains a script to complete SOC uncertainty analysis on global
#              DayCent simulations on a local machine.
#              It does the following:
#              1. creates look up table of 1001 simulated betas from LME
#              2. estimates uncertainty for each crop, irr, gridid
#              Runs with a bash script
#-----------------------------------------------------------------------------------------
# LIBRARIES
#-----------------------------------------------------------------------------------------
library(data.table)
library(stringr)
options(scipen = 999, digits = 4)
#-----------------------------------------------------------------------------------------
# ARGUMENTS
#-----------------------------------------------------------------------------------------
args   = commandArgs(trailingOnly = TRUE)
if (isFALSE(length(args) == 3))
  stop(
    'Need 3 command-line arguments (1. Scenario selection for uncertainty, 2. Crop, 3. Irrigation).'
  )
# args[1] Scenario selection (conv, res, ntill, ccg, ccl, ntill-res, ccg-res, ccl-res, ccg-ntill, ccl-ntill, rewild)
# args[2] Crop selection ('maiz','soyb','swht','wwht')
# args[3] Irrigation selection (0,1)
#-----------------------------------------------------------------------------------------
# DIRECTORIES & FUNCTIONS
#-----------------------------------------------------------------------------------------
dir = getwd()
print(dir)
dir = str_split(dir, '/r')
dir = dir[[1]][1]

setwd(dir)
source("test-data/LME-data/SimBetas_func.R")
LME_data = paste(dir, 'data/LME-data', sep = '/')
m_data   = paste(dir, 'data/uncertainty-input', sep = '/') 
o_data   = paste(dir, 'data/uncertainty-output', sep = '/') 
#-----------------------------------------------------------------------------------------
# Set MC conditions
#-----------------------------------------------------------------------------------------
# number of replicates   
nrep   = 1001
# run
crop_n = args[2]
irr_l  = args[3]

# scenario
scen   = args[1]
f      = paste('historical', scen, 'SOC-results.csv', sep ='-')
#-----------------------------------------------------------------------------------------
# Functions
#-----------------------------------------------------------------------------------------
# Calculate dSOC (g m-2 yr-1), annual
delta_soc                 = function(x) {
  delta_soc               = x - data.table::shift(x)
}
#-----------------------------------------------------------------------------------------
# DayCent data
#-----------------------------------------------------------------------------------------
SOC_dt = fread(paste(m_data, f, sep ='/'))
gc()

# restrict to complete runs (20)
rn     = 20L
SOC_dt = SOC_dt[run_yrs == rn,] # 2016-2035 (2016 is inclusive)
gc()

# restrict to scenario
SOC_dt = SOC_dt[scenario %in% scen,]
setorder(SOC_dt, gridid)
#-----------------------------------------------------------------------------------------
# LME Model Estimates
#-----------------------------------------------------------------------------------------
soc_beta   = fread(paste(LME_data, 'SOC_Beta.csv', sep = '/'))
soc_cov    = fread(paste(LME_data, 'SOC_Cov.csv', sep = '/'))
minmax_soc = fread(paste(LME_data, 'SOC_MinMax.csv', sep = '/'))

# extract values
{
    soc_cmb_beta        = soc_beta[, 2][[1]] # vector of values
    names(soc_cmb_beta) = soc_beta[, 1][[1]] # names
  
    soc_cmb_cov           = soc_cov[, -1]      # drop first col
    rownames(soc_cmb_cov) = soc_cov[, 1][[1]]  # keep row name
    colnames(soc_cmb_cov) = soc_cov[, 1][[1]]  # keep col name
    
    # Simulate betas 
    soc_sim_beta = sim.betas(fitted.betas = soc_cmb_beta,  
                             covariance   = soc_cmb_cov,  
                             nreps        = nrep, 
                             iseed        = 11162024)
    
}
gc()
#-----------------------------------------------------------------------------------------
# Monte Carlo Simulation
#-----------------------------------------------------------------------------------------
  for (c in crop_n) {
    for (ir in irr_l) {
      # get unique dt of grid
      grs = unique(SOC_dt[crop %in% c &
                                irr %in% ir, gridid])
      for(id in grs) {
        ## Create Data Table  ##
        # create MC dt
        dt_s = SOC_dt[crop %in% c &
                          irr %in% ir &
                          gridid %in% id,]
        {
        dt_c = data.table(gcm = rep('historical',nrep))
        # create dt table with responses for selected climate for each iteration
        dt = rbindlist(lapply(dt_c[, gcm], function(cl) {
          dt_s[gcm %in% cl,]
        }))
        # bind to rep
        reps = data.table(rep = rep(1:nrep,rn))
        setorder(reps, rep)
        dt = cbind(dt, reps)
        setcolorder(dt, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 'gcm',
                          'time', 'run_yrs', 'rep'))
        }
        ## Empty dt for storing results by gridid ##
        adj_somsc_gr_dt = data.table()
        
        ## Model Creation - Monte Carlo  ##
          print(paste0('Running uncertainty iteration for ', scen, ' ', c, ' ',
                       ir, ' gridid ', id, '.'))
          {
            adj_somsc_list = vector("list", nrep) 
            
            # Prepare covariates
            SOC_fmtd = data.table(
              "reps"        = sort(rep(1:nrep,rn)),
              "(Intercept)" = 1,
              "ln_sim"      = log(dt$somsc) # convert to log
            )
          }
          ## Calculate adjusted SOC with variability for each replicate ##
          {
            for (n in 1:nrep) {
              SOC_fmtd_n = SOC_fmtd[reps %in% n, c("(Intercept)", "ln_sim")]
              adj_somsc_list[[n]] = exp(as.matrix(SOC_fmtd_n) %*% soc_sim_beta[n, ]) # get diff beta
            }
            # Combine results into a data frame then data.table
            adj_somsc_dt           = do.call(rbind, adj_somsc_list)
            adj_somsc_dt           = setDT(as.data.table(adj_somsc_dt))
            setnames(adj_somsc_dt, "V1", "adj_somsc")
          }
          ## Create Uncertainty Table ##
          { # keep headers
          dt[, somsc   := NULL]
          dt[, run_yrs := NULL]
          # join
          adj_somsc_dt = cbind(dt, adj_somsc_dt)

          ## Calculate cumulative change over time ##
          adj_somsc_dt[, delta_adj_somsc := delta_soc(adj_somsc), by = .(gridid, crop, 
                                                                         scenario, irr, 
                                                                         ssp, gcm, rep)]
          adj_somsc_dt[, delta_adj_somsc := replace(delta_adj_somsc, is.na(delta_adj_somsc), 0)]
          
          # Calculate dSOC (g C m-2), cumulative
            { # 10-yrs
            adj_somsc_10dt = copy(adj_somsc_dt)
            adj_somsc_10dt = adj_somsc_10dt[time <= 2025,]
            adj_somsc_10dt[, delta_adj_somsc_sum := round(cumsum(delta_adj_somsc), digits = 2), 
                                          by = .(gridid, crop, scenario, irr, ssp, gcm, rep)]
            # subset years for output
            adj_somsc_10dt = adj_somsc_10dt[time %in% c(2025)]
            # clean table
            adj_somsc_10dt = adj_somsc_10dt[, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 
                                            'gcm', 'time', 'rep', 'delta_adj_somsc_sum')]
            setcolorder(adj_somsc_10dt, c('gridid', 'crop', 'irr'))
            setnames(adj_somsc_10dt, 'delta_adj_somsc_sum', 's_somsc')
            adj_somsc_gr_10dt = rbind(adj_somsc_gr_dt, adj_somsc_10dt)
            print(paste0('Writing 10-yr uncertainty results for ', scen, ' ', c, ' ',
                         ir, ' gridid ', id, ' to csv file.'))
            fwrite(adj_somsc_gr_10dt, file = paste0(o_data, '/10-yr/s_somsc-10yr-uncertainty-',scen,'-',c,
                                                  '-',ir,'.csv'),
                   append = TRUE)
            }
            { # 20-yrs
            adj_somsc_dt[, delta_adj_somsc_sum := round(cumsum(delta_adj_somsc), digits = 2), 
                         by = .(gridid, crop, scenario, irr, ssp, gcm, rep)]
            # subset years for output
            adj_somsc_dt = adj_somsc_dt[time %in% c(2035)]
            # clean table
            adj_somsc_dt = adj_somsc_dt[, c('gridid', 'crop', 'scenario', 'irr', 'ssp', 
                                            'gcm', 'time', 'rep', 'delta_adj_somsc_sum')]
            setcolorder(adj_somsc_dt, c('gridid', 'crop', 'irr'))
            setnames(adj_somsc_dt, 'delta_adj_somsc_sum', 's_somsc')
            adj_somsc_gr_dt = rbind(adj_somsc_gr_dt, adj_somsc_dt)
            print(paste0('Writing 20-yr uncertainty results for ', scen, ' ', c, ' ',
                         ir, ' gridid ', id, ' to csv file.'))
            fwrite(adj_somsc_gr_dt, file = paste0(o_data, '/20-yr/s_somsc-20yr-uncertainty-',scen,'-',c,
                                                  '-',ir,'.csv'),
                   append = TRUE)
          }
        }
      }
    }
  }
