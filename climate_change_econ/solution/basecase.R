########################################
# PROJECT: CLIMATE CHANGE ECONOMICS    #
# ASSIGNMENT                           #
# BASE CASE                            #
#                                      #
# NABIG CHAUDHRY                       #
########################################


#####################################################################################################
# SET WORKING DIRECTORY
#####################################################################################################

setwd("/") # R Skill: Setting working directory

source("source/helper_functions.R") # R Skill: sourcing (in this case, a function)

#####################################################################################################
# READ IN MODEL INPUT DATA
#####################################################################################################

#Set RCP scenario (current)
#rcp_scenario = "rcp85"
rcp_scenario = "rcp6"

#Read RCP files 
raw_radforc = read.csv(file.path("data", paste(rcp_scenario, "_radiativeforcing.csv", sep="")), skip=58)
raw_emiss   = read.csv(file.path("data", paste(rcp_scenario, "_emission.csv", sep="")), skip=36)
raw_conc    = read.csv(file.path("data", paste(rcp_scenario, "_concentration.csv", sep = "")), skip = 37)

#Read SSP4 scenario data
raw_ssp_scen = read.csv(file.path("data", paste("SSP4 Scenario Data.csv", sep="")), skip=15)

#Read Backstop data
backstop_data = read.csv(file.path("data", "backstop.csv"))

#Read SCC data
scc_data = read.csv(file.path("data", "scc.csv"))

#####################################################################################################
# MANIPULATE MODEL INPUT DATA
#####################################################################################################

#Set start & end year of model
start_year = 2010
end_year   = 2300
years = seq(2010, 2300)

#Isolate data for years
radforc = raw_radforc[raw_radforc[,"YEARS"] >= start_year & raw_radforc[,"YEARS"] <= end_year, ]
emiss   = raw_emiss[raw_emiss[,"YEARS"] >= start_year & raw_emiss[,"YEARS"] <= end_year, ]
conc    = raw_conc[raw_conc[,"YEARS"] >= start_year & raw_conc[,"YEARS"] <= end_year, ]
kaya = raw_ssp_scen[raw_ssp_scen[,"year"] >= start_year & raw_ssp_scen[,"year"] <= end_year, ]
backstop_data = backstop_data[backstop_data[,"Year"] >= start_year & backstop_data[,"Year"] <= end_year, ]
scc = scc_data[scc_data[,"Year"] >= start_year & scc_data[,"Year"] <= end_year, ]

#Isolate data from KAYA
pop = kaya[, "population"]
gdp = kaya[, "pc_gdp"]
ei = kaya[, "energy_intensity"]
ci = kaya[, "carbon_intensity"]
tfp = kaya [, "tfp"]

#Isolate scc data
scc = scc[,"SCC"]

#Subtract CO2 RF from total anthropogenic RF to avoid double counting
exogenous_rf = radforc[,"TOTAL_INCLVOLCANIC_RF"] - radforc[,"CO2_RF"]

#Add fossil fuel and land use change + other sources CO2 emissions together
other_co2_emissions = emiss[,"OtherCO2"]
#anthrop_co2_emissions = (emiss[,"FossilCO2"] + emiss[,"OtherCO2"])

#Get N2O concentrations (used in CO2 radiative forcing calculations)
n2o_conc = conc[,"N2O"]

#Manipulate Backstop data
prices = c(rep(backstop_data[,"Backstop"][1], 5), approx(backstop_data[, "Year"], backstop_data[, "Backstop"], xout=seq(2010, 2295))$y)
backstop_interpolated=data.frame(years, prices)

#######################################################################################################
# SET MODEL PARAMETER VALUES
########################################################################################################

# Number of timesteps to run model
n_steps = length(start_year:end_year)

#Carbon cycle parameters
co2_0   = 278.0                                 # Pre-industrial atmospheric concentration of CO2.
r0      = 32.4                                  # Pre-industrial iIRF100. Table 1 of Millar et al, which I will give to students.
rc      = 0.019                                 # Increase in iIRF100 with cumulative carbon uptake (yr/GtC). Table 1 of Millar et al, which I will give to students.
rt      = 4.165                                 # Increase in iIRF100 with warming (yr/C). Table 1 of Millar et al, which I will give to students.
a       = c(0.2173, 0.2240, 0.2824, 0.2763)     # Fraction of emissions entering each carbon pool (geological reabsorption[1], deep ocean invasion/equilibration[2], biospheric uptake/ocean thermocline invasion[3], rapid biospheric uptake/ocean mixed-layer invasion[4]).
tau     = c(10.0^6, 394.4, 36.54, 4.304)       # Decay time constants for each carbon pool in 'a'. Table 1 of Millar et al, which I will give to students.
ppm2gtc = 2.123                                 # Conversion factor between ppm and GtC (with 1 ppm = 2.123 GtC). 

#Climate dynamics parameters
a1    = -2.4e-7         # CO2 raditive forcing constant. (HINT: see Etminan et al., paper, Table 1 which I will give to students.
b1    = 7.2e-4          # CO2 raditive forcing constant.(HINT: see Etminan et al., paper, Table 1 which I will give to students.)
c1    = -2.1e-4         # CO2 raditive forcing constant.(HINT: see Etminan et al., paper, Table 1 which I will give to students).
n2o_0 = 272.95961       # Pre-industrial atmospheric concentration of N2O (HINT: you data sources include N20 concentration in ppb, define pre-industrial as 1765)
q     = c(0.33, 0.41)   # q1 (thermal equilibration of deep ocean) & q2 (thermal adjustment of upper ocean) in KW-1 m2. (HINT: see Table 1 of Millar et al)
#q     = c(0.14, 0.26)  # optimistic
#q     = c(0.57, 0.63)  # pessimistic
d     = c(239.0, 4.1)   # d1 (thermal equilibration of deep ocean) & d2 (thermal adjustment of upper ocean) in years. (HINT: see Table 1 of Millar et al)

#Economic output parameters
alpha = .3
depreciation = .1
savings = .22

#Damage function parameters
dam1 = 0.000
dam2 = 0.00236
dam3 = 2.00
elasticity = 0
y0 = 70115

#####################################################################################################
# MODEL
#####################################################################################################
#Turn model into function to run multiple scenarios for policy questions

run_model = function(co2_policy){
    
    #Initialize dataframe to store results for each time step (each row is a year, each column is a new variable)
    output = data.frame(matrix(nrow=n_steps, ncol=28))
    
    #Assign each column of 'output' dataframe the name model variable being calculated)
    colnames(output) = c("years","alpha", "r1", "r2", "r3", 
                         "r4", "co2", "cacc","temp_j1", "temp_j2", 
                         "co2_rf", "total_rf", "temperature", "kaya_co2", "total_co2", 
                         "mitigated_kaya_co2", "gross_output", "sigma", "policy_cost_coefficient", "policy_costs_share", 
                         "total_abatement_costs", "net_economic_output", "capital", "investment", "damages_share",
                         "total_damages","consumption", "percap_consumption")
    
    #Assign model years to output data frame (just for convenience)
    output[,"years"] = start_year:end_year
    
    #Loop through the model
    for (t in 1:n_steps){
        
        #Set intial conditions for all components
        if (t==1) {
            
            ###Intial carbon cycle###
            
            #Kaya identity
            output[t, "kaya_co2"] = 8.014287
            output[t, "mitigated_kaya_co2"] = 8.014287
            output[t, "total_co2"] = 9.209787
            
            output[t, "sigma"] = (ci[t] * ei[t] * 1000 * (44/12))
            output[t,"policy_cost_coefficient"] = backstop_interpolated[t, "prices"] * output[t, "sigma"] / 2.6 / 1000
            
            #Add in columns that calculate CO2 emissions 
            output[t, "r1"] = 45.9128 * output[t, "total_co2"] / ppm2gtc * 0.5
            output[t, "r2"] = 30.7770 * output[t, "total_co2"] / ppm2gtc * 0.5
            output[t, "r3"] = 10.4211 * output[t, "total_co2"] / ppm2gtc * 0.5
            output[t, "r4"] = 2.08870 * output[t, "total_co2"] / ppm2gtc * 0.5
            
            #Initial state-dependent scaling factor
            output[t,"alpha"] = .2968
            
            #Initial atmospheric CO2 concentration
            output[t,"co2"] = 367.1996
            
            #Initial carbon stock perturbation
            output[t,"cacc"] = 263.8763
            
            
            ###Intial Climate Dynamics###
            
            output[t,"co2_rf"] = 1.4916
            output[t,"total_rf"] = 1.6579
            
            #Initial temperature change for two reponse times.
            output[t,"temp_j1"] = .0747
            output[t,"temp_j2"] = .6305
            
            #Set initial surface temperature anomaly to 0.0
            output[t,"temperature"] = 0.7051
            
            #Solow component
            output[t, "capital"] = 135000
            output[t, "investment"] = 14726.14515
            output[t, "gross_output"] = tfp[t] * (output[t, "capital"] ** alpha) * (pop[t] ** (1 - alpha))
            
            ###Damage Function###
            
            #Dice damage function
            output[t, "damages_share"] = (dam2 * output[t,"temperature"]^dam3)* ((output[t, "gross_output"] / y0)^elasticity)
            
            #Weitzman damage function
            #output[t, "damages_share"] = 1 - (1/(1+(output[t,"temperature"]/20.46)^2 + (output[t,"temperature"]/6.081)^6.754))
            
            #Total damages
            output[t, "total_damages"] = output[t, "damages_share"] * output [t, "gross_output"]
            
            #Economic policy  
            output[t, "policy_costs_share"] = output[t,"policy_cost_coefficient"] * co2_policy[t] ^ 2.6
            output[t, "total_abatement_costs"] = output[t, "gross_output"] * output[t, "policy_costs_share"]
            output[t, "net_economic_output"] = output[t, "gross_output"] - output[t, "total_abatement_costs"] - output[t, "total_damages"]
            output[t, "investment"] = savings * output[t, "net_economic_output"]
            output[t, "consumption"] = output[t, "net_economic_output"] - output[t, "investment"]
            output[t, "percap_consumption"] = (output [t, "consumption"])/(pop[t])
            
            
        } else {
            
            ###CO2 Emissions Equations###
            output[t, "kaya_co2"] = (pop[t] * gdp[t] * ei[t] * ci[t]) + scc[t]
            output[t, "mitigated_kaya_co2"] = (1-co2_policy[t]) * output[t, "kaya_co2"]
            output[t, "total_co2"] = output[t, "mitigated_kaya_co2"] + other_co2_emissions[t]
            
            
            ###Carbon Cycle Equations###
            
            #Equation 8 in FAIR
            iirft100 = r0 + rc * output[t-1,"cacc"] + rt * output[t-1,"temperature"]
            
            #Set an upper bound to avoid unstable/non-physical results
            if (iirft100 >= 97.0) {
                #Set alpha to it's limiting value
                output[t,"alpha"] = 113.7930278
            } else {
                # Solve for alpha, given current state of carbon and climate systems
                output[t,"alpha"] = find_alpha(iirft100, a, tau, output[t-1,"alpha"])
            }
            
            #Updated carbon cycle time constants and CO2 concentrations in 4 carbon pools
            for (i in 1:4){
                output[t, paste("r",i,sep="")] = output[t-1, paste("r",i,sep="")] * exp((-1.0/(tau[i]* output[t,"alpha"]))) + 0.5 * a[i] * (output[t, "total_co2"] + output[t-1, "total_co2"]) / ppm2gtc
            } 
            
            #Change in CO2 concentrations across all pools and the current atmospheric concentration
            output[t,"co2"] = sum(output[t, c("r1","r2","r3","r4")]) + co2_0
            
            #Accumulated perturbation of carbon stock
            output[t,"cacc"] = output[t-1,"cacc"] + output[t, "total_co2"] - (output[t,"co2"] - output[t-1,"co2"]) * ppm2gtc
            
            
            ###Climate Dynamic Equations###
            
            #N_hat term and difference in CO2 concentrations as temporary variables (for convenience)
            n_hat = 0.5 * (n2o_conc[t] + n2o_0)
            co2_diff = output[t,"co2"] - co2_0
            
            #CO2 radiative forcing
            output[t,"co2_rf"] = (a1*co2_diff^2 + b1*abs(co2_diff) + c1*n_hat + 5.36) * log(output[t,"co2"] / co2_0)
            
            #Total radiative forcing.
            output[t,"total_rf"] = output[t,"co2_rf"] + exogenous_rf[t]
            
            #Temperature change for the two different thermal response times
            output[t,"temp_j1"] = output[t-1,"temp_j1"] * exp((-1.0)/d[1]) + 0.5 * q[1] * (output[t-1,"total_rf"] + output[t,"total_rf"]) * (1 - exp((-1.0)/d[1]))
            output[t,"temp_j2"] = output[t-1,"temp_j2"] * exp((-1.0)/d[2]) + 0.5 * q[2] * (output[t-1,"total_rf"] + output[t,"total_rf"]) * (1 - exp((-1.0)/d[2]))
            
            #Global mean surface temperature anomaly
            output[t,"temperature"] = output[t,"temp_j1"] + output[t, "temp_j2"]
            
            
            ###Solow Economic Output###
            
            output[t, "capital"] = (output[t-1, "capital"] * (1-depreciation)) + output[t-1, "investment"]
            output[t, "gross_output"] = tfp[t] * (output[t, "capital"] ** alpha) * (pop[t] ** (1 - alpha))
            
            
            ###CO2 Policy Equations###
            
            output[t, "sigma"] = (ci[t] * ei[t] * 1000 * (44/12))
            output[t,"policy_cost_coefficient"] = backstop_interpolated[t, "prices"] * output[t, "sigma"] / 2.6 / 1000
            
            ###Damage Function###
            
            #Dice damage function
            output[t, "damages_share"] = (dam2 * output[t,"temperature"]^dam3)* ((output[t, "gross_output"] / y0)^elasticity)
            
            #Weitzman damage function
            #output[t, "damages_share"] = 1 - (1/(1+(output[t,"temperature"]/20.46)^2 + (output[t,"temperature"]/6.081)^6.754))
            
            #Total damages
            output[t, "total_damages"] = output[t, "damages_share"] * output [t, "gross_output"]
            
            #Economic policy  
            output[t, "policy_costs_share"] = output[t,"policy_cost_coefficient"] * co2_policy[t] ^ 2.6
            output[t, "total_abatement_costs"] = output[t, "gross_output"] * output[t, "policy_costs_share"]
            output[t, "net_economic_output"] = output[t, "gross_output"] - output[t, "total_abatement_costs"] - output[t, "total_damages"]
            output[t, "investment"] = savings * output[t, "net_economic_output"]
            output[t, "consumption"] = output[t, "net_economic_output"] - output[t, "investment"]
            output[t, "percap_consumption"] = (output [t, "consumption"])/(pop[t])
            
        } 
    }
    return(output)
}