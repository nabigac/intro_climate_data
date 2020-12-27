########################################
# PROJECT: CLIMATE CHANGE ECONOMICS    #
# ASSIGNMENT                           #
# POLICY                               #
#                                      #
# NABIG CHAUDHRY                       #
########################################

#####################################################################################################
# SET WORKING DIRECTORY
#####################################################################################################

setwd("/")

source("solution/basecase.R")

#####################################################################################################
# MANIPULATE BASECASE DATA FOR GRAPHING
#####################################################################################################

co2_policy_scenarios = read.csv(file.path("/data", paste("co2_policy_4_scenarios.csv", sep = "")), skip=0)

#Separating out the scenarios
co2_policy = co2_policy_scenarios$no_policy

#Running model with and without SCC
my_results = run_model(co2_policy)
my_results_scc = run_model(co2_policy)

#Calculating marginal damage
marginal_damage = my_results_scc$total_damages-my_results$total_damages

#Discount rate
dr_2.5 = rep(0, length(seq(291)))
dr_3 = rep(0, length(seq(291)))
dr_5 = rep(0, length(seq(291)))

for (i in seq(2010,2300)){
  dr_2.5[i-2009] = 1/((1+0.025)^(i-2010))
  dr_3[i-2009] = 1/((1+0.03)^(i-2010))
  dr_5[i-2009] = 1/((1+0.05)^(i-2010))
}

#Ramsey parameters
prt_1 = .01
prt_0 = 0
prt_3 = .03
smile = 1
c = my_results$consumption
cgr = rep(0, 291)
for (t in 1:n_steps){
  if (t>1)
    cgr[t] = (c[t] - c[t-1]) / c[t]}

#Ramsey rate
ramsay_1 = prt_1 + cgr[t]
ramsay_0 = prt_0 + cgr[t]
ramsay_3 = prt_3 + cgr[t]

t = start_year:end_year
r_0 = 1/((1+ramsay_0)^(t-start_year))
r_1 = 1/((1+ramsay_1)^(t-start_year))
r_3 = 1/((1+ramsay_3)^(t-start_year))

#Calculating NPV for discount rate of 2.5%
dr_2.5_npv = marginal_damage*dr_2.5
dr_2.5_total_npv = sum(dr_2.5_npv)
dr_2.5_co2_equiv = sum(dr_2.5_npv)*12/44

#Calculating NPV for discount rate of 3%
dr_3_npv = marginal_damage*dr_3
dr_3_total_npv = sum(dr_3_npv)
dr_3_co2_equiv = sum(dr_3_npv)*12/44

#Calculating NPV for discount rate of 5%
dr_5_npv = marginal_damage*dr_5
dr_5_total_npv = sum(dr_5_npv)
dr_5_co2_equiv = sum(dr_5_npv)*12/44

#Calculating NPV for Ramsey rate of 1%
r_1_npv = marginal_damage*r_1
r_1_total_npv = sum(r_1_npv)
r_1_co2_equiv = sum(r_1_npv)*12/44

#Calculating NPV for Ramsey rate of 0%
r_0_npv = marginal_damage*r_0
r_0_total_npv = sum(r_0_npv)
r_0_co2_equiv = sum(r_0_npv)*12/44

#Calculating NPV for Ramsey rate of 3%
r_3_npv = marginal_damage*r_3
r_3_total_npv = sum(r_3_npv)
r_3_co2_equiv = sum(r_3_npv)*12/44

#Print out results
print(dr_2.5_co2_equiv)
print(dr_3_co2_equiv)
print(dr_5_co2_equiv)
print(r_0_co2_equiv)
print(r_1_co2_equiv)
print(r_3_co2_equiv)

#Graph all the lines
plot(my_results$years, marginal_damage, xlim = c(2010, 2300), ylim = c(0,20), type="l", col="blue", xlab="Year", ylab="Damgages ($)", main="Marginal Damages Over Time")
lines(my_results$years, dr_2.5_npv, type="l", col="red", xlab="Year", ylab="Damgages ($)", main="Marginal Damages Over Time")
lines(my_results$years, dr_3_npv, type="l", col="red", xlab="Year", ylab="Damgages ($)", main="Marginal Damages Over Time")
lines(my_results$years, dr_5_npv, type="l", col="red", xlab="Year", ylab="Damgages ($)", main="Marginal Damages Over Time")
lines(my_results$years, r_1_npv, type="l", col="purple", xlab="Year", ylab="Damgages ($)", main="Marginal Damages Over Time")
lines(my_results$years, r_0_npv, type="l", col="purple", xlab="Year", ylab="Damgages ($)", main="Marginal Damages Over Time")
lines(my_results$years, r_3_npv, type="l", col="purple", xlab="Year", ylab="Damgages ($)", main="Marginal Damages Over Time")
legend(x=2000, y=21, legend=c("undiscounted", "discount rate", "ramsay discount"), col=c("blue", "red", "purple"), lty=c(1,1))




