library(triangle)
library(MonteCarlo)
library(dplyr)
library(plyr)
library(ggplot2)
library(lifecontingencies)
library(ggridges)
library(highcharter)
library(fitdistrplus)

### Options
options(scipen=999)


### Vector Parameters To EValuate
vector_size<-10

###  Reach By Channel (Percentage of Audience)
reach_1<- .1  
reach_2<- .2
reach_3<- .3
reach_4<- .4


### Variance Parameters (Enter For Variance)
min_variance <- .95
max_variance <- 1.05

### Monte Carlo Simulation Sainsbury


monte_carlo_simulation_sainsbury4 <- function(min_variance, max_variance)  {
  
  ### Reach Vectors
  reach1_sim <-runif(1, min_variance, max_variance)
  reach2_sim <-runif(1, min_variance, max_variance)
  reach3_sim <-runif(1, min_variance, max_variance)
  reach4_sim <-runif(1, min_variance, max_variance)
  
  ### Reach Vectors
  reach1_vector <-(reach1_sim * reach_1)
  reach2_vector <-(reach2_sim * reach_2)
  reach3_vector <-(reach3_sim * reach_3)
  reach4_vector <-(reach4_sim * reach_4)

  
 reach_sainsbury <-as.numeric((1-reach1_vector/100) *(1-reach2_vector/100)*(1-reach3_vector/100) * (1- reach4_vector))

  
  return(list("reach_sainsbury"= reach_sainsbury))
 
}

### Parameters For Monte Carlo Simulation
min_variance <-seq(.90,1,0.01)
max_variance <-seq(1,1.1,0.01)

### Parameter List
param_list4=list("min_variance"= min_variance, "max_variance" = max_variance)

### Number of Monte Carlo RUns
NREP<-1000
max_grid<-100000000

### Monte Carlo Simulation
MC_result<-MonteCarlo(func=monte_carlo_simulation_sainsbury4, nrep=NREP, param_list= param_list4, max_grid = max_grid)


monte_carlo_simultation_DF <-MakeFrame(MC_result)
head(monte_carlo_simultation_DF)

#nrow(monte_carlo_simultation_DF)

### Descriptive STATs
max_reach_sainsbury4 <-max(monte_carlo_simultation_DF$reach_sainsbury)
min_reach_sainsbury4 <-min(monte_carlo_simultation_DF$reach_sainsbury)
mean_reach_sainsbury4 <-mean(monte_carlo_simultation_DF$reach_sainsbury)
median_reach_sainsbury4 <-median(monte_carlo_simultation_DF$reach_sainsbury)
sim_reach_sainsbury4 <-monte_carlo_simultation_DF$reach_sainsbury

### Paths
gg_plot_monte_carlo_4_Channels <-function(m) {
  
  m_p <-dplyr::percent_rank(m)
  df <-data.frame(m, m_p)
  colnames(df) <-c("Total", "Sainsbury Reach 4")
  
  
   gg_MROI <-ggplot(df, aes(x=Total)) +
     
    geom_density(color="darkblue", fill="lightblue") +
     
    labs(title="Simulation of Sainsbury Reach: Four Channels",
         
         y = "Density of Outcome", x="Sainsbury Reach") +
    
    theme(legend.position="none") + geom_vline(xintercept = mean_reach_sainsbury4 )
  
  return(gg_MROI) 
}

### 
total_MROI_density_GG <-gg_plot_monte_carlo_4_Channels(sim_reach_sainsbury4)
total_MROI_density_GG


### Probability Distribution Function

gg_plot_monte_carlo <-function(m, x,y) {
  
  m_p <-dplyr::percent_rank(m)
  df <-data.frame(m, m_p)
  colnames(df) <-c("Total", "Simulation of Sainsbury Reach: Four Channels")
  
  
  pdf_profit <-ggplot(df, aes(df[,1])) + stat_ecdf(geom = "line") +
    
    geom_hline(aes(yintercept = x , colour='purple2')) +
    
    geom_hline(aes(yintercept = y , colour='purple2')) +
    
    geom_vline(aes(xintercept =mean(Total) , colour='red')) +
    
    geom_vline(aes(xintercept =median(Total) , colour='green')) +
    
    
    labs(title="Simulation of Outcomes: Sainsbury Reach",
         subtitle = "Red=Mean, Blue=Median, and Green=Top 5%",
         
         y = "Cumulatitve Probality", x="Range of Reach") +
    
    theme(legend.position="none")
  
  return(pdf_profit) 
}



###  Higher and Lower Tails
CI_95 <-.95
CI_95_Lower_Tail <-.05

### Graph
total_sainsbury_GG <-gg_plot_monte_carlo(sim_reach_sainsbury4,CI_95,CI_95_Lower_Tail)
total_sainsbury_GG
