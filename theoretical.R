#Load required libraries
library(ggplot2)
library(deSolve)
library(reshape2)

# Model inputs

initial_state_values=c(S = 241, E = 0, I = 1, R = 0)
parameters=c(beta = 15*4*10^(-5), sigma = 1 / (2*24*60*60/20), gamma = 1 / (4*24*60*60/20))

# Time points

time=(timels - 30620)/20

# SEIR model function 

seir_model <- function(time,state,parameters){
  with(as.list(c(state,parameters)),{
    N=S+E+I+R
    lambda=beta*(I/N) 
    dS=-lambda*S
    dE=lambda*S-sigma*E
    dI= sigma*E - gamma*I
    dR=gamma*I
    
    return(list(c(dS,dE,dI,dR)))
  }
  )
}


#Solving the differential equations
output<-as.data.frame(ode(y=initial_state_values,func = seir_model,parms=parameters,times = time))


out_long = melt(output,id="time")
ggplot() + 
  geom_line(data = out_long[out_long$variable == "S", ], 
            aes(x = time, y = 242 - value)) + 
  xlab("Time (per 20 seconds)") + 
  ylab("The cases including E, I, R groups")


# To plot the proportion of susceptible, infected and recovered individuals over time
sub <- out_long[out_long$variable != "S", ]
ggplot(data = out_long[out_long$variable != "S", ],          
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() +xlab("Time (days)")+ylab("Proportion of the population")+scale_color_discrete(name="State")
view raw
