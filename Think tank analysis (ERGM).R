############################
# Think tanks network ERGM # 
############################
# If any questions come up, do feel free to 
# send an Email to timo.damm.bs@icloud.com ~Timo
library(finalfit)
library(statnet)
library(network)
library(sna)
library(mice)
library(scales)
library(GGally)
library(Amelia)

#----data import and general preparations----
net_1998 <- read.csv("/home/td/random_coding/think tank work/data/thinktank_overlap_1998.csv", header=T)
net_2007 <- read.csv("/home/td/random_coding/think tank work/data/thinktank_overlap_2007.csv", header=T)
net_2011 <- read.csv("/home/td/random_coding/think tank work/data/thinktank_overlap_2011.csv", header=T)
nodal_attr_1998 <-read.csv("/home/td/random_coding/think tank work/data/TTattributes1998_reduced.csv", header=T)
nodal_attr_2007 <-read.csv("/home/td/random_coding/think tank work/data/TTattributes2007_reduced.csv", header=T)
nodal_attr_2011 <-read.csv("/home/td/random_coding/think tank work/data/TTattributes2011_reduced.csv", header=T)

#ordering the dataframes
net_1998 <- net_1998[order(net_1998$X),]
net_2007 <- net_2007[order(net_2007$X),]
net_2011 <- net_2011[order(net_2011$X),]
nodal_attr_1998 <- nodal_attr_1998[order(nodal_attr_1998$X),]
nodal_attr_2007 <- nodal_attr_2007[order(nodal_attr_2007$X),]
nodal_attr_2011 <- nodal_attr_2011[order(nodal_attr_2011$X),]

# Check if it is a square matrix 
dim(net_1998)
rownames(net_1998) <- net_1998[,1]
net_1998 <- net_1998[,-1]
dim(net_2007)
rownames(net_2007) <- net_2007[,1]
net_2007 <- net_2007[,-1]
dim(net_2011)
rownames(net_2011) <- net_2011[,1]
net_2011 <- net_2011[,-1]
dim(net_1998)
dim(net_2007)
dim(net_2011)

#----creating the network----
net_1998 <- as.matrix(net_1998, mode = "undirected", weighted = TRUE)
tt_net_1998 <- as.network(net_1998,directed=F, loops = T, multiple = F) 
#explanation of the code: net_1998 is a data.frame when importing as .csv. 
#If read in as a data.frame it creates parallel edges. If imported as a weighted
#matrix however, it creates weighted edges.
is.multiplex(tt_net_1998) # should therefore be FALSE

net_2007 <- as.matrix(net_2007, mode = "undirected", weighted = TRUE)
tt_net_2007 <- as.network(net_2007,directed=F, loops = T, multiple = F) 
is.multiplex(tt_net_2007)

net_2011 <- as.matrix(net_2011, mode = "undirected", weighted = TRUE)
tt_net_2011 <- as.network(net_2011,directed=F, loops = T, multiple = F) 
is.multiplex(tt_net_2011)

#----imputing values for numeric variables----
set.seed(666)
imp_1998 <- cbind(nodal_attr_1998$brookings_dummy, 
             nodal_attr_1998$heritage_dummy, 
             nodal_attr_1998$mps_dummy, 
             nodal_attr_1998$rand_dummy, 
             nodal_attr_1998$ssrc_dummy,
             nodal_attr_1998$no_archetype_dummy, 
             nodal_attr_1998$Founded, 
             nodal_attr_1998$company_idealogy_v2, 
             nodal_attr_1998$brookings_perc, 
             nodal_attr_1998$heritage_perc, 
             nodal_attr_1998$rand_perc) 

imp_1998 <- mice(imp_1998)
imp_1998 <- complete(imp_1998)
colnames(imp_1998) <- c("brookings_dummy", "heritage_dummy", "mps_dummy",
                   "rand_dummy", "ssrc_dummy", "no_archetype_dummy","Founded",
                   "company_idealogy_v2","brookings_perc", "heritage_perc", "rand_perc" )
nodal_attr_1998$brookings_dummy <- imp_1998$brookings_dummy
nodal_attr_1998$heritage_dummy <- imp_1998$heritage_dummy
nodal_attr_1998$mps_dummy <- imp_1998$mps_dummy
nodal_attr_1998$rand_dummy <- imp_1998$rand_dummy
nodal_attr_1998$ssrc_dummy <- imp_1998$ssrc_dummy
nodal_attr_1998$no_archetype_dummy <- imp_1998$no_archetype_dummy
nodal_attr_1998$Founded <- imp_1998$Founded
nodal_attr_1998$company_idealogy_v2 <- imp_1998$company_idealogy_v2
nodal_attr_1998$brookings_perc <- imp_1998$brookings_perc
nodal_attr_1998$heritage_perc <- imp_1998$heritage_perc
nodal_attr_1998$rand_perc <- imp_1998$rand_perc

imp_2007 <- cbind(nodal_attr_2007$brookings_dummy, 
                  nodal_attr_2007$heritage_dummy, 
                  nodal_attr_2007$mps_dummy, 
                  nodal_attr_2007$rand_dummy, 
                  nodal_attr_2007$ssrc_dummy,
                  nodal_attr_2007$no_archetype_dummy, 
                  nodal_attr_2007$Founded, 
                  nodal_attr_2007$company_idealogy_v2, 
                  nodal_attr_2007$brookings_perc, 
                  nodal_attr_2007$heritage_perc, 
                  nodal_attr_2007$rand_perc) 

imp_2007 <- mice(imp_2007)
imp_2007 <- complete(imp_2007)
colnames(imp_2007) <- c("brookings_dummy", "heritage_dummy", "mps_dummy",
                        "rand_dummy", "ssrc_dummy", "no_archetype_dummy","Founded",
                        "company_idealogy_v2","brookings_perc", "heritage_perc", "rand_perc" )
nodal_attr_2007$brookings_dummy <- imp_2007$brookings_dummy
nodal_attr_2007$heritage_dummy <- imp_2007$heritage_dummy
nodal_attr_2007$mps_dummy <- imp_2007$mps_dummy
nodal_attr_2007$rand_dummy <- imp_2007$rand_dummy
nodal_attr_2007$ssrc_dummy <- imp_2007$ssrc_dummy
nodal_attr_2007$no_archetype_dummy <- imp_2007$no_archetype_dummy
nodal_attr_2007$Founded <- imp_2007$Founded
nodal_attr_2007$company_idealogy_v2 <- imp_2007$company_idealogy_v2
nodal_attr_2007$brookings_perc <- imp_2007$brookings_perc
nodal_attr_2007$heritage_perc <- imp_2007$heritage_perc
nodal_attr_2007$rand_perc <- imp_2007$rand_perc

imp_2011 <- cbind(nodal_attr_2007$brookings_dummy, 
                  nodal_attr_2007$heritage_dummy, 
                  nodal_attr_2007$mps_dummy, 
                  nodal_attr_2007$rand_dummy, 
                  nodal_attr_2007$ssrc_dummy,
                  nodal_attr_2007$no_archetype_dummy, 
                  nodal_attr_2007$Founded, 
                  nodal_attr_2007$company_idealogy_v2, 
                  nodal_attr_2007$brookings_perc, 
                  nodal_attr_2007$heritage_perc, 
                  nodal_attr_2007$rand_perc) 

imp_2011 <- mice(imp_2011)
imp_2011 <- complete(imp_2011)
colnames(imp_2011) <- c("brookings_dummy", "heritage_dummy", "mps_dummy",
                        "rand_dummy", "ssrc_dummy", "no_archetype_dummy","Founded",
                        "company_idealogy_v2","brookings_perc", "heritage_perc", "rand_perc" )
nodal_attr_2011$brookings_dummy <- imp_2011$brookings_dummy
nodal_attr_2011$heritage_dummy <- imp_2011$heritage_dummy
nodal_attr_2011$mps_dummy <- imp_2011$mps_dummy
nodal_attr_2011$rand_dummy <- imp_2011$rand_dummy
nodal_attr_2011$ssrc_dummy <- imp_2011$ssrc_dummy
nodal_attr_2011$no_archetype_dummy <- imp_2011$no_archetype_dummy
nodal_attr_2011$Founded <- imp_2011$Founded
nodal_attr_2011$company_idealogy_v2 <- imp_2011$company_idealogy_v2
nodal_attr_2011$brookings_perc <- imp_2011$brookings_perc
nodal_attr_2011$heritage_perc <- imp_2011$heritage_perc
nodal_attr_2011$rand_perc <- imp_2011$rand_perc

#----adding nodal characteristics to the network----
tt_net_1998%v%"brookings"<-nodal_attr_1998$brookings_dummy 
tt_net_1998%v% "heritage"<-nodal_attr_1998$heritage_dummy
tt_net_1998%v% "MPS"<-nodal_attr_1998$mps_dummy
tt_net_1998%v% "RAND"<-nodal_attr_1998$rand_dummy
tt_net_1998%v% "SSRC"<-nodal_attr_1998$ssrc_dummy
tt_net_1998%v% "No"<-nodal_attr_1998$no_archetype_dummy 
tt_net_1998%v% "Founded"<-nodal_attr_1998$Founded
tt_net_1998%v% "company_idealogy_v2" <- nodal_attr_1998$company_idealogy_v2
tt_net_1998%v%"brookings_perc"<-nodal_attr_1998$brookings_perc
tt_net_1998%v% "heritage_perc"<-nodal_attr_1998$heritage_perc
tt_net_1998%v% "RAND_perc"<-nodal_attr_1998$rand_perc

tt_net_2007%v%"brookings"<-nodal_attr_2007$brookings_dummy 
tt_net_2007%v% "heritage"<-nodal_attr_2007$heritage_dummy
tt_net_2007%v% "MPS"<-nodal_attr_2007$mps_dummy
tt_net_2007%v% "RAND"<-nodal_attr_2007$rand_dummy
tt_net_2007%v% "SSRC"<-nodal_attr_2007$ssrc_dummy
tt_net_2007%v% "No"<-nodal_attr_2007$no_archetype_dummy 
tt_net_2007%v% "Founded"<-nodal_attr_2007$Founded
tt_net_2007%v% "company_idealogy_v2" <- nodal_attr_2007$company_idealogy_v2
tt_net_2007%v%"brookings_perc"<-nodal_attr_2007$brookings_perc
tt_net_2007%v% "heritage_perc"<-nodal_attr_2007$heritage_perc
tt_net_2007%v% "RAND_perc"<-nodal_attr_2007$rand_perc

tt_net_2011%v%"brookings"<-nodal_attr_2011$brookings_dummy 
tt_net_2011%v% "heritage"<-nodal_attr_2011$heritage_dummy
tt_net_2011%v% "MPS"<-nodal_attr_2011$mps_dummy
tt_net_2011%v% "RAND"<-nodal_attr_2011$rand_dummy
tt_net_2011%v% "SSRC"<-nodal_attr_2011$ssrc_dummy
tt_net_2011%v% "No"<-nodal_attr_2011$no_archetype_dummy 
tt_net_2011%v% "Founded"<-nodal_attr_2011$Founded
tt_net_2011%v% "company_idealogy_v2" <- nodal_attr_2011$company_idealogy_v2
tt_net_2011%v%"brookings_perc"<-nodal_attr_2011$brookings_perc
tt_net_2011%v% "heritage_perc"<-nodal_attr_2011$heritage_perc
tt_net_2011%v% "RAND_perc"<-nodal_attr_2011$rand_perc

#############################################
#Part II: Descripitive Statistics and Plots # (done in the edgelist analysis, can be deleted here?)
#############################################
#getting descriptive statistics and plots so we can run an informed ergm

plot(tt_net_1998)  #a simple plot 

plot(tt_net_1998,vertex.col="brookings", displayisolates = F)  #looking at homophily by Brookings
plot(tt_net_1998,vertex.col="heritage", displayisolates = F)  #looking at homophily by Heritage

plot(tt_net_1998,vertex.col="heritage",vertex.cex=nodal_attr$heritage/1.5) #degree by nodal characteristics-heritage

#recoloring based on template
nodal_attr$template <- paste(nodal_attr$brookings_dummy, nodal_attr$heritage_dummy, nodal_attr$rand_dummy)
nodal_attr$template <- dplyr::recode(nodal_attr$template, "1 0 0" = "brookings", 
                              "0 1 0" = "heritage" , "0 0 1" = "RAND", "0 0 0" = "not disclosed",
                              "1 1 0" = "brookings + heritage " ,
                              "1 0 1" = "brookings + RAND",
                              "0 1 1" = "heritage + RAND",
                              "1 1 1" = "brookings + heritage + RAND")

palette("default")
plot(tt_net_1998,vertex.col= factor(nodal_attr$template)) #colour by template
#adding usecurve = T to the graph makes it more messy, but easier to identify 
#multiplex edges
colors <- palette("default")
legend(-170, 30, legend = c("brookings", "heritage", "RAND", "not disclosed", "brookings + heritage", 
                            "brookings + RAND", "heritage + RAND", "brookings + heritage + RAND" )
       , fill = colors)

############################################
#Part III: ERGM on a Think tanks Network ## 
############################################
#----1998 ERGMs----
#this year has many models which turned out to be insignificant and were thus not
#used for subsequent analyses. However if  you would like to use these models
#for other years, they are still here.
set.seed(666)
#empty model with just the observed network predicted by the edges
mod.empty_1998 <- ergm(tt_net_1998 ~ edges, burnin=200000, MCMCsamplesize=10000, 
                  control = control.ergm(force.main = F)) 
summary(mod.empty_1998)
#MCMC diagnostics does not make sense here, because the model is fit using MCMLE

#model with the endogenous network variables
mod.control_1998 <- ergm(tt_net_1998 ~ edges + gwesp(0.4), verbose = 1 ,
               burnin=200000,MCMCsamplesize=10000)
summary(mod.control_1998)
mod.control_1998$loglikelihood
mcmc.diagnostics(mod.control_1998)

#homophily model with the templates as predictors, not endogneous controls (dyadic independent)
mod.homoph_1998 <-ergm(tt_net_1998 ~ edges + nodematch("brookings") + 
                           nodematch("heritage") + nodematch("RAND") + gwesp(0.4, fixed = F), verbose = 1,
                         burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph_1998)
mcmc.diagnostics(mod.homoph_1998)

#model without endogenous network controls (bit without differential homophily)
mod.homoph2_1998 <-ergm(tt_net_1998 ~ edges + nodematch("brookings") + 
                    nodematch("heritage")+ nodematch("RAND"), 
                  burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph2_1998)

#allowing for differential homophily on brookings, heritage and RAND
#this was in here at the beginning but does not make a lot of sense, since they are
#dummy variables
mod.homoph3_1998 <-ergm(tt_net_1998 ~ edges + gwesp(0.4, fixed = F) + nodematch("brookings",diff=T) + 
                          nodematch("heritage",diff=T)+ nodematch("RAND",diff=T), 
                          burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph3_1998)
mod.brookings_1998 <-ergm(tt_net_1998 ~ edges + nodematch("brookings"), 
                burnin=200000,MCMCsamplesize=10000)
summary(mod.brookings_1998)

#estimating a model with only one template variable (1 if 1 on brookings, heritage or rand)
nodal_attr_1998$template_dummy <- paste(nodal_attr_1998$brookings_dummy, nodal_attr_1998$heritage_dummy, nodal_attr_1998$rand_dummy)
nodal_attr_1998$template_dummy <- dplyr::recode(nodal_attr_1998$template, "1 0 0" = 1, 
                                     "0 1 0" = 1 , "0 0 1" = 1, "0 0 0" = 0, "1 1 0" = 1,
                                     "1 0 1" = 1, "0 1 1" = 1, "1 1 1" = 1)
tt_net_1998%v% "any_template"<-nodal_attr_1998$template_dummy
mod.homoph4_1998 <- ergm(tt_net_1998 ~ edges + nodematch("any_template") + gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                  control = control.ergm(force.main = F)) 
summary(mod.homoph4_1998)

#homophily on SSRC and MPS
mod.homoph5_1998 <- ergm(tt_net_1998 ~ edges + nodematch("MPS") + nodematch("SSRC") + 
                      gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                    control = control.ergm(force.main = F)) 
summary(mod.homoph5_1998)

#homophily with percentage values for brookings, heritage and rand
mod.homoph6_1998 <-ergm(tt_net_1998 ~ edges + nodecov("brookings_perc") + 
                               nodecov("heritage_perc") + nodecov("RAND_perc") + gwesp(0.4, fixed = F), 
                             burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph6_1998)
mcmc.diagnostics(mod.homoph6_1998)

#founding year
mod.founded_1998 <- ergm(tt_net_1998 ~ edges + nodematch("Founded") + 
                    gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                    control = control.ergm(force.main = F)) 
summary(mod.founded_1998)

#ideology
mod.ideology.simple_1998 <- ergm(tt_net_1998 ~ edges + nodematch("company_idealogy_v2") , burnin=200000, MCMCsamplesize=10000, 
                          control = control.ergm(force.main = F))
summary(mod.ideology.simple_1998)

#ideology with noise correction
mod.ideology_1998 <- ergm(tt_net_1998 ~ edges + nodematch("company_idealogy_v2") + 
                      gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                    control = control.ergm(force.main = F))
summary(mod.ideology_1998)
mcmc.diagnostics(mod.ideology_1998)

#----2007 ERGMs----
#This is a slimmer version, only containing the analysis that were found to be 
#valueable after exmaining the 1998 results.
set.seed(666)
#empty model with just the observed network predicted by the edges
mod.empty_2007 <- ergm(tt_net_2007 ~ edges, burnin=200000, MCMCsamplesize=10000, 
                       control = control.ergm(force.main = F)) 
summary(mod.empty_2007)
#MCMC diagnostics does not make sense here, because the model is fit using MCMLE

#model with the endogenous network variables
mod.control_2007 <- ergm(tt_net_2007 ~ edges + gwesp(0.4, fixed = F),
                         burnin=200000,MCMCsamplesize=10000)
summary(mod.control_2007)
mod.control_2007$loglikelihood
mcmc.diagnostics(mod.control_2007)

#homophily model with the templates as predictors, not endogneous controls (dyadic independent)
mod.homoph_2007 <-ergm(tt_net_2007 ~ edges + nodematch("brookings") + 
                         nodematch("heritage") + nodematch("RAND") + gwesp(0.4, fixed = F), 
                       burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph_2007)
mcmc.diagnostics(mod.homoph_2007)

#model without endogenous network controls (bit without differential homophily)
mod.homoph2_2007 <-ergm(tt_net_2007 ~ edges + nodematch("brookings") + 
                          nodematch("heritage")+ nodematch("RAND"), 
                        burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph2_2007)

#estimating a model with only one template variable (1 if 1 on brookings, heritage or rand)
nodal_attr_2007$template_dummy <- paste(nodal_attr_2007$brookings_dummy, nodal_attr_2007$heritage_dummy, nodal_attr_2007$rand_dummy)
nodal_attr_2007$template_dummy <- dplyr::recode(nodal_attr_2007$template, "1 0 0" = 1, 
                                                "0 1 0" = 1 , "0 0 1" = 1, "0 0 0" = 0, "1 1 0" = 1,
                                                "1 0 1" = 1, "0 1 1" = 1, "1 1 1" = 1)
tt_net_2007%v% "any_template"<-nodal_attr_2007$template_dummy
mod.homoph4_2007 <- ergm(tt_net_2007 ~ edges + nodematch("any_template") + gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                         control = control.ergm(force.main = F)) 
summary(mod.homoph4_2007)

#homophily on SSRC and MPS
mod.homoph5_2007 <- ergm(tt_net_2007 ~ edges + nodematch("MPS") + nodematch("SSRC") + 
                           gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                         control = control.ergm(force.main = F)) 
summary(mod.homoph5_2007)

#ideology
mod.ideology.simple_2007 <- ergm(tt_net_2007 ~ edges + nodematch("company_idealogy_v2") 
                                 , burnin=200000, MCMCsamplesize=10000, 
                                 control = control.ergm(force.main = F))
summary(mod.ideology.simple_2007)
mcmc.diagnostics(mod.ideology.simple_2007)

#ideology +GWESP
mod.ideology_2007 <- ergm(tt_net_2007 ~ edges + nodematch("company_idealogy_v2") + 
                            gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                          control = control.ergm(force.main = F))
summary(mod.ideology_2007)
mcmc.diagnostics(mod.ideology_2007)

#ideology with noise correction and GWESP
mod.ideology_2007 <- ergm(tt_net_2007 ~ edges + nodematch("company_idealogy_v2") + 
                            gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                          control = control.ergm(force.main = F), obs.constraints = ~dyadnoise(0.01, 0.01))
summary(mod.ideology_2007)
mcmc.diagnostics(mod.ideology_2007)

#----2011 ERGMs----
set.seed(666)
#empty model with just the observed network predicted by the edges
mod.empty_2011 <- ergm(tt_net_2011 ~ edges, burnin=200000, MCMCsamplesize=10000, 
                       control = control.ergm(force.main = F)) 
summary(mod.empty_2011)
#MCMC diagnostics does not make sense here, because the model is fit using MCMLE

#model with the endogenous network variables
mod.control_2011 <- ergm(tt_net_2011 ~ edges + gwesp(0.4, fixed = F),
                         burnin=200000,MCMCsamplesize=10000)
summary(mod.control_2011)
mod.control_2011$loglikelihood
mcmc.diagnostics(mod.control_2011)

#homophily model with the templates as predictors, not endogneous controls (dyadic independent)
mod.homoph_2011 <-ergm(tt_net_2011 ~ edges + nodematch("brookings") + 
                         nodematch("heritage") + nodematch("RAND") + gwesp(0.4, fixed = F), 
                       burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph_2011)
mcmc.diagnostics(mod.homoph_2011)

#model without endogenous network controls (bit without differential homophily)
mod.homoph2_2011 <-ergm(tt_net_2011 ~ edges + nodematch("brookings") + 
                          nodematch("heritage")+ nodematch("RAND"), 
                        burnin=200000,MCMCsamplesize=10000)
summary(mod.homoph2_2011)

#estimating a model with only one template variable (1 if 1 on brookings, heritage or rand)
nodal_attr_2011$template_dummy <- paste(nodal_attr_2011$brookings_dummy, nodal_attr_2011$heritage_dummy, nodal_attr_2011$rand_dummy)
nodal_attr_2011$template_dummy <- dplyr::recode(nodal_attr_2011$template, "1 0 0" = 1, 
                                                "0 1 0" = 1 , "0 0 1" = 1, "0 0 0" = 0, "1 1 0" = 1,
                                                "1 0 1" = 1, "0 1 1" = 1, "1 1 1" = 1)
tt_net_2011%v% "any_template"<-nodal_attr_2011$template_dummy
mod.homoph4_2011 <- ergm(tt_net_2011 ~ edges + nodematch("any_template") + gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                         control = control.ergm(force.main = F)) 
summary(mod.homoph4_2011)

#homophily on SSRC and MPS
mod.homoph5_2011 <- ergm(tt_net_2011 ~ edges + nodematch("MPS") + nodematch("SSRC") + 
                           gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                         control = control.ergm(force.main = F)) 
summary(mod.homoph5_2011)


#ideology
mod.ideology.simple_2011 <- ergm(tt_net_2011 ~ edges + nodematch("company_idealogy_v2") 
                            , burnin=200000, MCMCsamplesize=10000, 
                          control = control.ergm(force.main = F))
summary(mod.ideology.simple_2011)
mcmc.diagnostics(mod.ideology.simple_2011)

#ideology + GWESP
mod.ideology_2011 <- ergm(tt_net_2011 ~ edges + nodematch("company_idealogy_v2") + 
                            gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                          control = control.ergm(force.main = F))
summary(mod.ideology_2011)
mcmc.diagnostics(mod.ideology_2011)

#ideology with noise correction and GWESP
mod.ideology_2011 <- ergm(tt_net_2011 ~ edges + nodematch("company_idealogy_v2") + 
                            gwesp(0.4, fixed = F), burnin=200000, MCMCsamplesize=10000, 
                          control = control.ergm(force.main = F), obs.constraints = ~dyadnoise(0.01, 0.01))
summary(mod.ideology_2011)
mcmc.diagnostics(mod.ideology_2011)

# Check out
# https://rpubs.com/sbmrtnz/ina_day1
# https://rstudio-pubs-static.s3.amazonaws.com/471073_d45a4acd780b4987932dc8fc47c46dd5.html
# http://statnet.org/Workshops/ergm_tutorial.html

#This is it for this file, as mentioned earlier, feel free to contact me with questions ~Timo

 
