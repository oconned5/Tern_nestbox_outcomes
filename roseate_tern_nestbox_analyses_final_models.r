########################################################################################
# Analyses of 15 years of Roseate Tern nest outcomes, assessing the effects of next box
# use on the lay date, number of eggs laid, proportion of eggs which hatch and proportion 
# of nestlings which go on to fledge
# Additionally, the relationship between natural cover and number of eggs laid, proportion 
# of eggs which hatch and proportion of nestlings which go on to fledge for nests laid 
# outside of nest boxes in 2016


# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

# Packages that may be needed, remove the # to install

#install.packages("arm")
#install.packages("lme4")
#install.packages("MuMIn")
#install.packages("car")
#install.packages("LMERConvenienceFunctions")
#install.packages("ggplot2")
#install.packages("effects")

#################################################################################

# Relationship between nest box use and the proportion of eggs which hatched from nests

dframe1 <- read.csv(file.choose())    # Select "roseate_tern_nestbox_analyses"
head(dframe1)
summary(dframe1)


# Create a matrix called "Proportion_hatched" containing (no. of successes, no. of failures)
dframe1$Proportion_hatched <- cbind(dframe1$no.nestlings, dframe1$no.eggs - dframe1$no.nestlings) #create matrix of prob of fledging 
dframe1$Proportion_hatched       # This will be our dependent variable


#### set box Yes/No to be a factor
dframe1$box <- as.factor(dframe1$box)

str(dframe1$box)


library(lme4)   ### package required for GLMMs
global.model1 <- glmer(Proportion_hatched  ~                 # the dependent vaiable
                         box + clutch_initiation_day  # fixed terms 
                       + (1|year),                # the random term 
                       na.action = na.pass,
                       family = "binomial" (link=logit), data = dframe1) # binomial model of proportional data 
# logit link function used as we have a binomial explanatory variable

summary(global.model1) #the warning message here suggests we have a scaling issue, need to standardise variablies


# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.model1 <- standardize(global.model1, standardize.y = FALSE)
#adjusts variables going in so the parameter will be comparable
summary(stdz.model1)

## other methods of reporting model results
drop1(stdz.model1, test = "Chi")
library(car)
Anova(stdz.model1, type = 3) 

# Model validation

display(stdz.model1)

### plots of model fit
library(LMERConvenienceFunctions)
mcp.fnc(stdz.model1)
plotLMER.fnc(stdz.model1)


## R-squarded
library(MuMIn)
r.squaredGLMM(stdz.model1)

### extract and plot residuals
sresid <- resid(stdz.model1, type = "pearson")
hist(sresid)

fits <- fitted(stdz.model1)
plot(sresid ~ fits)


### Figures ###

library(effects)
plot(allEffects(global.model1))

pdf(file = "Supplemenatry_Figure_2_the_effects_of_nestbox_and_lay_date_on_proportion_hatched.pdf", width = 8, height = 6, family = "Helvetica")
plot(allEffects(global.model1))
dev.off()

tiff("Supplemenatry_Figure_2_the_effects_of_nestbox_and_lay_date_on_proportion_hatched.tiff", height = 12, width = 17, units = 'cm', res = 300)
plot(allEffects(global.model1))
dev.off()

### GGplot
library(ggplot2)

# plot with smoothing 
p1 <- ggplot(dframe1, aes(clutch_initiation_day, prop.hatched, colour = box)) +
  geom_point(colour = "white") +
  geom_smooth(method = "loess")  
p2 <- p1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
p3 <- p2 + ylab("Proportion of eggs hatched")
p4 <- p3 + xlab("Clutch initiation date")
p5 <- p4 + theme(axis.line = element_line(colour = "black"))
p6 <- p5 + scale_color_manual(values=c("black", "green"))
p7 <- p6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
p8 <- p7 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
p9 <- p8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
p9


### exporting high res image
# PDF
pdf(file = "Figure_4_the_effects_of_nestbox_and_lay_date_on_proportion_hatched.pdf", width = 8, height = 6, family = "Helvetica")
p9
dev.off()

# TIFF
tiff("Figure_4_the_effects_of_nestbox_and_lay_date_on_proportion_hatched.tiff", height = 12, width = 17, units = 'cm', res = 300)
p9
dev.off()
##############################################################################################

# Relationship between nest box use and proportion of fledglings out of the nestlings hatched
## select just the nexts where there were hatchlings
dframe2 <- subset(dframe1, no.fledged != "NA",
                  select=c(year,	no.nestlings, no.fledged, 
                           prop.fledged,	clutch_initiation_day, box))

head(dframe2)
summary(dframe2)

# Create a matrix called "Proportion_hatch" containing (no. of successes, no. of failures)
dframe2$Proportion_fledged <- cbind(dframe2$no.fledged, dframe2$no.nestlings - dframe2$no.fledged) #create matrix of prob of fledging 
dframe2$Proportion_fledged       # This will be our dependent variable

dframe2$box <- as.factor(dframe2$box)

str(dframe2$box)



#  model adding quadratic terms as we have a humped relationship

library(lme4)
global.model2 <- glmer (Proportion_fledged  ~              # the dependent vaiable
                           box + clutch_initiation_day + I(clutch_initiation_day^2) # fixed terms 
                         + box:I(clutch_initiation_day^2)     # interaction
                           +(1|year),                 # the random term 
                         na.action = na.pass,
                         family = "binomial"  (link=logit), data = dframe2) # binomial model of proportional data 

# logit link function used as we have a binomial explanatory variable

summary(global.model2) #the warning message here suggests we have a scaling issue, need to standardise variablies


# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.model2 <- standardize(global.model2, standardize.y = FALSE)
#adjusts variables going in so that all variables are comparable
summary(stdz.model2)
drop1(stdz.model2, test = "Chi")

library(car)
Anova(stdz.model2, type = 3) 

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.model2)
plotLMER.fnc(stdz.model2)

library(MuMIn)
r.squaredGLMM(stdz.model2)

sresid <- resid(stdz.model2, type = "pearson")
hist(sresid)

fits <- fitted(stdz.model2)
plot(sresid ~ fits)

## Figures

library(effects)
plot(allEffects(global.model2))


pdf(file = "Supplemenatry_Figure_3_the_effects_of_nestbox_and_lay_date_on_proportion_fledged.pdf", width = 8, height = 6, family = "Helvetica")
plot(allEffects(global.model2))
dev.off()

tiff("Supplemenatry_Figure_3_the_effects_of_nestbox_and_lay_date_on_proportion_fledged.tiff", height = 12, width = 20, units = 'cm', res = 300)
plot(allEffects(global.model2))
dev.off()


### GGplot
library(ggplot2)

# plots smoothed relationship

a1 <- ggplot(dframe2, aes(clutch_initiation_day, prop.fledged, colour = box)) +
  geom_point(colour = "white") +
  geom_smooth(method = "loess") 
a2 <- a1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
a3 <- a2 + ylab("Proportion of nestlings fledged")
a4 <- a3 + xlab("Clutch initiation date")
a5 <- a4 + theme(axis.line = element_line(colour = "black"))
a6 <- a5 + scale_color_manual(values=c("black", "green"))
a7 <- a6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
a8 <- a7 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
a9 <- a8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
a9 

### exporting high res image
# PDF
pdf(file = "Figure_5_the_effects_of_nestbox_and_lay_date_on_proportion_fledged.pdf", width = 8, height = 6, family = "Helvetica")
a9
dev.off()

# TIFF
tiff("Figure_5_the_effects_of_nestbox_and_lay_date_on_proportion_fledged.tiff", height = 12, width = 17, units = 'cm', res = 300)
a9
dev.off()


#################################################################

# Relationship between clutch initiation and whether or not it was in a box

#### set box Yes/No to be a factor
dframe1$box <- as.factor(dframe1$box)

str(dframe1$box)



library(lme4)
global.model3 <- glmer(box  ~                 # the dependent vaiable
                         clutch_initiation_day +       # fixed term
                         (1|year),                # the random term 
                       na.action = na.pass,
                       family = "binomial" (link=cloglog), data = dframe1) # binomial model of proportional data 
# cloglog link function this time as it allows for asymmetry, i.e. unequal numbers amongst 1s(boxes) and 0s(not boxes)

summary(global.model3) #the warning message here suggests we have a scaling issue, need to standardise variablies



# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.model3 <- standardize(global.model3, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.model3)
drop1(stdz.model3, test = "Chi")

library(car)
Anova(stdz.model3, type = 3) 

## model validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.model3)
plotLMER.fnc(stdz.model3)

library(MuMIn)
r.squaredGLMM(stdz.model3)


library(effects)
plot(allEffects(global.model3))


sresid <- resid(stdz.model3, type = "pearson")
hist(sresid)

fits <- fitted(stdz.model3)
plot(sresid ~ fits)


## figure for nest box ~ lay date (horizontal boxplot)

dev.off()

boxplot(dframe1$clutch_initiation_day ~ dframe1$box,
         ylab="dframe1$clutch_initiation_day", xlab="dframe1$box", horizontal = "TRUE")

box(bty="L")
axis(1, at=130:215, las=1, tcl=0.5)
title(xlab="Clutch initiation date", cex.lab=1.5)
axis(2,at=1:2,c("Not in box","In box"),
     font=3, cex.axis=1.5, tcl=0.5)
title(ylab="Box",cex.lab=1.4)


# extract boxplot 
#PDF
pdf(file = "Figure_2_relationship_between_clutch_initiation_date_and_nest_site_choice.pdf", width = 8, height = 6, family = "Helvetica")

boxplot(dframe1$clutch_initiation_day ~ dframe1$box,
        ylab="dframe1$clutch_initiation_day", xlab="dframe1$box", horizontal = "TRUE")

box(bty="L")
axis(1, at=130:215, las=1, tcl=0.5)
title(xlab="Clutch initiation date", cex.lab=1.5)
axis(2,at=1:2,c("Not in box","In box"),
     font=3, cex.axis=1.5, tcl=0.5)
title(ylab="Box",cex.lab=1.4)

dev.off()


# TIFF

tiff("Figure_2_relationship_between_clutch_initiation_date_and_nest_site_choice.tiff", height = 12, width = 17, units = 'cm', res = 300)

boxplot(dframe1$clutch_initiation_day ~ dframe1$box,
        ylab="dframe1$clutch_initiation_day", xlab="dframe1$box", horizontal = "TRUE")

box(bty="L")
axis(1, at=130:215, las=1, tcl=0.5)
title(xlab="Clutch initiation date", cex.lab=1.5)
axis(2,at=1:2,c("Not in box","In box"),
     font=3, cex.axis=1.5, tcl=0.5)
title(ylab="Box",cex.lab=1.4)
dev.off()


#################################
#### Relationship between clutch size and next box use
################################


#### set box Yes/No to be a factor
dframe1$box <- as.factor(dframe1$box)

str(dframe1$box)



global.model4b <- glmer (no.eggs  ~  box + clutch_initiation_day# fixed terms 
                         + (1|year),                # the random term 
                         na.action = na.pass,
                         family = "poisson" (link=identity), data = dframe1) # poisson model of count data 

summary(global.model4b) #the warning message here suggests we have a scaling issue, need to standardise variablies


# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.model4b <- standardize(global.model4b, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates dreged later will be comparable
summary(stdz.model4b)
drop1(stdz.model4b, test = "Chi")

library(car)
Anova(stdz.model4b, type = 3) 

## ,odel validation

library(LMERConvenienceFunctions)
mcp.fnc(stdz.model4b)
plotLMER.fnc(stdz.model4b)

library(MuMIn)
r.squaredGLMM(stdz.model4b)

library(rsq)
rsq.glmm(stdz.model4b)

sresid <- resid(stdz.model4b, type = "pearson")
hist(sresid)

fits <- fitted(stdz.model4b)
plot(sresid ~ fits)

## Figures

library(effects)
plot(allEffects(global.model4b))

pdf(file = "Supplemenatry_Figure_1_the_effects_of_nestbox_and_lay_date_on_clutch_size.pdf", width = 8, height = 6, family = "Helvetica")
plot(allEffects(global.model4b))
dev.off()

tiff("Supplemenatry_Figure_1_the_effects_of_nestbox_and_lay_date_on_clutch_size.tiff", height = 12, width = 17, units = 'cm', res = 300)
plot(allEffects(global.model4b))
dev.off()



library(ggplot2)


b1 <- ggplot(dframe1, aes(clutch_initiation_day, no.eggs, colour = box)) +
  geom_point(colour = "white") +
  geom_smooth(method = "loess") + ylim(0, 2.5)
b2 <- b1 + theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank()) +
  theme(panel.background = element_blank())
b3 <- b2 + ylab("Number of eggs laid")
b4 <- b3 + xlab("Clutch initiation date")
b5 <- b4 + theme(axis.line = element_line(colour = "black"))
b6 <- b5 + scale_color_manual(values=c("black", "green"))
b7 <- b6 + theme(axis.title.y=element_text(face="bold", size=15, vjust=1.5))
b8 <- b7 +  theme(axis.text.x=element_text(face="bold", size=15, vjust=1.5, colour = "black")) +
  theme(axis.text.y=element_text(face="bold", size=15, colour = "black"))
b9 <- b8 + theme(axis.title.x=element_text(face="bold", size=15, vjust=1.5))
b9 

### exporting high res image
# PDF
pdf(file = "Figure_3_the_effects_of_nestbox_and_lay_date_on_clutch_size.pdf", width = 8, height = 6, family = "Helvetica")
b9
dev.off()

# TIFF
tiff("Figure_3_the_effects_of_nestbox_and_lay_date_on_clutch_size.tiff", height = 12, width = 17, units = 'cm', res = 300)
b9
dev.off()
#####################################################################
######################################################################################

# Affect of cover on hatching - just 2016

# Proportion of nestlings hatched out of eggs laid in relation to natural cover,
# for nests not laid in nest boxes

# dframe for only nest laid outside of boxes from 2016 when cover proportions were measured
dframe3 <- subset(dframe1, cover != "NA",
                  select=c(no.eggs, no.nestlings, 
                           prop.hatched,	clutch_initiation_day, cover))

head(dframe3)
summary(dframe3)

# Create a matrix called "Proportion_hatched" containing (no. of successes, no. of failures)
dframe3$Proportion_hatched <- cbind(dframe3$no.nestlings, dframe3$no.eggs - dframe3$no.nestlings) #create matrix of prob of fledging 
dframe3$Proportion_hatched       # This will be our dependent variable

hist(dframe3$Proportion_hatched)


global.model5 <- glm (Proportion_hatched  ~                 # the dependent vaiable
                        cover + clutch_initiation_day,   
                      na.action = na.pass,
                      family = "binomial" (link=logit), data = dframe3) # binomial model of proportional data 
# logit link function used as we have a binomial explanatory variable

summary(global.model5) 
summary.lm(global.model5)

## model validation

AIC(global.model5)
plot(global.model5)

Model_R2global.model5 <- with(summary(global.model5), 1 - deviance/null.deviance)
Model_R2global.model5

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.model5 <- standardize(global.model5, standardize.y = FALSE)
#adjusts variables going in so the parameter estimates will be comparable
summary(stdz.model5)

## model validation

AIC(stdz.model5)
plot(stdz.model5)

Model_R2stdz.model5 <- with(summary(stdz.model5), 1 - deviance/null.deviance)
Model_R2stdz.model5


######################################################################################

# Affect of cover on numbers of nestlings which have fledged


### binomial for proportion data - logit link

# Proportion fledging out of the nestling hatched, nest box vs no nest box
dframe4 <- subset(dframe1, cover != "NA" & no.fledged != "NA",
                  select=c(no.nestlings, no.fledged,
                           prop.fledged,	clutch_initiation_day, cover))

head(dframe4)
summary(dframe4)

# Create a matrix called "Proportion_fledged" containing (no. of successes, no. of failures)
dframe4$Proportion_fledged <- cbind(dframe4$no.fledged, dframe4$no.nestlings - dframe4$no.fledged) #create matrix of prob of fledging 
dframe4$Proportion_fledged       # This will be our dependent variable

hist(dframe4$Proportion_fledged)


global.model6a <- glm (Proportion_fledged  ~                 # the dependent vaiable
                         cover + clutch_initiation_day,   
                       na.action = na.pass,
                       family = "binomial" (link=logit), data = dframe4) # binomial model of proportional data 
# logit link function used as we have a binomial explanatory variable

summary(global.model6a) 

## model validation

AIC(global.model6a)
plot(global.model6a)

Model_R2global.model6a <- with(summary(global.model6a), 1 - deviance/null.deviance)
Model_R2global.model6a

# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.model6a <- standardize(global.model6a, standardize.y = FALSE)
#adjusts variables going in
summary(stdz.model6a)

## model validation

AIC(stdz.model6a)
plot(stdz.model6a)

Model_R2stdz.model6a <- with(summary(stdz.model6a), 1 - deviance/null.deviance)
Model_R2stdz.model6a


######################################################################################

# Affect of cover on eggs laid


### poisson for count data - log link

global.model7a <- glm (no.eggs  ~                 # the dependent vaiable
                         cover + clutch_initiation_day,   
                       na.action = na.pass,
                       family = "poisson" (link=log), data = dframe3) # poisson model of count data 
# logit link function used as we have a poisson explanatory variable

summary(global.model7a) 
summary.lm(global.model7a)

## model validation

AIC(global.model7a)
plot(global.model7a)


# Standardise the global model's parameters so that SD = 0.5,
# to make them directly comparable despite them being measured on different scales

library(arm)                                    
stdz.model7a <- standardize(global.model7a, standardize.y = FALSE)
#adjusts variables going in
summary(stdz.model7a)

## model validation

AIC(stdz.model7a)
plot(stdz.model7a)

Model_R2stdz.model7a <- with(summary(stdz.model7a), 1 - deviance/null.deviance)
Model_R2stdz.model7a


######################################################################################
