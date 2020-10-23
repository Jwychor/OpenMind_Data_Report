#######################################################################################
#
#         DISCLAIMER:
#
#         The goal of this project was to answer 4 questions as part of the interview
#         process at OpenMind. Code was written with the purpose of being functional
#         and reusable by making use of the 'tidyverse' package and leaving patterns
#         in variables (i.e. D1 - D6 refer to demographic variables. This makes them
#         easy to group using `starts_with("D")` in a `select` method). Additionally,
#         assumption testing was not reported and handled exclusively in this document.
#
#         AUTHOR:
#
#         Jack Wychor
#
#######################################################################################

####Packages####
library(tidyverse)
library(psych)
library(jmv)
library(corrr)
library(stringi)
library(stats)
library(gridExtra)
library(reshape2)
library(car)
library(lme4)
library(effects)
library(sjPlot)
library(sjmisc)
library(glmmTMB)
library(robustHD)
library(lattice)





#####Loading####
#setwd("C:\\Users\\Jack Wychor\\Desktop\\R files")
df<-read.csv("task_dat.csv")

colnames(df)[1]<- "ID"

#Turnoff scientific notation
options(scipen = 999)

#Function for pasting dataframes in Excel format
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}





#####Analysis####
####Q1####
##Check for missing values
df %>% select(starts_with("IH")) %>%
  summarise_all(funs(sum(is.na(.)))) ##IH3Pre has an NA


##Reverse items
reverser<- function(x) {
  return(8 - x)
}

IHScale<- df %>% 
  select(IH1Pre, IH2Pre, IH5Pre, IH6Pre) %>%
  mutate_all(.funs = reverser)

#Create object for IH scale
IHScale<- data.frame(IHScale, IH3Pre = df$IH3Pre, IH4Pre = df$IH4Pre)


##Analyze Cronbach's Alpha and item statistics for IH scale
#Remove missing values and reorder columns
IHScaleComp<- IHScale %>% 
  select(sort(current_vars())) %>%
  filter(complete.cases(.))

IHScaleComp %>%
  alpha(.) #%>% .$item.stats %>% mutate_all(.funs = round, 2) %>% write.excel(.,col = F, row = F)
  #Uncomment above to copy values to clipboard

IHScaleComp %>% 
  jmv::pca(eigen = T, screePlot = T) #%>% .$loadings %>% as.data.frame(.) %>% mutate_if(.predicate = is.numeric, .funs = round, 2) %>% write.excel(.,col = F, row = F)
  #Uncomment above to copy values to clipboard


##Alpha for subscales
#Scale 1
IHScaleComp %>%
  select(IH1Pre, IH2Pre, IH5Pre, IH6Pre) %>%
  alpha(.) #%>% .$item.stats %>% mutate_all(.funs = round, 2) %>% write.excel(.,col = F, row = F)
  #Uncomment above to copy values to clipboard

#Scale 2
IHScaleComp %>% 
  select(IH3Pre, IH4Pre) %>%
  alpha(.) #%>% .$item.stats %>% mutate_all(.funs = round, 2) %>% write.excel(.,col = F, row = F)

#NOTE: The spearman-brown formula may be more accurate than
#using Cronbach's Alpha given that this is a 2-item scale.
#Because Cronbach's Alpha is a conservative estimate however,
#the Spearman-Brown estimation will only appear higher than
#the current, acceptable estimation.




####Q2####
##Correlation matrix of numeric variables with affective polarization
df %>% 
  select_if(is.numeric) %>%
  correlate() %>%
  select(rowname, AffPol2Pre, AffPol2Post) #%>% as.data.frame(.) %>% mutate_if(.predicate = is.numeric, .funs = round, 2) %>% write.excel(.,col = F, row = F)
  #Uncomment above to copy values to clipboard


##Dummy code race/ethnicity by matching strings
#Count the max number of ethnicities for a single person
max_eth<- max(str_count(df$D3, "\\)"),na.rm = T)

#Create a list of unique races/ethnicities
races<- df %>%
  separate(col = D3, sep = "\\)", into = paste("Eth",seq(1,max_eth,1)), extra = "drop") %>%
  select(starts_with("Eth")) %>%
  unlist(.) %>%
  str_remove("\\(") %>%
  unique(.) %>%
  stri_remove_empty_na(.)

#Empty dataframe to store race info
racedf<- matrix(ncol = length(races), nrow = nrow(df)) %>%
  as.data.frame(.)

colnames(racedf)<- races

#Loop through each person and code if they belong to a race/ethnicity (1,0)
for(i in 1:length(races)){
  racedf[,i]<- ifelse(grepl(races[i], df$D3),1,0)
  racedf[,i]<- as.factor(racedf[,i])
}

#Count number of people for each race
racedf %>%
  mutate_all(funs(as.numeric)) %>%
  summarize_all(.funs = sum)


##Create a dataframe for the ANOVA
demodf<- data.frame(df %>% 
           select(starts_with("D"),AffPol2Pre),
           racedf
)


##Factorial Anova of categorical demographic vars on AffPol2Pre scores
lm1<- lm(AffPol2Pre ~ D2  + D4 + D6 + Hispanic.Latino +
           African.American.Black + South.Asian + East.or.Southeast.Asian +
           White.Caucasian + Other + American.Indian.or.Alaska.Native + 
           Prefer.not.to.say, data = demodf)


##Test Assumptions
#Heteroscidasticity
plot(lm1, 2)

anova(lm1) #%>% mutate_if(.predicate = is.numeric, .funs = round,2) %>% write.excel(col = F, row = F)
  #Uncomment above to copy values to clipboard

#Reorder political factor from left to right.
#NOTE: Only very liberal > very conservative individuals were recorded
#with AffPol2 assessments. The object `allpol` stores all possible values of the
#factor.
allpol<- levels(as.factor(demodf$D4))
demodf$D4 <- factor(demodf$D4, levels = rev(c("Very conservative/right",
                                              "Conservative/right",
                                              "Slightly conservative/right",
                                              "Slightly progressive/left",
                                              "Progressive/left",
                                              "Very progressive/left")))



##Plot means
demodf$D2<- as.factor(demodf$D2)

gg1<- demodf %>%
  na.omit() %>%
  ggplot(aes(x = D2, y = AffPol2Pre)) + stat_summary(fun = mean, geom = 'col') + 
    theme_bw() +
    theme(axis.title.x = element_blank()) +
    labs(title = "Mean AP levels by Gender")
    
gg2<- demodf %>%
  na.omit() %>%
  ggplot(aes(x = D4, y = AffPol2Pre)) + stat_summary(fun = mean, geom = 'col') +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1), axis.title.x = element_blank()) + 
    labs(title = "Mean AP levels by Political Affiliation")
    
grid.arrange(gg1, gg2)

##Planned Contrasts
#Gender
contrast1<- c(-1,1,0,0)
contrast2<- c(-1,-1,2,0)
contrast3<- c(-1,-1,-1,3)

#Political Affiliation
contrast4<- c(-1,0,0,0,0,1)
contrast5<- c(-2,1,1,1,1,-2)
contrast6<- c(-1,-1,-1,1,1,1)

contrasts(demodf$D2)<- cbind(contrast1, contrast2, contrast3)
contrasts(demodf$D4)<- cbind(contrast4, contrast5, contrast6)

lm2<- aov(AffPol2Pre ~ D2 + D4, data = demodf)

#Results of planned contrasts
summary.lm(lm2)




####Q3####
##Create a linear mixed effects model
#Create a unique identifier for each permutation of assessments
df$treatcode <- 
  df %>%
  select(ends_with("Pre")) %>%
  select(-starts_with("I")) %>%
  mutate_all(funs(case_when(
    is.na(.) ~ 0,
    !is.na(.) ~ 1
  ))) %>%
  unite(fill,everything(), sep = "") %>%
  .$fill

#Pivot values for analysis into long format
dflong<- data.frame(ID = df$ID,
                     df %>%
      select(starts_with("D"), ends_with("Pre"), ends_with("Post"),treatcode) %>%
      select(-starts_with("IH"))
)

dflong<- dflong %>% 
  pivot_longer(cols = ends_with("Pre")| ends_with("Post"),
               names_to = "treatment", values_to = "value",
               values_drop_na = F) %>%
  mutate(time = ifelse(str_detect(treatment,"Pre"),1,2))

#Remove Pre- and Post- distinction from treatment column
dflong$treatment<- dflong$treatment %>%
  str_replace("Pre|Post","")

#Create a dataframe of only affpol2 data to check Mixed Model Assumptions
dflongaff<- dflong %>%
  filter(treatment == "AffPol2" & !is.na(value))


##Mixed Effects Models 
#NOTE: all models include AP as the only measured value
me1<- lmer(value ~ time + 
            (1|ID) + 
            (1|treatcode), 
            data = dflongaff)

#Without treatmentcode
me2<- lmer(value ~ time + 
            (1|ID), 
            data = dflongaff)

#Compare Mixed Effects Models
anova(me1, me2)


##Standard Linear Model
lm3<-lm(value ~ time, 
   data = dflongaff)

#Compare Mixed Effect model to Linear Model
anova(me2, lm3)


##Test Assumptions
#Linearity
plot(dflongaff$value, resid(me2)) #Follows a line

#Homogeneity of Variance
dflongaff$me2res<- abs(residuals(me2))^2
hov<- lm(me2res ~ ID, data = dflongaff)
anova(hov) #Not significant (p = 0.26)

#Normality
qqmath(me2) #Looks relatively normal with some deviation at the edges of the distribution


##Analyze best performing model
summary(me2)
confint(me2)



####Q4####
##Analyze interaction of age:time and gender:time on AP
me3<- lmer(value ~ time + time:D1 + time:D2 + time:D2 +
             (1|ID),data = dflongaff)

summary(me3)

plot_model(me3,
           axis.labels = rev(c("OpenMind", "OpenMind: Age", "OpenMind: (Gender) Male",
                               "OpenMind: (Gender) Other", "OpenMind: (Gender) Prefer not to say")),
           title = "Effect of OpenMind and the Interactions of OpenMind with Age and Gender on Affective Polarization Scores",
           show.values = T, show.p = T)

#Analyze model specifics
summary(me3)
confint(me3)


##Test Assumptions
#Linearity
plot(dflongaff$value, resid(me3)) #Follows a line

#Homogeneity of variance
dflongaff$me3res<- abs(residuals(me3))^2
hov<- lm(me3res ~ ID, data = dflongaff)
anova(hov) #Not significant (p = 0.26)

#Normality
qqmath(me3) #Looks relatively normal with some deviation at the edges of the distribution


##Record gender counts
dflongaff %>% 
  filter(time == 1) %>%
  select(D2) %>%
  table()


##Plot AP Pre- and Post- means for each gender
dflongaff %>%
  ggplot(aes(x = time, y = value, group = D2, color = D2)) +
  scale_x_discrete(limits = c("1","2")) +
  geom_jitter(width = 0.01) + geom_smooth(method = 'lm', se = F, size = 2) +
  coord_cartesian(xlim = c(1.5,1.5)) +
  labs(color = "Gender") +
  labs(x = "Time", y = "Affective Polarization", title = "Group Means for each Gender on Affective Polarization")
