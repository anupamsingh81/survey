library(strengejacke)
library(tidyverse)

remove.packages("tidyverse")

library(sjlabelled,lib.loc = "/usr/local/lib/R/site-library")


library(sjlabelled)

jhar= jhar %>% select(-c(X1,X1_1))

jhar2 = jhar %>% select(one:Twenty.three) %>% set_label(questions$Questions) # set_label for variable,set_labels for value

jhar3= jhar %>% select(-c(one:Twenty.three)) # remove unmodified column
jhar4 = bind_cols(jhar3,jhar2) # bind modified column

jhar5 = jhar4 %>% select(one:Twenty.three) %>% set_labels(labels = c("Very satisfied" =5,"Satisfied"=4,"Neutral"=3,"Dissatisfied"=2,"Very dissatisfied"=1)) #select column,modify
 
jhar6 = jhar4 %>% select(-c(one:Twenty.three)) %>% bind_cols(jhar5) # remove columns add again in single step 

sval = function (x) {
  set_labels(x,labels = c("Very satisfied" =5,"Satisfied"=4,"Neutral"=3,"Dissatisfied"=2,"Very dissatisfied"=1)) #select column,modify
  
}

jhar %>% mutate_at(vars(one:Twenty.three),sl)

svar= function (df,x) {
 x %>%names() %>%   set_label(questions$Questions)
  
}

# set_label,get_label dont work well with mutate at, solution to write a function and then throw mutate_at at them..good strategy


jhar1= jhar %>% mutate_at(vars(one:Twenty.three),sval) 


k=cor.test(jhar6$one,jhar6$Twenty.three)$estimate

# multiple correlations

jhar6 %>% select(one:Twenty.three) %>% map_df(~cor.test(.,jhar$Twenty.three)$estimate) %>% gather(key,value)


fit=lm(MeanScore~Interpersonal+Accessibility+Physical_Environment+Availability+Quality+Sex+Age+Income+Education,data=jhar6)
set_theme("forest",
          axis.title.size = .85, 
          axis.textsize = .85, 
          legend.size = .8, 
  
                geom.label.size = 3.5)
fit
sjp.lm(fit)
sjp.lm(fit, type = "std")

avi = function(x){
  ifelse(x>mean(x),1,0)
}

skval = function (x) {
  set_labels(x,labels = c("Above average" =1,"Below Average"=0)) #select column,modify
  
}

recodex = function(x)(recode(x, 1 ="Above Average",0= "Below Average"))

jhar7=jhar6 %>% select(MeanScore,Interpersonal,Accessibility,Physical_Environment,Availability,Quality) %>% mutate_all(funs(avi)) %>%mutate_all(as.factor) %>% mutate_all(skval)


fit2=glm(MeanScore~Interpersonal+Accessibility+Physical_Environment+Availability+Quality,family=binomial(),data=jhar7)
sjp.glm(fit2)

sjp.frq(jhar6$)