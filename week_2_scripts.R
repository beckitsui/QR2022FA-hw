#Many municipalities implementing smart-growth principles do so by designated specific areas—sometimes called clusters —with more 
#flexible zoning. Usually, the intent is to obtain a mix of uses in a more walkable environment. 
#Sometimes, these clusters get their own special zoning area or “planned unit development” in the zoning map. 
#Sometimes, they are conceived as overlay districts: special areas that cross over two or more zoning districts, 
#but in which regulated exceptions from the basic zoning guidelines are allowed.  
#In such clusters or flexible development areas, towns usually provide incentives for developers to build at higher densities.


# install libraries
install.packages("xtable")
install.packages("janitor")
install.packages("tidyverse")

#import libraries
library(xtable) # cross tabulation
library(janitor)  # percentage tabulation
library(tidyverse) # R packages for data science
library("readxl")


# setting up directory
getwd()
# if getwd() doesn't work, then choose Session menu on the top -> Set working directory -> choose directory then choose the directory where this R file is at.
setwd("/Users/beckyxu/Documents/MIT FA 2022/Quantitative Reasoning/Week2 HW")

# loading data
masszoning <- read_xls("MASSZONING.xls")

# view data
View(masszoning)

#Question 1 : minimal required plot size; histogram with binsize of 10; how do I change the grid proportions?
mu_clparcel <- mean(masszoning$clparcel, na.rm = TRUE)
mu_clparcel

str(masszoning$clparcel)

min_parcel <- ggplot(masszoning, aes(x=clparcel), na.rm = TRUE)+
  geom_histogram(bins = 10, color = "white", fill = "light blue")+
  geom_vline(aes(xintercept=mu_clparcel), color = "blue", linetype = "dashed", size = 1)+
  labs(title = "Minimum Required Parcel Size", x = 'parcel size')+
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid.major = element_line(color = 'lightgrey', size = .2),
        panel.grid.minor = element_line(color = 'lightgrey', size = .1))
min_parcel


#Question 2 : median minimum plot size. Answer: 7.75
median(masszoning$clparcel, na.rm = TRUE) 


#Question 3: Display the histogram of the distribution of open space requirements in new cluster developments 
#(variable name: clopen), using 40 equally-spaced bins. Can you see any interesting numerical patterns?

openspace <- ggplot(masszoning, aes(x = clopen), na.rm = TRUE)+
  geom_histogram(bins = 40, color = "white", fill = "lightgreen")+
  labs(title = 'open space requirements in new cluster developments', x = 'open space requirement')+
  theme(panel.background = element_rect(fill = "white", color = 'black'),
        panel.grid.major = element_line(color = 'lightgrey', size = .2),
        panel.grid.minor = element_line(color = 'lightgrey', size = .1))

openspace

#answer: the incremental number for required open space seems to be 5.



#Question 4: What percentage of towns exclude wetlands, easements, 
#or sloped land from calculations of minimum land area requirements (variable name: mlaexclud)?
#Does this exclusion make it harder or easier 
#to develop housing and other real estate there?

percentage_exclusion <- sum(masszoning$mlaexclud)/nrow(masszoning)
percentage_exclusion
  
# The percentage is 62.5%
# The exclusion made less buildable land available. However, it does not make the development there
# harder (or how we define harder). Wetlands are protected by EPA and are important to preserve for LEED.
# Easement may require going around laws and more capitals for settlement.
# Developing on slope also require more money for construction (breaking rocks and laying foundations)



#Question 5: What percentage of towns impose shape rules on lots for development (variable name: shaprule)? 
#What do you think might be their rationale to do so?

percentage_shaperules <- sum(masszoning$shaprule != 0)/nrow(masszoning)
percentage_shaperules

# The percentage is 43.3%
# Shaperule may help prevent developments to take up too much (if not all) lot floor areas.
# The setbacks can be for green space / public uses etc



#Question 6: What fraction of municipalities have inclusionary zoning provisions (in variable include)? 
#Of which type? Show your results in a bar chart or other graph that correctly represents numerical proportions. 
#You can label the chart if you want but there is no need to.

inclusion <- data.frame("Percentage" = c(percent(sum(masszoning$include == 0) / nrow(masszoning)),
                        percent(sum(masszoning$include == 1) / nrow(masszoning)),
                        percent(sum(masszoning$include == 2) / nrow(masszoning)),
                        percent(sum(masszoning$include == 3) / nrow(masszoning))),
                        "Types" = c('No inclusionary','Optional','Mandatory','Both optional and mandatory')
                        )
#percent(sum(masszoning$include == 1) / nrow(masszoning))
head(inclusion)

inclusion$Types <- factor(inclusion$Types, inclusion$Types[order(inclusion$Percentage, decreasing = TRUE)])
inclusion
str(inclusion)

ggplot(inclusion, aes(x = "", y = Percentage, fill = Types))+
  geom_bar(stat="identity", width=1)+
  geom_text(aes(label = Percentage),
            color = "darkgrey",
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y")+
  scale_fill_brewer("Types",palette = 2, direction = -1) +
  labs(title = "Percentage of Inclusionary Zoning Provisions", x = '', y = '')


inclusion %>% print(.$Types)

#Question 8 : One of the variables in the survey captures whether the town allows  
#apartments above ground-floor commercial uses (variable name: mfmixed). 
#Another question asks which process (e.g., special permit) is used for developers to be allowed to build multi-family housing, 
#if they are at all (variable name: mfallow). Provide cross-tabulations of these two variables. 
#What is the combination of the two that is more frequent in suburban Massachusetts?
mftab <- masszoning[c('mfmixed','mfallow')] #subsetting df with only mfmixed and mfallow

crosstab_mf <- xtabs(~mfallow+mfmixed, data = mftab)

plot_xtab(mftab$mfallow, mftab$mfmixed, margin = "row", bar.pos = "stack", coord.flip = FALSE)+
  theme_sjplot(base_size = 10, base_family = "")

#Question 9: Some municipalities allow attached single-family homes, others do not (variable name: townhous). 
#Similarly, some allow accessory units (e.g., a rental unit on the basement or attic)–variable accesapt. 
#Cross-tabulate these variables. Do you see any notable patterns?
attachroomtab <- masszoning[c('accesapt','townhous')] #subsetting df
crosstab_attachroom <- xtabs(~accesapt+townhous, data = attachroomtab)

plot_xtab(attachroomtab$accesapt, attachroomtab$townhous, margin = "row", bar.pos = "stack", coord.flip = FALSE)

#cross tabulation
xtabs(~nonwhite_perc_cat, data=mydata) 

tabyl(mydata, nonwhite_perc_cat)

xtabs(~median_income_quantile+nonwhite_perc_cat, data=mydata)

xtabs(~nonwhite_perc_cat, median_income_quantile==5, 
      data=mydata)

xtabs(~median_income_quantile+nonwhite_perc_cat, 
      owner_occupied_quantile==5,
      data=mydata)

# pie chart
tbl <- tabyl(mydata$nonwhite_perc_cat)
pie(tbl$percent, 
    # We add labels with percentage
    labels=paste0(tbl$`mydata$nonwhite_perc_cat`, 
                  ": ", 
                  floor(tbl$percent * 100), "%"),
    # We add a title
    main = "Pie Chart of the variable 'nonwhite_perc_cat'")




df1 <- data.frame(number = c(29767,41261,20978,31473,51655,32744,64745,47836))
ggplot(df1, aes(y=number))+geom_boxplot()
sort(df1$number)
median(df1$number)
quantile(df1$number)
