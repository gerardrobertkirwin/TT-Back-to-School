#Download Data ------------------------------------------------------------

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

# Upload packages ---------------------------------------------------------

library(readxl)
library(tidyverse)
library(ggplot2)
library(stringr)
library(tidyr)
library(plyr)
library(ggthemes)

# Clean up Data -----------------------------------------------------------
az_schools <- school_diversity %>% 
  filter(ST == 'AZ') 

az_unified_schools <- az_schools %>% 
  filter(!grepl('elementary|high school|accommodation|accom|regional', LEA_NAME) & !is.na(d_Locale_Txt))

az_unified_schools_long <- gather(az_unified_schools, Race, Percent, AIAN:Multi, factor_key=TRUE) %>% 
  rename('District Type' = d_Locale_Txt)


# plot of of school population vs % of racial group facet by racial group name ----------------------------------
az_unified_schools_long %>% 
ggplot(aes(x=Percent, y=Total, color=`District Type`)) +
geom_jitter()+
theme_classic()+
theme(panel.border = element_rect(linetype = "dashed", fill = NA)) +
labs(title ="Racial Distribution in Unified Arizona School Districts", x = "Percent", y = "Total Student Population") +
facet_grid(Race~SCHOOL_YEAR)


# calculate student population change, sorted by biggest ----------------------------------
az_unified_schools_diff <-  az_unified_schools %>% 
  mutate(SCHOOL_YEAR= ifelse(SCHOOL_YEAR == "1994-1995", 1,2)) %>%  
  mutate(change = ifelse(SCHOOL_YEAR - 1 == lag(SCHOOL_YEAR), ((Total - lag(Total))/lag(Total)*100), NA)) %>% 
  arrange(desc(change))

#plot stacked bar based on editorial choice of largest ----------------------------------
growth <- c("higley unified school district", "vail unified district", "florence unified school district", "maricopa unified school district", "cave creek unified district", "sahuarita unified district")

pick <- function(condition){
  function(d) d %>% filter_(condition)
}

az_unified_schools_long %>% filter(LEA_NAME %in% growth) %>%
mutate(LEA_NAME=str_replace_all(LEA_NAME, "unified school district", "")) %>% 
mutate(LEA_NAME=str_replace_all(LEA_NAME, "unified district", "")) %>% 
ggplot(aes(y=Percent, x=LEA_NAME, fill=Race), stat="identity") +
geom_col(position = "stack")+
theme(plot.title=element_text(family="OfficinaSanITC-Book"),
        text=element_text(family="OfficinaSanITC-Book")) +  
geom_text(data= pick(~Percent > 5.6), aes(y = Percent, x = LEA_NAME,label = paste0(round(Percent,2),"%")),
           colour="black", family="OfficinaSanITC-Book", size=4, position = position_stack(vjust = 0.55)) +
facet_grid(~SCHOOL_YEAR) +
theme_economist() + 
scale_fill_economist() +
theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +  
labs(x="District Name", y="Percentage") +
ggtitle("Racial Distribution in Fastest Growing Arizona School Districts")  

