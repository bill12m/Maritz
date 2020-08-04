library(stringr)
library(stringi)
library(dplyr)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

cat("\014")
complaints_raw <- read.csv("complaints.csv")
##Filter data by searching for keywords in the Consuper Narrative
discrimination_df <- complaints_raw[str_detect(complaints_raw$Consumer.complaint.narrative, 'ethnic') | 
                                    str_detect(complaints_raw$Consumer.complaint.narrative, ' race ') |
                                    str_detect(complaints_raw$Consumer.complaint.narrative, 'racial'),]

##The code below was for counting the number of times each product appeared in 
##the data
#product_count <- discrimination_df %>%
#  group_by(Product) %>%
#  summarise(counts = n())
#product_count

##After counting the frequency of each product, take the top 4 and plot the
##results
product_count <- discrimination_df[str_detect(discrimination_df$Product,'Mortgage') |
                                     str_detect(discrimination_df$Product,'credit repair services')|
                                     str_detect(discrimination_df$Product,'Credit card or prepaid card') |
                                     str_detect(discrimination_df$Product,'Checking or savings account'),] %>%
  group_by(Product) %>%
  summarise(counts = n())
top_4_products <- ggplot(product_count, aes(x = Product, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  ggtitle('Products with the Most Mentions \n of Racial Discrimination')+
  scale_x_discrete(labels = c('Checking/\nSavings', 'Credit card', 'Credit score \nreporting', 'Mortgage')) +
  theme_pubclean()
update_labels(top_4_products,list(x = 'Product', y = 'No. of Cases '))

##Calculate the number of cases with no narrative or a 
##narrative containing some form of the string 'discriminate'
complaints_raw$Year <- as.numeric(stri_sub(complaints_raw$Date.received,1,4))
blanks <- as.numeric(nrow(complaints_raw[stri_isempty(complaints_raw$Consumer.complaint.narrative) == TRUE &
                                           complaints_raw$Year > 2014,]))
discriminate <- as.numeric(nrow(complaints_raw[str_detect(complaints_raw$Consumer.complaint.narrative,'discrimin'),]))
cat('Percent of blank narrtives since 2015: ', blanks / nrow(complaints_raw))
complaints_raw$Year <- NULL

##Find which companies appear on multiple lists
multiple_offenders_count <- discrimination_df %>%
  group_by(Company, Product, add = TRUE) %>%
  summarise(Cases = n())
ids <- multiple_offenders_count$Company
multiple_offenders_count <- multiple_offenders_count[duplicated(ids) |
                                                       duplicated(ids, fromLast = TRUE),]
multiple_offenders_count <- arrange(multiple_offenders_count,desc(Cases))
multiple_offenders_count <- multiple_offenders_count[1:10,]
multiple_offenders_count <- arrange(multiple_offenders_count,Company)
#write.csv(multiple_offenders_count,'offenders.csv')

## Find the states where most occurrences are
#state_count <- discrimination_df %>%
#  group_by(State) %>%
#  summarise(Cases = n())
## Took this dataframe to bubble_map.R to add latitude and longitude
library(maps)
usa <- map_data("state")
state_count<-read.csv('states.csv')

ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), alpha = 0.2, color = "white") + 
  geom_point(data = state_count, aes(x = long, y = lat, size = Cases, color = Cases)) +
  ggtitle('No. of Cases Reporting Racial Discrimination by State') +
  coord_fixed(1.3) +
  guides(fill=FALSE) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
