library(stringr)
library(knitr)
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

#write.csv(discrimination_df,'discrimination.csv')
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

##The most interesting thing about this plot is that the specific categories
##don't match up with the anecdotes given in the blog posts.
top_4_products <- ggplot(product_count, aes(x = Product, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
 ggtitle('Products with the Most Mentions \n of Racial Discrimination')+
  #coord_flip() +
  scale_x_discrete(labels = c('Checking/Savings', 'Credit card', 'Credit score \nreporting', 'Mortgage')) +
  theme_pubclean()
update_labels(top_4_products,list(x = 'Product', y = 'No. of Cases '))

##Find the mortgage companies who are the worst offenders
mortgage_discrimination_df <- discrimination_df[discrimination_df$Product == 'Mortgage',]

company_count <- mortgage_discrimination_df %>%
  group_by(Company) %>%
  summarise(Cases = n())

company_count <- arrange(company_count,desc(Cases))
company_count <- company_count[1:10,]

#I don't know if I gain anything by including this plot. The list of values
#seems to work better
#top_company_offenders <- ggplot(company_count, aes(x = Company, y = counts)) +
#  geom_bar(fill = "red", stat = "identity") +
#  coord_flip() +
#  scale_x_discrete(label = abbreviate)+
#  theme_classic2()
#plot(top_company_offenders)
kable(company_count)

##Find which companies appear on multiple lists
multiple_offenders_count <- discrimination_df %>%
  group_by(Company, Product, add = TRUE) %>%
  summarise(Cases = n())
ids <- multiple_offenders_count$Company
multiple_offenders_count <- multiple_offenders_count[duplicated(ids) |
                                                       duplicated(ids, fromLast = TRUE),]
#multiple_offenders_count <- multiple_offenders_count %>%
#  group_by(Company) %>%
#  summarise(Cases = n())
multiple_offenders_count <- arrange(multiple_offenders_count,desc(Cases))
multiple_offenders_count <- multiple_offenders_count[1:10,]
multiple_offenders_count <- arrange(multiple_offenders_count,Company)
kable(multiple_offenders_count)

## Find the states where most occurrences are
#state_count <- discrimination_df %>%
#  group_by(State) %>%
#  summarise(Cases = n())
library(maps)
usa <- map_data("usa")

ggplot() + 
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), alpha = 0.2, color = "white") + 
  geom_point(data = state_count, aes(x = long, y = lat, size = Cases, color = Cases)) +
  coord_fixed(1.3) +
  guides(fill=FALSE)

##Check the following to determining company response
#Public response
public_response_count <- discrimination_df %>%
  group_by(Company.public.response) %>%
  summarise(Cases = n())
#response to consumer
response_to_consumer_count <- discrimination_df %>%
  group_by(Company.response.to.consumer) %>%
  summarise(Cases = n())
#timely response
timely_response_count <- discrimination_df %>%
  group_by(Timely.response.) %>%
  summarise(Cases = n())
