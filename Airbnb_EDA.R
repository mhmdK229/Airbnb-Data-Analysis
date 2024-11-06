# Exploratory Data Analysis of airbnb data.

# ******************************************
# SETUP
# ******************************************

rm(list = ls())

# Libraries

pacman::p_load(tidyverse,
               data.table,
               DT,
               visdat,
               lubridate,
               purrr,
               skimr,
               scales,
               DescTools,
               here,
               psych)

# Data
listings <- fread(here("datasets", "listings_clean.csv"))
calendar <- fread(here("datasets", "calendar_clean.csv"))

# Paths
figures <- here("figures")

# ******************************************
# MACRO LOOK
# ******************************************

# Fix variables of listings
listings[, ":="(host_is_superhost = ifelse(host_is_superhost == "t", 1, 0))]
listings <- listings %>% 
  mutate(across(.cols = where(is.character), ~ if_else(.x %in% c("", "N/A"), NA_character_, .x)))

# Listings macros
glimpse(listings)
vis_dat(listings)
vis_miss(listings, cluster = T)
listings %>% skim
listings %>% Desc
listings %>% select(where(is.numeric)) %>%  describe


# Calendar macros
glimpse(calendar)
vis_dat(calendar, warn_large_data = F)
vis_miss(calendar,  warn_large_data = F)




# ******************************************
# ONE VARIABLE ANALYSIS
# ******************************************

# response time
ggplot(listings) +
  aes(x = host_response_time) +
  geom_bar()

# host since
listings[,":="(first_year_part = if_else(host_since - floor_date(host_since, unit = "years") <= max(host_since) - floor_date(max(host_since), unit = "years"),"yes","no"))]
listings_2 <- listings[host_since - floor_date(host_since, unit = "years") <= max(host_since) - floor_date(max(host_since), unit = "years")]
ggplot(listings %>% filter(host_since >= "2009-01-01") ) +
  aes(x = factor(year(host_since)), fill = first_year_part) +
  geom_bar() +
  theme_classic() +
  scale_y_continuous(expand = c(0,NA)) +
  labs(title = "Number of new apartments per year", y = element_blank(), x = element_blank())+
  scale_fill_manual(values = c("navyblue", "dodgerblue"), breaks = c("yes","no"), name = "Dates existing \n in 2016 data" ) +
  theme(legend.position = "bottom")

ggsave("new appartments per year.png", path = figures)
table(year(listings$host_since))
summary(listings$host_since)


dt_apart_num <- listings %>% 
  arrange(host_since) %>% 
  mutate(n_apartments = 1:nrow(listings)) %>% 
  mutate(host_since = floor_date(host_since, unit = "months")) %>% 
  group_by(host_since) %>% 
  summarise(n_apartments = max(n_apartments))



ggplot(dt_apart_num, aes(x = host_since, y = n_apartments)) +
  geom_line(color = "navyblue") +
  theme_classic() +
  theme(panel.grid.major.y = element_line( size=0.75, color="gray90"))+
  scale_y_continuous(limits = c(0,3600), expand = c(0,NA), breaks = 500 * 0:7)+ 
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y",
               limits = c(min(dt_apart_num$host_since), max(dt_apart_num$host_since)+2),
               expand = c(0,0)) +
  labs(title = "Number of apartments in the market", x = element_blank(), y = element_blank())
ggsave("number of appartments in market.png", path = figures)


# number of listings per neighborhood
listings[, .(join_year = year(host_since))][,.N, by = join_year][order(join_year)]
listings[host_since >= "2009-01-01" & host_since <= "2015-12-31", .(join_month = month(host_since))][,.N, by = join_month][order(join_month)]


listings[, ":="(n_listings = .N), by = neighbourhood][,":="(neighbourhood_2 = ifelse(n_listings<50, "other", neighbourhood))]
ggplot(listings, aes(x = neighbourhood_2)) +
  geom_bar()+
  coord_flip()

table(listings$neighbourhood_2)
prop.table(table(listings$neighbourhood_2))

# reviews per month
fig_dat <- listings[,":="(reviews_per_month = ifelse(reviews_per_month > quantile(reviews_per_month, 0.99, na.rm = T),quantile(reviews_per_month, 0.99, na.rm = T), reviews_per_month))]

ggplot(fig_dat, aes(x = (reviews_per_month))) +
  geom_histogram(binwidth = 1, center = 0.5, fill = "navyblue") +
  theme_classic()+
  scale_y_continuous(expand = c(0,NA)) +
  scale_x_continuous(expand = c(0,NA), limits = c(0,10), breaks = 0:10) +
  labs(y = element_blank(), x = element_blank(), title = "Distribution of average monthly number of reviews")

ggsave("distribution of number of reviews.png", path = figures)
ggplot(listings, aes(y = reviews_per_month)) +
  geom_boxplot()
summary(listings$reviews_per_month)
table(listings$reviews_per_month)

# property type

ggplot(listings, aes(x = property_type)) +
  geom_bar()

# num of beds

ggplot(listings, aes(x = beds)) +
  geom_bar()
table(listings$beds)
summary(listings$beds)
quantile(listings$beds, 0.85, na.rm = T)
ggplot(listings, aes(x = beds)) +
  geom_boxplot(color = "navyblue") +
  theme_classic() +
  scale_x_continuous(breaks = 4 * 0:4)+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  labs(title = "Distribution of the number of beds", x = element_blank())

ggsave("distribution of the number of beds.png", path = figures, height = 3, width = 10)

listings %>% filter(beds >= 14) %>% pull(summary)

# review score rating

ggplot(listings, aes(x = review_scores_rating)) +
  geom_histogram(bins = 10)
ggplot(listings, aes(x = review_scores_rating)) +
geom_boxplot()
summary(listings$review_scores_rating)
sd(listings$review_scores_rating, na.rm = T)

ggplot(listings, aes(x = reviews_per_month, y = review_scores_rating)) +
  geom_point()

# review score accuracy

ggplot(listings, aes(x = review_scores_accuracy)) +
  geom_histogram(bins = 10)
ggplot(listings, aes(x = review_scores_accuracy)) +
  geom_boxplot()
summary(listings$review_scores_accuracy)
sd(listings$review_scores_accuracy, na.rm = T)
table(listings$review_scores_accuracy)
prop.table(table(listings$review_scores_accuracy))
ggplot(listings, aes(x = reviews_per_month, y = review_scores_accuracy)) +
  geom_point()

# review score cleanliness
ggplot(listings, aes(x = review_scores_cleanliness)) +
  geom_histogram(bins = 10)
ggplot(listings, aes(x = review_scores_cleanliness)) +
  geom_boxplot()
summary(listings$review_scores_cleanliness)
sd(listings$review_scores_cleanliness, na.rm = T)
table(listings$review_scores_cleanliness)
prop.table(table(listings$review_scores_cleanliness))
ggplot(listings, aes(x = reviews_per_month, y = review_scores_cleanliness)) +
  geom_point()

# review score checking
ggplot(listings, aes(x = review_scores_checkin)) +
  geom_histogram(bins = 10)
ggplot(listings, aes(x = review_scores_checkin)) +
  geom_boxplot()
summary(listings$review_scores_checkin)
sd(listings$review_scores_checkin, na.rm = T)
table(listings$review_scores_checkin)
prop.table(table(listings$review_scores_checkin))
ggplot(listings, aes(x = reviews_per_month, y = review_scores_checkin)) +
  geom_point()

# review score communication
ggplot(listings, aes(x = review_scores_communication)) +
  geom_histogram(binwidth = 1)
ggplot(listings, aes(x = review_scores_communication)) +
  geom_boxplot()
summary(listings$review_scores_communication)
sd(listings$review_scores_communication, na.rm = T)
table(listings$review_scores_communication)
prop.table(table(listings$review_scores_communication))
ggplot(listings, aes(x = reviews_per_month, y = review_scores_communication)) +
  geom_point()

# review score value
ggplot(listings, aes(x = review_scores_value)) +
  geom_histogram(binwidth = 1)
ggplot(listings, aes(x = review_scores_value)) +
  geom_boxplot()
summary(listings$review_scores_value)
sd(listings$review_scores_value, na.rm = T)
table(listings$review_scores_value)
prop.table(table(listings$review_scores_value))
ggplot(listings, aes(x = reviews_per_month, y = review_scores_value)) +
  geom_point()

# review score location
ggplot(listings, aes(x = review_scores_location)) +
  geom_histogram(binwidth = 1)
ggplot(listings, aes(x = review_scores_location)) +
  geom_boxplot()
summary(listings$review_scores_location)
sd(listings$review_scores_location, na.rm = T)
table(listings$review_scores_location)
prop.table(table(listings$review_scores_location))
ggplot(listings, aes(x = reviews_per_month, y = review_scores_location)) +
  geom_point()

# check correlation between different ratings 
listings %>% select(!c(id, n_listings, reviews_per_month)) %>% select(review_scores_rating, starts_with("review") ) %>% vis_cor()


# price_dollars
ggplot(listings, aes(x = price_dollars)) +
  geom_boxplot(color = "navyblue") +
  theme_classic() +
  scale_x_continuous(expand = c(0,NA), limits = c(0,4100), breaks = 500*0:8,
                     labels = scales::dollar(500*0:8))+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+
  labs(title = "Distribution of the number of beds", x = element_blank())

ggsave("price outlires.png", path = figures, height = 3, width = 10)
fig_dat <- listings[,":="(price_dollars = ifelse(price_dollars > quantile(price_dollars, 0.99, na.rm = T),quantile(price_dollars, 0.99, na.rm = T), price_dollars))]

ggplot(fig_dat, aes(x = price_dollars) ) +
  geom_histogram(binwidth = 25, center = 12.5, fill = "navyblue") +
  theme_classic()+
  scale_y_continuous(expand = c(0,NA)) +
  scale_x_continuous(expand = c(0,NA), limits = c(0,605), breaks = 25*0:24,
                     labels = scales::dollar(25*0:24)) +
  labs(y = element_blank(), x = element_blank(), title = "Distribution of prices")

ggsave("Distribution of prices.png", path = figures)

summary(listings$price_dollars)
table(listings$price_dollars) %>% sort()

mean(listings$price_dollars <= 250 & listings$price_dollars >=70)
mean(listings$price_dollars > 300)


# ******************************************
# TWO VARIABLE ANALYSIS
# ******************************************

# rating by neighborhood

ggplot(listings, aes(x = neighbourhood_2, y = review_scores_rating)) +
  geom_boxplot()

fig_dat <- listings[,.(review_scores_rating = mean(review_scores_rating, na.rm = T)), by = neighbourhood_2]
fig_dat$neighbourhood_2 <- fct_reorder(fig_dat$neighbourhood_2, fig_dat$review_scores_rating)
fig_dat <- na.omit(fig_dat)
ggplot(fig_dat, aes(x = neighbourhood_2, y = review_scores_rating)) +
  geom_segment(aes(x=neighbourhood_2,xend=neighbourhood_2,y=80, yend=review_scores_rating), size=15) +
  coord_flip()

# price by neighborhood

ggplot(listings, aes(x = neighbourhood_2, y = price_dollars)) +
  geom_boxplot()

fig_dat <- listings[,.(num = .N, price_dollars = mean(price_dollars, na.rm = T), review_scores_location = mean(review_scores_location, na.rm = T)), by = neighbourhood_2]
fig_dat$neighbourhood_2 <- fct_reorder(fig_dat$neighbourhood_2, fig_dat$price_dollars)
fig_dat <- na.omit(fig_dat)

ggplot(fig_dat, aes(x = neighbourhood_2, y = price_dollars, fill = review_scores_location)) +
  geom_bar(stat= "identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0,NA), breaks = 50 * 0:4, labels = scales::dollar(50 * 0:4)) +
  coord_flip() +
  scale_fill_gradient(low = "cyan", high = "navyblue", name = "Average rating \nof location") +
  labs(y = element_blank(), x = element_blank(), title = "It's all about the location", 
       subtitle = "Average price and average rating of location per neighborhood" ) 

ggsave("neighborhood price and rating of locatin.png", path = figures)



# price and reviews

cor(listings$price_dollars, listings$review_scores_rating, use = "complete.obs")

fig_dat <- listings[,
                    .(review_scores_rating, reviews_per_month,
                      price_dollars = ifelse(price_dollars > quantile(price_dollars, 0.99, na.rm = T),quantile(price_dollars, 0.99, na.rm = T), price_dollars))]
ggplot(fig_dat, aes(y = review_scores_rating, x = price_dollars)) +
  geom_point() +
  geom_smooth(method= "lm", se = F) +
  theme_classic()

# price and number of reviews
ggplot(fig_dat, aes(x = price_dollars, y = reviews_per_month)) +
  geom_point() +
  geom_smooth(method= "lm", se = F) +
  theme_classic()

#review and number of reviews

ggplot(listings , aes(x = review_scores_rating, y = reviews_per_month)) +
  geom_point() +
  geom_smooth(method= "lm", se = F) +
  theme_classic()

# rating and year of listing


ggplot(listings, aes(x = host_since, y = review_scores_rating)) +
  geom_point()+
  geom_smooth(method= "lm", se = F) +
  theme_classic()

# super-host reviews

ggplot(listings, aes(x = factor(host_is_superhost), y = review_scores_rating)) +
  geom_boxplot()

# host since by neighborhood

ggplot(listings %>% na.omit(), aes(x = neighbourhood_2, fill = factor(year(host_since)))) +
  geom_bar(position = position_dodge2(preserve = "single")) +
  theme_classic() +
  coord_flip()


ggplot(listings %>% na.omit() %>% filter(host_since >= "2011-01-01"),  aes(x = year(host_since), fill = first_year_part)) +
  facet_wrap(~ neighbourhood_2, scales = "free_x") +
  geom_bar() +
  theme_classic() +
  scale_y_continuous(expand = c(0,NA))+
  scale_x_continuous(limits = c(2010.5, 2016.5), breaks = 2011:2016, ) +
  scale_fill_manual(values = c("navyblue", "dodgerblue"), breaks = c("yes","no"), name = "Dates existing \n in 2016 data" ) +
  theme(strip.background = element_blank(), strip.text.x = element_text(face = "bold"),
        axis.text.x = element_text(angle = 315),legend.position = "right") +
  labs(y = element_blank(), x = element_blank(), 
       title = "Number of new apartments per year and neighborhood")
  
  
ggsave("Number of new apartments per year and neighborhood.png", path = figures)


dt_apart_num_neigh <- listings %>% 
  filter(!is.na(neighbourhood_2)) %>% 
  group_by(neighbourhood_2) %>% 
  arrange(neighbourhood_2,host_since) %>% 
  mutate(n_apartments = 1:n(), host_since = floor_date(host_since, unit = "weeks")) %>% 
  group_by(neighbourhood_2,host_since) %>% 
  summarise(n_apartments = max(n_apartments))

ggplot(dt_apart_num_neigh, aes(x = host_since, y = n_apartments)) +
  geom_line(color = "navyblue") +
  theme_classic() +
  facet_wrap(~ neighbourhood_2, scales = "fixed") +
  theme(strip.background = element_blank(), strip.text.x = element_text(face = "bold"),
        axis.text.x = element_text(angle = 315), panel.grid.major.y = element_line( size=0.75, color="gray90"))+
  scale_x_date(date_breaks = "2 year", 
               date_labels = "%Y",
               limits = c(min(dt_apart_num$host_since), max(dt_apart_num$host_since)+2),
               expand = c(0,0)) +
  labs(title = "Number of apartments in each neighborhood",
       subtitle = "Sep 2009 - Sep 2016",
       x = element_blank(), y = element_blank())
ggsave("number of appartments by neighborhood.png", path = figures)

# beds by host since
ggplot(listings, aes(x = host_since, y= beds)) +
  geom_point()+
  geom_smooth(method= "lm", se = F) +
  theme_classic()

listings %>% 
  mutate(year = year(host_since)) %>% 
  group_by(year) %>% 
  summarise(beds = mean(beds, na.rm = T)) %>% 
  ggplot(aes(x = year, y = beds)) +
  geom_bar(stat = "identity") +
  theme_classic()+
  scale_x_continuous(breaks = 2008:2016, labels = as.character(2008:2016))

# price by host since
fig_dat <- listings[,
                    .(host_since = round_date(host_since, unit = "month"), 
                      price_dollars = ifelse(price_dollars > quantile(price_dollars, 0.99, na.rm = T),quantile(price_dollars, 0.99, na.rm = T), price_dollars))]
ggplot(fig_dat, aes(x = host_since, y= price_dollars)) +
  geom_point()+
  geom_smooth(method= "lm", se = F) +
  theme_classic()



ggplot(fig_dat, aes(x = host_since, y= price_dollars)) +
  geom_bar(stat = "identity")
  

# Location review by neighbourhood

ggplot(listings, aes(x = neighbourhood_2, y = review_scores_location)) +
  geom_boxplot()

fig_dat <- listings[,.(review_scores_location = mean(review_scores_location, na.rm = T),
                       price_dollars = mean(price_dollars, na.rm = T)), by = neighbourhood_2]
fig_dat$neighbourhood_2 <- fct_reorder(fig_dat$neighbourhood_2, fig_dat$review_scores_location)
fig_dat <- na.omit(fig_dat)
ggplot(fig_dat, aes(x = neighbourhood_2, y = review_scores_location)) +
  geom_segment(aes(x=neighbourhood_2,xend=neighbourhood_2,y=7.5, yend=review_scores_location, color = price_dollars), size=15) +
  scale_color_gradient(low = "lightblue", high = "blue4")

#price by number of beds

fig_dat <- listings[,.(price_dollars = mean(price_dollars, na.rm = T)), by = beds]
ggplot(fig_dat, aes(x = beds, y = price_dollars)) +
  geom_bar(stat = "identity")

#reviews per month by neighborhood
fig_dat <- listings[,reviews_per_month = mean(reviews_per_month, na.rm = T), by = neighbourhood_2]
ggplot(listings, aes(x = neighbourhood_2, y = reviews_per_month)) +
  geom_boxplot()+
  coord_flip()

# length of description and rating

listings[,":="(desc_length = ifelse(!is.na(summary), nchar(summary), 0))]

ggplot(listings %>% filter(review_scores_rating >=80), aes(x = desc_length, y = review_scores_rating)) +
  geom_point()+
  geom_smooth(method= "lm", se = F) +
  theme_classic()

# correlations among reviews 
fig_dat <- listings %>% 
  pivot_longer(cols = c(review_scores_accuracy, review_scores_checkin, review_scores_cleanliness, 
                         review_scores_communication, review_scores_location, review_scores_value),
               names_to = "var", values_to = "rating", names_prefix = "review_scores_") %>% 
  group_by(var) %>% 
  summarise(cor_rating = cor(rating, review_scores_rating, use = "complete.obs"),
            cor_price = cor(price_dollars, rating, use = "complete.obs")) %>%
  mutate(var = factor(str_to_title(if_else(var != "checkin", var, "checking")))) %>% 
  mutate(var = fct_reorder(var, cor_rating)) %>% 
  pivot_longer(cols = starts_with("cor"), names_to = "outcome", values_to = "cor" ) 
   

ggplot(fig_dat, aes(x = var, y = cor, fill = outcome)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single"))+ 
  theme_classic() +
  scale_y_continuous(expand = c(0,0), limits = c(-0.01,0.85)) +
  geom_text(aes(label = round(cor, 2)), position = position_dodge(0.9), vjust = -0.5) +
  labs(x = element_blank(), y = "Correlation", title = "Correlation of ratings with price and with the overall rating") +
  geom_hline(yintercept = 0, color = "gray60") +
  scale_fill_manual(values = c("navyblue", "darkred"), name = "", labels = c("Correlation \n with price", "Correlation  with \n overall rating"))

ggsave("Correlation of ratings with price and with the overall rating.png", path = figures)
  
# ******************************************
# METRICS
# ******************************************

# Total number of nights growth - rounded at months - city level
dt <- merge(calendar,
            listings,
            by.x = "listing_id", by.y = "id")
dt[, ":="(month = floor_date(date, unit = "months"), week = floor_date(date, unit = "weeks"), year = year(date))]

calc_per_change_rounded_city <- function(dt, rounding_per){
  name_rounding_per <- as.name(rounding_per)
  out <- 
    dt[,.(total_num_nights = sum(1-available_category)), by = list(year, eval(name_rounding_per))][
      order(year, eval(name_rounding_per))][,":="(total_num_nights_prev = shift(total_num_nights,1))]
  out[,":="(per_change = ifelse(!is.na(total_num_nights_prev), 100 * (total_num_nights/ total_num_nights_prev - 1), NA))]
  setnames(out,"name_rounding_per", rounding_per)
  return(out)
}

dt_rounded_month_city <- calc_per_change_rounded_city(dt, "month")
dt_rounded_week_city <- calc_per_change_rounded_city(dt, "week")


compute_tot_num_of_nights_in_window <- function(vec,window){
  vec <- map_dbl(window:length(vec), ~sum(vec[(.x+1-window):.x]))
  vec <- c(rep(NA, window-1), vec)
  vec
}
calc_per_change_cont_city <- function(dt, window){
  out <- dt[,.(total_num_nights = sum(1-available_category)), by = date][order(date)]
  out <- out[,":="(total_num_nights_win = compute_tot_num_of_nights_in_window(total_num_nights, window))]
  out <- out[,total_num_nights_win_shift := shift(total_num_nights_win,window)]
  out[,":="(per_change = ifelse(!is.na(total_num_nights_win_shift), 100 * (total_num_nights_win/ total_num_nights_win_shift - 1), NA))]
  return(out)
}

dt_cont_city_30 <- calc_per_change_cont_city(dt, 30)
dt_cont_city_7 <- calc_per_change_cont_city(dt, 7)
ggplot(dt_cont_city_30 %>% na.omit(), aes(x = date, y = total_num_nights_win)) +
  geom_line()
ggplot(dt_cont_city_30 %>% na.omit(), aes(x = date, y = per_change)) +
  geom_line()

ggplot(dt_cont_city_7 %>% na.omit(), aes(x = date, y = per_change)) +
  geom_line()
ggplot(dt_rounded_month_city %>% na.omit(), aes(x = month, y = per_change))+
  geom_line()
ggplot(dt_rounded_week_city[3:52,] %>% na.omit(), aes(x = week , y = per_change))+
  geom_line()


calc_per_change_cont_neighborhood <- function(dt, window){
  dt <- dt[!is.na(neighbourhood) & neighbourhood !=""]
  out <- dt[,.(total_num_nights = sum(1-available_category)), by = list(date, neighbourhood)][order(neighbourhood,date)]
  out <- out[,":="(total_num_nights_win = compute_tot_num_of_nights_in_window(total_num_nights, window)), by = neighbourhood]
  out <- out[,total_num_nights_win_shift := shift(total_num_nights_win,window),by = neighbourhood]
  out[,":="(per_change = ifelse(!is.na(total_num_nights_win_shift), 100 * (total_num_nights_win/ total_num_nights_win_shift - 1), NA)),by = neighbourhood]
  return(out)
}
dt_cont_neigh_30 <- calc_per_change_cont_neighborhood(dt,30)

ggplot(dt_cont_neigh_30 %>% na.omit(), aes(x = date , y = total_num_nights_win))+
  geom_line() +
  facet_wrap(~ neighbourhood, scales = "free")



# change in number of apartments in the market

dt_metrics_city <- listings %>% 
  arrange(host_since) %>% 
  mutate(n_apartments = 1:nrow(listings)) %>% 
  group_by(host_since) %>% 
  summarise(n_apartments = max(n_apartments)) %>%
  mutate(year = year(host_since)) %>% 
  group_by(year) %>% 
  mutate(not_after_date = if_else(host_since <= ymd(paste0(year,"-09-06")), 1,0)) %>% 
  filter(not_after_date == 1) %>% 
  mutate(is_max_date = if_else(host_since == max(host_since), 1, 0)) %>% 
  filter(is_max_date == 1)

dt_metrics_city <- as.data.table(dt_metrics_city)
dt_metrics_city[,n_apartments_shift := shift(n_apartments, 1),]

dt_metrics_city <- dt_metrics_city %>% 
  mutate(per_change = 100 * (n_apartments - n_apartments_shift) / n_apartments_shift,
         tot_change = n_apartments - n_apartments_shift)

ggplot(dt_metrics_city %>% na.omit(), aes(x = year, y = per_change) ) +
  geom_point(color = "navyblue")+
  geom_line(color = "navyblue") +
  theme_classic() +
  scale_x_continuous(breaks = 2010:2016, labels = 2010:2016) +
  scale_y_continuous(limits = c(0,105),breaks = seq(10,100,10), labels = scales::percent(seq(0.1,1,0.1), accuracy = 1)) +
  labs(title = "Yearly % growth in total number of apartments", subtitle = "Calculated at september 6th", x = element_blank(), y = element_blank()) +
  geom_text(label = scales::percent(dt_metrics_city$per_change[2:8] / 100, accuracy = 0.1), nudge_y = 6)

ggsave("percent growth in total number of apartments - city.png", path = figures)

ggplot(dt_metrics_city %>% na.omit(), aes(x = year, y = tot_change) ) +
  geom_bar(stat = "identity", fill = "navyblue")+
  theme_classic() +
  scale_x_continuous(breaks = 2010:2016, labels = 2010:2016)  +
  scale_y_continuous(limits = c(0,1060), expand = c(0,NA))+
  labs(title = "Yearly growth in number of apartments", subtitle = "Calculated at september 6th", x = element_blank(), y = element_blank()) +
  geom_text(label = dt_metrics_city$tot_change[2:8], nudge_y = 40)
  
ggsave("yearly growth in number of apartments.png", path = figures)


dt_metrics_neigh <- listings %>% 
  group_by(neighbourhood_2) %>% 
  arrange(neighbourhood_2,host_since) %>% 
  mutate(n_apartments = 1:n()) %>% 
  group_by(neighbourhood_2,host_since) %>% 
  summarise(n_apartments = max(n_apartments)) %>%
  mutate(year = year(host_since)) %>% 
  group_by(neighbourhood_2,year) %>% 
  mutate(not_after_date = if_else(host_since <= ymd(paste0(year,"-09-06")), 1,0)) %>% 
  filter(not_after_date == 1) %>% 
  mutate(is_max_date = if_else(host_since == max(host_since), 1, 0)) %>% 
  filter(is_max_date == 1 & !is.na(neighbourhood_2))

dt_metrics_neigh <- as.data.table(dt_metrics_neigh)
dt_metrics_neigh[,n_apartments_shift := shift(n_apartments, 1), by = neighbourhood_2]

dt_metrics_neigh <- dt_metrics_neigh %>% 
  mutate(per_change = 100 * (n_apartments - n_apartments_shift) / n_apartments_shift,
         tot_change = n_apartments - n_apartments_shift)

ggplot(dt_metrics_neigh , aes(x = year, y = per_change) ) +
  geom_point(color = "navyblue")+
  geom_line(color = "navyblue") +
  facet_wrap(~ neighbourhood_2, scales = "free_y") +
  theme_classic() +
  scale_x_continuous(breaks = 2010:2016, labels = 2010:2016) +
  labs(title = "Yearly % growth in total number of apartments", subtitle = "Calculated at september 6th", x = element_blank(), y = element_blank()) 


ggplot(dt_metrics_neigh %>% na.omit(), aes(x = year, y = tot_change) ) +
  geom_bar(stat = "identity", fill = "navyblue")+
  theme_classic() +
  facet_wrap(~ neighbourhood_2, scales = "fixed") +
  scale_x_continuous(breaks = 2010:2016, labels = 2010:2016)  +
  scale_y_continuous(expand = c(0,NA), limits = c(0,120), breaks = 20 * 0:6)+
  theme(strip.background = element_blank(), strip.text.x = element_text(face = "bold"),
        axis.text.x = element_text(angle = 315),panel.grid.major.y = element_line( size=0.75, color="gray90")) +
  labs(title = "Yearly growth in number of apartments per neighborhood", subtitle = "Calculated at september 6th", x = element_blank(), y = element_blank()) 

ggsave("Yearly growth in number of apartments per neighborhood.png", path = figures)

# ******************************************
# Extreme Values
# ******************************************



fig_dat <- calendar %>% 
  filter(year(date) == 2017 & month(date) <= 6 & month(date) >= 3) %>% 
  group_by(date) %>% 
  summarise(price = mean(as.numeric(str_remove(price_dollars, "[$]")), na.rm = T), rented = sum(1 - available_category) / nrow(listings))

ggplot(fig_dat, aes(x = date, y = price)) +
  geom_line(color = "navyblue") +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", expand = c(0,4)) +
  theme(panel.grid.major.y = element_line( size=0.75, color="gray90")) +
  labs(title = "Average price per date", x = element_blank(), y = element_blank()) +
  geom_rect(xmin = ymd("2017-04-11"), xmax = ymd("2017-04-19"), ymin = 185, ymax = 220,
            alpha = 0, color = "red", linetype = "dashed", size = 0.75) +
  annotate(geom = "text", x = ymd("2017-04-05"), y = 215, 
           label = "Easter \n week", size = 4.5,
           fontface = "italic", hjust = 0.5,
           lineheight = 0.85, color = "gray40")

ggsave("mean price - easter.png", path = figures, dpi = 900)
  
dt <- dt %>% 
  mutate(price = as.numeric(str_remove(price_dollars.x, "[$]")))

dt_low_price <- dt %>% 
  filter(listing_id ==12736032)

check <- dt %>% 
  group_by(listing_id) %>% 
  summarise(mean(available_category), sum(available_category), n())



ggplot(fig_dat, aes(x = date, y = rented)) +
  geom_line()


fig_dat <- calendar %>% 
  mutate(date = floor_date(date, unit = "weeks") ) %>% 
  group_by(date) %>% 
  summarise(price = min(as.numeric(str_remove(price_dollars, "[$]")), na.rm = T), rented = sum(1 - available_category) / nrow(listings))

ggplot(fig_dat, aes(x = date, y = price)) +
  geom_line()

ggplot(fig_dat, aes(x = date, y = rented)) +
  geom_line()


calendar %>% 
  group_by(listing_id) %>% 
  summarise(mean_rented = mean(1 - available_category), price = mean(as.numeric(str_remove(price_dollars, "[$]")), na.rm = T)) %>% 
  view()

calendar %>% 
  mutate(date = floor_date(date, unit = "weeks")) %>% 
  filter(!date %in% c(min(date), max(date))) %>% 
  group_by(date) %>% 
  summarise(available_aparts = sum(available_category)) %>% 
  view()
  ggplot(aes(x= date, y = available_aparts)) +
  geom_line() +
  geom_point()
  
  
calendar %>% 
  group_by(date) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(date = floor_date(date, unit = "weeks")) %>% 
  filter(!date %in% c(min(date), max(date))) %>% 
  group_by(date) %>% 
  summarise(n = mean(n)) %>% 
  ggplot(aes(x = date, y = n)) +
  geom_point()+
  geom_line()

calendar