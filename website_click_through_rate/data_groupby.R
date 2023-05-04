## Apply group_by for each age_group and get the click_through rate for each age_group 
## step 1

data_grouped <- data1 %>% 
  group_by(age_group) %>% 
  summarize(total_clicks=sum(Clicks),total_impressions=sum(Impressions))

data_grouped
# # A tibble: 7 × 3
# age_group total_clicks total_impressions
# <fct>            <dbl>             <dbl>
#   1 <18               2065             69239
# 2 18-24             2167            203585
# 3 25-34            22417            975994
# 4 35-44             3662            355824
# 5 45-54             3232            322109
# 6 55-64             4556            224688
# 7 65+               4350            144120

#step 2 : now create a new variable click_through_rate 

data_grouped <- mutate(data_grouped,click_through_rate=total_clicks/total_impressions)

data_grouped
# # A tibble: 7 × 4
# age_group total_clicks total_impressions click_through_rate
# <fct>            <dbl>             <dbl>              <dbl>
#   1 <18               2065             69239             0.0298
# 2 18-24             2167            203585             0.0106
# 3 25-34            22417            975994             0.0230
# 4 35-44             3662            355824             0.0103
# 5 45-54             3232            322109             0.0100
# 6 55-64             4556            224688             0.0203
# 7 65+               4350            144120             0.0302