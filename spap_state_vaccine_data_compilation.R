###### author details: Taegyoon Kim, taegyoon@psu.edu
###### purpose: This script is used to compile a month-legislator data set with tweet count, vaccine-related tweet count, and anti-vaccine tweet count.
###### note: component data sets will be upload later.
###### last edit: 1 July 2021


# packages -------------------------

lapply(c('tidyverse', 'readr', 'zoo'), require, character.only = TRUE)


# paths -------------------------

path_spap_state <- 'Google Drive/spap_state/data/' 
path_spap_state_vaccine <- 'Google Drive/spap_state/spap_state_vaccine/' 


# read data set for tweets until 16 Feb 2021 -------------------------

tweets_20210216 <- read_csv(paste0(path_spap_state, 'full_tweets_2_16_2020.csv'), # tweet data
                            col_types = cols(
                              id_str = col_character(), 
                              user.id_str = col_character(), 
                              user.name = col_skip(), 
                              retweeted_status.full_text = col_skip(), 
                              quoted_status.full_text = col_skip()
                              )
                            )

tweets_20210216_vaccine_label_anti <- read_csv(paste0(path_spap_state_vaccine, 'tweets_20210216_vaccine_label_anti.csv'), # label data
                                               col_types = cols(
                                                 X1 = col_skip(), 
                                                 level_0 = col_skip(), 
                                                 id_str = col_character(), 
                                                 user.id_str = col_character()
                                                 )
                                               )

tweets_20210216_full <- left_join(tweets_20210216,  # tweet + label
                                  tweets_20210216_vaccine_label_anti[c('id_str', 'label', 'anti')], 
                                  by = c("id_str" = "id_str")
                                  )

tweets_20210216_full <- tweets_20210216_full %>% # replace NA with 0
  replace_na(list(label = 0, anti = 0))

tweets_20210216_full <- tweets_20210216_full %>% # drop duplicates
  distinct(id_str, .keep_all = TRUE)

tweets_20210216_full <- tweets_20210216_full %>% # remove "_"s in state names
  mutate(legislator_state = base::gsub("_", " ", legislator_state))

tweets_20210216_full <- tweets_20210216_full[which(tweets_20210216_full$user.id_str != '712842098228105216' | '9276672'),] # remove hand-identified irrelevant accounts


# read data set for tweets from 1 Feb to 12 Feb 2021 -------------------------

tweets_20210201_20210512 <- read_csv(paste0(path_spap_state, 'tweets_20210201_20210512.csv'), 
                                     col_types = cols(
                                       X1 = col_skip(), 
                                       id_str = col_character(), 
                                       user_id_str = col_character()
                                       )
                                     )

colnames(tweets_20210201_20210512)[4]<- 'user.id_str' 

tweets_20210201_20210512_vaccine_label_anti <- read_csv(paste0(path_spap_state_vaccine, 'tweets_20210201_20210512_vaccine_label_anti.csv'), 
                                                        col_types = cols(
                                                          X1 = col_skip(), 
                                                          `Unnamed: 0` = col_skip(), 
                                                          id_str_x = col_character(), 
                                                          user_id_str = col_character()
                                                          )
                                                        )

tweets_20210201_20210512_full <- left_join(tweets_20210201_20210512, # tweet + label
                                           tweets_20210201_20210512_vaccine_label_anti[c('id_str_x', 'label', 'anti')], 
                                           by = c("id_str" = "id_str_x"))

tweets_20210201_20210512_full <- tweets_20210201_20210512_full %>% # replace NA with 0
  replace_na(list(label = 0, anti = 0))

tweets_20210201_20210512_full <- tweets_20210201_20210512_full %>% # drop duplicates
  distinct(id_str, .keep_all = TRUE)

tweets_20210201_20210512_full <- tweets_20210201_20210512_full[which(tweets_20210201_20210512_full$user.id_str != '712842098228105216' | '9276672'),] # remove hand-identified irrelevant accounts 


id_screen_name_1 <- read_csv(paste0(path_spap_state, 'handle_lookup.csv'), # load user.screen_name/user.id data
                             col_types = cols(
                               X1 = col_skip(),
                               verified = col_skip(),
                               friends_count = col_skip(),
                               followers_count = col_skip(),
                               favourites_count = col_skip(),
                               statuses_count = col_skip(),
                               created_at = col_skip(),
                               id_str = col_character(), 
                               created_at = col_character()
                               )
                             )
colnames(id_screen_name_1) <- c('user.screen_name', 'user.id_str') 

id_screen_name_2 <- tweets_20210216_full[c('user.id_str', 'user.screen_name')] # extract user.screen_name/user.id_str data from tweets_20210216
id_screen_name_2 <- id_screen_name_2[!duplicated(id_screen_name_2$user.id_str),]

id_screen_name <- rbind(id_screen_name_1, id_screen_name_2) %>%  # join
  distinct(user.id_str, .keep_all = TRUE)

tweets_20210201_20210512_full <- left_join(tweets_20210201_20210512_full, id_screen_name, by = c('user.id_str')) # add screen_name column
table(is.na(tweets_20210201_20210512_full$user.screen_name)) 
unique(tweets_20210201_20210512_full[which(is.na(tweets_20210201_20210512_full$user.screen_name)==TRUE),]$user.id_str)


# reformat time stamps and bind -------------------------

tweets_20210216_full$created_at
colnames(tweets_20210216_full)[1] <- 'created_at_original'
format.str <- "%a %b %d %H:%M:%S %z %Y"
tweets_20210216_full$created_at <- as.POSIXct(strptime(tweets_20210216_full$created_at_original, format.str, tz = "UTC"), tz = "UTC") # http://willwest.nyc/using-twitters-timestamp-in-r.html
tweets_20210216_full <- subset(tweets_20210216_full, select = -c(created_at_original))

tweets_20210216_full$wave <- 1
tweets_20210201_20210512_full$ wave <- 2

tweets <- rbind(tweets_20210216_full, tweets_20210201_20210512_full)
tweets$created_at <- as.Date(tweets$created_at)
tweets <- tweets %>% distinct(id_str, .keep_all = TRUE)
tweets$user.screen_name <- tolower(tweets$user.screen_name) 

nrow(tweets) # 7327458
length(unique(tweets$user.id_str)) # 4551


# handle/party data cleaning -------------------------

handle_meta_1 <- read_csv(paste0(path_spap_state, 'handle_meta_round_1.csv'))
handle_meta_1 <- subset(handle_meta_1, select = -c(state_abbrev))
handle_meta_1 <- handle_meta_1 %>% 
  mutate(state = gsub("_", " ", state))

path_handle_meta_2 <- paste0(path_spap_state, 'handle_meta_round_2')
handle_meta_2 <- list.files(path = path_handle_meta_2, full.names = TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows
handle_meta_2 <- handle_meta_2[c('handle_1', 'party', 'state')]
colnames(handle_meta_2)[1] <- 'handle'
handle_meta_2 <- handle_meta_2[is.na(handle_meta_2$handle)==FALSE,]

handle_meta_2$party_first_2 <- substr(handle_meta_2$party, 1, 2) # DFL -> D, D-NPL -> D
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "De"] <- "(D" 
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "Re"] <- "(R" 
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "no"] <- "N" # norpartisan -> N
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "No"] <- "N" # norpartisan -> N
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "In"] <- "I" # Independent -> I
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "(I"] <- "I" # IPNY (Fred Thiele) -> I
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "(N"] <- "I" # (N) (Chad Mayes) -> I
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "(P"] <- "P" # VPP -> P
handle_meta_2$party_first_2[handle_meta_2$party_first_2 == "Pr"] <- "P" # VPP -> P
handle_meta_2$party_first_2 <- str_replace_all(handle_meta_2$party_first_2, "[^[:alnum:]]", "") 

handle_meta_2 <- subset(handle_meta_2, select = -c(party))
colnames(handle_meta_2)[3] <- 'party'
handle_meta_2$handle <- tolower(handle_meta_2$handle)
handle_meta_2 <- handle_meta_2 %>% 
  mutate(state = gsub("_", " ", state))

handle_meta <- rbind(handle_meta_2, handle_meta_1)
handle_meta <- handle_meta %>% distinct(handle, .keep_all = TRUE)

handles_new <- setdiff(unique(handle_meta_2$handle), # handles newly added in second wave handle collection
                       unique(handle_meta_1$handle)
                       )

handles_old <- setdiff(unique(handle_meta_1$handle), # handles which only appear in first wave
                       unique(handle_meta_2$handle)
                       )

handles_both <- intersect(unique(handle_meta_1$handle), # handles which appear both waves
                          unique(handle_meta_2$handle)
                          )

length(handles_new) + length(handles_old) + length(handles_both) == length(unique(handle_meta$handle))

handle_meta$transition <- case_when(
  handle_meta$handle %in% handles_old ~ "old",
  handle_meta$handle %in% handles_new ~ "new",
  handle_meta$handle %in% handles_both ~ "both"
  )

write.csv(paste0(path_spap_state, 'handle_meta.csv'))


# subset tweets written while in office -------------------------

tweets <- left_join(tweets, 
                    handle_meta[c('handle','party', 'transition')], 
                    by = c('user.screen_name' = 'handle')
                    )

state_transition <- read_csv(paste0(path_spap_state,'date_transition.csv')) # state election date data
state_transition$date_transition <- as.Date(state_transition$date_transition, format =  "%d-%b-%y")
tweets <- left_join(tweets, 
                    state_transition, 
                    by = c('legislator_state' = 'state')
                    )

table(is.na(tweets$party))
table(is.na(tweets$date_transition))

tweets$under_study <- case_when(
  is.na(tweets$date_transition) == TRUE ~ TRUE, # no transitions have taken place
  is.na(tweets$date_transition) == FALSE & tweets$transition == 'both' ~ TRUE, # accounts that appear in both waves
  is.na(tweets$date_transition) == FALSE & tweets$transition == 'old' & tweets$created_at > tweets$date_transition ~ FALSE, # handles that appear only in first wave (get tweets prior to transition) 
  is.na(tweets$date_transition) == FALSE & tweets$transition == 'old' & tweets$created_at < tweets$date_transition ~ TRUE,
  is.na(tweets$date_transition) == FALSE & tweets$transition == 'new' & tweets$created_at < tweets$date_transition ~ FALSE, # handles that appear only in second wave (get tweets after transition) 
  is.na(tweets$date_transition) == FALSE & tweets$transition == 'new' & tweets$created_at > tweets$date_transition ~ TRUE)

tweets <- tweets[which(tweets$under_study == TRUE),]

table(is.na(tweets$party))
table(is.na(tweets$user.screen_name))


# group by month-legislator -------------------------

tweets$created_at_month <- format(tweets$created_at, format = "%Y-%m") # year-month column
table(is.na(tweets$created_at))
table(is.na(tweets$created_at_month))

tweets_grouped <- tweets %>% 
  group_by(user.id_str, user.screen_name, created_at_month) %>%
  summarise(legislator_state = first(legislator_state),
            sum_tweet_all = n(),
            sum_tweet_vaccine = sum(label),
            sum_tweet_anti = sum(anti))

min(tweets_grouped$created_at_month)
max(tweets_grouped$created_at_month)

months_frame <- unique(format(seq(as.Date("2020-4-1"), as.Date("2021-5-31"), by = "days"), format = "%Y-%m"))
handles_frame <- unique(tweets_grouped$user.screen_name)
frame <- data.frame(created_at_month = rep(months_frame, length(handles_frame)), 
                    user.screen_name = rep(handles_frame, each = length(months_frame)))
tweets_monthly <- left_join(frame, tweets_grouped, by = c("user.screen_name", "created_at_month"))

tweets_monthly <- tweets_monthly %>% 
  group_by(user.screen_name) %>% 
  fill(user.id_str, legislator_state, .direction = "downup") %>% 
  replace_na(list(sum_tweet_all = 0, sum_tweet_vaccine = 0, sum_tweet_anti = 0))

tweets_monthly <- tweets_monthly %>% 
  drop_na(legislator_state)

write.csv(tweets_monthly, paste0(path_spap_state_vaccine, 'tweets_monthly.csv'))








