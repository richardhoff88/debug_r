###############################################################################
# Header: See global header in run file LGD_REN.R and 
#         comments before the call of this script there.
###############################################################################

# \brief LGD backtesting performing

# \validation_concept: Sec. 3.4

# \version 11/07/2023 CB3MOSM Inital implementation of back-testing

# edits from Richard Hoffmann (intern) for LGD validation report

# 
# usethis::ui_info(' ***** Running 11_backtesting.R  ***** ')
# log_print(' ***** Running 11_backtesting.R  ***** ')


###############################################################################
#     LGD Backtesting BK
##############################################################################
#install.packages("broom")
#install.packages("tidyr")
#install.packages("purrr")
library(tidyr)
library("dplyr")
library("broom")
library("purrr")


results_list <- list()

path_product <- "C:/Users/GA2HORG/Documents/model_backtest/Analysis_data/TR_BT_PL_DEF_01_PRODUCT_GROUP.rds"
path_yearclst <- "C:/Users/GA2HORG/Documents/model_backtest/Analysis_data/TR_BT_PL_DEF_03_YEAR_BEGIN_CLST.rds"

path_list <- list(
                  path_product,  # in the validation report
                  path_yearclst # in the validation report
                  )

for (i in seq_along(path_list)) {
  vd <- readRDS(path_list[[i]])
  results_list[[i]] <- vd[["result"]] 
}

#organized <- bind_rows(results_list[[2]], results_list[[4]])

#df <- organized %>% relocate(last_col(), .before = 1)


vd_filtered_2 <- results_list[[1]]

# lgd realised from mean
# and change lgd longrunav

LGD_LONGRUNAV <- "mean_forecasted"
LGD_REALISED_ALL_FLOORED <- "mean_realized"
LGD_LONGRUNAV_CLASSNAME <- "PRODUCTGROUP"
LGD_LONGRUNAV_CLASSNAME2 <- "YEAR_CLST"

# Eigene t Tests: 1
# Bestimmung von p_prog (all_results_greater)
overall_t_test_greater = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$LGD_REALISED_ALL_FLOORED,paired =TRUE,alternative="greater")
overall_result_greater = tidy(overall_t_test_greater)
overall_result_greater$LGD_LONGRUNAV_CLASSNAME="All"

grouped_results_greater <- vd_filtered_2 %>%
  group_by(LGD_LONGRUNAV_CLASSNAME)%>%
  filter(n()>1)%>%
  summarise(
    test_result =list(t.test(LGD_LONGRUNAV,LGD_REALISED_ALL_FLOORED,paired =TRUE, alternative ="greater")),
    .groups='drop'
  )%>%
  mutate(test_result =map(test_result,tidy))%>%
  unnest(test_result)
all_results_greater= bind_rows(grouped_results_greater,overall_result_greater)
# Bestimmung von p_cons (all_results_less)
overall_t_test_less = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$LGD_REALISED_ALL_FLOORED, paired =TRUE,alternative="less")
overall_result_less = tidy(overall_t_test_less)
overall_result_less$LGD_LONGRUNAV_CLASSNAME="All"

grouped_results_less = vd_filtered_2 %>%
  group_by(LGD_LONGRUNAV_CLASSNAME)%>%
  filter(n()>1)%>%
  summarise(
    test_result =list(t.test(LGD_LONGRUNAV,LGD_REALISED_ALL_FLOORED,paired=TRUE, alternative ="less")),.groups='drop'
  )%>%
  mutate(test_result =map(test_result,tidy))%>%
  unnest(test_result)
all_results_less= bind_rows(grouped_results_less,overall_result_less)
#Bestimmung von p_prog_tol
tolerance =0.025
vd_filtered_2 <- vd_filtered_2%>%
  mutate(Realized_upper=LGD_REALISED_ALL_FLOORED+tolerance,
         Realized_lower=LGD_REALISED_ALL_FLOORED-tolerance) # adding string and int?

overall_t_test_greater_upper = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$Realized_upper,paired =TRUE,alternative="greater")
overall_result_greater_upper = tidy(overall_t_test_greater_upper)
overall_result_greater_upper$LGD_LONGRUNAV_CLASSNAME="All"

grouped_results_greater_upper <- vd_filtered_2 %>%
  group_by(LGD_LONGRUNAV_CLASSNAME)%>%
  filter(n()>1)%>%
  summarise(
    test_result =list(t.test(LGD_LONGRUNAV,Realized_upper,paired =TRUE, alternative ="greater")),
    .groups='drop'
  )%>%
  mutate(test_result =map(test_result,tidy))%>%
  unnest(test_result)
all_results_greater_upper= bind_rows(grouped_results_greater_upper,overall_result_greater_upper)
#Bestimmung von p_cons_tol
tolerance =0.025
vd_filtered_2 <- vd_filtered_2%>%
  mutate(Realized_upper=LGD_REALISED_ALL_FLOORED+tolerance,
         Realized_lower=LGD_REALISED_ALL_FLOORED-tolerance)

overall_t_test_less_lower = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$Realized_lower,paired =TRUE,alternative="less")
overall_result_less_lower = tidy(overall_t_test_less_lower)
overall_result_less_lower$LGD_LONGRUNAV_CLASSNAME="All"

grouped_results_less_lower <- vd_filtered_2 %>%
  group_by(LGD_LONGRUNAV_CLASSNAME)%>%
  filter(n()>1)%>%
  summarise(
    test_result =list(t.test(LGD_LONGRUNAV,Realized_lower,paired =TRUE, alternative ="less")),
    .groups='drop'
  )%>%
  mutate(test_result =map(test_result,tidy))%>%
  unnest(test_result)
all_results_less_lower= bind_rows(grouped_results_less_lower,overall_result_less_lower )



# 
# # Eigene t Tests: 2
# # Bestimmung von p_prog (all_results_greater)
# overall_t_test_greater = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$LGD_REALISED_ALL_FLOORED,paired =TRUE,alternative="greater")
# overall_result_greater = tidy(overall_t_test_greater)
# overall_result_greater$LGD_LONGRUNAV_CLASSNAME2="All"
# 
# grouped_results_greater <- vd_filtered_2 %>%
#   group_by(LGD_LONGRUNAV_CLASSNAME2)%>%
#   filter(n()>1)%>%
#   summarise(
#     test_result =list(t.test(LGD_LONGRUNAV,LGD_REALISED_ALL_FLOORED,paired =TRUE, alternative ="greater")),
#     .groups='drop'
#   )%>%
#   mutate(test_result =map(test_result,tidy))%>%
#   unnest(test_result)
# all_results_greater= bind_rows(grouped_results_greater,overall_result_greater)
# # Bestimmung von p_cons (all_results_less)
# overall_t_test_less = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$LGD_REALISED_ALL_FLOORED, paired =TRUE,alternative="less")
# overall_result_less = tidy(overall_t_test_less)
# overall_result_less$LGD_LONGRUNAV_CLASSNAME2="All"
# 
# grouped_results_less = vd_filtered_2 %>%
#   group_by(LGD_LONGRUNAV_CLASSNAME2)%>%
#   filter(n()>1)%>%
#   summarise(
#     test_result =list(t.test(LGD_LONGRUNAV,LGD_REALISED_ALL_FLOORED,paired=TRUE, alternative ="less")),.groups='drop'
#   )%>%
#   mutate(test_result =map(test_result,tidy))%>%
#   unnest(test_result)
# all_results_less= bind_rows(grouped_results_less,overall_result_less)
# #Bestimmung von p_prog_tol
# tolerance =0.025
# vd_filtered_2 <- vd_filtered_2%>%
#   mutate(Realized_upper=LGD_REALISED_ALL_FLOORED+tolerance,
#          Realized_lower=LGD_REALISED_ALL_FLOORED-tolerance)
# 
# overall_t_test_greater_upper = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$Realized_upper,paired =TRUE,alternative="greater")
# overall_result_greater_upper = tidy(overall_t_test_greater_upper)
# overall_result_greater_upper$LGD_LONGRUNAV_CLASSNAME2="All"
# 
# grouped_results_greater_upper <- vd_filtered_2 %>%
#   group_by(LGD_LONGRUNAV_CLASSNAME2)%>%
#   filter(n()>1)%>%
#   summarise(
#     test_result =list(t.test(LGD_LONGRUNAV,Realized_upper,paired =TRUE, alternative ="greater")),
#     .groups='drop'
#   )%>%
#   mutate(test_result =map(test_result,tidy))%>%
#   unnest(test_result)
# all_results_greater_upper= bind_rows(grouped_results_greater_upper,overall_result_greater_upper)
# #Bestimmung von p_cons_tol
# tolerance =0.025
# vd_filtered_2 <- vd_filtered_2%>%
#   mutate(Realized_upper=LGD_REALISED_ALL_FLOORED+tolerance,
#          Realized_lower=LGD_REALISED_ALL_FLOORED-tolerance)
# 
# overall_t_test_less_lower = t.test(vd_filtered_2$LGD_LONGRUNAV,vd_filtered_2$Realized_lower,paired =TRUE,alternative="less")
# overall_result_less_lower = tidy(overall_t_test_less_lower)
# overall_result_less_lower$LGD_LONGRUNAV_CLASSNAME2="All"
# 
# grouped_results_less_lower <- vd_filtered_2 %>%
#   group_by(LGD_LONGRUNAV_CLASSNAME2)%>%
#   filter(n()>1)%>%
#   summarise(
#     test_result =list(t.test(LGD_LONGRUNAV,Realized_lower,paired =TRUE, alternative ="less")),
#     .groups='drop'
#   )%>%
#   mutate(test_result =map(test_result,tidy))%>%
#   unnest(test_result)
# all_results_less_lower= bind_rows(grouped_results_less_lower,overall_result_less_lower )


write.csv(all_results_greater,"C:/Users/GA2HORG/Documents/model_backtest/Results_prog_paired.csv")
write.csv(all_results_less,"C:/Users/GA2HORG/Documents/model_backtest/Results_cons_paired.csv")
write.csv(all_results_greater_upper,"C:/Users/GA2HORG/Documents/model_backtest/Results_prog_tol_paired.csv")
write.csv(all_results_less_lower,"C:/Users/GA2HORG/Documents/model_backtest/Results_cons_tol_paired.csv")
#Zweiseitiger t test:
