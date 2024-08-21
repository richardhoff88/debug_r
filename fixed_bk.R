library(tidyr)
library(dplyr)
library(broom)
library(purrr)

results_list <- list()

path_product <- "C:/Users/GA2HORG/Documents/model_backtest/Analysis_data/TR_BT_PL_DEF_01_PRODUCT_GROUP.rds"
path_yearclst <- "C:/Users/GA2HORG/Documents/model_backtest/Analysis_data/TR_BT_PL_DEF_03_YEAR_BEGIN_CLST.rds"

path_list <- list(path_product, path_yearclst)

for (i in seq_along(path_list)) {
  vd <- readRDS(path_list[[i]])
  results_list[[i]] <- vd[["result"]]
}

vd_filtered_2 <- results_list[[1]]

# Convert LGD_REALISED_ALL_FLOORED to numeric
vd_filtered_2 <- vd_filtered_2 %>%
  mutate(LGD_REALISED_ALL_FLOORED = as.numeric(LGD_REALISED_ALL_FLOORED))

# Check for NA values introduced by conversion
if(any(is.na(vd_filtered_2$LGD_REALISED_ALL_FLOORED))) {
  warning("NA values found after converting LGD_REALISED_ALL_FLOORED to numeric.")
}

# Add tolerance
tolerance = 0.025
vd_filtered_2 <- vd_filtered_2 %>%
  mutate(Realized_upper = LGD_REALISED_ALL_FLOORED + tolerance,
         Realized_lower = LGD_REALISED_ALL_FLOORED - tolerance)

# Perform t-tests
overall_t_test_greater = t.test(vd_filtered_2$LGD_LONGRUNAV, vd_filtered_2$LGD_REALISED_ALL_FLOORED, paired = TRUE, alternative = "greater")
print(overall_t_test_greater)

grouped_results_greater <- vd_filtered_2 %>%
  group_by(LGD_LONGRUNAV_CLASSNAME) %>%
  filter(n() > 1) %>%
  summarise(test_result = list(t.test(LGD_LONGRUNAV, LGD_REALISED_ALL_FLOORED, paired = TRUE, alternative = "greater")),
            .groups = 'drop') %>%
  mutate(test_result = map(test_result, tidy)) %>%
  unnest(test_result)
all_results_greater = bind_rows(grouped_results_greater, tidy(overall_t_test_greater))

# Save results to CSV
write.csv(all_results_greater, "C:/Users/GA2HORG/Documents/model_backtest/Results_prog_paired.csv")
write.csv(all_results_less, "C:/Users/GA2HORG/Documents/model_backtest/Results_cons_paired.csv")
write.csv(all_results_greater_upper, "C:/Users/GA2HORG/Documents/model_backtest/Results_prog_tol_paired.csv")
write.csv(all_results_less_lower, "C:/Users/GA2HORG/Documents/model_backtest/Results_cons_tol_paired.csv")
