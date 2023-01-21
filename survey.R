## Let's go:
pacman::p_load(tidyverse,tidymodels,readr,gt,vip,plumber,vetiver)

# survey_results <- read_csv("survey_data.csv")

survey_results_public <- read_csv("~/Downloads/stack-overflow-developer-survey-2022/survey_results_public.csv")

# View(survey_results_public)

# Lets think about what variables we want to include in our analysis

# colnames(survey_results)

# First step would be to filter for either fully remote or hybrid:-

to_import <- read_csv("to_import.csv")

survey <- survey_results_public %>%
  filter(
    RemoteWork %in% c("Fully remote", "Hybrid (some remote, some in-person)")
    &
      Country %in% c(
        "United States of America",
        "United Kingdom of Great Britain and Northern Ireland",
        "Germany"
      )
    &
      Currency %in% c("USD\tUnited States dollar",
                      "EUR European Euro",
                      "GBP\tPound sterling"
      ) 
  ) 
  
survey <- read_csv("to_import.csv")

# 
# # Decide on variables of interest:

survey %>%
  select(
    ResponseId,
    Employment,
    EdLevel,
    YearsCode,
    YearsCodePro,
    OrgSize,
    Country,
    # Gender,
    CompTotal,
    Currency,
    WorkExp,
    RemoteWork
  ) -> survey
# 
# # Next we need to change the currency:

survey %>%
  mutate(
    Currency = str_sub(Currency, 1, 3),
    Compensation_usd = case_when(
      Currency == "GBP" ~ CompTotal * 1.21,
      Currency == "EUR" ~ CompTotal * 1.06,
      TRUE ~ CompTotal
    )
  ) %>%
  select(-CompTotal,-Currency,-ResponseId) %>%
  mutate(YearsCode = as.double(YearsCode),
         YearsCodePro = as.double(YearsCodePro),
         Country = case_when(
           Country == "United States of America" ~ "USA",
           Country == "United Kingdom of Great Britain and Northern Ireland" ~ "GB",
           TRUE ~ Country
         )) %>% 
  na.omit() %>% 
  filter(Compensation_usd < 6000000 & Compensation_usd > 1000) -> survey # Mention this in the limitations

## Let the EDA begin:
# Distribution of the compensationa across countries:

values <- c("#EB0050", "#143C75", "#4BB9A0", "#ECBF08")

# Data Viz one:

survey %>% 
  filter(Compensation_usd>0) %>% 
  ggplot(aes(Compensation_usd, fill=Country)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0,1000000),
                     labels = scales::number_format())+
  facet_wrap(~Country,scales="free_y")+
  tidyquant::theme_tq()+
  scale_fill_manual(values=values)+
  labs(y="Count")+
  theme(legend.position = "none")

# Lets now do something akin to the one in the blog:

survey %>%
  filter(Compensation_usd < 6000000) %>% 
  ggplot(aes(
    x = WorkExp,
    y = Compensation_usd,
    color = RemoteWork,
    size = YearsCodePro
  )) +
  geom_point(alpha = 0.5) +
  facet_wrap(~Country, scales = "free_y")+
  scale_y_continuous(labels=scales::number_format())+
  scale_color_manual(values = values)+
  tidyquant::theme_tq()

# Box plot of distribution

survey %>% 
  ggplot(aes(Compensation_usd,
             Country,
             fill=Country))+
  geom_boxplot()+
  scale_fill_manual(values=values)+
  theme_light()+
  scale_x_continuous(labels = scales::comma)+
  theme(legend.position = "none")+
  facet_wrap(~RemoteWork)

# Statistically significant difference between compensation of 
# remote and non remote developers, considering and not considering
# country:

survey_usa <- survey %>% 
  filter(Country == "USA")

ignore_remote_usa <- lm(Compensation_usd ~ YearsCodePro+
                     WorkExp,
                    data=survey_usa)

account_for_remote_usa <- lm(Compensation_usd ~ YearsCodePro+
                     WorkExp+
                     RemoteWork,
                     data=survey_usa)

bind_rows(
  tidy(ignore_remote_usa) %>% mutate(Remote = "ignore"), 
  tidy(account_for_remote_usa) %>% mutate(Remote = "account for remote")) %>%
  filter(!str_detect(term, "Remote"), term != "(Intercept)") %>%
  ggplot(aes(estimate, term, color = Remote)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_errorbar(size = 1.4, alpha = 0.7,
                aes(xmin = estimate - 1.96 * std.error, xmax = estimate + 1.96 * std.error,
                    alpha=0.1)) +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::dollar) +
  theme(legend.position="bottom") +
  scale_color_manual(values=values)+
  tidyquant::theme_tq()

# Bootstrapped models:


set.seed(123)
ignore_remote_usa_intervals <- reg_intervals(Compensation_usd ~ YearsCodePro+
                          WorkExp,
                        data=survey_usa,
                        times = 500)

set.seed(123)
account_for_remote_usa <- 
  reg_intervals(Compensation_usd ~ YearsCodePro+
                 WorkExp+
                 RemoteWork,
               data=survey_usa,
               times = 500)

# Plot bootstrapped intervals:


bind_rows(
  ignore_remote_usa_intervals %>% mutate(Remote = "ignore"), 
  account_for_remote_usa %>% mutate(Remote = "account for remote")
) %>%
  filter(!str_detect(term, "Remote")) %>%
  ggplot(aes(.estimate, term, color = Remote)) +
  geom_vline(xintercept = 0, size = 1.5, lty = 2, color = "gray50") +
  geom_errorbar(size = 1.4, alpha = 0.7,
                aes(xmin = .lower, xmax = .upper)) +
  geom_point(size = 3) +
  scale_x_continuous(labels = scales::dollar) +
  scale_color_manual(values = values) +
  theme(legend.position="bottom") +
  labs(x = "Change in compensation (USD $)", y = NULL, color = "Include working status in model?",
       title = "Bootstrap confidence intervals for developer compensation in the United States",
       subtitle = "There is no difference in the effect of years and work esperience that developers have according to whether they are remote developers or not")+
  tidyquant::theme_tq()

survey %>% mutate_if(is.character,as.factor)->survey

# Let's start our workflow:

set.seed(1234)
Stack_split <- initial_split(survey, strata = RemoteWork)
Stack_train <- training(Stack_split)
Stack_test <- testing(Stack_split)

# No we specify our recipes:

Stack_recipe <- recipe(RemoteWork ~ ., data = survey) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# Model workflows:

# glm_spec <-
#   logistic_reg() |>
#   set_engine("glm")
# 
# tree_spec <-
#   rand_forest(min_n = tune()) |>
#   set_engine("ranger") |>
#   set_mode("classification")
# 
# mlp_brulee_spec <-
#   mlp(
#     hidden_units = tune(), epochs = tune(),
#     penalty = tune(), learn_rate = tune()
#   ) %>%
#   set_engine("brulee") %>%
#   set_mode("classification")

# Validation:

glm_spec <-
    logistic_reg(penalty = tune(),mixture = tune()) |>
    set_engine("glm")

cart_spec <- 
  decision_tree(cost_complexity = tune(), min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tree_spec <-
  rand_forest(min_n = tune(),trees=1000) |>
  set_engine("ranger") |>
  set_mode("classification")

# xgb_spec <- boost_tree(
#   trees = 1000, 
#   tree_depth = tune(), 
#   min_n = tune(), 
#  mtry = tune(),         ## randomness
#   learn_rate = tune(),                         ## step size
# ) %>% 
#   set_engine("xgboost") %>% 
#   set_mode("classification")

set.seed(1234)
Stack_folds <- vfold_cv(Stack_train,strata = RemoteWork,v = 5)
# 
# bayes_control <-
#   control_bayes(no_improve = 10L, time_limit = 20, save_pred = TRUE)

doParallel::registerDoParallel()
workflow_set <-
  workflow_set(
    preproc = list(Stack_recipe),
    models = list(
      glm = glm_spec,
      cart = cart_spec ,
      tree = tree_spec)
  ) 

# Grid:
grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

grid_results <-
  workflow_set %>%
  workflow_map(
    seed = 1503,
    resamples = Stack_folds,
    grid = 25,
    control = grid_ctrl
  )

rank_results(grid_results,
             rank_metric = "roc_auc",
             select_best = TRUE
) |>
  gt()

# Let's retry it with the bayes splits:

# bayes_control <-
#   control_bayes(no_improve = 10L, time_limit = 20, save_pred = TRUE)
# 
# workflow_set <-
#   workflow_set(
#     preproc = list(Stack_recipe),
#     models = list(
#       glm = glm_spec,
#       cart = cart_spec ,
#       tree = tree_spec)
#   )  |>
#   workflow_map("tune_bayes",
#                iter = 10L,
#                resamples = Stack_folds,
#                control = bayes_control
#   )
# 
# # Let's stick with the basic workflow:

best_model_id <- "recipe_glm"

best_fit <- 
  grid_results |>
  extract_workflow_set_result(best_model_id) |>
  select_best(metric = "roc_auc")
  
final_workflow <-
  workflow_set |>
  extract_workflow(best_model_id) |>
  finalize_workflow(best_fit)

final_fit <-
  final_workflow |>
  last_fit(Stack_split)

final_fit |>
  collect_metrics() |>
  gt()

final_fit %>% 
  collect_predictions() %>%
  roc_curve(RemoteWork,`.pred_Fully remote`) %>% 
  autoplot()

final_fit %>% 
  pluck(".workflow",1) %>% 
  extract_fit_parsnip() %>% 
  vip(geom = "point", aesthetics = list(fill = values[2], alpha = 0.8)) +
  scale_y_continuous(expand = c(0, 0))+
  theme_bw()

# Vetiver model:"
library(vetiver)

final_fit_to_deploy <- final_fit |> extract_workflow()

v <- vetiver_model(final_fit_to_deploy, model_name = "stack_model")

v

pr() %>%
  vetiver_api(v) %>%
  pr_run(port = 8088)


# Try this example:

[
  {
    "Employment": "Employed, full-time",
    "EdLevel": "Bachelor’s degree (B.A., B.S., B.Eng., etc.)",
    "YearsCode": 5,
    "YearsCodePro": 6,
    "OrgSize": "20 to 99 employees",
    "Country": "Germany",
    "WorkExp": 2,
    "Compensation_usd": 55000
  }
]






















