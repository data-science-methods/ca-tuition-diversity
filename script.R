library(tidyverse)
library(tidytuesdayR)
library(gghighlight)
library(ggbeeswarm)

library(assertthat)

theme_set(theme_bw())

data_dir = 'data'
tt_file = file.path(data_dir, 'tt_raw.Rds')

## Get the data ----
if (!file.exists(tt_file)) {
    ## <https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-03-10>
    dfs = tt_load('2020-03-10')
    write_rds(dfs, tt_file) 
} else {
    dfs = read_rds(tt_file)
}


## System and Tuition ----
## I *think* the original source is <https://www.chronicle.com/article/tuition-and-fees-1998-99-through-2018-19/>
## Sticker prices 2018-19
## But this is also used for `tuition_income`

## Names formatted as "University of California: Davis"
## Does not including Hastings College of Law
tuition_df = dfs$tuition_cost %>% 
    filter(state == 'California', degree_length == '4 Year') %>% 
    mutate(system = case_when(str_detect(name, '^University of California') ~ 'UC', 
                              str_detect(name, 'State( Polytechnic)? University|Maritime') ~ 'CSU', 
                              type == 'Public' ~ NA_character_,
                              type == 'For Profit' ~ 'for profit',
                              type == 'Private' ~ 'private', 
                              TRUE ~ 'other'))

tuition_df %>% 
    count(system, type)

tuition_df %>% 
    pull(system) %>% 
    negate(is.na)() %>% 
    all() %>% 
    assert_that(msg = 'NA in system')


tuition_df %>% 
    filter(system == 'UC') %>% 
    count(name) %>% 
    nrow() %>% 
    identical(9L) %>% 
    assert_that(msg = 'Require exactly 9 UC campuses')

tuition_df %>% 
    filter(system == 'CSU') %>% 
    count(name) %>% 
    nrow() %>% 
    identical(23L) %>% 
    assert_that(msg = 'Require exactly 23 CSU campuses')

## Some plots for system and tuition

ggplot(tuition_df, aes(system, fill = system)) +
    geom_bar()

ggplot(tuition_df, aes(fct_reorder(name, in_state_tuition),
                       in_state_tuition, 
                       color = system)) +
    geom_point(aes(color = system)) +
    coord_flip() +
    theme_bw()

ggplot(tuition_df, aes(in_state_tuition)) +
    geom_histogram()

ggplot(tuition_df, aes(in_state_tuition)) +
    geom_histogram(binwidth = 100)
ggplot(tuition_df, aes(in_state_tuition)) +
    geom_histogram(binwidth = 10000)

ggplot(tuition_df, aes(in_state_tuition, fill = system)) +
    geom_histogram(binwidth = 10000)


ggplot(tuition_df, aes(in_state_tuition)) +
    geom_density() +
    geom_rug()
ggplot(tuition_df, aes(in_state_tuition, color = system)) +
    geom_density()

ggplot(tuition_df, aes(in_state_tuition, color = system)) +
    stat_ecdf() +
    geom_hline(yintercept = c(.10, .50, .90), alpha = .5)


ggplot(tuition_df, aes(system, in_state_tuition, color = system)) +
    geom_violin(scale = 'width', 
                draw_quantiles = .5) +
    geom_beeswarm()



## Diversity ----
## Original source: <https://www.chronicle.com/article/student-diversity-at-4-725-institutions/>
## Fall 2014
## "Total Minority" includes Asian students; excludes White, non-resident foreign, and unknown
## Categories are exclusive, so Hispanic is treated as race rather than ethnicity

## UC names formatted as "University of California at Santa Cruz"
div_df = dfs$diversity_school %>% 
    filter(state == 'California') %>% 
    mutate(share = enrollment / total_enrollment) %>% 
    mutate(name = str_replace(name, ' at ', ': '), 
           name = str_replace(name, ' -', ': '), 
           name = str_replace(name, 'University Stanislaus', 'University: Stanislaus'), 
           name = str_replace(name, 'Cal State Maritime', 'California Maritime'))

message('Schools from tuition_df not matched in div_df')
anti_join(tuition_df, div_df, by = 'name') %>% 
    pull(name)

# ggplot(div_df, aes(name, share)) +
#     geom_point() +
#     facet_wrap(vars(category)) +
#     gghighlight(str_detect(name, '^University of California'),
#                 calculate_per_facet = TRUE,
#                 use_direct_label = FALSE) +
#     coord_flip() +
#     theme_bw()

# ggplot(div_df, aes(share, color = system)) +
#     geom_density() +
#     geom_rug(alpha = .5) +
#     facet_wrap(vars(category), scales = 'free_y')
# 
# ggplot(div_df, aes(system, share, color = system)) +
#     # geom_boxplot() +
#     geom_violin(draw_quantiles = .5) +
#     geom_beeswarm(alpha = .1) +
#     facet_wrap(vars(category)) +
#     ylim(.05, 1) +
#     theme_bw()


## Join ----
comb_df = inner_join(div_df, tuition_df, by = c('name', 'state'))

ggplot(comb_df, aes(system, share, color = system)) +
    geom_violin(scale = 'width',
                draw_quantiles = .5) +
    geom_beeswarm(alpha = .25, cex = 2) +
    facet_wrap(vars(category), scales = 'free')


ggplot(comb_df, aes(in_state_tuition, share)) +
    geom_point(aes(color = system)) +
    geom_smooth() +
    facet_wrap(vars(category), scales = 'free')


## Write out ----
write_rds(div_df, file.path(data_dir, 'diversity.Rds'))
write_csv(div_df, file.path(data_dir, 'diversity.csv'))

write_rds(tuition_df, file.path(data_dir, 'tuition.Rds'))
write_csv(tuition_df, file.path(data_dir, 'tuition.csv'))