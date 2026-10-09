library(survey)
library(tidyverse)
library(pps)
library(dplyr)

# load and reorganize

trinity <- read_csv("trinity_faculty.csv") 

trinity <- trinity |>
  select(stratum = "Stratum #", 
         department = "Dept name", 
         M_i = "# of faculty")

n_dept <- 20 # number of departments to select

trinity <- trinity[sample(1:nrow(trinity)), ] # randomize order of rows 
# since systematic sampling depends on order

t_x <- sum(trinity$M_i) # total number of faculty across all departments

trinity$pi_i <- n_dept * trinity$M_i / t_x # first-order inclusion 
# probability for each department

max(trinity$pi_i) # check assumption that nxi/tx <= 1 for all i. it's not...

# check which department pis > 1 --> treat as certainty cases
trinity |>
  filter(pi_i > 1) |>
  select(department, M_i, pi_i)

# dealing with certainty cases

# separate initial certainty and non-certainty departments
certainty <- trinity |>
  filter(pi_i >= 1) |>
  mutate(pi_i = 1)

remaining <- trinity |>
  filter(pi_i < 1)

# calculate number of additional departments to select
n_remaining <- n_dept - nrow(certainty) 

# recalculate pi_i for remaining departments
remaining$pi_i <- n_remaining * remaining$M_i / sum(remaining$M_i) 

# if recalculation creates additional certainty PSUs, 
# repeat until all remaining probabilities are below 1
while (any(remaining$pi_i >= 1)) {
  new_certainty <- remaining |>
    filter(pi_i >= 1) |>
    mutate(pi_i = 1)
  certainty <- bind_rows(certainty, new_certainty)
  remaining <- remaining |>
    filter(pi_i < 1)
  n_remaining <- n_dept - nrow(certainty)
  remaining$pi_i <- n_remaining * remaining$M_i / sum(remaining$M_i)
}

# select remaining cases with pps & add certainty cases
#| eval: false
set.seed(919)

# returns numbers of selected rows
selected_depts <- ppss(remaining$M_i, n_remaining) 

# keep selected departments
pps_remaining <- remaining[selected_depts, ] 

# combine certainty psus with pps-selected departments
pps_sample <- bind_rows(certainty, pps_remaining) 

# check 20 departments were selected
nrow(pps_sample) 

# stage 2 srs

# srs 5 faculty from each department
pps_sample$faculty_numbers <- lapply(
  pps_sample$M_i,
  function(M_i) sample(1:M_i, 5)
)

# view numbers of faculty selected
pps_sample |>
  select(department, M_i, pi_i, faculty_numbers) 

dept_info <- pps_sample |>
  select(department, M_i, pi_i, faculty_numbers) |>
  mutate(
    faculty_numbers = sapply(
      faculty_numbers,
      function(x) paste(sort(x), collapse = ", ")
    )
  )

write.csv(dept_info, "dept_info.csv", row.names = FALSE)

# DATA COLLECTION HERE: we created an excel file and filled in with relevant
# info from scholars.duke 

# read and clean collected data 

samples <- read.csv("pps_sample.csv") 

samples <- samples |>
  rename(
    woman = Sex..1...F..0...other.,
    grant = Ongoing.grant...1...Yes..0...No.,
    has_phd = PhD...1...Yes..0...No.,
    yrs_phd = Number.of.years.since.PhD,
    yrs_pub = Years.since.last.pub,
    intl_ugrad = International.undergrad.degree.
  )

dept_info <- read.csv("dept_info.csv")

# attach M_i and pi_i
samples <- samples |>
  left_join(
    dept_info |>
      select(department, M_i, pi_i),
    by = "department"
  )

# calculate faculty-level sampling weight
samples$weight <- samples$M_i / (5 * samples$pi_i)

# dealing with missing data

# z = 1 if y is observed, 0 if missing; obs = y * z 

# Q3: years since phd
samples$z_phd <- ifelse(!is.na(samples$yrs_phd) 
                        & samples$has_phd == 1, 1, 0)
samples$obs_phd <- ifelse(samples$z_phd == 1, samples$yrs_phd, 0)


# Q4a: years since last pub
samples$z_pub <- ifelse(!is.na(samples$yrs_pub), 1, 0)
samples$obs_pub <- ifelse(samples$z_pub == 1, samples$yrs_pub, 0)

# build survey
fac_des <- svydesign(
  ids = ~department,
  weights = ~weight,
  data = samples
)
