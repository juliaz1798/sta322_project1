# Question 1. What is an estimate of the fraction of faculty who are women?

prop_women <- svymean(~woman, fac_des)
prop_women

confint(prop_women, level = .95)


# Question 2. What fraction of faculty have at least one grant that is 
# ongoing in 2026?

prop_grant <- svymean(~grant, fac_des)
prop_grant

confint(prop_grant, level = .95)


# Question 3. Among faculty who have a Ph.D. degree, what is the average number 
# of years since they received the Ph.D. degree?

avg_phd <- svyratio(~obs_phd, ~z_phd, fac_des)
avg_phd

confint(avg_phd, level = .95)

# Question 4

# 4a: Among those who have published papers, what is the average number of 
# years since their last publication?

avg_pub <- svyratio(~obs_pub, ~z_pub, fac_des)
avg_pub

confint(avg_pub, level = .95)


# 4b: What fraction of faculty received their undergraduate degree outside 
# the United States?

prop_intl <- svymean(~intl_ugrad, fac_des, na.rm = TRUE)
prop_intl

confint(prop_intl, level = .95)

