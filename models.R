# load packages
suppressPackageStartupMessages(library(tidyverse))

# set wd
setwd("P:/jacobsb/Documents/datathon_project/outputs/")

# read in data 
data = readRDS("../datasets/all_datasets.rds")
demographics = readRDS("../datasets/demographics_cleaned.rds")
hads = readRDS("../datasets/hads_cleaned.rds")
edss = readRDS("../datasets/edss_cleaned.rds")
source("../scripts/functions.R")

# restrict to those with EDSS
population = demographics %>%
  filter(UserId %in% edss$UserId)
nrow(population)

# restrict to those with a HADS score 
population = population %>%
  filter(UserId %in% hads$UserId)
nrow(population)

# calculate time from baseline edss to hads 
baseline_edss = edss %>%
  filter(UserId %in% hads$UserId) %>%
  group_by(UserId) %>%
  slice_min(time_from_dx_to_edss,with_ties = F) %>%
  ungroup %>%
  rename("date_edss" = date_completed) %>%
  left_join(hads %>%
              rename("date_hads" = date_completed), 
            by = "UserId") %>%
  mutate(delta_hads_edss = abs(delta_dates_years(date_edss,date_hads))) %>%
  filter(delta_hads_edss <= 0.5 | date_hads < date_edss) %>%
  group_by(UserId) %>%
  slice_min(delta_hads_edss, with_ties = F) %>%
  ungroup()
nrow(baseline_edss)

ggplot(baseline_edss,aes(delta_hads_edss))+geom_histogram()
make_hist(baseline_edss,"delta_hads_edss")
summary(baseline_edss$time_from_dx_to_edss)
summary(baseline_edss$time_from_dx_to_hads)
summary(baseline_edss$delta_hads_edss)

baseline_edss = baseline_edss %>%
  mutate(time_from_sx_to_edss = delta_dates_years(date_edss,date_of_sx.x))
summary(baseline_edss$time_from_sx_to_edss)

baseline_edss %>%
  filter(time_from_dx_to_edss<0) %>%
  dplyr::select(date_edss,date_of_dx.x)

# check timing of hads 
hads_timing_check = population %>%
  left_join(
    hads %>%
      rename("date_hads" = date_completed) %>%
      dplyr::select(UserId,date_hads), 
    by = "UserId") %>%
  mutate(delta_hads_dx = delta_dates_years(date_hads,date_of_dx))
make_hist(hads_timing_check,"delta_hads_dx")

# restrict to this group 
population = population %>%
  filter(UserId %in% baseline_edss$UserId)
nrow(population)

# check baseline EDSS of population
make_hist(baseline_edss,"EDSS")

get_prop(baseline_edss %>%
           mutate(EDSS_6 = ifelse(EDSS >=6,"yes","no")),
         "EDSS_6")


# restrict population to those whose baseline EDSS is under 6 
first_edss_below_6 = baseline_edss %>%
  filter(EDSS < 6)
population = population %>% filter(UserId %in% first_edss_below_6$UserId)
nrow(population)

# now combine with HADS data
hads_for_merge = baseline_edss %>% 
  dplyr::select(UserId,anxious,depressed,date_hads) %>%
  dplyr::rename("hads_date" = date_hads) %>%
  filter(UserId %in% population$UserId)

population = population %>%
  left_join(hads_for_merge,by="UserId")

# now work out time from baseline visit to EDSS 6
population = population %>%
  left_join(
    first_edss_below_6 %>%
      select(UserId,EDSS,date_edss),
    by="UserId"
  ) %>%
  rename("baseline_EDSS" = EDSS,
         "baseline_EDSS_date" = date_edss)
population$baseline_EDSS_date
make_hist(population,"baseline_EDSS_date")

# calculate time to edss 6 
time_to_edss_6 = edss %>%
  arrange(date_completed) %>% # sort by date
  group_by(UserId) %>% # group
  mutate(edss_reading = row_number()) %>% # find baseline & later readings
  filter(edss_reading != 1) %>% # get rid of baseline readings
  filter(EDSS >=6) %>% # find readings above 6 
  left_join(
    population %>%
      select(UserId,baseline_EDSS_date),
    by="UserId"
  ) %>%
  dplyr::select(UserId,EDSS,date_completed,baseline_EDSS_date) %>%
  mutate(time_from_baseline_to_edss = delta_dates_years(date_completed,baseline_EDSS_date)) %>%
  filter(abs(time_from_baseline_to_edss) >= 0.5) %>%
  slice_min(date_completed,with_ties = F) %>%
  ungroup %>%
  dplyr::rename("first_EDSS_above_6" = EDSS, "date_of_first_EDSS_above_6" = date_completed)

# get time to censoring for those who didn't have an event
last_edss_within_window = edss %>%
  arrange(date_completed) %>%
  group_by(UserId) %>%
  mutate(edss_reading = row_number()) %>%
  filter(edss_reading != 1) %>%
  filter(EDSS <6) %>%
  dplyr::select(UserId,EDSS,date_completed) %>%
  left_join(
    population %>%
      select(UserId,baseline_EDSS_date),
    by="UserId"
  ) %>%
  dplyr::select(UserId,EDSS,date_completed,baseline_EDSS_date) %>%
  mutate(time_from_baseline_to_edss = delta_dates_years(date_completed,baseline_EDSS_date)) %>%
  filter(abs(time_from_baseline_to_edss) >= 0.5) %>%
  group_by(UserId) %>%
  slice_max(date_completed,with_ties = F) %>%
  ungroup %>%
  dplyr::rename("last_EDSS_within_window" = EDSS, "date_of_last_edss" = date_completed)

# define EDSS milestone
population = population %>%
  left_join(time_to_edss_6,by="UserId") %>%
  mutate(edss_6_reached = ifelse(!is.na(first_EDSS_above_6),1,0))

# join with latest edss
population = population %>%
  left_join(last_edss_within_window,by="UserId") 

# define time from baseline EDSS to censor/event time
population = population %>%
  mutate(time_from_dx_to_edss = ifelse(
    edss_6_reached==1,
    abs(delta_dates_years(baseline_EDSS_date,date_of_first_EDSS_above_6)),
    abs(delta_dates_years(baseline_EDSS_date,date_of_last_edss))
    )) %>%
  mutate(time_from_dx_to_edss = time_from_dx_to_edss - 0.5)

summary(population$time_from_dx_to_edss)


subsequent_edss_readings = edss %>%
  left_join(
  first_edss_below_6 %>%
    select(UserId,date_edss) %>%
    rename("date_of_baseline_edss" = date_edss),
  by="UserId"
  ) %>%
  mutate(time_from_baseline_to_this_reading = delta_dates_years(date_of_baseline_edss,date_completed)) %>%
  group_by(UserId) %>%
  mutate(edss_reading = row_number()) %>%
  filter(edss_reading > 1) %>%
  ungroup()

# add this information to main table 
population = population %>%
  mutate(has_subsequent_readings = ifelse(UserId %in% subsequent_edss_readings$UserId,"yes","no"))

# define sustained EDSS 6 
subsequent_edss_readings_over_6 = subsequent_edss_readings %>%
  filter(EDSS >= 6)

population %>%
  filter(edss_6_reached==1 & has_subsequent_readings == "yes") %>%
  mutate(sustained_edss6 = ifelse(UserId %in% subsequent_edss_readings_over_6$UserId,"yes","no")) %>%
  dplyr::count(sustained_edss6) %>%
  mutate(n/sum(n))

# calculate age at baseline EDSS 
population = population %>%
  mutate(age_at_baseline_edss = delta_dates_years(baseline_EDSS_date.x,dob)) %>%
  mutate(disease_duration_at_baseline_edss = delta_dates_years(baseline_EDSS_date.x,date_of_sx)) %>%
  mutate(fu_time_first_to_last_edss = delta_dates_years(date_of_last_edss,baseline_EDSS_date.x)) 

# define PMS-onset 
population = population %>%
  mutate(pms_onset = case_when(
    MSAtDiagnosis == "PPMS" ~ "PPMS",
    !is.na(MSAtDiagnosis) & MSAtDiagnosis != "PPMS" & MSAtDiagnosis != "Unknown" ~ "Other",
    is.na(MSAtDiagnosis) | MSAtDiagnosis == "Unknown" ~ "NA"
  )
  ) %>%
  mutate(pms_onset = ifelse(pms_onset=="NA",NA,pms_onset))
    

# DMT data 
dmt = data$`/Portal/DMT_PRO` %>%
  filter(UserId %in% population$UserId) %>%
  left_join(population,by="UserId") %>% 
  mutate(dmt_start = as.Date(StartDate,format="%Y-%m-%d")) %>%
  filter(dmt_start < baseline_EDSS_date.x)
population = population %>%
  mutate(had_dmt_pre_baseline = ifelse(UserId %in% dmt$UserId,"Yes","No"))

# define high-efficacy 
high_efficacy_drugs = c("Alemtuzumab","Cladribine","Fingolimod","Natalizumab","Ocrelizumab")
higheffdmt = dmt %>% filter(DMT %in% high_efficacy_drugs)
population = population %>%
  mutate(had_high_eff_dmt_pre_baseline = ifelse(UserId %in% higheffdmt$UserId,"Yes","No"))

# get FU visits 
population = population %>% left_join(
  edss %>%
    dplyr::count(UserId) %>%
    dplyr::rename("n_EDSS_readings" = n),
  by="UserId"
)

# get relapse data 
population = population %>% left_join(
  data$`/Portal/StudyId_PRO`,
  by="UserId")

relapses = data$`/Clinical/Relapse_Clinical` %>%
  left_join(data$`/Clinical/StudyId_Clinical`,by="ClinicalId") %>%
  filter(StudyId %in% population$StudyId) 

relapses = relapses %>%
  left_join(
    population %>% 
      dplyr::select(StudyId,UserId,baseline_EDSS_date.x) %>%
      na.omit(),
    by="StudyId") %>%
  mutate(relapse_date = as.Date(visit_date,format="%Y-%m-%d")) %>%
  filter(relapse_date < baseline_EDSS_date.x) %>%
  dplyr::count(UserId) %>%
  dplyr::rename("n_relapses_before_baseline" = n)


# in clinical data 
in_relapse_data = data$`/Clinical/Relapse_Clinical` %>%
  left_join(data$`/Clinical/StudyId_Clinical`,by="ClinicalId") %>%
  filter(StudyId %in% population$StudyId) %>%
  left_join(
    population %>% 
      dplyr::select(StudyId,UserId,baseline_EDSS_date.x) %>%
      na.omit(),
    by="StudyId")

population = population %>%
  left_join(relapses,by="UserId") %>%
  mutate(n_relapses_before_baseline = ifelse(
    (UserId %in% in_relapse_data$UserId & UserId %in% relapses$UserId),
    n_relapses_before_baseline,ifelse(
      (UserId %in% in_relapse_data$UserId & !(UserId %in% relapses$UserId) ),
      0,
      NA
    )))
hist(population$n_relapses_before_baseline)
summary(population$n_relapses_before_baseline)
table(population$n_relapses_before_baseline)
table(population$UserId %in% in_relapse_data$UserId)

# look at relapses around HADS 
in_relapse_data %>%
  left_join(population,by="UserId") %>%
  mutate(relapse_to_hads = abs(delta_dates_years(as.Date(visit_date,format="%Y-%m-%d"),hads_date))) %>%
  group_by(UserId) %>%
  slice_min(relapse_to_hads, with_ties = F) %>%
  filter(relapse_to_hads<1/12) %>%
  nrow()

# stats

comp = compareGroups::compareGroups(data =population,
                                    formula = edss_6_reached ~ age_at_dx + 
                                      age_at_sx +
                                      age_at_baseline_edss +
                                      disease_duration_at_baseline_edss +
                                      fu_time_first_to_last_edss +
                                      n_EDSS_readings + 
                                      Gender +
                                      pms_onset+
                                      baseline_EDSS+
                                      had_high_eff_dmt_pre_baseline+
                                      depressed+
                                      anxious,
                                    method = c(2,2,2,2,2,2,3,3,2,3,3,3))

x=compareGroups::createTable(comp)
compareGroups::export2word(x,"../outputs/demographics_table.doc")

population %>%
  dplyr::count(Gender) %>%
  mutate(prop = n/sum(n))

population %>%
  dplyr::count(MSAtDiagnosis) %>%
  mutate(prop = n/sum(n))

population %>%
  dplyr::count(Ethnicity) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(desc(prop))

population %>%
  summarise_at(
    vars(c(age_at_dx,age_at_sx)),
    c("median","IQR"),
    na.rm=T
  )
# cox models 
library(survival)
library(survminer)

# define time to event 
data_extract_date = as.Date("01-06-2022",format = "%d-%m-%Y")

# basic K-M plots
cox_basic_model = survfit(Surv(time_from_dx_to_edss,edss_6_reached,type="right") ~ depressed,
                          data = population)
p=ggsurvplot(fit = cox_basic_model,data=population, conf.int = F,
             risk.table = F,
             pval = F,
             palette = c("orange","darkblue"),
             xlim = c(0,7),
             ylim=c(0.5,1),
             break.time.by=1,
             legend=c(0.2,0.3),
             legend.labs=c("Depressed","Not depressed"))+
  labs(x = "Time from baseline visit (years)", y = "Probability of remaining \nat EDSS <6")
print(p)

# relevel factors
population$depressed = relevel(factor(population$depressed),ref="not_depressed")
population$anxious= relevel(factor(population$anxious),ref="not_anxious")


# cox models
cox_model_all = coxph(data = population %>%
                        dplyr::select(time_from_dx_to_edss,edss_6_reached,
                        age_at_baseline_edss, 
                        disease_duration_at_baseline_edss,
                        pms_onset,
                        Gender,
                        baseline_EDSS,
                        depressed) %>%
                        na.omit(),
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ 
                     age_at_baseline_edss + 
                     disease_duration_at_baseline_edss + 
                     pms_onset + 
                     Gender +
                     baseline_EDSS +
                     depressed)

summary(cox_model_all)
MASS::stepAIC(object = cox_model_all,direction = "both",)

cox_model00 = coxph(data = population,
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ tt(age_at_baseline_edss) + pms_onset + Gender + depressed)
summary(cox_model00)
cox_model0 = coxph(data = population,
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + pms_onset + Gender + depressed)

cox_model1 = coxph(data = population,
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + disease_duration_at_baseline_edss + pms_onset + Gender + depressed)

cox_model2 = coxph(data = population,
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + baseline_EDSS + pms_onset + Gender + depressed)

cox_model3 = coxph(data = population %>% filter(MSAtDiagnosis=="RRMS"),
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + Gender + depressed)

cox_model4 = coxph(data = population %>% filter(Gender=="MALE"),
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + pms_onset + depressed)
cox_model5 = coxph(data = population %>% filter(Gender=="FEMALE"),
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + pms_onset + depressed)

cox_model6 = coxph(data = population %>% filter(baseline_EDSS < 4),
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + pms_onset + Gender + depressed)

cox_model7 = coxph(data = population,
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + had_high_eff_dmt_pre_baseline+ Gender + depressed)

# add smoking status
smoking_status = data$`/Portal/Demographics_PRO` %>%
  dplyr::select(UserId,v3_smoking) %>%
  filter(!is.na(v3_smoking)) %>%
  mutate(smoking = ifelse(v3_smoking==1,"yes","no")) 
population = population %>% left_join(smoking_status,by="UserId")
table(population$smoking)
cox_model8 = coxph(data = population,
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + smoking)

summary(cox_model8)
cox_model9 = coxph(data = population,
                   Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss + pms_onset + Gender + anxious)
summary(cox_model9)



# combine results
res_df = bind_rows(
get_coefs_from_cox(cox_model0,"Primary analysis"),
get_coefs_from_cox(cox_model1,"Disease duration"),
get_coefs_from_cox(cox_model2,"Baseline EDSS"),
get_coefs_from_cox(cox_model3,"Just RRMS"),
get_coefs_from_cox(cox_model4,"Men only"),
get_coefs_from_cox(cox_model5,"Women only"),
get_coefs_from_cox(cox_model6,"Baseline EDSS <=4"),
get_coefs_from_cox(cox_model7,"DMT"))
print(res_df)


# forest plot
res_df$model = factor(res_df$model,levels=res_df$model,ordered=T)
res_df$model = fct_rev(res_df$model)
p1=ggplot(res_df,
       aes(HR,model))+
  geom_point(size=3,shape=15)+
  geom_errorbarh(mapping = aes(xmin = lower_ci,xmax = upper_ci,y=model),height=0.3)+
  theme_minimal()+
  scale_x_log10(limits=c(0.3,30))+
  labs(x="Hazard Ratio for progression\nto EDSS 6")+
  geom_vline(xintercept = 1,alpha=0.8,linetype="dashed")

print(p1)
png("../outputs/figure_2.png",res=300,units="in",width=6,height=6)
gridExtra::grid.arrange(p$plot,p1)
dev.off()

# diagnostics 
cox.zph(cox_model0)
ggcoxzph(cox.zph(cox_model0))
ggcoxdiagnostics(cox_model0,type = "deviance",linear.predictions = F)
ggcoxfunctional(Surv(time_from_dx_to_edss,edss_6_reached) ~ age_at_baseline_edss, data=population)

# look at colliding 
colliding = demographics %>%
  mutate(in_study_pop = ifelse(UserId %in% population$UserId,"study_pop","non_study_pop"))

# compare
colliding$DiagnosisYear = as.numeric(colliding$DiagnosisYear)

comp = compareGroups::compareGroups(data =colliding,
                                    formula = in_study_pop ~ age_at_dx + 
                                      Gender +
                                      Ethnicity + 
                                       MSAtDiagnosis +
                                      DiagnosisYear,
                                    method = c(2,3,3,3,3,2))

x=compareGroups::createTable(comp)
compareGroups::export2word(x,"../outputs/demographics_table2.doc")

