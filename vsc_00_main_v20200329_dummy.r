# -------------------------------------------------------------------- #
#
#   Project COVID19 - optimal allocation of samples
#
#
#   purpose: main file - VSC
#   date: 29.03.2020
#   author: S.Herzog
# -------------------------------------------------------------------- #

# setwd(Sys.getenv('VSC_SCRATCH'))
#   
# # ------------------------- #
# #  PATHS ----
# # ------------------------- #
# 
#   path_Rfiles <- paste0(Sys.getenv('VSC_DATA'),
#                        "/covid19/Rfiles/")  
#   
#   path_Rdata <- paste0(Sys.getenv('VSC_DATA'),
#                        "/covid19/Rdata/") 
#   
#   path_Routput <- paste0(Sys.getenv('VSC_DATA'),
#                        "/covid19/Routput/") 

# ------------------------- #
#  LIBRARIES ----
# ------------------------- #

  library(tidyverse)
  library(ggplot2)
  library(readxl)
  library(doParallel)

  print('libraries loaded')

# ------------------------- #
#  FUNCTIONS ----
# ------------------------- #

 

  # source(file = paste0(path_Rfiles,"01_functions_v20200327.R"))
  
  print('functions loaded')
  
# --------------------------------------------------------------------------- #
#  INPUT ----
# --------------------------------------------------------------------------- #
  # ------------------------- #
  #  Name save all ----
  # ------------------------- #
  
    # name to save simulation run
      date_sim_run <- as.Date(x = "29.03.2020", format = "%d.%m.%Y")
      nam_infosim <- "_pattern868_nsim1000"
      nam_file_belgsim <- paste0(date_sim_run, "_simulation", nam_infosim,".Rdata")
      
  # # ------------------------- #
  # #  Belgium population ----
  # # ------------------------- #
  #   # source: Federal Planning Bureau
  # 
  #  
  #   # names of file to import to R
       nam_file_belgpop <- "20200326_popbelfr.xlsx"
         sheet_name_belgpop <- "Belgique"
         range_belgpop <- "A4:AE115"
  #     
  #   # year using for population
  #     year_popbelgium <- 2020 
  #     
  #     
  #   # Definition of age categories - should be in line with reported cases data
  #     age_breaks <- c(0, seq(10, 90, 10), Inf) 
  #       # ! use 0 only if 'age_right'=FALSE
  #       # otherwise use '-Inf'
  #     
  #     age_right <- FALSE 
  #       # logical, indicating if the intervals should be closed on
  #       # the right (and open on the left) or vice versa.
  #     
  #     
  #   # info about data
  #     info_population_belgium <-
  #       tibble(date_access = "26.03.2020",
  #              name_file = nam_file_belgpop,
  #              year_population = year_popbelgium)
  #     
  #     
  # # ------------------------- #
  # #  Belgium reported cases ----
  # # ------------------------- #
  #   # source: Bulletin Sciensano
  #   #         Figure on page 2  
  #   # https://epidemio.wiv-isp.be/ID/Pages/2019-nCoV_epidemiological_situation.aspx  
  #   #    
  #   
  #     
  #     
  #   # names of file to import to R
  #     nam_file_belgcases <- "20200327-CoVid-19-numbers_from_update_27032020.xlsx"    
  #       sheet_name_belgcases <- "Data"
  #       range_belgcases <- "A2:D12"
  #       range_belgcases_info <- "A15:F16"
  #     
  #       
  #   # with which total should factor (=estimated/observed) be calulated?
  #       # options: 'figure' or 'text'     
  #     tot_workwith <- "figure"
  #       
  #       
  # # ------------------------- #
  # #  Belgium estimated prevalence ----
  # # ------------------------- #
  #   # Source: nowcasting model Christel Faes - Table 2
  #     
  #   # names of file to import to R
  #     nam_file_belgestprev <- "predicton_I(t)_27032020.xlsx"
  #       sheet_name_belgestprev <- "Table 1"
  #       range_belgestprev <- "A56:E96"
  #       range_belgestprev_info <- "A1:A5"
  #   
  #   # define date to use for calculating estimated prevalence
  #     prop_asymptomatic <- 0.5  
  #     day_difference <- 12 
  #       #  now confirmed and dates back from incubation period + hospitalisation 
  #       # which is about 12 days on average
  #     date_to_use <- as.POSIXct(as.Date(x = "26.03.2020", format = "%d.%m.%Y") + day_difference)
  #     
  #   
      
  # ------------------------- #
  #  Getting underlying 'true' prevalences ----
  # ------------------------- #
     # uniform - apply estimated prevalence to all age classes
        # prev_estimated_cat
     
     # reported cases - use reported cases 
        # prev_repcases_cat
      
     # apply factor "underreporting etc." to prev_repcases_cat
        # prev_repcases_factor_cat
      
     # prevalence expected according to contact pattern (Lander)
        # prev_contact_cat
      
      
  # ------------------------- #
  #  Simulation ----
  # ------------------------- #    
      nsimulations <- 1000
      proportion_options <- seq(0.1, 0.30, 0.05)

      info_simulation <- list(list(
                              combination = 1,
                              seed = 122,
                              # Which age categories should be investigated in the simulations?
                              age_cat_investigated_sim = c("[5,10)",
                                                           "[10,20)",
                                                           "[20,30)",
                                                           "[30,40)",
                                                           "[40,50)",
                                                           "[50,60)",
                                                           "[60,70)"),
                              age_breaks = c(0, 5, seq(10, 70, 10), Inf),
                              age_right = FALSE,
                              nsimulations = nsimulations, # number of simulations
                              max_samples = 4000, # maximum number of samples
                              proportion_options = proportion_options # Which proportions can be observed?
                              ),
                              
                              list(
                                combination = 2,
                                seed = 3049,
                                # Which age categories should be investigated in the simulations?
                                age_cat_investigated_sim = c("[5,10)",
                                                             "[15,20)",
                                                             "[25,30)",
                                                             "[35,40)",
                                                             "[45,50)",
                                                             "[55,60)",
                                                             "[65,70)"),
                                age_breaks = c(0, 5, seq(10, 70, 5), Inf),
                                age_right = FALSE,
                                nsimulations = nsimulations, # number of simulations
                                max_samples = 4000, # maximum number of samples
                                proportion_options = proportion_options # Which proportions can be observed?
                              )
      )
      
      
      # Note
        # for age_breaks: use 0 only if 'age_right'=FALSE otherwise use '-Inf'
        # for age_right <- FALSE # logical, indicating if the intervals should 
        #                  be closed on the right (and open on the left) or vice versa.

  # ------------------------- #
  #  foreach - parallel ----
  # ------------------------- #    
     # nr.clusters <- 8 # number of clusters to be used for the parallel NOT NEEDED FOR VSC

      # function to get nice output from foreach loop
              comb <- function(...) {
                mapply('cbind', ..., SIMPLIFY=FALSE)
              }
      
      print('input ready')    
      
# --------------------------------------------------------------------------- #
#  DATA PREPERATION ----
# --------------------------------------------------------------------------- #
#   
#   # ------------------------- #
#   #  Belgium population ----
#   # ------------------------- #
#  
#     dt_pop_belgium_orig <-
#       read_excel(path = paste0(path_Rdata, nam_file_belgpop),
#                  sheet = sheet_name_belgpop,
#                  range = range_belgpop)
#     
#     dt_pop_belgium <-
#       dt_pop_belgium_orig %>%
#       rename("age_orig" = contains("...1"),
#              "N_age" = contains(paste0(year_popbelgium))) %>%
#       mutate(age = readr::parse_number(age_orig),
#              age_cat = cut(age, breaks = age_breaks,
#                            right = age_right)) %>%
#       
#       select(age, age_cat, N_age)
#     
#     dt_pop_belgium_cat <-
#       dt_pop_belgium %>%
#       group_by(age_cat) %>%
#       mutate(N_age_cat = sum(N_age)) %>%
#       filter(row_number() == 1) %>%
#       ungroup %>%
#       select(age_cat, N_age_cat)
#     
#     # check sum btw categorizing and total 
#     
#       
#       if((dt_pop_belgium_cat %>% pull(N_age_cat) %>% sum) !=
#         (dt_pop_belgium %>% pull(N_age) %>% sum)){
#         message("!!Attention!! - Counts of persons in population don't match with count after 
#                 generating age categories
#                 => look at: dt_pop_belgium_cat, dt_pop_belgium")
#       }
#       
#      
#     
#     # rm what is not needed anymore
#       rm(age_breaks, age_right, 
#          dt_pop_belgium_orig,
#          sheet_name_belgpop,
#          range_belgpop)
#     
#       
#   # ------------------------- #
#   #  Belgium reported cases ----
#   # ------------------------- #
#       
#       
#       dt_cases_belgium_cat <-
#         read_excel(path = paste0(path_Rdata, nam_file_belgcases),
#                    sheet="Data",range = range_belgcases) %>%
#         mutate(N_cases_cat = Men + Woman,
#                age_cat = factor(age_cat))
#       
#       
#       
#       info_cases_belgium <-
#         read_excel(path = paste0(path_Rdata, nam_file_belgcases),
#                    sheet=sheet_name_belgcases,range = range_belgcases_info) 
#       
#       # tot_cases_figure <- dt_cases_belgium_cat %>% pull(N_cases_cat) %>% sum
#       # tot_cases_text <- info_cases_belgium$Total_Text
#       # 
#        
#       prev_reported <- (dt_cases_belgium_cat %>% pull(N_cases_cat) %>% sum) / 
#                         (dt_pop_belgium %>% pull(N_age) %>% sum)
#       
#     # rm
#       rm(sheet_name_belgcases, range_belgcases_info, range_belgcases)
#       
#       
#       
# # ------------------------- #
# #  Belgium estimated prevalence ----
# # ------------------------- #
#   
#    
#     info_estprev_belgium <-
#       read_excel(path = paste0(path_Rdata, nam_file_belgestprev),
#                  sheet = sheet_name_belgestprev,
#                  range = range_belgestprev_info) %>%
#       filter(row_number()!=2)  %>%
#       rename(date_report = `Nowcasting - COVID-19`)  %>%
#       mutate(date_start_data = date_report[2],
#              date_end_data = date_report[3]) %>%
#       filter(row_number() == 1) %>%
#         mutate(date_used = date_to_use,
#                day_difference = day_difference,
#                prop_asymptomatic = prop_asymptomatic,
#                Source = sub("xlsx", "pdf", nam_file_belgestprev))
#       
#     
#    
#     dt_estprev_belgium_orig <-
#       read_excel(path = paste0(path_Rdata, nam_file_belgestprev),
#                  sheet = sheet_name_belgestprev,
#                  range = range_belgestprev) %>%
#       rename("date" = contains("...1")) 
#   
#     
#     dt_estprev_belgium <-
#       dt_estprev_belgium_orig %>%
#         filter(date <= date_to_use) %>%
#         mutate(cum_cases = cumsum(`I(t)`),
#                cum_cases_adj_asymptomatic = cum_cases/prop_asymptomatic)
#     
#     prev_estimated <- (dt_estprev_belgium %>% 
#                          filter(date == date_to_use) %>%
#                          pull(cum_cases_adj_asymptomatic)) / 
#                       (dt_pop_belgium %>% pull(N_age) %>% sum)
#     
#   
#     # rm
#       rm(sheet_name_belgestprev, range_belgestprev, range_belgestprev_info,
#          date_to_use, prop_asymptomatic)
#     
#       print('datasets included')  
#         
# --------------------------------------------------------------------------- #
#  GETTING UNDERLYING 'TRUE' PREVALENCES ----
# --------------------------------------------------------------------------- #
# 
# 
#   # ------------------------- #
#   #  Combine data Belgium: population, cases & estimated prevalence ----
#   # ------------------------- #
# 
#     # factor btw reported overall prevalence and estimated overall prevalence 
#       f_prevalence_est_div_obs <- prev_estimated/prev_reported
#       
#     # create   dt_belgium_prevalence_cat
#       dt_belgium_prevalence_cat <-
#         left_join(x = dt_pop_belgium_cat,
#                   y = dt_cases_belgium_cat %>% 
#                     select(-c(Men, Woman, Age)) %>%
#                     rename(N_repcases_cat = N_cases_cat),
#                   by = c("age_cat" = "age_cat")) %>%
# 
#       # uniform - apply estimated prevalence to all age classes
#         # prev_estimated_cat
#           mutate(prev_estimated_cat = prev_estimated) %>%
#         
#       # reported cases - use reported cases 
#         # prev_repcases_cat
#           mutate(prev_repcases_cat = N_repcases_cat/N_age_cat)  %>%
#       
#         
#         
#       # apply factor "underreporting etc." to prev_repcases_cat
#         # prev_repcases_factor_cat
#           mutate(prev_repcases_factor_cat = prev_repcases_cat *f_prevalence_est_div_obs ) %>%
#       
#       # prevalence expected according to contact pattern (Lander) <- OPEN!!!!!!!
#         # prev_contact_cat
#           mutate(prev_contact_cat = NA)
#       
#       print('true prevalence defined')  
#       
# --------------------------------------------------------------------------- #
#  SIMULATION ----
# --------------------------------------------------------------------------- #
      print('save before simulation')
      save(list = ls(all.names = TRUE), file = "beforesimulation.RData")
#       
#       print('start simulation')
#       
   list_nam_file_sim_comb <- NULL
#  i_comb <- 1 #HIIIIEEEEEEEEEERRRRRRRR TO CHANGE ----      
   for(i_comb in 1:length(info_simulation)){  
    
    nam_file_sim_comb <- paste0(date_sim_run, "_simulation_",nam_infosim,
                               "_combination_",
                               info_simulation[[i_comb]]$combination,".Rdata")
   list_nam_file_sim_comb <- c(list_nam_file_sim_comb,
                               nam_file_sim_comb)
    
#   # ------------------------- #
#   #  Translate age specific prevalence to age categories of interest ----
#   # ------------------------- #
#  
#       dt_for_simulation_translation <-
#             left_join(x = dt_pop_belgium,
#                       y = dt_belgium_prevalence_cat,
#                       by = c("age_cat" = "age_cat"))  %>%
#            
#             mutate(age_cat_sim = cut(age, 
#                                      breaks = info_simulation[[i_comb]]$age_breaks, 
#                                      right = info_simulation[[i_comb]]$age_right)) %>%
#             group_by(age_cat_sim) %>%
#             mutate(N_age_cat_sim = sum(N_age),
#                   
#                    prev_estimated_cat_sim = sum(N_age * prev_estimated_cat)/ N_age_cat_sim,
#                    prev_repcases_cat_sim = sum(N_age * prev_repcases_cat)/ N_age_cat_sim,
#                    prev_repcases_factor_cat_sim = sum(N_age * prev_repcases_factor_cat)/ N_age_cat_sim,
#                    prev_contact_cat_sim = sum(N_age * prev_contact_cat)/ N_age_cat_sim) %>%
# 
#             ungroup() 
#       
#       dt_for_simulation_cat <-
#            dt_for_simulation_translation %>%
#                 select(contains("_sim")) %>%
#                 group_by(age_cat_sim) %>%
#                 filter(row_number() == 1) %>%
#                 ungroup
#       
#       
#       dt_for_simulation_cat_reduced <-
#           dt_for_simulation_cat %>%
#                 filter(age_cat_sim %in% info_simulation[[i_comb]]$age_cat_investigated_sim) %>%
#                 mutate(age_cat_sim = fct_drop(age_cat_sim))
# 
#     # ------------------------- #
#     #  What are the possible patterns? ----
#     # ------------------------- #
#       
#       # number of age classes   
#         nageclasses <-  dt_for_simulation_cat_reduced %>% nrow()   
#       
#       # What are the possible patterns?
#         dt_patterns <-
#             gtools::permutations(n = length(info_simulation[[i_comb]]$proportion_options),
#                                  r = nageclasses,
#                                  v = info_simulation[[i_comb]]$proportion_options,
#                                  set = TRUE,
#                                  repeats.allowed = TRUE)
#         
#         colnames(dt_patterns) <- paste0("V", 1:nageclasses)
#         
#         dt_patterns <-
#         dt_patterns %>%
#             as_tibble() %>%
#             # reduce to patterns for which sum = 1
#               mutate(s = rowSums(.)) %>%
#               filter(s ==1) %>%
#               select(-s) %>%
#             # give age category names
#               rename_at(vars(contains("V")),
#                         ~ levels(dt_for_simulation_cat_reduced$age_cat_sim)
#                                  ) %>%
#               mutate(name = paste0("p", row_number())) %>%
#               select(name, everything())
# 
#         dt_patterns_mat <-
#               dt_patterns %>%
#               select(-name) %>%
#               as.matrix()
#         
#         
    # ------------------------- #
    #  Loop with foreach loop ----
    # ------------------------- #
        print('save before parallel job')
        save(list = ls(all.names = TRUE), file = "beforeloop.RData")

        print('start parallel job')

        start <- Sys.time()

        set.seed(info_simulation[[i_comb]]$seed)

        # start foreach loop
        registerDoParallel(cores = 28)

        # dummy
        dt_patterns_mat <- matrix(NA,nrow=10,ncol=10)

        loop_output <-
              foreach(i_pattern = 1:nrow(dt_patterns_mat),
                      .combine='comb',
                      .packages = c("tidyverse"),         # load packages
                      .inorder = FALSE,                   # if the order is not important,
                      .multicombine = TRUE                # set TRUE if the .combine function can accept more than two arguments
                    ) %dopar% {#  loop
# 
#             # prev_estimated_cat_sim
#               res <-
#                 fn_sim(age_cat = levels(dt_for_simulation_cat_reduced$age_cat_sim),
#                        prevalence = dt_for_simulation_cat_reduced$prev_estimated_cat_sim,
#                        max_samples = info_simulation[[i_comb]]$max_samples,
#                        allocation = dt_patterns_mat[i_pattern,] ,
#                        nsimulations = info_simulation[[i_comb]]$nsimulations,
#                        seed = NULL)
# 
#                       res_vec <- c( dt_patterns$name[i_pattern], 
#                                     res$dat_sum$nsim[1],
#                                     res$dat_sum$dispersion, 
#                                     res$sum_dispersion,
#                                     res$max_dispersion,
#                                     paste0(res$max_dispersion_agecat, collapse = ", "))
#                       names(res_vec) <- c("name", 
#                                           "nsim",
#                                           paste0("dispersion_",res$input$age_cat), 
#                                           "dispersion_sum",
#                                           "max_dispersion",
#                                           "max_dispersion_agecat")
# 
#                       res_estimated_cat_vec <-res_vec
#                       
#                       rm(res, res_vec)
#                       
#              # prev_repcases_cat_sim
#                 res <-
#                         fn_sim(age_cat = levels(dt_for_simulation_cat_reduced$age_cat_sim),
#                                prevalence = dt_for_simulation_cat_reduced$prev_repcases_cat_sim,
#                                max_samples = info_simulation[[i_comb]]$max_samples,
#                                allocation = dt_patterns_mat[i_pattern,] ,
#                                nsimulations = info_simulation[[i_comb]]$nsimulations,
#                                seed = NULL)
#                       
#                 res_vec <- c( dt_patterns$name[i_pattern], 
#                               res$dat_sum$nsim[1],
#                               res$dat_sum$dispersion, 
#                               res$sum_dispersion,
#                               res$max_dispersion,
#                               paste0(res$max_dispersion_agecat, collapse = ", "))
#                 names(res_vec) <- c("name", 
#                                     "nsim", 
#                                     paste0("dispersion_",res$input$age_cat), 
#                                     "dispersion_sum",
#                                     "max_dispersion",
#                                     "max_dispersion_agecat")
#                       
#                       res_repcases_cat_vec <-res_vec
#                       
#                     rm(res, res_vec)  
#                     
#                     
#           # prev_repcases_factor_cat_sim
#                 res <-
#                       fn_sim(age_cat = levels(dt_for_simulation_cat_reduced$age_cat_sim),
#                              prevalence = dt_for_simulation_cat_reduced$prev_repcases_factor_cat_sim,
#                              max_samples = info_simulation[[i_comb]]$max_samples,
#                              allocation = dt_patterns_mat[i_pattern,] ,
#                              nsimulations = info_simulation[[i_comb]]$nsimulations,
#                              seed = NULL)
#                     
#                 res_vec <- c( dt_patterns$name[i_pattern], 
#                               res$dat_sum$nsim[1],
#                               res$dat_sum$dispersion, 
#                               res$sum_dispersion,
#                               res$max_dispersion,
#                               paste0(res$max_dispersion_agecat, collapse = ", "))
#                 names(res_vec) <- c("name", 
#                                     "nsim", 
#                                     paste0("dispersion_",res$input$age_cat), 
#                                     "dispersion_sum",
#                                     "max_dispersion",
#                                     "max_dispersion_agecat")
#                     
#                     res_repcases_factor_cat_vec <-res_vec
#                     
#                     rm(res, res_vec)  
#                     
#                     
#           # prev_contact_cat_sim <- NOT YET !!!!!!
#               res_contact_cat_vec <- NULL
#               # res <-
#               #         fn_sim(age_cat = levels(dt_for_simulation_cat_reduced$age_cat_sim),
#               #                prevalence = dt_for_simulation_cat_reduced$prev_contact_cat_sim,
#               #                max_samples = info_simulation[[i_comb]]$max_samples,
#               #                allocation = dt_patterns_mat[i_pattern,] ,
#               #                nsimulations = info_simulation[[i_comb]]$nsimulations,
#               #                seed = NULL)
#               #       
#               # res_vec <- c( dt_patterns$name[i_pattern], 
#               #               res$dat_sum$dispersion, 
#               #               res$sum_dispersion,
#               #               res$max_dispersion,
#               #               paste0(res$max_dispersion_agecat, collapse = ", "))
#               # names(res_vec) <- c("name", 
#               #                     paste0("dispersion_",res$input$age_cat), 
#               #                     "dispersion_sum",
#               #                     "max_dispersion",
#               #                     "max_dispersion_agecat")
#               #       
#               #       res_contact_cat_vec <-res_vec
#               #       
#               #       rm(res, res_vec)  
# 
#           return(list(res_estimated_cat_vec = res_estimated_cat_vec,
#                       res_repcases_cat_vec = res_repcases_cat_vec,
#                       res_contact_cat_vec = res_contact_cat_vec,
#                       res_repcases_factor_cat_vec = res_repcases_factor_cat_vec))
#                       
#                       
                     } # end foreach
 
           #stopCluster(cl)
# 
           end <- Sys.time()

        time_diff <- end - start

        print(i_comb)
        print(time_diff)
        print("----------")



      # ------------------------- #
      #  Save foreach loop output ----
      # ------------------------- #

        info_simulation_comb <- info_simulation[[i_comb]]

        save( info_simulation_comb,
              # dt_for_simulation_translation,
              # dt_for_simulation_cat,
              # dt_for_simulation_cat_reduced,
              # nageclasses,
              # dt_patterns,
              # dt_patterns_mat,
              # loop_output,
              time_diff,
             file = paste0("results/covid19/", nam_file_sim_comb))

} # loop over combinations of simulation

        print('simulation done')

# ------------------------- #
#  SAVE ----
# ------------------------- #

    save(nam_file_belgpop,
#          dt_pop_belgium,
#          dt_pop_belgium_cat,
#          year_popbelgium,
#          info_population_belgium,
#          
#          
#          
#          nam_file_belgcases,
#          dt_cases_belgium_cat,
#          prev_reported,
#          info_cases_belgium,
#          
#          
#          nam_file_belgestprev,
#          dt_estprev_belgium,
#          prev_estimated,
#          info_estprev_belgium,
#          
#          dt_belgium_prevalence_cat,
#          
#          info_simulation,
#          list_nam_file_sim_comb,
#          
         file = paste0("results/covid19/", nam_file_belgsim)
         )

    print('main output saved')

    print('job terminated')
