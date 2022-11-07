library(magrittr)
library(ggplot2)
library(plotly)
library(dplyr)

# get data from the  line below, unzip and store all csv's in the "data_dir" directory
# https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/
data_dir <- "data directory"
all_files <- list.files(data_dir)

csv_files <- grep(".csv", all_files, value = TRUE)
kd_files <- grep("k_d_t", csv_files, value = TRUE, invert = TRUE)

kd_colnames <- c("Kod_stacji",
                 "Nazwa_stacji",                        
                 "Rok",                                    
                 "Miesi¹c",                                 
                 "Dzieñ",                                   
                 "Maksymalna_temperatura_dobowa",    
                 "Status_pomiaru_TMAX",                     
                 "Minimalna_temperatura_dobowa",
                 "Status_pomiaru_TMIN" ,                   
                 "Œrednia_temperatura_dobowa" ,   
                 "Status_pomiaru_STD",                      
                 "Temperatura_minimalna_przy_gruncie", 
                 "Status_pomiaru_TMNG",                  
                 "Suma_dobowa_opadów",
                 "Status_pomiaru",
                 "Rodzaj_opadu",
                 "Wysokoœæ_pokrywy_œnie¿nej",
                 "Status_pomiaru_PKSN")    


vlist_kd <- list()

j <- 1
for (file in kd_files) {
  vlist_kd[[j]] <- read.csv(paste0(data_dir, file), header = FALSE)
  j <- j + 1
}

data_kd <- do.call(rbind, vlist_kd)
colnames(data_kd) <- kd_colnames
dict_codes_stations <- data_kd %>% 
  dplyr::distinct(Kod_stacji, Nazwa_stacji)

# 1. Temperature analysis
data_kd_agg <- data_kd %>% 
  dplyr::mutate(diff_temp_max_min = Maksymalna_temperatura_dobowa - Minimalna_temperatura_dobowa) %>% 
  dplyr::group_by(Rok, Miesi¹c, Kod_stacji) %>% 
  dplyr::summarise(srednia_temp_max = mean(Maksymalna_temperatura_dobowa, na.rm = TRUE),
                   srednia_temp_min = mean(Minimalna_temperatura_dobowa, na.rm = TRUE),
                   srednia_ilosc_opadu = mean(Suma_dobowa_opadów, na.rm = TRUE),
                   srednia_diff_temp_max_min = mean(diff_temp_max_min, na.rm = TRUE),
                   srednia_Temperatura_minimalna_przy_gruncie = mean(Temperatura_minimalna_przy_gruncie, na.rm = TRUE),
                   srednia_Wysokoœæ_pokrywy_œnie¿nej = mean(Wysokoœæ_pokrywy_œnie¿nej, na.rm = TRUE),
                   
                   median_temp_max = median(Maksymalna_temperatura_dobowa, na.rm = TRUE),
                   median_temp_min = median(Minimalna_temperatura_dobowa, na.rm = TRUE),
                   median_ilosc_opadu = median(Suma_dobowa_opadów, na.rm = TRUE),
                   median_diff_temp_max_min = median(diff_temp_max_min, na.rm = TRUE),
                   median_Temperatura_minimalna_przy_gruncie = median(Temperatura_minimalna_przy_gruncie, na.rm = TRUE),
                   median_Wysokoœæ_pokrywy_œnie¿nej = median(Wysokoœæ_pokrywy_œnie¿nej, na.rm = TRUE),
                   
                   sd_temp_max = sd(Maksymalna_temperatura_dobowa, na.rm = TRUE),
                   sd_temp_min = sd(Minimalna_temperatura_dobowa, na.rm = TRUE),
                   sd_ilosc_opadu = sd(Suma_dobowa_opadów, na.rm = TRUE),
                   sd_diff_temp_max_min = sd(diff_temp_max_min, na.rm = TRUE),
                   sd_Temperatura_minimalna_przy_gruncie = sd(Temperatura_minimalna_przy_gruncie, na.rm = TRUE),
                   sd_Wysokoœæ_pokrywy_œnie¿nej = sd(Wysokoœæ_pokrywy_œnie¿nej, na.rm = TRUE),
                   
                   
                   srednia_temp_sred = mean(Œrednia_temperatura_dobowa, na.rm = TRUE),
                   srednia_suma_dobowa_opadów = mean(Suma_dobowa_opadów, na.rm = TRUE),
                   suma_miesieczna_opadów = sum(Suma_dobowa_opadów, na.rm = TRUE),
                   
                   
                   sd_temp_sred = mean(Œrednia_temperatura_dobowa, na.rm = TRUE),
                   sd_suma_dobowa_opadów = sd(Suma_dobowa_opadów, na.rm = TRUE))


data_kd_agg <- data_kd_agg %>%
  dplyr::left_join(dict_codes_stations, by.x = "Kod_stacji", by.y = "Kod_stacji") 

data_kd_agg <- data_kd_agg %>%
  dplyr::mutate(date = as.Date(paste0(Rok, "-", Miesi¹c, "-01")))

n_obs <- data_kd_agg %>%
  dplyr::group_by(Kod_stacji) %>% 
  dplyr::mutate(n_obs = dplyr::n(),
                max_date = max(date)) %>% 
  # select observations longer than 700 months and with most curent data in "2022-08-01"
  dplyr::filter(n_obs >= 700 & max_date == "2022-08-01")

unique_codes <- n_obs$Kod_stacji %>% unique()
 
data_kd_agg <- data_kd_agg %>%
  dplyr::filter(Kod_stacji %in% unique_codes)

df_months <- data.frame(month_name = c("January", "February", "March", 
                                       "April", "May", "June", "July", 
                                       "August", "September", "October",
                                       "November", "December"), 
                        Miesi¹c = 1:12)
data_kd_agg2 <- data_kd_agg %>% 
  dplyr::left_join(df_months)

data_kd_agg2$month_name = factor(data_kd_agg2$month_name,
                                 levels=c("January", "February", "March", 
                                          "April", "May", "June", "July", 
                                          "August", "September", "October",
                                          "November", "December"))



# 1 Temperature

p1 <- ggplot(data_kd_agg2,
             aes(x = as.Date(date),
                 y = srednia_temp_sred,
                 group = 1,
                 text = paste0('mean monthly temperature: ', 
                               round(srednia_temp_sred, 2), 
                               "<br>",
                               'date: ',
                               substr(date, 1, 7)))) +
  geom_line(colour = "red") +
  xlab("Year")+ 
  ylab("Mean temperature [Celsius degrees]") +
  theme_bw() +
  scale_y_continuous(breaks = seq(-21, 30, by = 3)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  margin = margin(0, 0, -200, 0),
                                  size = 20),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7)) +
  ggtitle("Mean monthly temperatures in years 1951-2022 for selected cities in Poland") +
  facet_grid(Nazwa_stacji ~month_name)


p1 <- plotly::ggplotly(p1, 
                       width = 1400,
                       height = 4000,
                       tooltip = c("text")) %>% config(displayModeBar = F, scrollZoom = FALSE)
p1

p2 <- ggplot(data_kd_agg2,
             aes(x = as.Date(date),
                 y = srednia_temp_sred,
                 group = 1,
                 text = paste0('mean monthly temperature: ', 
                               round(srednia_temp_sred, 2), 
                               "<br>",
                               'date: ',
                               substr(date, 1,7)))) +
  geom_line(colour = "red") +
  xlab("Year")+ 
  ylab("Mean temperature [Celsius degrees]") +
  theme_bw() +
  scale_y_continuous(breaks = seq(-21, 30, by = 3)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  margin = margin(0, 0, -200, 0),
                                  size = 20),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(2, "lines")) +
  ggtitle("Mean monthly temperatures in years 1951-2022 for selected cities in Poland") +
  facet_grid(Nazwa_stacji ~month_name)


p2 <- plotly::ggplotly(p2, 
                       width = 4000,
                       height = 8000, 
                       tooltip = c("text")) %>% config(displayModeBar = F,
                                                       responsive = F,
                                                       doubleClick = F,
                                                       scrollZoom = FALSE) %>% 
  # remove responsivenes of the subplots
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) %>% 
  layout(xaxis2 = list(fixedrange = TRUE), yaxis2 = list(fixedrange = TRUE)) %>% 
  layout(xaxis3 = list(fixedrange = TRUE), yaxis3 = list(fixedrange = TRUE)) %>% 
  layout(xaxis4 = list(fixedrange = TRUE), yaxis4 = list(fixedrange = TRUE)) %>% 
  layout(xaxis5 = list(fixedrange = TRUE), yaxis5 = list(fixedrange = TRUE)) %>% 
  layout(xaxis6 = list(fixedrange = TRUE), yaxis6 = list(fixedrange = TRUE)) %>% 
  layout(xaxis7 = list(fixedrange = TRUE), yaxis7 = list(fixedrange = TRUE)) %>% 
  layout(xaxis8 = list(fixedrange = TRUE), yaxis8 = list(fixedrange = TRUE)) %>% 
  layout(xaxis9 = list(fixedrange = TRUE), yaxis9 = list(fixedrange = TRUE)) %>% 
  layout(xaxis10 = list(fixedrange = TRUE), yaxis10 = list(fixedrange = TRUE)) %>% 
  layout(xaxis11 = list(fixedrange = TRUE), yaxis11 = list(fixedrange = TRUE)) %>% 
  layout(xaxis12 = list(fixedrange = TRUE), yaxis12 = list(fixedrange = TRUE)) %>% 
  layout(xaxis13 = list(fixedrange = TRUE), yaxis13 = list(fixedrange = TRUE)) %>% 
  layout(xaxis14 = list(fixedrange = TRUE), yaxis14 = list(fixedrange = TRUE)) %>% 
  layout(xaxis15 = list(fixedrange = TRUE), yaxis15 = list(fixedrange = TRUE)) %>% 
  layout(xaxis16 = list(fixedrange = TRUE), yaxis16 = list(fixedrange = TRUE)) %>% 
  layout(xaxis17 = list(fixedrange = TRUE), yaxis17 = list(fixedrange = TRUE)) %>% 
  layout(xaxis18 = list(fixedrange = TRUE), yaxis18 = list(fixedrange = TRUE)) %>% 
  layout(xaxis19 = list(fixedrange = TRUE), yaxis19 = list(fixedrange = TRUE)) %>% 
  layout(xaxis20 = list(fixedrange = TRUE), yaxis20 = list(fixedrange = TRUE)) %>% 
  layout(xaxis21 = list(fixedrange = TRUE), yaxis21 = list(fixedrange = TRUE)) %>% 
  layout(xaxis22 = list(fixedrange = TRUE), yaxis22 = list(fixedrange = TRUE)) %>% 
  layout(xaxis23 = list(fixedrange = TRUE), yaxis23 = list(fixedrange = TRUE)) %>% 
  layout(xaxis24 = list(fixedrange = TRUE), yaxis24 = list(fixedrange = TRUE)) %>% 
  layout(xaxis25 = list(fixedrange = TRUE), yaxis25 = list(fixedrange = TRUE)) %>% 
  layout(xaxis26 = list(fixedrange = TRUE), yaxis26 = list(fixedrange = TRUE)) %>% 
  layout(xaxis27 = list(fixedrange = TRUE), yaxis27 = list(fixedrange = TRUE)) 

p2

# 2 Precipation 

p3 <- ggplot(data_kd_agg2,
             aes(x = as.Date(date),
                 y = data_kd_agg2$suma_miesieczna_opadów,
                 group = 1,
                 text = paste0('sum monthly precipation: ', 
                               round(data_kd_agg2$suma_miesieczna_opadów, 2), 
                               "<br>",
                               'date: ',
                               substr(date, 1,7)))) +
  geom_line(colour = "blue") +
  xlab("Year")+ 
  ylab("Precipation [mm]") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 400, by = 40)) +
  theme(plot.title = element_text(hjust = 0.5,
                                  margin = margin(0, 0, -200, 0),
                                  size = 20),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7)) +
  ggtitle("Monthly precipation in years 1951-2022 for selected cities in Poland") +
  facet_grid(Nazwa_stacji ~month_name)

p3 <- plotly::ggplotly(p3, width = 1400, height = 4000, tooltip = c("text")) %>%
  config(displayModeBar = F)
p3
