# Peter Boyd
# Oregon State University
# Assessing the contagiousness of mass shootings
# with nonparametric Hawkes processes
# February 2021


# Packages ----------------------------------------------------------------

library(tidyverse)
library(gridExtra)
library(lubridate)
# devtools::install_github("boydpe/nphawkes")
library(nphawkes)

# Data --------------------------------------------------------------------

brady = read.csv("brady_data.csv")
stanford = read.csv("stanford_data.csv")
mj = read.csv("mj_data.csv")
gva = read.csv("gva_data.csv")


# Brady -------------------------------------------------------------------

brady = brady[which(brady$state != "AK"),]
brady = brady[which(brady$state != "HI"),]
brady = brady[which(brady$naff > 2),]
brady$date = as.Date(brady$date, format = "%m/%d/%Y")

k_bins = c(0, 4, 5, 8, 100) 
g_bins = c(0, 14, 93, 183, 365, 8000)

out_brady = nph(dates = brady$date, 
                ref_date = "02-01-2005",
                marks = brady$naff,
                time_breaks = g_bins,
                mark_breaks = k_bins)


st_brady = super_thin(K = "median_ci",
                      model = out_brady,
                      method = "superthin")

# Stanford ----------------------------------------------------------------

stanford = stanford[which(stanford$State != "Alaska"),]
stanford = stanford[which(stanford$State != "Hawaii"),]
stanford$Date = as.Date(stanford$Date, format = "%m/%d/%Y")


k_bins = c(0, 4, 5, 7, 100) 
g_bins = c(0, 14, 93, 183, 365, 8000) 


out_stanford = nph(dates = stanford$Date, 
                   ref_date = "01-01-1999",
                   marks = stanford$Total.Number.of.Victims,
                   time_breaks = g_bins,
                   mark_breaks = k_bins)



st_stanford = super_thin(K = "median_ci",
                         model = out_stanford,
                         method = "superthin")

# Mother Jones ------------------------------------------------------------

mj = mj[which(mj$State != "Hawaii"),]
mj$Date = as.Date(mj$Date, format = "%m/%d/%Y")


k_bins = c(0, 6, 10, 17, 800) 
g_bins = c(0, 14, 93, 183, 365, 8000) 

out_mj = nph(dates = mj$Date, 
             ref_date = "01-01-1999",
             marks = mj$Affected,
             time_breaks = g_bins,
             mark_breaks = k_bins)

st_mj = super_thin(K = "median_ci",
                   model = out_mj,
                   method = "superthin")

# GVA ---------------------------------------------------------------------

gva = gva[which(gva$State != "Alaska"),]
gva = gva[which(gva$State != "Hawaii"),]
gva$Date = as.Date(gva$Date, format = "%m/%d/%Y")

k_bins = c(0, 4, 5, 9, 800) 
g_bins = c(0, 14, 93, 183, 365, 8000) 

out_gva = nph(dates = gva$Date, 
              ref_date = "01-01-2013",
              marks = gva$Affected,
              time_breaks = g_bins,
              mark_breaks = k_bins)

st_gva = super_thin(K = "median_ci",
                    model = out_gva,
                    method = "superthin")


# Figure One --------------------------------------------------------------

# plot of all events over time

df_brady  = brady 
df_mj = mj 
df_mj = df_mj[which(df_mj$Time > 0 ),]
df_stanford = stanford
df_stanford$Date = lubridate::mdy(df_stanford$Date)
df_stanford$Year = lubridate::year(df_stanford$Date)
df_stanford$Month = lubridate::month(df_stanford$Date)

df_gva = gva
df_gva$Date = lubridate::mdy(df_gva$Date)
df_gva$Year = lubridate::year(df_gva$Date)
df_gva$Month = lubridate::month(df_gva$Date)

df_brady_counts  = df_brady %>% group_by(year, month) %>%  
  summarize(n = n(), day = 1) %>% 
  mutate(date = make_date(year, month, day))

df_stanford_counts  = df_stanford %>% group_by(Year, Month) %>% 
  summarize(n = n(), Day = 1) %>% 
  mutate(date = make_date(Year, Month, Day))

df_gva_counts  = df_gva %>% group_by(Year, Month) %>% 
  summarize(n = n(), Day = 1) %>% 
  mutate(date = make_date(Year, Month, Day))

df_mj_counts  = df_mj %>% group_by(Year, Month) %>% 
  summarize(n = n(), Day = 1) %>% 
  mutate(date = make_date(Year, Month, Day))

min = as.Date("1999-1-1")
max = as.Date("2020-08-31")

p_brady = ggplot(df_brady_counts, aes(x = date, y = n)) + 
  geom_segment(aes(xend = date, yend = 0)) + 
  ylab("") + 
  scale_x_date(limits=c(min, max), 
               minor_breaks = "5 years",
               date_minor_breaks = "1 year") + 
  ggtitle("Brady") + 
  xlab(NULL)

p_stanford = ggplot(df_stanford_counts, aes(x = date, y = n)) + 
  geom_segment(aes(xend = date, yend = 0)) + 
  ylab("") + 
  scale_x_date(limits=c(min, max), 
               minor_breaks = "5 years",
               date_minor_breaks = "1 year") + 
  ggtitle("Stanford") + 
  xlab(NULL)

p_mj = ggplot(df_mj_counts, aes(x = date, y = n)) + 
  geom_segment(aes(xend = date, yend = 0)) + 
  ylab("") + 
  scale_x_date(limits=c(min, max), 
               minor_breaks = "5 years",
               date_minor_breaks = "1 year") + 
  scale_y_continuous(minor_breaks = NULL) +
  ggtitle("Mother Jones") + 
  xlab(NULL)

p_gva = ggplot(df_gva_counts, aes(x = date, y = n)) + 
  geom_segment(aes(xend = date, yend = 0)) + 
  ylab("") + 
  scale_x_date(limits=c(min, max), 
               minor_breaks = "5 years",
               date_minor_breaks = "1 year") + 
  ggtitle("GVA") +
  xlab(NULL)

pp = grid.arrange(p_brady, p_stanford, p_mj, p_gva,
                  nrow = 2, ncol = 2, left = "Number of Events Per Month",
                  bottom = "Year")

ggsave("Figure1.pdf", 
       pp, width = 7, height = 5)


# Figure Two --------------------------------------------------------------

# plot of magnitude of all events 

ms_brady = data.frame(vic = df_brady$naff,
                      group = 0,
                      dat = "brady")
ms_stanford = data.frame(vic = df_stanford$Total.Number.of.Victims, 
                         group = 0, 
                         dat = "stanford")
ms_mj = data.frame(vic = df_mj$Affected, 
                   group = 0, 
                   dat = "mj")
ms_gva = data.frame(vic = df_gva$Affected, 
                    group = 0, 
                    dat = "gva")

for (i in 1:nrow(ms_brady)) {
  if (ms_brady$vic[i] >= 3 & ms_brady$vic[i] < 5){
    ms_brady$group[i] = "3-4"
  } else if (ms_brady$vic[i] >= 5 & ms_brady$vic[i] < 7) {
    ms_brady$group[i] = "5-6"
  } else if (ms_brady$vic[i] >= 7 & ms_brady$vic[i] < 9) {
    ms_brady$group[i] = "7-8"
  } else if (ms_brady$vic[i] >= 9 & ms_brady$vic[i] < 11) { 
    ms_brady$group[i] = "9-10"
  } else if (ms_brady$vic[i] >= 11 & ms_brady$vic[i] < 16) {
    ms_brady$group[i] = "11-15"
  } else if (ms_brady$vic[i] >= 16 & ms_brady$vic[i] < 21) {
    ms_brady$group[i] = "16-20"
  } else if (ms_brady$vic[i] >= 21 & ms_brady$vic[i] < 51 ) {
    ms_brady$group[i] = "21-50"
  } else {
    ms_brady$group[i] = "50+"
  }
} 

for (i in 1:nrow(ms_stanford)) {
  if (ms_stanford$vic[i] >= 3 & ms_stanford$vic[i] < 5){
    ms_stanford$group[i] = "3-4"
  } else if (ms_stanford$vic[i] >= 5 & ms_stanford$vic[i] < 7) {
    ms_stanford$group[i] = "5-6"
  } else if (ms_stanford$vic[i] >= 7 & ms_stanford$vic[i] < 9) {
    ms_stanford$group[i] = "7-8"
  } else if (ms_stanford$vic[i] >= 9 & ms_stanford$vic[i] < 11) { 
    ms_stanford$group[i] = "9-10"
  } else if (ms_stanford$vic[i] >= 11 & ms_stanford$vic[i] < 16) {
    ms_stanford$group[i] = "11-15"
  } else if (ms_stanford$vic[i] >= 16 & ms_stanford$vic[i] < 21) {
    ms_stanford$group[i] = "16-20"
  } else if (ms_stanford$vic[i] >= 21 & ms_stanford$vic[i] < 51 ) {
    ms_stanford$group[i] = "21-50"
  } else {
    ms_stanford$group[i] = "50+"
  }
} 

for (i in 1:nrow(ms_mj)) {
  if (ms_mj$vic[i] >= 3 & ms_mj$vic[i] < 5){
    ms_mj$group[i] = "3-4"
  } else if (ms_mj$vic[i] >= 5 & ms_mj$vic[i] < 7) {
    ms_mj$group[i] = "5-6"
  } else if (ms_mj$vic[i] >= 7 & ms_mj$vic[i] < 9) {
    ms_mj$group[i] = "7-8"
  } else if (ms_mj$vic[i] >= 9 & ms_mj$vic[i] < 11) { 
    ms_mj$group[i] = "9-10"
  } else if (ms_mj$vic[i] >= 11 & ms_mj$vic[i] < 16) {
    ms_mj$group[i] = "11-15"
  } else if (ms_mj$vic[i] >= 16 & ms_mj$vic[i] < 21) {
    ms_mj$group[i] = "16-20"
  } else if (ms_mj$vic[i] >= 21 & ms_mj$vic[i] < 51 ) {
    ms_mj$group[i] = "21-50"
  } else {
    ms_mj$group[i] = "50+"
  }
} 

for (i in 1:nrow(ms_gva)) {
  if (ms_gva$vic[i] >= 3 & ms_gva$vic[i] < 5){
    ms_gva$group[i] = "3-4"
  } else if (ms_gva$vic[i] >= 5 & ms_gva$vic[i] < 7) {
    ms_gva$group[i] = "5-6"
  } else if (ms_gva$vic[i] >= 7 & ms_gva$vic[i] < 9) {
    ms_gva$group[i] = "7-8"
  } else if (ms_gva$vic[i] >= 9 & ms_gva$vic[i] < 11) { 
    ms_gva$group[i] = "9-10"
  } else if (ms_gva$vic[i] >= 11 & ms_gva$vic[i] < 16) {
    ms_gva$group[i] = "11-15"
  } else if (ms_gva$vic[i] >= 16 & ms_gva$vic[i] < 21) {
    ms_gva$group[i] = "16-20"
  } else if (ms_gva$vic[i] >= 21 & ms_gva$vic[i] < 51 ) {
    ms_gva$group[i] = "21-50"
  } else {
    ms_gva$group[i] = "50+"
  }
} 

ms_brady = ms_brady %>% 
  count(group) %>% 
  mutate(perc = n / nrow(ms_brady)) %>% 
  mutate(dat = "Brady")

ms_stanford = ms_stanford %>% 
  count(group) %>% 
  mutate(perc = n / nrow(ms_stanford)) %>% 
  mutate(dat = "Stanford")

ms_mj = ms_mj %>% 
  count(group) %>% 
  mutate(perc = n / nrow(ms_mj)) %>% 
  mutate(dat = "Mother Jones")

ms_gva = ms_gva %>% 
  count(group) %>% 
  mutate(perc = n / nrow(ms_gva)) %>% 
  mutate(dat = "GVA")

mag_df = rbind(ms_brady, ms_stanford, 
               ms_mj, ms_gva) 

positions = c("3-4", "5-6", "7-8", "9-10", "11-15",
              "16-20", "21-50", "50+")

p_brady = ggplot(ms_brady, aes(x = group, y = perc)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = positions) +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour=NA, fill=NA),
    strip.text=element_text(hjust=0),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank()) + 
  xlab(NULL) + 
  ylab(NULL) +
  ggplot2::ggtitle("Brady")

p_stanford = ggplot(ms_stanford, aes(x = group, y = perc)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = positions) +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(colour=NA, fill=NA),
    strip.text=element_text(hjust=0),
    axis.ticks.x=element_blank(),
    axis.text.x=element_blank()) + 
  xlab(NULL) + 
  ylab(NULL) +
  ggplot2::ggtitle("Stanford")

p_mj = ggplot(ms_mj, aes(x = group, y = perc)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = positions) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(colour=NA, fill=NA),
        strip.text=element_text(hjust=0)) + 
  xlab(NULL) + 
  ylab(NULL) + 
  ggplot2::ggtitle("Mother Jones")

p_gva = ggplot(ms_gva, aes(x = group, y = perc)) + 
  geom_bar(stat = "identity") + 
  scale_x_discrete(limits = positions) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_rect(colour=NA, fill=NA),
        strip.text=element_text(hjust=0)) + 
  xlab(NULL) + 
  ylab(NULL) + 
  ggplot2::ggtitle("GVA")

pp2 = grid.arrange(p_brady, p_stanford, p_mj, p_gva,
                   nrow = 2, ncol = 2, 
                   left = "Proportion of Data Set",
                   bottom = "Number of Victims")

ggsave("Figure2.pdf", 
       pp2, width = 7, height = 5)


# Figure Three ------------------------------------------------------------

# histogram estimators of temporal and magnitude triggering functions
trig_brady = trig_plots(model = out_brady,
                        g_xlim = c(0, 375), 
                        k_xlim = c(3, 18),
                        g_ylim = c(0, 0.02),
                        k_ylim = c(0, 1.6),
                        mag_label = "number of victims",
                        plot_title = "Brady")

trig_stanford = trig_plots(model = out_stanford, 
                           g_xlim = c(0, 375), 
                           k_xlim = c(3, 18),
                           g_ylim = c(0, 0.05),
                           k_ylim = c(0, 1.7),
                           mag_label = "number of victims",
                           plot_title = "Stanford") 

trig_mj = trig_plots(model = out_mj,
                     g_xlim = c(0, 375), 
                     k_xlim = c(3, 18),
                     g_ylim = c(0, 0.065),
                     k_ylim = c(0, 1.6),
                     mag_label = "number of victims",
                     plot_title = "Mother Jones")

trig_gva = trig_plots(model = out_gva,
                      g_xlim = c(0, 375), 
                      k_xlim = c(3, 18),
                      g_ylim = c(0, 0.065),
                      k_ylim = c(0, 1.6),
                      mag_label = "number of victims",
                      plot_title = "GVA")



all_trig = gridExtra::grid.arrange(trig_brady, trig_stanford,
                                   trig_mj, trig_gva,
                                   nrow = 4, ncol = 1)
ggplot2::ggsave("Figure3.pdf",
                all_trig, width = 7, height = 8)



# Figure Four -------------------------------------------------------------

# conditional intensity plotted against monthly number of observed events

ci_brady = ci_plot(model = out_brady, 
                   superthin = st_brady,
                   plot_title = "Brady") + 
  ggplot2::ylab("") + 
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"),
                            to = as.Date("2012-01-01"),
                            by = "1 year"),
               labels = seq(2005, 2012, 1),
               minor_breaks = "1 year") + 
  ggplot2::xlab("") +
  ggplot2::theme(axis.text = element_text(size = 20))

ci_stanford = ci_plot(model = out_stanford, 
                      superthin = st_stanford,
                      plot_title = "Stanford") + 
  scale_x_date(breaks = seq(from = as.Date("1999-01-01"),
                            to = as.Date("2016-01-01"),
                            by = "2 years"),
               labels = seq(1999, 2016, 2),
               minor_breaks = "1 year") 
  # ggplot2::labs(caption = "Vertical lines representing observed counts, 
  #               black line representing estimated conditional intensity.") + 
  # ggplot2::theme(plot.caption = element_text(size = 14),
  #                plot.title = element_text(size = 18))

ci_mj = ci_plot(model = out_mj, 
                superthin = st_mj,
                plot_title = "Mother Jones") + ggplot2::ylab("") +  
  ggplot2::xlab("") + 
  scale_x_date(breaks = seq(from = as.Date("1999-01-01"),
                            to = as.Date("2020-01-01"),
                            by = "2 years"),
               labels = seq(1999, 2020, 2),
               minor_breaks = "1 year") + 
  scale_y_continuous(minor_breaks = NULL)

ci_gva = ci_plot(model = out_gva, 
                 superthin = st_gva,
                 plot_title = "GVA") + ggplot2::ylab("") + 
  scale_x_date(breaks = seq(from = as.Date("2013-01-01"),
                            to = as.Date("2020-08-31"),
                            by = "1 years"),
               labels = seq(2013, 2020, 1),
               minor_breaks = NULL) 

all_ci = gridExtra::grid.arrange(ci_brady, ci_stanford,
                                 ci_mj, ci_gva,
                                 nrow = 4, ncol = 1, 
                                 left = "Number of Monthly Events")

ggplot2::ggsave("Figure4.pdf",
                all_ci, width = 7, height = 7)


# Figure Five -------------------------------------------------------------

# histogram of superthinned process

hist_brady = ci_hist(superthin = st_brady, 
                     plot_title = "Brady")

hist_stanford = ci_hist(superthin = st_stanford, plot_title = "Stanford")

hist_mj = ci_hist(superthin = st_mj,
                  plot_title = "Mother Jones")

hist_gva = ci_hist(superthin = st_gva, 
                   plot_title = "GVA")

hist_brady = hist_brady + xlab(NULL) + ylab(NULL) + 
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"),
                            to = as.Date("2013-01-01"),
                            by = "2 year"),
               labels = seq(2005, 2013, 2),
               minor_breaks = "1 year") 

hist_stanford = hist_stanford + xlab(NULL) + ylab(NULL) +
  scale_x_date(breaks = seq(from = as.Date("1999-01-01"),
                            to = as.Date("2016-01-01"),
                            by = "3 years"),
               labels = seq(1999, 2016, 3),
               minor_breaks = "1 year") 

hist_mj = hist_mj + xlab(NULL) + ylab(NULL) +
  scale_x_date(breaks = seq(from = as.Date("1999-01-01"),
                            to = as.Date("2020-01-01"),
                            by = "3 years"),
               labels = seq(1999, 2020, 3),
               minor_breaks = "1 year") 

hist_gva = hist_gva + xlab(NULL) + ylab(NULL) +
  scale_x_date(breaks = seq(from = as.Date("2013-01-01"),
                            to = as.Date("2020-08-31"),
                            by = "2 years"),
               labels = seq(2013, 2020, 2),
               minor_breaks = "1 year") 

pp3 = grid.arrange(hist_brady, hist_stanford,
                   hist_mj, hist_gva,
                   left = "Frequency",
                   bottom = "Date")

ggplot2::ggsave("Figure5.pdf",
                pp3, width = 7, height = 7)




# Figure Six --------------------------------------------------------------

# superthinning classification plot

st_plot_brady = st_plot(superthin = st_brady) + 
  scale_x_date(breaks = seq(from = as.Date("2005-01-01"),
                            to = as.Date("2013-01-01"),
                            by = "1 year"),
               labels = seq(2005, 2013, 1),
               minor_breaks = NULL)

ggsave("Figure6.pdf",
       st_plot_brady, width = 7, height = 5)