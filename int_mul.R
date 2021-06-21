dataset <- read.csv("https://raw.githubusercontent.com/ruthenian8/int_mul/master/uq_infs_genders.csv")
library(ggthemes)
install.packages("rstatix")
library(rstatix)
install.packages("coin")
install.packages("tinytex")
tinytex::install_tinytex()
library("ggpubr")
View(dataset)
dataset %>% 
  ggplot(aes(mean_sl)) +
  geom_histogram(binwidth = 4)

dataset <- dataset %>%
  filter(inf_gender1 == "f" | inf_gender1 == "m")

dataset %>% 
  ggplot(aes(x = inf_gender1, y = mean_ul, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "Before removal")

dataset %>% 
  ggplot(aes(x = inf_gender1, y = mean_sl, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "Before removal")

dataset <- dataset %>% 
  filter(mean_sl <= 40 & mean_ul < 110)

dataset %>% 
  ggplot(aes(x = inf_gender1, y = mean_ul, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "After removal")

dataset %>% 
  ggplot(aes(x = inf_gender1, y = mean_sl, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "After removal")
####################################################################

dataset %>% 
  wilcox_test(mean_sl ~ inf_gender1) %>% 
  add_significance()

dataset %>% 
  wilcox_effsize(mean_sl ~ inf_gender1)

dataset %>% 
  wilcox_test(mean_ul ~ inf_gender1) %>% 
  add_significance()

dataset %>% 
  wilcox_effsize(mean_ul ~ inf_gender1)
#####################################################################
dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "informal") %>% 
  wilcox_test(mean_ul ~ inf_gender1) %>% 
  add_significance()

dataset <- dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal"))



dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "informal") %>% 
  wilcox_test(mean_sl ~ inf_gender1) %>% 
  add_significance()

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "formal") %>% 
  wilcox_test(mean_ul ~ inf_gender1) %>% 
  add_significance()

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "formal") %>% 
  wilcox_test(mean_sl ~ inf_gender1) %>% 
  add_significance()

#####################################################################
dataset %>% 
  ggplot(aes(x = inf_gender1, y = mean_sl, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "Gender groups overall", y = "Mean sent. len.") + 
  theme_economist(base_size = 55) + 
  scale_color_economist()



dataset %>% 
  ggplot(aes(mean_ul)) + 
  geom_histogram(binwidth = 3)+
  labs(x="Number of tokens",
     y="Count",
     title="Mean utterance length")+
  geom_vline(aes(xintercept = median(mean_ul)),col='red',size=2) +
  theme_minimal()

dataset %>% 
  ggplot(aes(mean_sl)) + 
  geom_histogram(binwidth = 3)+
  labs(x="Number of tokens",
       y="Count",
       title="Mean sentence length")+
  geom_vline(aes(xintercept = median(mean_sl)),col='red',size=2) +
  theme_minimal()

dataset %>% 
  filter(inf_gender1 == "f") %>% 
  ggplot(aes(mean_sl)) + 
  geom_histogram(binwidth = 3)

dataset %>%
  filter(inf_gender1 == "m") %>% 
  ggplot(aes(mean_sl)) + 
  geom_histogram(binwidth = 3)

allfs <- dataset %>% 
  filter(inf_gender1 == "f")

allms <- dataset %>%
  filter(inf_gender1 == "m")

wilcox.test(allfs$mean_sl, allms$mean_sl)
?wilcox.test
t.test(allfs$mean_sl, allms$mean_sl)

dataset %>% 
  ggplot(aes(mean_sl, mean_ul, colour=inf_gender1)) +
  geom_point() +
  xlab("Mean sent. len.") + 
  ylab("Mean ut. len.") +
  theme_minimal()

dataset %>%
  filter(inf_gender1 == "f" & sob_gender1 != sob_gender2 & sob_gender2 != "") %>% 
  ggplot(aes(mean_ul)) + 
  geom_histogram(binwidth = 3)

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  ggplot(aes(x = sit.type, y = mean_ul, fill = sit.type)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  ggplot(aes(x = sit.type, y = mean_sl, fill = sit.type)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")


formal <- dataset %>% 
  filter(sob_gender3 == "" & sob_gender4 == "") %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "formal") %>% 
  mutate(isMixed = ifelse(sob_gender1 != sob_gender2, "Mixed", ifelse(sob_gender1 == "f", "allFemale", "allMale")))

ggline(formal, x = "isMixed", y = "mean_sl", color = "inf_gender1",
       add = c("mean_se"),
       palette = c("#00AFBB", "#E7B800"))

dataset %>% 
  filter(sob_gender2 != "") %>% 
  mutate(is.allF = ifelse(if_all(starts_with("sob"), ~ . %in% c("f","")), "Fonly", "Other")) %>%
  mutate(is.allM = ifelse(if_all(starts_with("sob"), ~ . %in% c("m","")), "Monly", "Other")) %>% 
  mutate(group.gender = ifelse(is.allF == "Fonly", "Fonly", ifelse(is.allM == "Monly", "Monly", "Mixed"))) %>% 
  select(inf_gender1, group.gender, mean_ul, mean_sl) %>% 
  filter(inf_gender1 == "f") %>% 
  ggplot(aes(x = group.gender, y = mean_ul, fill = group.gender)) +
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(notch = FALSE) + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 6, color = "white")

dataset %>% 
  filter(sob_gender2 != "") %>% 
  mutate(is.allF = ifelse(if_all(starts_with("sob"), ~ . %in% c("f","")), "Fonly", "Other")) %>%
  mutate(is.allM = ifelse(if_all(starts_with("sob"), ~ . %in% c("m","")), "Monly", "Other")) %>% 
  mutate(group.gender = ifelse(is.allF == "Fonly", "Fonly", ifelse(is.allM == "Monly", "Monly", "Mixed"))) %>% 
  select(inf_gender1, group.gender, mean_ul, mean_sl) %>% 
  filter(inf_gender1 == "m") %>% 
  ggplot(aes(x = group.gender, y = mean_ul, fill = group.gender)) +
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(notch = FALSE) + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 6, color = "white")

dataset %>% 
  filter(sob_gender2 != "") %>% 
  mutate(is.allF = ifelse(if_all(starts_with("sob"), ~ . %in% c("f","")), "Fonly", "Other")) %>%
  mutate(is.allM = ifelse(if_all(starts_with("sob"), ~ . %in% c("m","")), "Monly", "Other")) %>% 
  mutate(group.gender = ifelse(is.allF == "Fonly", "Fonly", ifelse(is.allM == "Monly", "Monly", "Mixed"))) %>% 
  select(inf_gender1, group.gender, mean_ul, mean_sl) %>% 
  mutate(inf2group = paste(inf_gender1, group.gender, sep = "2")) %>% 
  ggplot(aes(x = inf2group, y = mean_sl, fill = inf2group)) +
  stat_boxplot(geom = "errorbar", width = 0.5) + 
  geom_boxplot(notch = FALSE) + 
  stat_summary(fun = mean, geom = "point", shape = 20, size = 6, color = "white")

grouped <- dataset %>% 
  filter(sob_gender2 != "") %>% 
  mutate(is.allF = ifelse(if_all(starts_with("sob"), ~ . %in% c("f","")), "Fonly", "Other")) %>%
  mutate(is.allM = ifelse(if_all(starts_with("sob"), ~ . %in% c("m","")), "Monly", "Other")) %>% 
  mutate(group.gender = ifelse(is.allF == "Fonly", "Fonly", ifelse(is.allM == "Monly", "Monly", "Mixed"))) %>% 
  select(inf_gender1, group.gender, mean_ul, mean_sl) %>% 
  mutate(inf2group = paste(inf_gender1, group.gender, sep = "2"))

summary(aov_model <- aov(mean_ul ~ inf2group, grouped))
TukeyHSD(aov_model)
pairwise.t.test(grouped$mean_ul, grouped$inf2group, p.adjust.method = "BH")

?TukeyHSD
dataset %>% 
  filter(sob_gender1 == "m" & sob_gender2 == "m" & sob_gender3 == "") %>% 
  summarise(n = n())

formal %>% 
  filter(inf_gender1 == "m") %>% 
  ggplot(aes(x = isMixed, y = mean_ul, fill = isMixed)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")  

formal %>% 
  filter(isMixed == "Mixed") %>% 
  ggplot(aes(x = inf_gender1, y = mean_ul, fill = inf_gender1)) +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white") 

formal %>% 
  ggplot(aes(x = inf_gender1, y = mean_ul, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "informal") %>% 
  ggplot(aes(x = inf_gender1, y = mean_ul, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "Informal group")

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "formal") %>% 
  ggplot(aes(x = inf_gender1, y = mean_sl, fill = inf_gender1)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "formal group") + 
  theme_economist(base_size = 55) + 
  scale_color_economist()

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(inf_gender1 == "f") %>% 
  ggplot(aes(x = sit.type, y = mean_sl, fill = sit.type)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = TRUE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "Female group", x = "Situation type", y = "Mean sent. len.") + 
  theme_economist(base_size = 55) + 
  scale_color_economist()
###########################
dataset %>% 
  mutate(int.num = as.factor(ifelse(sob_gender4 != "", 4,
                          ifelse(sob_gender3 != "", 3,
                                 ifelse(sob_gender2 != "", 2, 1))))) %>% 
  ggplot(aes(x = int.num, y = mean_sl, fill = int.num)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = FALSE) + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")+
  labs(title = "All groups", x = "N interviewers", y = "Mean sent. len.") + 
  theme_economist(base_size = 55) +
  theme(axis.text.x = element_text(vjust = 5),
        axis.text.y = element_text(hjust = 2),
        axis.title.x.bottom = element_text(margin = margin(0, 10, 0, 0)),
        axis.title.y.left = element_text(margin = margin(3, 0, 0, 0))) + 
  scale_color_economist()
##############################
dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(inf_gender1 == "m") %>% 
  wilcox_test(mean_sl ~ sit.type)
  #ggplot(aes(x = sit.type, y = mean_sl, fill = sit.type)) + 

install.packages("ggthemes")


dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>% 
  filter(sit.type == "formal") %>%
  wilcox_test(mean_ul ~ inf_gender1)


####
###ONEINF
####

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>%
  filter(sit.type == "informal") %>%
  mutate(part.genders = ifelse(inf_gender1 == "f",
                               ifelse(sob_gender1 == "f", "F2F", "F2M"),
                               ifelse(sob_gender1 == "f", "M2F", "M2M"))) %>% 
  ggplot(aes(x = part.genders, y = mean_ul, fill = part.genders)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = FALSE) + 
  labs(title = "Mean utterance length", x = "Participants' genders") +
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>%
  filter(sit.type == "informal") %>%
  mutate(part.genders = ifelse(inf_gender1 == "f",
                               ifelse(sob_gender1 == "f", "F2F", "F2M"),
                               ifelse(sob_gender1 == "f", "M2F", "M2M"))) %>% 
  ggplot(aes(x = part.genders, y = mean_sl, fill = part.genders)) + 
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(notch = FALSE) + 
  labs(title = "Mean sentence length", x = "Participants' genders") + 
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="white")

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>%
  filter(sit.type == "informal") %>%
  mutate(part.genders = ifelse(inf_gender1 == "f",
                               ifelse(sob_gender1 == "f", "F2F", "F2M"),
                               ifelse(sob_gender1 == "f", "M2F", "M2M"))) %>% 
  group_by(part.genders) %>% 
  summarise(mean(mean_ul))

dataset %>% 
  mutate(sit.type = ifelse(sob_gender2 == "", "informal", "formal")) %>%
  filter(sit.type == "informal") %>%
  mutate(part.genders = ifelse(inf_gender1 == "f",
                               ifelse(sob_gender1 == "f", "F2F", "F2M"),
                               ifelse(sob_gender1 == "f", "M2F", "M2M"))) %>% 
  group_by(part.genders) %>% 
  summarise(mean_MSL = mean(mean_sl), n = n())

all(c(1, 2, 3, 4) > 0)
