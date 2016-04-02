library("ProjectTemplate")
load.project()

# Example 1 -Comparison of methods --------------------------------
aov_data <- read_excel("anova_data-a.xlsx", sheet = 1)


boxplot(aov_data)

aov_data_st <- gather(data=aov_data,key=Result,value=Value,na.rm=FALSE,Spectro,Enzyme,AOAC)

aov_1 <- aov(Value~Result,aov_data_st)
summary(aov_1)

TukeyHSD(aov_1)

# Example 2 ---- impact of sampling ------------------------------------

aov_data_2 <- read_excel("anova_data-b.xlsx", sheet = 1)

boxplot(aov_data_2)

aov_data_st2 <- gather(data=aov_data_2,key=Position,value=Value,na.rm=FALSE,Top,Middle,Bottom)

aov_2 <- aov(Value~Position, aov_data_st2)
summary(aov_2)
TukeyHSD(aov_2)

n <- nrow(aov_data_2)

mean.sqr <- summary(aov_2)[1][[1]][[3]]
method_sd <- sqrt(mean.sqr[2])
sampling_sd <- sqrt((mean.sqr[1]-mean.sqr[2])/n)

analysis_sd <- sqrt(method_sd^2 + sampling_sd^2)

pct_method_variance <- round((method_sd^2)*100/(method_sd^2 + sampling_sd^2),1)

# Example 3 -- Two analysts, two techniques (A) ---------------------------

aov_data_3 <- read_excel("anova_data-d.xlsx", sheet = 1)

aov_data_st3 <- gather(data=aov_data_3,key=Analyst,value=Result,na.rm=FALSE,Quinn,Martin)

aov_data_3_summary <- aov_data_st3 %>%
        group_by(Analyst, Direction) %>%
        summarise(Mean = mean(Result), SD = sd(Result))
aov_data_3_summary

aov_3_plot <- ggplot(aov_data_3_summary, aes(x = Direction, y = Mean, group = Analyst, colour = Analyst)) +
        geom_point(size = 5) +
        geom_line() +
        geom_errorbar(aes(ymin=Mean-SD, ymax = Mean+SD), width=0.1)

aov_3_plot

aov_3 <- aov(Result~Direction*Analyst, aov_data_st3)
summary(aov_3)

# Example 4 -- Two analysts, two techniques (B) ---------------------------

aov_data_4 <- read_excel("anova_data-e.xlsx", sheet = 1)

aov_data_st4 <- gather(data=aov_data_4,key=Analyst,value=Result,na.rm=FALSE,Quinn,Martin)

aov_data_4_summary <- aov_data_st4 %>%
        group_by(Analyst, Direction) %>%
        summarise(Mean = mean(Result), SD = sd(Result))
aov_data_4_summary

aov_4_plot <- ggplot(aov_data_4_summary, aes(x = Direction, y = Mean, group = Analyst, colour = Analyst)) +
        geom_point(size = 5) +
        geom_line()+
        geom_errorbar(aes(ymin=Mean-SD, ymax = Mean+SD), width=.1)

aov_4_plot

aov_4 <- aov(Result~Direction*Analyst, aov_data_st4)
summary(aov_4)

