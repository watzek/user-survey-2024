
columnName<-"frequencyIndividualStudy"


subset<-userData %>%
  select(c(onCampus, !!sym(columnName)))

subset<-na.omit(subset)

#sub<-subset %>%
#  group_by(onCampus, frequencyIndividualStudy) %>%
#  summarize(count=n()) %>%
#  arrange(match(frequencyIndividualStudy, c("Daily", "Weekly", "Monthly", "Once a semester", "Never")))

#sub$frequencyIndividualStudy <- factor(sub$frequencyIndividualStudy, levels = sub$frequencyIndividualStudy)

percentages <- subset %>%
  group_by(onCampus, !!sym(columnName)) %>%
  summarize(count = n()) %>%
  mutate(percent = count / sum(count) * 100) %>%
  arrange(match(!!sym(columnName), c("Daily", "Weekly", "Monthly", "Once a semester", "Never")))

order <- c("Daily", "Weekly", "Monthly", "Once a semester", "Never")

# Convert frequencyIndividualStudy to a factor with specified order
percentages[[columnName]] <- factor(percentages[[columnName]], levels = order)

percentages <- percentages %>%
  mutate(onCampus = case_when(
    onCampus == "No" ~ "Off Campus",
    onCampus == "Yes" ~ "On Campus",
    TRUE ~ onCampus
  ))


#percentages$frequencyIndividualStudy <-factor(percentages$frequencyIndividualStudy, levels=percentages$frequencyIndividualStudy)
ggplot(percentages, mapping=aes(x=frequencyIndividualStudy, y=percent, fill=onCampus)) +
  geom_bar(stat="identity",width=.5, position="dodge")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 290, vjust = 0.5, hjust=0))

