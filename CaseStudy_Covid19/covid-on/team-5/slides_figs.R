ggplot(data=fitting, aes(date, new_cases)) + geom_point(colour="#24a69a") +
  xlab("Date") +
  ylab("COVID-19 Incidence in Ontario")