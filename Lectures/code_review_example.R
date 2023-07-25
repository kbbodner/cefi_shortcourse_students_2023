library(palmerpenguins)
library(dplyr)
library(ggplot2)

(penguins
	%>% filter(island == "Torgersen")
	%>% group_by(year, species) 
	%>% summarise(count = n()) 
	%>% ggplot(aes(x = year, y = count, color = species)) 
	+ geom_line()
	+ geom_point()
	+ labs(title = "Species Count on Torgersen")
)

(penguins
	%>% filter(island == "Biscoe")
	%>% group_by(year, species) 
	%>% summarise(count = n()) 
	%>% ggplot(aes(x = year, y = count, color = species)) 
	+ geom_line()
	+ geom_point()
	+ labs(title = "Species Count on Biscoe")
)

(penguins
	%>% filter(island == "Dream")
	%>% group_by(year, species) 
	%>% summarise(count = n()) 
	%>% ggplot(aes(x = year, y = count, color = species)) 
	+ geom_line()
	+ geom_point()
	+ labs(title = "Species Count on Dream")
)