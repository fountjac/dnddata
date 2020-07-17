devtools::load_all()
library(purrr)
library(ggplot2)
library(plotly)
library(stringr)
library(dplyr)
library(magrittr)
legitClasses = c("Warlock", "Monk", "Wizard", "Barbarian", "Sorcerer", "Paladin", "Fighter", "Druid", "Ranger", "Rogue","Cleric","Bard",'Artificer')


barPalette = c('#7DD4A6','#C15BC5','#D65242','#415455',
			   '#D2A75C','#8FD25B','#D15B86','#A5B5BE','#727EC6',
			   '#567441','#754334','#5E3A60','#77B0D0',"#CCEBC5",
			   "#D9D9D9","#FCCDE5")

dnd_chars_singleclass


classes = dnd_chars_singleclass$justClass %>% str_split('\\|') %>% unlist
archetypes = dnd_chars_singleclass$subclass %>% str_split('\\|') %>% unlist


archeFrame = data.frame(classes,archetypes) %>% filter(archetypes !='')
classSum = archeFrame$classes %>% table %>% sort(decreasing = TRUE)

archeFrame %<>% filter(classes %in% legitClasses) %>%
	group_by(classes,archetypes) %>% summarize(count = n()) %>%
	arrange(classes,(count)) %>% filter(classes %in% names(which(classSum>2))) %>%
	ungroup() %>%
	mutate(archetypes = factor(archetypes,levels = archetypes)) %>%
	group_by(classes) %>%
	mutate(ratio = count/sum(count)*100) %>%
	mutate(classArcheID = as.integer(archetypes) - max(as.integer(archetypes)) +1) %>% ungroup() %>% mutate(classArcheID = as.factor(classArcheID)) %>%
	mutate(`%` = round(ratio))

archeFrame %>%
	ggplot(aes(x = classes,y = ratio,fill = classArcheID,
			   label = archetypes,hede = count,hodo = `%`)) +
	geom_bar(stat='identity') + cowplot::theme_cowplot() +
	theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1 ),
		  legend.position = 'none') +
	scale_fill_manual(values = barPalette) +
	ggtitle('Archetype choices') + xlab('') + ylab('archetype % within class')->p

ply = ggplotly(p,tooltip = c('label','hodo','hede')) %>% layout(xaxis=list(fixedrange=TRUE)) %>%
	config(displayModeBar = F) %>%
	layout(yaxis=list(fixedrange=TRUE))
ply

