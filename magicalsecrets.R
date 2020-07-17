library(dplyr)
library(purrr)
library(wizaRd)
devtools::load_all()

magic_initiate = dnddata::dnd_chars_singleclass_list %>% purrr::map_lgl(function(x){any(x$feats %in% 'Magic Initiate')})

bard = dnddata::dnd_chars_singleclass_list %>% purrr::map_lgl(function(x){x$class[[1]]$class == 'Bard'})


relevant =  dnddata::dnd_chars_singleclass_list[bard & !magic_initiate]




bardSpells = spells %>% map_lgl(function(x){'bard' %in% x$tags})

bardSpells = spells[bardSpells]
bardSpellNames = bardSpells %>% purrr::map_chr('name')


relevant %>% purrr::map(function(x){
	x$spells %>% purrr::map_lgl(function(spell){
		spell$processedSpell %in% bardSpellNames | spell$processedSpell == ''
	}) -> isBard

	spellNames = x$spells %>% purrr::map_chr('processedSpell')
	spellNames[!isBard]
}) -> possibleMagicalSecrets

possibleMagicalSecrets = possibleMagicalSecrets[possibleMagicalSecrets %>% sapply(length) %>% {.!=0}]
