library('RSocrata')
library('forcats')
library('tidyverse')

# https://data.baltimorecity.gov/analytics

inventory <- read.socrata('https://data.baltimorecity.gov/dataset/Public-Asset-Inventory/tqff-376f/')

# don't include derived views
inventory <- inventory %>% filter(provenance == 'official' &
                                  Derived.View == 'false')

table(inventory$Type)

inventory <- inventory %>% filter(Type == 'dataset')
inventory <- inventory %>% mutate(Department2 = ifelse(Department == '', data_provided_by, Department))
inventory %>% filter(Department2 == '') %>% select(Name) # appears to be internal stuff
inventory <- inventory %>% filter(Department2 != '')

table(inventory$Department2)
inventory %>% filter(Department2 == 'Non-Governmental') # all BNIA-JFI
inventory <- inventory %>% mutate(Department3 = case_when(Department2 == 'Non-Governmental'|
                                                          Department2 == 'The Baltimore Neighborhood Indicators Alliance-Jacob France Institute at the University of Baltimore (known as BNIA-JFI)' ~ 'BNIA-JFI (Non-Governmental)',
                                                          TRUE ~ Department2))

inventory_by.dept <- inventory %>% group_by(Department3) %>% summarise(n = n())
number <- sum(inventory_by.dept$n)

inventory_by.dept$Department3.f <- fct_reorder(inventory_by.dept$Department3,
                                               inventory_by.dept$n)

inventory_by.dept %>% arrange(desc(n))
ggplot(data = inventory_by.dept, 
       aes(x = Department3.f, y = n)) +
  geom_bar(stat = 'identity', fill = '#333333') +
  geom_text(aes(label = n), size = 3, hjust = -.5) +
  scale_y_continuous(limits = c(0, 32), expand = c(0,0)) +
  labs(x = '', y = '', title = paste(number, 'official datasets uploaded to OpenBaltimore\nby department (excl. maps and derived datasets)'), 
       caption = 'Source: Open Baltimore Asset Inventory as of Oct. 18, 2018.\nBNIA-JFI is the Baltimore Neighborhood Indicators Alliance-Jacob France Institute\nat the University of Baltimore.') +
  coord_flip() +
  theme_minimal() +
  theme(axis.ticks = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(), 
        plot.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        plot.caption = element_text(hjust = 0, color = '#999999', size = 8))
        #axis.text.y = element_text(hjust=0, angle=0))

ggsave('plot.png', width = 9, height = 5)







