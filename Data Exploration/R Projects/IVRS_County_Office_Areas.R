



IVRS_County_Office_Area <- data.frame('County' = c('Adair', 'Adams','Allamakee','Appanoose','Audubon',
                                                   'Benton','Black Hawk','Boone','Bremer','Buchanan',
                                                   'Buena Vista','Butler','Calhoun','Carroll','Cass',
                                                   'Cedar','Cerro Gordo','Cherokee','Chickasaw','Clarke',
                                                   'Clay','Clayton','Clinton','Crawford','Dallas',
                                                   'Davis','Decatur','Delaware','Des Moines','Dickinson',
                                                   'Dubuque','Emmet','Fayette','Floyd','Franklin',
                                                   'Fremont','Greene','Grundy','Guthrie','Hamilton',
                                                   'Hancock','Hardin','Harrison','Henry','Howard',
                                                   'Humboldt','Ida','Iowa','Jackson','Jasper',
                                                   'Jefferson','Johnson','Jones','Keokuk','Kossuth',
                                                   'Lee','Linn','Louisa','Lucas','Lyon',
                                                   'Madison','Mahaska','Marion','Marshall','Mills',
                                                   'Mitchell','Monona','Monroe','Montgomery','Muscatine',
                                                   'O Brien','Osceola','Page','Palo Alto','Plymouth',
                                                   'Pocahontas','Polk','Pottawattamie','Poweshiek','Ringgold',
                                                   'Sac','Scott','Shelby','Sioux','Story',
                                                   'Tama','Taylor','Union','Van Buren','Wapello',
                                                   'Warren','Washington','Wayne','Webster','Winnebago',
                                                   'Winneshiek','Woodbury','Worth','Wright'),
                                      'Office_Area' = c('Council Bluffs','Council Bluffs','Mason City','Ottumwa','Ames',
                                                               'Cedar Rapids','Waterloo','Ames','Waterloo','Waterloo',
                                                               'Fort Dodge','Waterloo','Fort Dodge','Ames','Council Bluffs',
                                                               'Iowa City','Mason City','Fort Dodge','Waterloo','Council Bluffs',
                                                               'Fort Dodge','Dubuque','Davenport','Ames','Des Moines',
                                                               'Ottumwa','Council Bluffs','Dubuque','Burlington','Fort Dodge',
                                                               'Dubuque','Fort Dodge','Waterloo','Mason City','Mason City',
                                                               'Council Bluffs','Ames','Waterloo','Ames','Fort Dodge',
                                                               'Mason City','Ames','Council Bluffs','Burlington','Mason City',
                                                               'Fort Dodge','Sioux City','Iowa City','Davenport','Ames',
                                                               'Ottumwa','Iowa City','Cedar Rapids','Ottumwa','Fort Dodge',
                                                               'Burlington','Cedar Rapids','Burlington','Ottumwa','Sioux City',
                                                               'Council Bluffs','Ottumwa','Ottumwa','Ames','Council Bluffs',
                                                               'Mason City','Sioux City','Ottumwa','Council Bluffs','Davenport',
                                                               'Sioux City','Sioux City','Council Bluffs','Fort Dodge','Sioux City',
                                                               'Fort Dodge','Des Moines','Council Bluffs','Ames','Council Bluffs',
                                                               'Fort Dodge','Davenport','Council Bluffs','Sioux City','Ames',
                                                               'Ames','Council Bluffs','Council Bluffs','Ottumwa','Ottumwa',
                                                               'Ottumwa','Iowa City','Ottumwa','Fort Dodge','Mason City',
                                                               'Mason City','Sioux City','Mason City','Fort Dodge'))
# write to file
write.csv(IVRS_data_transformed, "C:/Users/joelm/Documents/GitHub/DHR-Disabilities/Data Exploration/Datasets/IVRS_County_Office_Areas.csv", row.names = FALSE)



