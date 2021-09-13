devtools::install_github("idiv-biodiversity/LCVP@LCVP2")
devtools::install_github("idiv-biodiversity/lcvplants@Lcvp_2")

library(lcvplants)

# single names
lcvp_search("Micranthocereus auriazureus")

# Can also include infra specific names and authorities
lcvp_search("Fulcaldea stuessyi Roque & V.A.Funk")

# Also works on vectors of names
search_result <- lcvp_search(c("Fulcaldea stuessyi", "Ormosia timboensis", 
                               "Pavonia palmeirensis", "Helicteres rufipila",
                               "Senegalia ricoae","Passiflora timboensis", 
                               "Acritopappus harleyi", "Rayleya bahiensis",
                               "Xyris fibrosa","Acritopappus pintoi", 
                               "Micranthocereus streckeri","Microlicia subalata", 
                                "Vellozia canelinha"))

# You can see the summary results
lcvp_summary(search_result)

# Misspeled name
lcvp_search("Fulcaldea stuessyi", max.distance = 2)

# Misspelings in a vector of names
res_ex <- lcvp_search(c("Fulcaldea stuessyi", "Ormosia timboensis"), 
                      max.distance = 1)
lcvp_summary(res_ex)


# You may also want to check all the possible matches 
lcvp_fuzzy_search("Fulcaldea stuessyii", max.distance = 2)

# Search for the genus AA
lcvp_group_search("Fulcaldea", search_by = "Genus")

# It also accepts a vector of names
lcvp_group_search(c("Fulcaldea", "Rayleya"), search_by = "Genus")


#The package also facilitates the comparison between two lists of species vascular plant names:
  
# Species list 1
splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[1:10])

# Species list 2
splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[1:10])

# Compare the two lists
lcvp_match(splist1, splist2)


#Lastly, you can use lcvplants two join two tables based on vascular plant names:
  
# data.frame1 
splist1 <- sample(LCVP::tab_lcvp$Input.Taxon[2:10])
x <- data.frame("Species" = splist1, "Trait1" = runif(length(splist1)))

# data.frame2
splist2 <- sample(LCVP::tab_lcvp$Input.Taxon[1:8])
y <- data.frame("Species" = splist2,  "Trait2" = runif(length(splist2)), "Trait3" = runif(length(splist2)))

# Full join the two tables
lcvp_join(x, y, c("Species", "Species"), type = "full")

citation("lcvplants")
