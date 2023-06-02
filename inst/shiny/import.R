

columnMeta <- read.csv("metadataColumns.csv")
orderedFactMeta <- read.csv("metadataOrderedFactors.csv")

nonTraits <- columnMeta[columnMeta$trait == "No", "colname"]



source("./3.Data for DUS testing/modomaDists.R")




getClosestPair("3.Data for DUS testing/Floricode_Gerbera_DUS_data.csv",
               idCol = "VKCNr")
getClosestPair("3.Data for DUS testing/Floricode_ROSA_DUS_data.csv",
               idCol = "VKCNr")

getClosestPair("3.Data for DUS testing/Naktuinbouw_Gerbera_DUS_data.csv",
               idCol = "RVPnr")
getClosestPair("3.Data for DUS testing/Naktuinbouw_ROSA_DUS_data.csv",
               idCol = "RVPnr")


indat <- readFile("../shiny/testData/Floricode_Gerbera_DUS_data.csv")

res <- getClosestRef(114666, indat, c("Bloeiwijze", "BloemRHS"), 10)
View(res)

res2 <- getClosestTraits(indat,
                         list(Bloeiwijze = "Afgeplat bolvormig", Bloemkleur = "roze"))


res2 <- getClosestRef("ROO4603",
                  "3.Data for DUS testing/Naktuinbouw_ROSA_DUS_data.csv",
                  NULL,
                  10)
View(res2)


