\dontrun{
# The data was generated through downloading via the API
library(pxweb)
SCB <- get_pxweb_data(
  url = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101B/BefolkningMedelAlder",
  dims = list(Region = c('01', '03', '12', '14'),
              Kon = c('1', '2'),
              ContentsCode = c('BE0101G9'),
              Tid = c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007',
                      '2008', '2009', '2010', '2011', '2012', '2013')),
  clean = TRUE)

# Some cleaning was needed before use
SCB$region <- factor(substring(as.character(SCB$region), 4))
Swe_ltrs <- c("å" = "&aring;",
              "Å" = "&Aring;",
              "ä" = "&auml;",
              "Ä" = "&Auml;",
              "ö" = "&ouml;",
              "Ö" = "&Ouml;")
for (i in 1:length(Swe_ltrs)){
  levels(SCB$region) <- gsub(names(Swe_ltrs)[i],
              Swe_ltrs[i],
              levels(SCB$region))
}

SCB <- reshape::cast(SCB, year ~ sex + region)

save(SCB, file = "data/SCB.rda")
}
