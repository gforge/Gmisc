library(Gmisc)
library(grid)
library(glue)

flowchart(source = glue("Stockholm population",
                        "n = {pop}",
                        pop = txtInt(1632798),
                        .sep = "\n"),
          eligible = glue("Eligible",
                          "n = {pop}",
                          pop = txtInt(10032),
                          .sep = "\n"),
          included = glue("Randomized",
                          "n = {incl}",
                          incl = txtInt(122),
                          .sep = "\n"),
          groups = list(
            glue("Treatment A",
                 "n = {recr}",
                 recr = txtInt(43),
                 .sep = "\n"),
            glue("Treatment B",
                 "n = {recr}",
                 recr = txtInt(122 - 43 - 30),
                 .sep = "\n")
          )) |>
  spread(axis = "y") |>
  spread(subelement = "groups", axis = "x") |>
  insert(list(excluded = boxHeaderGrob(header = glue("Excluded (n = {tot}):", tot = 30),
                                       body = glue(" - not interested: {uninterested}",
                                                   " - contra-indicated: {contra}",
                                                   uninterested = 12,
                                                   contra = 30 - 12,
                                                   .sep = "\n"),
                                       bjust = "left",
                                       header_gp = getOption("boxGrobTxt", default = gpar(
                                         color = "black",
                                         cex = 1
                                       )))),
         after = "eligible",
         name = "excluded") |>
  move(name = "excluded", x = .8) |>
  connect("source", "eligible", type = "vert") |>
  connect("eligible", "included", type = "vert") |>
  connect("included", "groups", type = "N") |>
  connect("eligible", "excluded", type = "L", label = "Excluded")