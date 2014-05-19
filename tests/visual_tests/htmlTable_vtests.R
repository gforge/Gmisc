mx <- matrix(1:6, ncol=3)
colnames(mx) <- LETTERS[1:3]
rownames(mx) <- letters[1:2]
## altcol does not break rgroupCSSstyle
htmlTable(mx, n.rgroup=c(2), rgroup=c("Nice!"),
          n.cgroup=c(2,1), cgroup=c("First", "Second"),
          rgroupCSSstyle = "font-weight:900; background-color:#f2f2f2;")


mx <- matrix(1:9, ncol=3)
colnames(mx) <- LETTERS[1:3]
rownames(mx) <- letters[1:3]

mx_3_times <- rbind(mx,
                    mx,
                    mx)
htmlTable(mx_3_times,
          tspannerCSSseparator="border-top: 2px solid red;",
          rgroup = rep(c("Group a", "Group b and c"), times=3),
          n.rgroup = rep(c(1,2), times=3),
          tspanner=c("First", "Second", "Third"),
          n.tspanner=rep(nrow(mx), times=3),
          rowlabel = '', 
          altcol = c('white','lightblue1'),
          tfoot = "Some footer text",
          caption="Caption text")

htmlTable(mx_3_times,
          tspannerCSSseparator=c("border-top: 2px solid red;",
                                 "border-top: 12px solid blue;"),
          rgroup = rep(c("Group a", "Group b and c"), times=3),
          n.rgroup = rep(c(1,2), times=3),
          tspanner=c("First", "Second", "Third"),
          n.tspanner=rep(nrow(mx), times=3),
          rowlabel = '', 
          altcol = c('white','lightblue1'),
          tfoot = "Some footer text",
          caption="Caption text")

htmlTable(mx_3_times,
          tspannerCSSseparator=c("border-top: 2px solid red;",
                                 "border-top: 12px solid blue;"),
          rgroup = rep(c("Group a", "Group b and c"), times=3),
          n.rgroup = rep(c(1,2), times=3),
          tspanner=c("First", "Second", "Third"),
          n.tspanner=rep(nrow(mx), times=3),
          rowlabel = '', 
          tfoot = "Some footer text",
          caption="Caption text")


htmlTable(mx_3_times, 
          tspannerCSSstyle="font-weight: 900;",
          tspannerCSSseparator="border-top: 2px solid red;",
          rgroup = rep(c("Group a", "Group b and c"), times=3),
          n.rgroup = rep(c(1,2), times=3),
          tspanner=c("First", "Second", "Third"),
          n.tspanner=rep(nrow(mx), times=3),
          rowlabel = '', 
          tfoot = "Some footer text",
          caption="Caption text")