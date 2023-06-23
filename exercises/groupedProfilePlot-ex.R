library(aqp)

# sample data
data(sp1)
# convert colors from Munsell to hex-encoded RGB
sp1$soil_color <- with(sp1, munsell2rgb(hue, value, chroma))

# promote to SoilProfileCollection
depths(sp1) <- id ~ top + bottom
site(sp1) <- ~ group

# add a groups
sp1$group.2 <- sprintf("%s-%s", rev(LETTERS[1:3]), sp1$group)

# convert fake group to factor with new levels
sp1$group.3 <- factor(sp1$group.2, levels=c('C-2', 'B-2', 'A-2', 'C-1', 'B-1', 'A-1'))

# plot profiles, sorted and annotated by 'group.3'
# custom margins
# do not clip at margins
par(mar = c(1,1,3,1), xpd = NA)
# leave extra room at the bottom by extending via max.depth
groupedProfilePlot(sp1, groups='group.3', max.depth = 250, group.name.offset = c(-10, -20), id.style='side', break.offset = -0.65)

# get last sketch metadata
lastPP <- get("last_spc_plot", envir = aqp.env)

# there is a lot in here
str(lastPP, 1)

# profile IDs, or any site-level attribute
# original order
.ids <- profile_id(sp1)

# get sketch order
.order <- lastPP$plot.order

# max depths by profile, original order
.bottoms <- profileApply(sp1, max)

# annotate 10cm below each profile, using updated ordering
text(x = seq_along(1:length(sp1)), y = .bottoms[.order] + 10, labels = .ids[.order], cex = 0.8, font = 2)


