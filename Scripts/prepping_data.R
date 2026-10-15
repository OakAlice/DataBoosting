# prepping the data -------------------------------------------------------
# this script isn't included in the workflow, it's something I did manually first oop
# basically, my workflow requires the Time column to be diff(Time)-able
# So I manually converted times to Posixct or datetime objects where they weren't already

species <- "Vehkaoja_Dog"
path <- file.path("Data", species, "Formatted_raw_data.csv")

dat <- fread(path)

dat$Time[1]

if(species == 'Dunford_Cat'){
  dat <- dat %>%
  mutate(Time = as.POSIXct(paste("01/01/2000", Time), format = "%d/%m/%Y %H:%M:%OS", tz = "UTC"),
         Time = Time + 86400 * cumsum(c(0, diff(Time) < 0)))
}



fwrite(dat, path)
