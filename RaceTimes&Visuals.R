###libraries needed
#tidyverse, tidyr, dplyr, ggplot2, lubridate, here, readr, tibble, chron, skimr
#read in file
OriginalRaceData <- read.csv('~/Data Analytics/Course 8 - Portfolio/Matthew Running Data/RStudio/Attempt 2/RunningDataAtmpt2.csv')

#alter Time and MinMilePace to correct format
PacePerMile <- times(OriginalRaceData$MinMilePace)
FullRaceTime <- times(OriginalRaceData$Time)

#create a base table with all column types converted
#add to the table the race count for each race
BaseRaceTable <- OriginalRaceData %>%
  select(Race, City, State, DistanceMiles, Overall.Finish.Position, Overall.Finishers, Age.Group.Finish.Position, Age.Group.Finisher, Year) %>%
  mutate(PacePerMile, FullRaceTime) %>%
  add_count(Race)

#rename columns for better read/understanding
BaseRaceTable <- BaseRaceTable %>%
  rename(AllFinishPosition = Overall.Finish.Position) %>%
  rename(AllFinishers = Overall.Finishers) %>%
  rename(AgeFinishPosition = Age.Group.Finish.Position) %>%
  rename(AgeFinishers = Age.Group.Finisher) %>%
  rename(RaceAttendance = n)

#filter to 5k races
All5kRaces <- BaseRaceTable %>%
  filter(DistanceMiles == "3.1") %>%
  arrange(Year)

#filter to 10k races ran
All10kRaces <- BaseRaceTable %>%
  filter(DistanceMiles == "6.2") %>%
  arrange(Year)

#filter to half marathon races
AllHalfRaces <- BaseRaceTable %>%
  filter(DistanceMiles == "13.1") %>%
  arrange(Year)

#pull out obscure races (non-trad miles ran)
AllObscureRaces <- BaseRaceTable %>%
  filter(DistanceMiles != "3.1" & DistanceMiles != "6.2" & DistanceMiles != "13.1")


###data manipulation & visuals to accompany
#5k pace over time
Pace5k <- ggplot(All5kRaces) +
  geom_smooth(mapping = aes(Year, PacePerMile)) +
  scale_y_chron(format = "%M:%S") +
  labs(title = "5k Pace Over Years", subtitle = "Years between 2017 & 2024", caption = "Data Source: Personal Attendance for Races", x = "Year", y = "Mile Pace(min)")

#5k pace for each race ran more than once (over years)
RacePace5k <- ggplot(filter(All5kRaces, RaceAttendance > 1)) +
  geom_line(mapping = aes(Year, PacePerMile)) +
  scale_y_chron(format = "%M:%S") +
  labs(title = "5K Pace per Race", subtitle = "Races Attended Multiple Years", caption = "Data Source: Personal Attendance for Races", x = "Years Attended", y = "Mile Pace(min)") +
  facet_wrap(~Race)


#10k pace over time
Pace10k <- ggplot(All10kRaces) +
  geom_smooth(mapping = aes(Year, PacePerMile)) +
  scale_y_chron(format = "%M:%S") +
  labs(title = "10k Pace Over Years", subtitle = "Years between 2018 & 2024", caption = "Data Source: Personal Attendance for Races", x = "Year", y = "Mile Pace(min)")

#10k pace for each race ran more than once (over years)
RacePace10k <- ggplot(filter(All10kRaces, RaceAttendance > 1)) +
  geom_line(mapping = aes(Year, PacePerMile)) +
  scale_y_chron(format = "%M:%S") +
  labs(title = "10K Pace per Race", subtitle = "Races Attended Multiple Years", caption = "Data Source: Personal Attendance for Races", x = "Years Attended", y = "Mile Pace(min)") +
  facet_wrap(~Race)


#grid arrange 5k and 10k pace graphs
##note: y-axis tick marks are not equivilant 
grid.arrange(Pace5k, Pace10k, ncol = 2)
