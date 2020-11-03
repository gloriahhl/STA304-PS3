#cleaning survey dataset
reduced_data_survey <- 
  raw_data_survey %>% 
  select(vote_2020,
         vote_intention,
         registration,
         gender,
         state,
         household_income,
         race_ethnicity,
         education,
         age)

reduced_data_survey$age<-as.numeric(reduced_data_survey$age)
filtered_data_survey<-reduced_data_survey %>% 
  filter(registration=="Registered"&
           (vote_intention=="Yes, I will vote"|
              vote_intention=="Not sure")&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))

filtered_data_survey<-na.omit(filtered_data_survey)
rm(raw_data_survey,reduced_data_survey)


#cleaning census dataset
reduced_data_census <- 
  raw_data_census %>% 
  select(perwt,
         citizen,
         age,
         sex, 
         educd,
         stateicp,
         hhincome,
         race
  )
reduced_data_census$age<-as.numeric(reduced_data_census$age)+2
filtered_data_census<-reduced_data_census %>% filter(age>=18 & (citizen=="naturalized citizen"|citizen=="born abroad of american parents"))
filtered_data_census$hhincome<-ifelse(filtered_data_census$hhincome==9999999,
                                      NaN,filtered_data_census$hhincome)

filtered_data_census<-na.omit(filtered_data_census)
rm(raw_data_census,reduced_data_census)

#create age group
filtered_data_survey<-filtered_data_survey %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 35 ~ '21 to 35',
                              age >35  & age <= 50 ~ '35 to 50',
                              age >50  & age <= 65 ~ '50 to 65',
                              age >65  & age <= 80 ~ '65 to 80',
                              age >80 ~ 'above 80'
  )) 
filtered_data_census<-filtered_data_census %>% 
  mutate(agegroup = case_when(age <=20 ~ '20 or less',
                              age >20  & age <=  35 ~ '21 to 35',
                              age >35  & age <= 50 ~ '35 to 50',
                              age >50  & age <= 65 ~ '50 to 65',
                              age >65  & age <= 80 ~ '65 to 80',
                              age >80 ~ 'above 80' 
  ))

unique(filtered_data_census$agegroup)
unique(filtered_data_survey$agegroup)

#clean sex/gender
unique(filtered_data_census$sex)
unique(filtered_data_survey$gender)
filtered_data_census$sex<-ifelse(filtered_data_census$sex=="female","Female","Male")
filtered_data_census<-rename(filtered_data_census,gender=sex)
unique(filtered_data_census$gender)
unique(filtered_data_survey$gender)

#clean education
unique(filtered_data_census$educd)
unique(filtered_data_survey$education)

#Survey
filtered_data_survey$education[filtered_data_survey$education=="Other post high school vocational training"]<-"High school graduate"
filtered_data_survey$education[filtered_data_survey$education=="Completed some graduate, but no degree"]<-"College Degree (such as B.A., B.S.)"
#Census
grade3.less<-c("no schooling completed","nursery school, preschool","kindergarten","grade 1","grade 2","grade 3")
grade4to8<-c("grade 4","grade 5","grade 6","grade 7","grade 8")
grade9to11<-c("grade 9","grade 10","grade 11","12th grade, no diploma")
edu.highsch<-c("ged or alternative credential","regular high school diploma")
edu.somecoll<-c("some college, but less than 1 year",
                "1 or more years of college credit, no degree")
filtered_data_census<-filtered_data_census %>% 
  mutate(newedu = case_when(educd =="associate's degree, type not specified" ~ 'Associate Degree',
                            educd=="doctoral degree"~'Doctorate degree',
                            educd =="master's degree" ~ 'Masters degree',
                            educd=="professional degree beyond a bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd =="bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd %in% edu.somecoll~"Completed some college, but no degree",
                            educd %in% edu.highsch~"High school graduate",
                            educd %in% grade9to11~"Completed some high school",
                            educd %in% grade4to8~"Middle School - Grades 4 - 8",
                            educd %in% grade3.less ~"3rd Grade or less"
  )) 
#drop educd & rename educd2
filtered_data_census<-rename(filtered_data_census,education=newedu)
filtered_data_census$educd<-NULL
unique(filtered_data_census$education)
unique(filtered_data_survey$education)

# clean stateicp
filtered_data_census<-filtered_data_census %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY",
                           stateicp=="district of columbia"~"DC")) 
filtered_data_census$stateicp<-NULL

unique(filtered_data_census$state)
unique(filtered_data_survey$state)

# clean household income
x<-unique(filtered_data_survey$household_income)
min(filtered_data_census$hhincome)
max(filtered_data_census$hhincome)
filtered_data_census<-filtered_data_census %>% 
  mutate(household_income = case_when(hhincome<=14999 ~ "Less than $14,999",
                                      hhincome>=15000 & hhincome<=19999~"$15,000 to $19,999",
                                      hhincome>=20000 & hhincome<=24999~"$20,000 to $24,999",
                                      hhincome>=25000 & hhincome<=29999~"$25,000 to $29,999",
                                      hhincome>=30000 & hhincome<=34999~"$30,000 to $34,999",
                                      hhincome>=35000 & hhincome<=39999~"$35,000 to $39,999",
                                      hhincome>=40000 & hhincome<=44999~"$40,000 to $44,999",
                                      hhincome>=45000 & hhincome<=49999~"$45,000 to $49,999",
                                      hhincome>=50000 & hhincome<=54999~"$50,000 to $54,999",
                                      hhincome>=55000 & hhincome<=59999~"$55,000 to $59,999",
                                      hhincome>=60000 & hhincome<=64999~"$60,000 to $64,999",
                                      hhincome>=65000 & hhincome<=69999~"$65,000 to $69,999",
                                      hhincome>=70000 & hhincome<=74999~"$70,000 to $74,999",
                                      hhincome>=75000 & hhincome<=79999~"$75,000 to $79,999",
                                      hhincome>=80000 & hhincome<=84999~"$80,000 to $84,999",
                                      hhincome>=85000 & hhincome<=89999~"$85,000 to $89,999",
                                      hhincome>=90000 & hhincome<=94999~"$90,000 to $94,999",
                                      hhincome>=95000 & hhincome<=99999~"$95,000 to $99,999",
                                      hhincome>=100000 & hhincome<=124999~"$100,000 to $124,999",
                                      hhincome>=125000 & hhincome<=149999~"$125,000 to $149,999",
                                      hhincome>=150000 & hhincome<=174999~"$150,000 to $174,999",
                                      hhincome>=175000 & hhincome<=199999~"$175,000 to $199,999",
                                      hhincome>=200000 & hhincome<=249999~"$200,000 to $249,999",
                                      hhincome>=250000~"$250,000 and above"
  )) 

filtered_data_census$hhincome<-NULL

unique(filtered_data_census$household_income)
unique(filtered_data_survey$household_income)

#clean race ethnicity
length(unique(filtered_data_survey$race_ethnicity))
length(unique(filtered_data_census$race))

otherasian<-c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Other)","Asian (Korean)","Asian (Filipino)",
              "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
              "Pacific Islander (Samoan)","Pacific Islander (Guamanian)")
#survey data
filtered_data_survey<-filtered_data_survey %>% 
  mutate(race = case_when(race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity %in% otherasian ~"other asian or pacific islander",
                          race_ethnicity =="White" ~ 'White',
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native",
                          race_ethnicity=="Other race "~"Other race"
  )) 
filtered_data_survey$race_ethnicity<-NULL

#census data
filtered_data_census<-filtered_data_census %>% 
  mutate(race2 = case_when(race=="white"~"White",
                           race=="chinese"~"Chinese",
                           race=="black/african american/negro"~"Black, or African American",
                           race=="two major races"~"Other race",
                           race=="other race, nec"~"Other race",
                           race=="japanese"~"Japanese",
                           race=="american indian or alaska native"~"American Indian or Alaska Native",
                           race=="three or more major races"~"Other race",
                           race=="other asian or pacific islander"~"other asian or pacific islander"
  )) 
unique(filtered_data_census$race2)

filtered_data_census$race<-filtered_data_census$race2
filtered_data_census$race2<-NULL

unique(filtered_data_census$race)
unique(filtered_data_survey$race)
