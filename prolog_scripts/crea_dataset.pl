:- ensure_loaded('database_cleaned'). 

startt :- 
    tell('dataset.pl'),  % Scrive i fatti in un nuovo file
    
    setof(Age, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(Age, A, B, C, D, E, F, G, H, I, J, K, L, M, N), AttributiAge),
    write('a(age,'), writeq(AttributiAge), writeln(').'),

    setof(Gender, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, Gender, B, C, D, E, F, G, H, I, J, K, L, M, N), AttributiGender),
    write('a(gender,'), writeq(AttributiGender), writeln(').'),
    
    setof(Role, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, Role, C, D, E, F, G, H, I, J, K, L, M, N), AttributiRole),
    write('a(role,'), writeq(AttributiRole), writeln(').'),

    setof(Industry, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, Industry, D, E, F, G, H, I, J, K, L, M, N), AttributiIndustry),
    write('a(industry,'), writeq(AttributiIndustry), writeln(').'),

    setof(Yoe, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, Yoe, E, F, G, H, I, J, K, L, M, N), AttributiYoe),
    write('a(yoe,'), writeq(AttributiYoe), writeln(').'),

    setof(Work_location, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, Work_location, F, G, H, I, J, K, L, M, N), AttributiWorkLocation),
    write('a(work_location,'), writeq(AttributiWorkLocation), writeln(').'),

    setof(Hours_Worked_Per_Week, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, Hours_Worked_Per_Week , G, H, I, J, K, L, M, N), AttributiHoursWorkedPerWeek),
    write('a(hours_worked_per_week,'), writeq(AttributiHoursWorkedPerWeek), writeln(').'),

    setof(Work_Life_Balance, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, G, Work_Life_Balance, H, I, J, K, L, M, N), AttributiWorkLifeBalance),
    write('a(work_life_balance,'), writeq(AttributiWorkLifeBalance), writeln(').'),

    setof(Stress_Level, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, G, H, Stress_Level, I, J, K, L, M, N), AttributiStressLevel),
    write('a(stress_level,'), writeq(AttributiStressLevel), writeln(').'),

    setof(Social_Isolation, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, G, H, I, J, Social_Isolation, K, L, M, N), AttributiSocialIsolation),
    write('a(social_isolation,'), writeq(AttributiSocialIsolation), writeln(').'),

    setof(Satisfaction_with_Remote_Work, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, G, H, I, J, K, Satisfaction_with_Remote_Work, L, M, N), AttributiSatisfactionWithRemoteWork),
    write('a(satisfaction_with_remote_work,'), writeq(AttributiSatisfactionWithRemoteWork), writeln(').'),

    setof(Physical_Activity, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, G, H, I, J, K, L, Physical_Activity, M, N), AttributiPhysicalActivity),
    write('a(physical_activity,'), writeq(AttributiPhysicalActivity), writeln(').'),

    setof(Sleep_Quality, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, G, H, I, J, K, L, M, Sleep_Quality, N), AttributiSleepQuality),
    write('a(sleep_quality,'), writeq(AttributiSleepQuality), writeln(').'),

    setof(Region, A^B^C^D^E^F^G^H^I^J^K^L^M^N^aa(A, B, C, D, E, F, G, H, I, J, K, L, M, N, Region), AttributiRegion),
    write('a(region,'), writeq(AttributiRegion), writeln(').'),


    starttt.


starttt :-
    aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Unknown', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
    write('e(unknown,['),
    write('age = '),writeq(Age), write(', '),
    write('gender = '),writeq(Gender), write(', '),
    write('role = '),writeq(Role), write(', '),
    write('industry = '),writeq(Industry), write(', '),
    write('yoe = '),writeq(Yoe), write(', '),
    write('work_location = '),writeq(Work_location), write(', '),
    write('hours_worked_per_week = '),writeq(Hours_Worked_Per_Week), write(', '),
    write('work_life_balance = '),writeq(Work_Life_Balance), write(', '),
    write('stress_level = '),writeq(Stress_Level), write(', '),
    write('social_isolation = '),writeq(Social_Isolation), write(', '),
    write('satisfaction_with_remote_work = '),writeq(Satisfaction_with_Remote_Work), write(', '),
    write('physical_activity = '),writeq(Physical_Activity), write(', '),
    write('sleep_quality = '),writeq(Sleep_Quality), write(', '),
    write('region = '),writeq(Region), write(']).'), nl,
    fail.

starttt :-
    aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Depression', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
    write('e(depression,['),
    write('age = '),writeq(Age), write(', '),
    write('gender = '),writeq(Gender), write(', '),
    write('role = '),writeq(Role), write(', '),
    write('industry = '),writeq(Industry), write(', '),
    write('yoe = '),writeq(Yoe), write(', '),
    write('work_location = '),writeq(Work_location), write(', '),
    write('hours_worked_per_week = '),writeq(Hours_Worked_Per_Week), write(', '),
    write('work_life_balance = '),writeq(Work_Life_Balance), write(', '),
    write('stress_level = '),writeq(Stress_Level), write(', '),
    write('social_isolation = '),writeq(Social_Isolation), write(', '),
    write('satisfaction_with_remote_work = '),writeq(Satisfaction_with_Remote_Work), write(', '),
    write('physical_activity = '),writeq(Physical_Activity), write(', '),
    write('sleep_quality = '),writeq(Sleep_Quality), write(', '),
    write('region = '),writeq(Region), write(']).'), nl,
    fail.

starttt :-
    aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Anxiety', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
    write('e(anxiety,['),
    write('age = '),writeq(Age), write(', '),
    write('gender = '),writeq(Gender), write(', '),
    write('role = '),writeq(Role), write(', '),
    write('industry = '),writeq(Industry), write(', '),
    write('yoe = '),writeq(Yoe), write(', '),
    write('work_location = '),writeq(Work_location), write(', '),
    write('hours_worked_per_week = '),writeq(Hours_Worked_Per_Week), write(', '),
    write('work_life_balance = '),writeq(Work_Life_Balance), write(', '),
    write('stress_level = '),writeq(Stress_Level), write(', '),
    write('social_isolation = '),writeq(Social_Isolation), write(', '),
    write('satisfaction_with_remote_work = '),writeq(Satisfaction_with_Remote_Work), write(', '),
    write('physical_activity = '),writeq(Physical_Activity), write(', '),
    write('sleep_quality = '),writeq(Sleep_Quality), write(', '),
    write('region = '),writeq(Region), write(']).'), nl,
    fail.

starttt :-
    aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Burnout', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
    write('e(burnout,['),
    write('age = '),writeq(Age), write(', '),
    write('gender = '),writeq(Gender), write(', '),
    write('role = '),writeq(Role), write(', '),
    write('industry = '),writeq(Industry), write(', '),
    write('yoe = '),writeq(Yoe), write(', '),
    write('work_location = '),writeq(Work_location), write(', '),
    write('hours_worked_per_week = '),writeq(Hours_Worked_Per_Week), write(', '),
    write('work_life_balance = '),writeq(Work_Life_Balance), write(', '),
    write('stress_level = '),writeq(Stress_Level), write(', '),
    write('social_isolation = '),writeq(Social_Isolation), write(', '),
    write('satisfaction_with_remote_work = '),writeq(Satisfaction_with_Remote_Work), write(', '),
    write('physical_activity = '),writeq(Physical_Activity), write(', '),
    write('sleep_quality = '),writeq(Sleep_Quality), write(', '),
    write('region = '),writeq(Region), write(']).'), nl,
    fail.

starttt :- told.
