:- ensure_loaded('database_cleaned').

create_datasets :-
    findall(
        aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
        aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
        AllData
    ),

    length(AllData, Total),
    TrainingSize is round(0.7 * Total),

    random_permutation(AllData, ShuffledData),

    length(TrainingData, TrainingSize),
    append(TrainingData, TestData, ShuffledData),

    tell('training_set.pl'),
    write_data(TrainingData, e),
    told,

    tell('test_set.pl'),
    write_data(TestData, s),
    told.

write_data([], _).
write_data([aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region)|Rest], Type) :-
    write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region, Type),
    write_data(Rest, Type).

write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Unknown', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'unknown', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region).

write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Burnout', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'burnout', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region).

write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Depression', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'depression', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region).

write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, 'Anxiety', Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'anxiety', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region).


e(Label, Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region) :-
    write('e('), write(Label), write(',['),
    write('age = '), writeq(Age), write(', '),
    write('gender = '), writeq(Gender), write(', '),
    write('role = '), writeq(Role), write(', '),
    write('industry = '), writeq(Industry), write(', '),
    write('yoe = '), writeq(Yoe), write(', '),
    write('work_location = '), writeq(Work_location), write(', '),
    write('hours_worked_per_week = '), writeq(Hours_Worked_Per_Week), write(', '),
    write('work_life_balance = '), writeq(Work_Life_Balance), write(', '),
    write('stress_level = '), writeq(Stress_Level), write(', '),
    write('social_isolation = '), writeq(Social_Isolation), write(', '),
    write('satisfaction_with_remote_work = '), writeq(Satisfaction_with_Remote_Work), write(', '),
    write('physical_activity = '), writeq(Physical_Activity), write(', '),
    write('sleep_quality = '), writeq(Sleep_Quality), write(', '),
    write('region = '), writeq(Region), writeln(']).'). 

s(Label, Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region) :-
    write('s('), write(Label), write(',['),
    write('age = '), writeq(Age), write(', '),
    write('gender = '), writeq(Gender), write(', '),
    write('role = '), writeq(Role), write(', '),
    write('industry = '), writeq(Industry), write(', '),
    write('yoe = '), writeq(Yoe), write(', '),
    write('work_location = '), writeq(Work_location), write(', '),
    write('hours_worked_per_week = '), writeq(Hours_Worked_Per_Week), write(', '),
    write('work_life_balance = '), writeq(Work_Life_Balance), write(', '),
    write('stress_level = '), writeq(Stress_Level), write(', '),
    write('social_isolation = '), writeq(Social_Isolation), write(', '),
    write('satisfaction_with_remote_work = '), writeq(Satisfaction_with_Remote_Work), write(', '),
    write('physical_activity = '), writeq(Physical_Activity), write(', '),
    write('sleep_quality = '), writeq(Sleep_Quality), write(', '),
    write('region = '), writeq(Region), writeln(']).'). 
