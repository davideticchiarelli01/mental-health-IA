:- ensure_loaded('database_cleaned').

create_datasets :-
    % Recupera tutti i dati e rimuove i duplicati
    findall(
        aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
           Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, 
           Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
        aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
           Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, 
           Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region),
        AllData
    ),
    sort(AllData, UniqueData),

    % Raggruppa i dati in base alla colonna "Mental_Health_Condition"
    group_by_label(UniqueData, GroupedData),

    % Dividi ogni gruppo in training e test
    divide_groups(GroupedData, TrainingGroups, TestGroups),

    % Appiattisci i gruppi per ottenere i dataset finali
    flatten(TrainingGroups, TrainingData),
    flatten(TestGroups, TestData),

    % Scrivi il training set
    tell('ds_training.pl'),
    write_data(TrainingData, e),
    told,

    % Scrivi il test set
    tell('ds_test.pl'),
    write_data(TestData, s),
    told.

% Raggruppa i record in base alla colonna "Mental_Health_Condition"
group_by_label(Records, Grouped) :-
    % Trova tutte le etichette uniche
    findall(Label, member(aa(_, _, _, _, _, _, _, _, _, Label, _, _, _, _, _), Records), Labels),
    sort(Labels, UniqueLabels),  % Elimina duplicati
    % Raggruppa i record per ogni etichetta
    findall(
        Group,
        (
            member(Label, UniqueLabels),  % Per ogni etichetta
            include(has_label(Label), Records, Group)  % Filtra i record con quell'etichetta
        ),
        Grouped
    ).

% Predicato di supporto per filtrare i record con una specifica etichetta
has_label(Label, aa(_, _, _, _, _, _, _, _, _, Label, _, _, _, _, _)).

% Dividi ogni gruppo in training e test
divide_groups([], [], []).
divide_groups([Group | RestGroups], [Train | RestTrain], [Test | RestTest]) :-
    length(Group, Total),
    TrainingSize is round(0.7 * Total),  % 70% per il training
    length(Train, TrainingSize),
    random_permutation(Group, Shuffled),  % Mescola i record
    append(Train, Test, Shuffled),  % Dividi in training e test
    divide_groups(RestGroups, RestTrain, RestTest).

% Scrive i dati nei file
write_data([], _).
write_data([aa(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
               Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, 
               Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region) | Rest], Type) :-
    write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
                Work_Life_Balance, Stress_Level, Mental_Health_Condition, Social_Isolation, 
                Satisfaction_with_Remote_Work, Physical_Activity, Sleep_Quality, Region, Type),
    write_data(Rest, Type).

% Scrive un singolo record
write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
            Work_Life_Balance, Stress_Level, 'Unknown', Social_Isolation, Satisfaction_with_Remote_Work, 
            Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'unknown', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
         Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, 
         Physical_Activity, Sleep_Quality, Region).

write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
            Work_Life_Balance, Stress_Level, 'Burnout', Social_Isolation, Satisfaction_with_Remote_Work, 
            Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'burnout', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
         Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, 
         Physical_Activity, Sleep_Quality, Region).

write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
            Work_Life_Balance, Stress_Level, 'Depression', Social_Isolation, Satisfaction_with_Remote_Work, 
            Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'depression', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
         Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, 
         Physical_Activity, Sleep_Quality, Region).

write_entry(Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
            Work_Life_Balance, Stress_Level, 'Anxiety', Social_Isolation, Satisfaction_with_Remote_Work, 
            Physical_Activity, Sleep_Quality, Region, Type) :-
    call(Type, 'anxiety', Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
         Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, 
         Physical_Activity, Sleep_Quality, Region).

% Scrittura nei file (formato specifico)
e(Label, Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
  Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, 
  Physical_Activity, Sleep_Quality, Region) :-
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

s(Label, Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
  Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, 
  Physical_Activity, Sleep_Quality, Region) :-
    e(Label, Age, Gender, Role, Industry, Yoe, Work_location, Hours_Worked_Per_Week, 
      Work_Life_Balance, Stress_Level, Social_Isolation, Satisfaction_with_Remote_Work, 
      Physical_Activity, Sleep_Quality, Region).