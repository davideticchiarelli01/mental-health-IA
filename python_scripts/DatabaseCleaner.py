import pandas as pd

# Load the CSV file from the specified path
csv = pd.read_csv(
    r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database.csv")

pd.set_option('display.max_columns', None)  # Display all columns

# Drop the specified columns from the dataframe
csv = csv.drop(
    columns=['Employee_ID',
             'Number_of_Virtual_Meetings',
             'Access_to_Mental_Health_Resources',
             'Productivity_Change',
             'Company_Support_for_Remote_Work'])

# Rename specific columns to more user-friendly names
csv = csv.rename(columns={
    'Social_Isolation_Rating': 'Social_Isolation',
    'Work_Life_Balance_Rating': 'Work_Life_Balance'
})

# Definisci i bin e le etichette
hours_bins = [0, 30, 40, 60, float('inf')]  # Aggiunto 0 come primo bin per includere tutti i valori > 0
hours_labels = ['Part-time', 'Full-Time', 'Overtime', 'Unknown']

# Utilizza pd.cut per categorizzare i dati
csv['Hours_Worked_Per_Week'] = pd.cut(csv['Hours_Worked_Per_Week'], bins=hours_bins, labels=hours_labels, right=False)


# Categorize 'Age' into bins using pd.cut and assign corresponding labels
age_bins = [20, 29, 49, 60, float('inf')]
age_labels = ['Youth', 'Adult', 'Senior', 'Unknown']
csv['Age'] = pd.cut(csv['Age'], bins=age_bins, labels=age_labels, right=True)

# Replace 'Prefer not to say' in the Gender column with 'Unknown'
csv['Gender'] = csv['Gender'].replace('Prefer not to say', 'Unknown')

# Categorize 'Years_of_Experience' into bins using pd.cut and assign corresponding labels
yoe_bins = [1, 10, 20, 30, 40, float('inf')]
yoe_labels = ['Junior', 'Mid', 'Expert', 'Senior', 'Unknown']
csv['Years_of_Experience'] = pd.cut(csv['Years_of_Experience'], bins=yoe_bins, labels=yoe_labels, right=True)

# Map numerical values to descriptive labels for Work-Life Balance
wlb_map = {1: 'Minimal', 2: 'Low', 3: 'Moderate', 4: 'High', 5: 'Extreme'}
csv['Work_Life_Balance'] = csv['Work_Life_Balance'].map(wlb_map).fillna('Unknown')

# Map numerical values to descriptive labels for Social Isolation
si_map = {1: 'Minimal', 2: 'Low', 3: 'Moderate', 4: 'High', 5: 'Extreme'}
csv['Social_Isolation'] = csv['Social_Isolation'].map(si_map).fillna('Unknown')

# Fill any missing values in 'Physical_Activity' with 'Unknown'
csv['Physical_Activity'] = csv['Physical_Activity'].fillna('Unknown')

csv = csv.fillna('Unknown')

# Save the cleaned dataframe to a new CSV file
csv.to_csv(r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database_cleaned.csv", index=False)

# Mostra le prime righe del DataFrame aggiornato
print(csv)
