import pandas as pd
from sklearn.model_selection import train_test_split

# Load the CSV file
data = pd.read_csv(
    r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\mental_health_dataset\database_cleaned.csv"
)

# Split the dataset into training and testing sets
train_set, test_set = train_test_split(data, test_size=0.3, random_state=42, stratify=data['Mental_Health_Condition'])


# Function to generate Prolog facts
def generate_facts(dataset, file_path, prefix):
    """
    This function generates Prolog facts for each row in the dataset and writes them to the specified file.

    Parameters:
    dataset (pd.DataFrame): The input dataset to process.
    file_path (str): The path to the file where the Prolog facts will be written.
    prefix (str): The prefix to use for the Prolog facts (e.g., 'e' for training, 's' for test).
    """
    with open(file_path, "w") as file:
        # Iterate over each row in the dataset
        for _, row in dataset.iterrows():
            # Extract the mental health condition and convert it to lowercase
            condition = row['Mental_Health_Condition'].lower()

            # Create a list of attributes for the Prolog fact
            attributes = ', '.join([
                # Format each attribute as 'attribute_name = value'
                f"{col.lower().replace(' ', '_')} = '{row[col]}'" for col in ['Age', 'Gender', 'Job_Role', 'Industry',
                                                                              'Years_of_Experience', 'Work_Location',
                                                                              'Hours_Worked_Per_Week',
                                                                              'Work_Life_Balance',
                                                                              'Stress_Level', 'Social_Isolation',
                                                                              'Satisfaction_with_Remote_Work',
                                                                              'Physical_Activity', 'Sleep_Quality',
                                                                              'Region']
            ])
            # Create the Prolog fact using the condition and attributes
            fact = f"{prefix}({condition},[{attributes}]).\n"

            # Write the Prolog fact to the file
            file.write(fact)


# Paths to the output files
train_file = r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\prolog_script\train_set.pl"
test_file = r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\prolog_script\test_set.pl"

# Generate Prolog facts for the training and test sets
generate_facts(train_set, train_file, 'e')
generate_facts(test_set, test_file, 's')
