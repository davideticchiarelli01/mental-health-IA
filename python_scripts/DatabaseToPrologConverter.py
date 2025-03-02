import pandas as pd

# Load the CSV file from the specified path
data = pd.read_csv(
    r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database_cleaned.csv")

output_prolog_file = r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database_cleaned.pl"

# Open the output Prolog file
with open(output_prolog_file, "w") as file:
    # Iterate over each row in the DataFrame and create a Prolog fact
    for _, row in data.iterrows():
        # Generate a Prolog fact using the column values
        fact = "aa(" + ", ".join([repr(val) for val in row]) + ").\n"
        file.write(fact)
