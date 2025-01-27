import pandas as pd

# Load the CSV file from the specified path
data = pd.read_csv(
    r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database_cleaned.csv")

output_prolog_file = r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\attributes.pl"

# Open the output Prolog file
with open(output_prolog_file, "w") as file:
    # Iterate over each column in the DataFrame
    for column in data.columns:
        # Get unique values in the column, removing NaN values
        unique_values = data[column].dropna().unique()

        # Convert the unique values into a Prolog-compatible list
        values_str = "[" + ", ".join([repr(val) for val in unique_values]) + "]"

        # Generate the Prolog fact
        fact = f"a({column},{values_str}).\n"

        # Write the fact to the file
        file.write(fact)

print(f"Prolog facts saved to {output_prolog_file}")
