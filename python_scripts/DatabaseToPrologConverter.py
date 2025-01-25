import pandas as pd

# Load the CSV file from the specified path
csv = pd.read_csv(
    r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database_cleaned.csv")

output_prolog = r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database_cleaned.pl"

# Apri il file di output Prolog
with open(output_prolog, "w") as file:
    # Per ogni riga del DataFrame, crea un fatto Prolog
    for index, row in csv.iterrows():
        # Genera un fatto Prolog con i valori delle colonne
        fatto = "aa(" + ", ".join([repr(val) for val in row]) + ").\n"
        file.write(fatto)

print(f"Okay, {output_prolog}")


