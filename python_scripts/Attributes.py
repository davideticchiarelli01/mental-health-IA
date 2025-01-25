import pandas as pd

# Carica il file CSV dal percorso specificato
csv = pd.read_csv(
    r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\database_cleaned.csv")
output_prolog = r"C:\Users\david\OneDrive\Documenti\GitHub\mental-health-IA\attributes.pl"

with open(output_prolog, "w") as file:
    for column in csv.columns:
        unique_values = csv[column].dropna().unique()  # Rimuove i valori NaN
        values_str = "[" + ", ".join([repr(val) for val in unique_values]) + "]"
        fatto = f"a({column},{values_str}).\n"



    file.write(fatto)
