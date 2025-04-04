import pandas as pd

params_invariant = pd.read_csv("metadata/table1.csv")
params_instantaneous = pd.read_csv("metadata/table2.csv")
params_accumulations = pd.read_csv("metadata/table3.csv")
mean_rates_and_fluxes = pd.read_csv("metadata/table4.csv")
params_vertical_instantaneous = pd.read_csv("metadata/table6.csv")

param_md = pd.concat([params_invariant, params_instantaneous, params_accumulations, mean_rates_and_fluxes, params_vertical_instantaneous], ignore_index=True)