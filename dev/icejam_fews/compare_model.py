import openpyxl
import sys
import os

# Add current dir to path for imports
sys.path.append('.')
from dev.icejam_fews.icejam_transcribe import run_model

# Mapping Excel columns to model keys
# G: Cloud (%), H: Albedo, I: OW (%), L: Tmax, M: Tmin, P: Yobs, Q: dY, R: LocJam, S: Jave
# T: Tair_corr, U: Tavg, V: Corr_Tavg, W: Deg-days
# AB: SWmax, AC: SWin, AD: SWnet, AE: Net_Rad
# AI: Ti, AJ: dS_melt, AK: Fr, AL: Fd, AM: Fr - Fd

COL_MAP = {
    7:  ('cloud_obs', 'cloud_obs'),
    8:  ('albedo_ice', 'albedo_ice'),
    9:  ('ow_pct', 'ow_pct'),
    12: ('tair_max', 'tair_max'),
    13: ('tair_min', 'tair_min'),
    16: ('Y_obs', 'Y_obs'),
    17: ('Y_rises', 'Y_rises'),
    18: ('loc_jam', 'loc_jam'),
    19: ('jave', 'jave'),
    20: ('tair_corr', 'Tair_corr'),
    21: ('tair_avg', 'Tavg'),
    22: ('tair_corr_avg', 'Tair_corr_avg'),
    23: ('deg_days', 'deg_days'),
    28: ('sw_max', 'SWmax'),
    29: ('sw_in', 'SWin'),
    30: ('sw_net', 'SWnet'),
    31: ('net_rad', 'NetRad'),
    35: ('Ti', 'Ti'),
    36: ('dS_melt', 'dS_melt'),
    37: ('Fr', 'Fr'),
    38: ('Fd', 'Fd'),
    39: ('force_diff', 'Fr_minus_Fd')
}

xlsx_path = r'C:\Users\esniede\Documents\Breakup_model_YukonRiver_2025.xlsx'
wb = openpyxl.load_workbook(xlsx_path, data_only=True)
ws = wb['Inputdata']

# Run the model (it likely needs inputs or reads from file, 
# but icejam_transcribe.py often has its internal global data or reads a specific CSV.
# Let's see if run_model() returns the results list.
results = run_model()

mismatches = []
summaries = {}

# According to the transcribe script, results usually correspond to the sequence of dates starting April 1.
# Row 12 in Excel is April 1. Rows 12-56.
# index in results = excel_row - 12

for r_idx in range(12, 57):
    res_idx = r_idx - 12
    if res_idx >= len(results):
        break
    
    res = results[res_idx]
    if res is None:
        continue

    for col_idx, (py_key, label) in COL_MAP.items():
        xl_val = ws.cell(row=r_idx, column=col_idx).value
        # Check if attribute exists on res (it might be a dict or object)
        try:
            py_val = getattr(res, py_key) if hasattr(res, py_key) else res.get(py_key)
        except:
            continue
            
        if xl_val is None or py_val is None:
            continue
            
        try:
            diff = abs(py_val - xl_val)
            mismatches.append((r_idx, label, py_val, xl_val, diff))
            
            if label not in summaries or diff > summaries[label]:
                summaries[label] = diff
        except:
            continue

# Print top 30
mismatches.sort(key=lambda x: x[4], reverse=True)
print(f"{'Row':<5} {'Metric':<15} {'Py':<12} {'Xlsx':<12} {'Abs Diff':<12}")
for m in mismatches[:30]:
    print(f"{m[0]:<5} {m[1]:<15} {m[2]:<12.4f} {m[3]:<12.4f} {m[4]:<12.4f}")

print('\nPer-metric max abs diff summary:')
for label, max_diff in sorted(summaries.items()):
    print(f"{label:<15}: {max_diff:.4f}")

