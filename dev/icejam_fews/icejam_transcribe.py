"""
Yukon River at Dawson - Ice Breakup Model
Transcribed from Breakup_model_YukonRiver_2025.xlsx (Inputdata + SW sheets)

The model tracks daily ice deterioration and hydraulic forcing from April 1
through ~May 15, computing whether the resisting force (Fr) of the ice cover
has been overcome by the driving force (Fd) from backwater pressure.

Inputs per day:
    tair_max, tair_min  - daily air temperature (°C)
    cloud_obs           - observed cloud cover (%), None = use empirical estimate
    ow_pct              - % open water
    albedo_ice          - ice surface albedo
    Y_obs               - observed water level gauge reading (m)
    Y_rises             - daily observed water level change (m, can be negative)
    loc_jam             - local jam thickness/location parameter (-)
    jave                - average jam thickness (-)
"""

import math
import argparse
from datetime import date, timedelta
from typing import Optional


# =============================================================================
# FIXED PARAMETERS
# =============================================================================

# Ice characterization
freeze_up_intensity = 0.7       # G2: Freeze-up intensity (0 to 1)
ice_thickness_known = None      # G3: Known ice thickness (cm); None → use formula
CDDFmax = 2676                  # G4: Max cumulative freezing degree-days (°C-days)

# Rating curve  Q = ((Y + Ydatum - Ybed) / rating_a)^(1/rating_b)
Q_last = 585.0                  # AB2: Last measured discharge (m³/s)
Y_at_Q_last = 0.959             # AB3: Water level at last Q measurement (m)
rating_a = 0.049                # AB7
rating_b = 0.551                # AB8

# Physical constants / site geometry
rho_w = 1000.0                  # AF2: Water density (kg/m³)
g_accel = 9.81                  # AF3: Gravitational acceleration (m/s²)
Ybed = 309.95                   # AF4: Bed elevation (m asl)
Ydatum = 311.918                # AF5: Station datum elevation (m asl)
river_width = 350.0             # AL5: River width (m)

# Air temperature correction
Tair_corr_0 = 3.5               # F7: Initial temperature correction (°C)
Tair_corr_step = 0.05           # F8: Daily increment to correction

# Ice thickness melt
ECDD_melt_threshold = 80.0      # H7: ECDDT threshold for melt (°C-days)
melt_rate = 0.0015              # H8: Melt rate (m per °C-day above threshold)
tice_init_coeff = 0.018         # I8: Initial ice thickness coefficient

# Empirical cloud cover estimation from temperature range
cloud_range_max = 18.4          # L7
cloud_range_scale = 0.1124      # L8

# Solar radiation (SW sheet)
latitude_deg = 64.0             # H2 (degrees)
solar_constant = 1368.0         # I2 (W/m²)
south_sun_hour = 13             # E2: Hour of solar noon
DOY_april1 = 31 + 28 + 31 + 1  # F2: Day-of-year for April 1 (= 91, hardcoded in sheet)
mount_shade = 0.1               # I5: Mountain shading threshold on sin(elevation)

# Snow / albedo intensity thresholds
snow_int_high = 0.9             # R7
snow_int_low = 0.7              # R8

# Absorbed shortwave → resisting force
SWabs_norm = 2000.0             # T8: Normalization denominator
Frloss_exp1 = 1.2               # U7
Frloss_exp2 = 2.0               # U8
Fr_max_kPa = 100.0              # V8: Maximum resisting force (kPa)

# Hydraulics / backwater
Y_offset = 0.5                  # AE7: Depth offset in velocity calculation (m)
channel_area = 350.0            # AE8: Cross-section area parameter (m²)
jam_thickness_factor = 2.0      # AF8: Jam thickness multiplier for water column
jam_roughness_factor = 1.5      # AG8

# Manning's n
n_base = 0.02                   # AI6
n_freeze_coeff = 0.02           # coefficient on (freeze_up_intensity - 0.5)
n_melt_coeff = 0.05             # AI6 melt contribution coefficient
n_jam_linear = 0.03             # AI7
n_jam_quad = 0.02               # AI8

# Energy slope
slope_base = 0.0005             # AJ6
slope_jam_linear = 0.0006       # AJ7
slope_jam_quad = 0.0004         # AJ8

# Backwater threshold for driving force
BW_threshold_pct = 20.0         # AK8: Below this backwater %, driving force = 0

# BCC (breakup critical condition)
BCC_factor = 1500.0             # AL6
BCC_width = 12.0                # AL7
BCC_k = BCC_factor / (river_width - 2.0 * BCC_width)   # AL8 ≈ 4.601

# Model start date
year = 2025
year_offset = year - 2024       # B8 = 1
start_date = date(year - year_offset, 4, 1)   # April 1, 2024


# =============================================================================
# OBSERVED DAILY INPUT DATA  (46 rows: April 1 – May 16, 2024)
# Rows 12-56 in the spreadsheet.  Index 0 = April 1 (row 12).
# None means the cell was blank.
# =============================================================================

# =============================================================================
# OBSERVED DAILY INPUT DATA  (46 rows: April 1 – May 16, 2024)
# Rows 12-56 in the spreadsheet.  Index i = row (12+i) = April (1+i).
# None means the cell was blank.
# =============================================================================

# Tair Max (°C) – column C
tair_max_data = [
    2.7, 4.9, 10.8, 9.9, 11.9,    # i=0-4  (Apr 1-5,  rows 12-16)
    11.4, 8.8, 10.4, 8.7, 9.0,    # i=5-9  (Apr 6-10, rows 17-21)
    5.7, 6.2, 7.4, 8.7, 6.8,      # i=10-14 (Apr 11-15, rows 22-26)
    8.3, 11.7, 8.2, 7.7, 5.3,     # i=15-19 (Apr 16-20, rows 27-31)
    7.5, 8.9, 9.8, 6.2, 8.1,      # i=20-24 (Apr 21-25, rows 32-36)
    11.7, 13.8, 15.3, 10.0, 15.0, # i=25-29 (Apr 26-30, rows 37-41)
    10.0, 7.0, 7.0, 7.0, None,    # i=30-34 (May 1-5,  rows 42-46)
    None, None, None, None, None,  # i=35-39 (May 6-10)
    None, None, None, None, None, None,  # i=40-45 (May 11-16)
]

# Tair Min (°C) – column D
tair_min_data = [
    -8.9, -9.6, -4.2, -4.6, -2.9,   # i=0-4
    -1.0, -2.8, -4.5, -1.4, -4.9,   # i=5-9
    -1.9, -4.3, -5.4, -0.5, -5.1,   # i=10-14
    -3.8,  3.7,  1.6, -1.0, -2.6,   # i=15-19
    -4.4, -3.5, -3.9, -0.8, -1.9,   # i=20-24
    -4.9, -1.3,  1.4,  1.0,  5.0,   # i=25-29
     0.0, -3.0, -2.0,  0.0, None,   # i=30-34
    None, None, None, None, None,
    None, None, None, None, None, None,
]

# Observed cloud cover (%), None = use empirical estimate (column K)
cloud_obs_data = [
    None, 25, None, 25, None,     # i=0-4
      50, 30,  40, 20,  20,       # i=5-9
      80,  5,  90, 60,  50,       # i=10-14
      75, 70,  70, 60,  40,       # i=15-19
      30, 70, None, 50,  90,      # i=20-24
    None, None, 40,  60,  10,     # i=25-29
    None, None, None, None, None,
    None, None, None, None, None,
    None, None, None, None, None, None,
]

# % open water (column N)
ow_pct_data = [
    3.0, 3.0, 3.0, 3.0, 3.0,        # i=0-4
    3.0, 3.0, 3.0, 3.0, 3.0,        # i=5-9
    3.0, 3.5, 4.0, 4.5, 5.0,        # i=10-14
    5.5, 6.0, 6.5, 7.0, 7.5,        # i=15-19
    8.0, 9.0, 10.0, 10.0, 10.0,     # i=20-24
    16.0, 18.0, 20.0, 22.0, 27.0,   # i=25-29
    29.0, 31.0, 33.0, 35.0, None,   # i=30-34
    None, None, None, None, None,
    None, None, None, None, None, None,
]

# Ice surface albedo (column O)
albedo_ice_data = [
    0.900, 0.900, 0.900, 0.900, 0.900,  # i=0-4
    0.900, 0.900, 0.900, 0.890, 0.880,  # i=5-9
    0.875, 0.868, 0.861, 0.854, 0.847,  # i=10-14
    0.840, 0.833, 0.826, 0.819, 0.800,  # i=15-19
    0.781, 0.762, 0.743, 0.700, 0.850,  # i=20-24 (i=24 fresh snow: O=0.850)
    0.800, 0.750, 0.740, 0.730, 0.720,  # i=25-29
    0.710, 0.700, 0.690, 0.680, None,   # i=30-34
    None, None, None, None, None,
    None, None, None, None, None, None,
]

# Observed water level gauge reading (m) (column X)
Y_obs_data = [
    0.902, 0.888, 0.868, 0.849, 0.834,       # i=0-4
    0.820, 0.818, 0.827, 0.857, 0.869,       # i=5-9
    0.872, 0.902, 0.928, 0.926, 0.934,       # i=10-14
    0.937, 0.946, 0.980, 1.032, 1.101,       # i=15-19
    1.178, 1.201, 1.205, 1.210, 1.277,       # i=20-24
    1.251, 1.338, 1.400, 1.598, 1.7852,      # i=25-29
    2.06168, 2.393456, 2.6174048, 2.79656384, None,  # i=30-34
    None, None, None, None, None,
    None, None, None, None, None, None,
]

# Observed daily water-level change / "rises" (m) (column Y, can be negative)
Y_rises_data = [
    None, None, None, None, None,      # i=0-4
    None, None, None, None, None,      # i=5-9
    None, None, None, None, None,      # i=10-14
    None, None, None, None, None,      # i=15-19
    None, None, None, None, -0.08,     # i=20-24 (i=24, Apr 25, row 36)
    -0.03, -0.02, -0.04, None, None,   # i=25-29
    None, None, None, None, None,
    None, None, None, None, None,
    None, None, None, None, None, None,
]

# Local jam location / thickness parameter (column AF, dimensionless)
loc_jam_data: list[Optional[float]] = [None] * 46   # all blank in current dataset

# Average jam thickness (column AG, dimensionless)
jave_data: list[Optional[float]] = [None] * 46
jave_data[29] = 0.6   # AG41 = 0.6 (index 29 = row 41 = April 30)

# Initial backwater % override (row 12 only, column AB)
BW_initial = 64.0


# =============================================================================
# SOLAR RADIATION LOOKUP  (replaces SW sheet)
# Compute daily average max solar radiation (SWmax24) for each day of the run.
# =============================================================================

def compute_SWmax24(doy: int) -> float:
    """
    Compute 24-hour average solar radiation (W/m²) for a given day-of-year.
    Mirrors the SW sheet logic: hourly irradiance averaged over the full day.
    """
    lat_rad = math.radians(latitude_deg)
    decl_rad = math.radians(23.45 * math.cos(math.radians(360.0 / 365.0 * (172 - doy))))
    total = 0.0
    for t in range(24):
        hour_angle_rad = math.radians((t - south_sun_hour) * 15.0)
        sin_elev = (
            math.sin(lat_rad) * math.sin(decl_rad)
            + math.cos(lat_rad) * math.cos(decl_rad) * math.cos(hour_angle_rad)
        )
        sw = 0.0 if (sin_elev - mount_shade) < 0 else solar_constant * sin_elev
        total += sw
    return total / 24.0


# Pre-compute SWmax24 for each of the 46 days (Apr 1 through May 16)
sw_table = [compute_SWmax24(DOY_april1 + i) for i in range(46)]


# =============================================================================
# RATING CURVE HELPERS
# =============================================================================

def rating_Q(Y_gauge: float) -> float:
    """Free-flowing Q from gauge reading (m³/s). Q = ((Y+datum-bed)/a)^(1/b)"""
    h = Y_gauge + Ydatum - Ybed
    return (h / rating_a) ** (1.0 / rating_b)


def rating_Y(Q: float) -> float:
    """Invert rating curve: gauge reading from Q."""
    return rating_a * (Q ** rating_b) + Ybed - Ydatum


# =============================================================================
# MAIN COMPUTATION LOOP
# =============================================================================

def run_model():
    n_days = 46
    results = []

    # State variables carried forward
    ECDDT_prev = 0.0
    tice = 0.0
    tice_initial = 0.0
    SWabs_cum = 0.0
    cumul_rises = 0.0  # Z: cumulative observed level changes
    BW_prev = BW_initial
    AD_prev = 0.0      # YBW accumulator (set on first day)

    for i in range(n_days):
        day_date = start_date + timedelta(days=i)
        tmax = tair_max_data[i]
        tmin = tair_min_data[i]

        # Skip days with no input data
        if tmax is None or tmin is None:
            results.append({"date": day_date, "note": "no data"})
            continue

        # --- Air temperature (E) ---
        tair = (tmax + tmin) / 2.0

        # --- Temperature correction (F): increases by F8 each day ---
        tair_corr = Tair_corr_0 + i * Tair_corr_step

        # --- Effective cooling degree days (G) ---
        if i == 0:
            ECDDT = max(0.0, tair + tair_corr)
        else:
            ECDDT = max(0.0, tair + tair_corr + ECDDT_prev)
        ECDDT_prev = ECDDT

        # --- Ice thickness reduction (H) ---
        tice_red = 0.0 if ECDDT < ECDD_melt_threshold else (ECDDT - ECDD_melt_threshold) * melt_rate

        # --- Ice thickness (I) ---
        ow = ow_pct_data[i] if ow_pct_data[i] is not None else 0.0
        if i == 0:
            if ice_thickness_known is not None:
                tice = ice_thickness_known / 100.0
            else:
                tice = (tice_init_coeff * math.sqrt(CDDFmax)
                        + max(0.0, freeze_up_intensity - 0.5))
            tice_initial = tice
        else:
            if ow > 80.0:
                tice = 0.0
            else:
                tice = max(0.0, tice_initial - tice_red)

        # --- Solar radiation lookup (J) ---
        SWmax24 = sw_table[i]

        # --- Empirical cloud cover (L) ---
        delta_T = tmax - tmin
        cloud_emp = max(0.0, min(100.0, (cloud_range_max - delta_T) / cloud_range_scale))

        # --- Net shortwave (M) ---
        k_cloud = cloud_obs_data[i]
        cloud_frac = (k_cloud / 100.0) if k_cloud is not None else (cloud_emp / 100.0)
        SW_net = (1.0 - 0.12) * SWmax24 * (0.35 + 0.5 * (1.0 - cloud_frac))

        # --- Albedo average (P) ---
        albedo_ice = albedo_ice_data[i] if albedo_ice_data[i] is not None else 0.9
        albedo_ave = (ow / 100.0) * 0.1 + (1.0 - ow / 100.0) * albedo_ice

        # --- Net shortwave per ice depth (Q) ---
        SWnet_depth = (1.0 - albedo_ave) * SW_net / tice if tice > 0 else 0.0

        # --- Snow intensity factor (R) ---
        eff_T = tair + tair_corr
        if eff_T < 0.0:
            snow_int = 0.0
        elif albedo_ice >= snow_int_high:
            snow_int = 0.0
        elif albedo_ice < snow_int_low:
            snow_int = 1.0
        else:
            snow_int = (snow_int_high / (snow_int_high - snow_int_low)
                        - (1.0 / (snow_int_high - snow_int_low)) * albedo_ice)

        # --- Absorbed shortwave (S, cumulative) ---
        increment = SWnet_depth * snow_int
        if i == 0:
            SWabs_cum = increment
        else:
            SWabs_cum += increment

        # --- Ratio & force loss (T, U, V, W) ---
        ratio = SWabs_cum / SWabs_norm
        Frloss_pct = (1.0 - ratio ** Frloss_exp1) ** Frloss_exp2 * 100.0
        Frloss_pct = max(0.0, min(100.0, Frloss_pct))
        Fr_kPa = 0.0 if ow > 80.0 else (Frloss_pct / 100.0) * Fr_max_kPa
        Fr_MN = Fr_kPa * tice * river_width / 1000.0

        # --- Observed water level (X) and rises (Y) ---
        Y_obs = Y_obs_data[i]
        if Y_obs is None:
            results.append({"date": day_date, "note": "no level data"})
            continue

        yr = Y_rises_data[i] if Y_rises_data[i] is not None else 0.0

        # --- Cumulative rises (Z) and smoothed level (AA) ---
        if i == 0:
            cumul_rises = yr
        else:
            cumul_rises += yr
        smooth_Y = Y_obs - cumul_rises

        # --- Backwater % (AB), estimated Q (AC), YBW accumulator (AD) ---
        if i == 0:
            BW_pct = BW_initial
            Q_est = (1.0 - BW_pct / 100.0) * rating_Q(Y_obs)
            AD_cur = Y_obs - (rating_a * (Q_est ** rating_b) + Ybed - Ydatum)
        else:
            AD_cur = AD_prev + yr
            h_adj = Y_obs - AD_cur + Ydatum - Ybed
            Q_est = (h_adj / rating_a) ** (1.0 / rating_b) if h_adj > 0 else 0.0
            Q_open = rating_Q(Y_obs)
            BW_pct = (1.0 - Q_est / Q_open) * 100.0 if Q_open > 0 else 0.0

        AD_prev = AD_cur

        # --- Backwater depth (AD already computed above as AD_cur) ---
        YBW = AD_cur

        # --- Flow velocity (AE) ---
        depth_for_vel = Y_obs + Ydatum - Ybed - Y_offset - 0.92 * tice
        U_vel = Q_est / (channel_area * depth_for_vel) if depth_for_vel > 0 else 0.0

        # --- Local jam parameters ---
        loc_jam_raw = loc_jam_data[i]
        jave_raw = jave_data[i]
        loc_jam = float(loc_jam_raw) if loc_jam_raw is not None else 0.0
        jave = float(jave_raw) if jave_raw is not None else 0.0

        # --- Water column height (AH) ---
        Ywcol = (Y_obs + Ydatum - (Ybed + Y_offset)
                 - 0.92 * tice
                 - loc_jam * jam_thickness_factor)

        # --- Manning's n (AI) ---
        n_coeff = (n_base
                   + n_freeze_coeff * max(0.0, freeze_up_intensity - 0.5)
                   + n_melt_coeff * tice_red
                   + n_jam_linear * loc_jam
                   + n_jam_quad * loc_jam ** 2)

        # --- Energy slope (AJ) ---
        slope = slope_base + slope_jam_linear * jave + slope_jam_quad * jave ** 2

        # --- Driving force Fd (AK, kPa) ---
        if BW_pct < BW_threshold_pct:
            Fd_kPa = 0.0
        else:
            hydraulic_R = Ywcol / 2.0 if Ywcol > 0 else 1e-9
            Fd_kPa = (
                (rho_w * g_accel * n_coeff ** 2
                 * (U_vel + jave * jam_roughness_factor) ** 2
                 / (hydraulic_R ** (1.0 / 3.0))) / 1000.0
                + rho_w * g_accel * 0.92 * (tice + loc_jam * jam_thickness_factor)
                * slope / 1000.0
            )

        # --- BCC (AL, kPa) and driving force in MN (AM) ---
        if tice_initial and tice_initial > 0:
            BCC_kPa = (8.0 * (BCC_width * 2.0) * Fd_kPa * BCC_k ** 2
                       / ((BCC_k - 0.5) * tice_initial))
            # Excel AM uses the current daily ice thickness (column I), not initial thickness.
            Fd_MN = BCC_kPa * river_width * tice / 1000.0
        else:
            BCC_kPa = 0.0
            Fd_MN = 0.0

        results.append({
            "date":         day_date,
            "tair":         tair,
            "tair_corr":    tair_corr,
            "ECDDT":        ECDDT,
            "tice_red":     tice_red,
            "tice":         tice,
            "SWmax24":      SWmax24,
            "cloud_emp":    cloud_emp,
            "SW_net":       SW_net,
            "ow_pct":       ow,
            "albedo_ave":   albedo_ave,
            "SWnet_depth":  SWnet_depth,
            "snow_int":     snow_int,
            "SWabs_cum":    SWabs_cum,
            "ratio":        ratio,
            "Frloss_pct":   Frloss_pct,
            "Fr_kPa":       Fr_kPa,
            "Fr_MN":        Fr_MN,
            "Y_obs":        Y_obs,
            "cumul_rises":  cumul_rises,
            "smooth_Y":     smooth_Y,
            "BW_pct":       BW_pct,
            "Q_est":        Q_est,
            "YBW":          YBW,
            "U_vel":        U_vel,
            "loc_jam":      loc_jam,
            "jave":         jave,
            "Ywcol":        Ywcol,
            "n_coeff":      n_coeff,
            "slope":        slope,
            "Fd_kPa":       Fd_kPa,
            "BCC_kPa":      BCC_kPa,
            "Fd_MN":        Fd_MN,
            "breakup":      Fd_MN > Fr_MN,
        })

    return results


def _require_matplotlib():
    """Import matplotlib only when plotting is requested."""
    try:
        # matplotlib is an optional dependency for this module.
        import matplotlib.pyplot as plt
    except ImportError as exc:
        raise ImportError(
            "matplotlib is required for plotting. Install with: pip install matplotlib"
        ) from exc
    return plt


def _valid_rows(rows):
    """Filter rows that contain full computed outputs."""
    return [r for r in rows if "note" not in r]


def _breakup_start_date(rows):
    """Return first breakup date if present, otherwise None."""
    for r in rows:
        if r.get("breakup"):
            return r["date"]
    return None


def _full_plot_limits():
    """Return fixed x-axis bounds for the full modeled period (Apr 1 to May 16)."""
    return start_date, start_date + timedelta(days=45)


def _series_with_gaps(rows, key):
    """Return a numeric series with NaN for rows without computed values."""
    return [r.get(key, float("nan")) for r in rows]


def _first_no_data_date(rows):
    """Return first date that has a no-data note, otherwise None."""
    for r in rows:
        if "note" in r:
            return r["date"]
    return None


def plot_inputs(rows=None, show=True, save_path=None):
    """
    Plot key model inputs over time.

    Panels:
      1) Air temperatures (max/min/mean)
      2) Cloud and open-water observations
      3) Albedo and observed water level
    """
    plt = _require_matplotlib()
    rows = run_model() if rows is None else rows

    dates = [r["date"] for r in rows]
    tair = _series_with_gaps(rows, "tair")
    tair_max = [tair_max_data[i] for i in range(len(rows))]
    tair_min = [tair_min_data[i] for i in range(len(rows))]
    cloud_obs = [cloud_obs_data[i] for i in range(len(rows))]
    cloud_emp = _series_with_gaps(rows, "cloud_emp")
    ow_pct = _series_with_gaps(rows, "ow_pct")
    albedo = [albedo_ice_data[i] if albedo_ice_data[i] is not None else float("nan") for i in range(len(rows))]
    y_obs = _series_with_gaps(rows, "Y_obs")

    fig, axes = plt.subplots(3, 1, figsize=(12, 10), sharex=True)

    axes[0].plot(dates, tair_max, label="Tair max", color="#d95f02", lw=1.8)
    axes[0].plot(dates, tair_min, label="Tair min", color="#1b9e77", lw=1.8)
    axes[0].plot(dates, tair, label="Tair mean", color="#7570b3", lw=2.0)
    axes[0].set_ylabel("Temperature (C)")
    axes[0].set_title("Model Inputs: Weather and Hydrograph")
    axes[0].grid(alpha=0.25)
    axes[0].legend(loc="best")

    axes[1].plot(dates, cloud_emp, label="Cloud empirical", color="#66a61e", lw=2.0)
    axes[1].scatter(
        dates,
        [v if v is not None else float("nan") for v in cloud_obs],
        label="Cloud observed",
        color="#e7298a",
        s=20,
        zorder=3,
    )
    ax1b = axes[1].twinx()
    ax1b.plot(dates, ow_pct, label="Open water", color="#1f78b4", lw=1.8, ls="--")
    axes[1].set_ylabel("Cloud (%)")
    ax1b.set_ylabel("Open water (%)")
    axes[1].grid(alpha=0.25)
    l1, lb1 = axes[1].get_legend_handles_labels()
    l2, lb2 = ax1b.get_legend_handles_labels()
    axes[1].legend(l1 + l2, lb1 + lb2, loc="best")

    axes[2].plot(dates, y_obs, label="Observed water level", color="#2c7fb8", lw=2.0)
    ax2b = axes[2].twinx()
    ax2b.plot(dates, albedo, label="Ice albedo", color="#fdae61", lw=1.8)
    axes[2].set_ylabel("Gauge level (m)")
    ax2b.set_ylabel("Albedo (-)")
    axes[2].set_xlabel("Date")
    axes[2].grid(alpha=0.25)
    l1, lb1 = axes[2].get_legend_handles_labels()
    l2, lb2 = ax2b.get_legend_handles_labels()
    axes[2].legend(l1 + l2, lb1 + lb2, loc="best")

    x0, x1 = _full_plot_limits()
    for ax in axes:
        ax.set_xlim(x0, x1)

    no_data_start = _first_no_data_date(rows)
    if no_data_start is not None:
        axes[0].axvspan(no_data_start, x1, color="#d9d9d9", alpha=0.15, label="No input data")
        axes[0].legend(loc="best")

    fig.autofmt_xdate()
    fig.tight_layout()
    if save_path:
        fig.savefig(save_path, dpi=180, bbox_inches="tight")
    if show:
        plt.show()
    return fig, axes


def plot_state(rows=None, show=True, save_path=None):
    """
    Plot core model state variables through time.

    Panels:
      1) ECDDT and ice thickness
      2) SW forcing chain (SWmax24, SWabs cumulative)
      3) Backwater and estimated discharge
    """
    plt = _require_matplotlib()
    rows = run_model() if rows is None else rows
    dates = [r["date"] for r in rows]

    ecddt = _series_with_gaps(rows, "ECDDT")
    tice = _series_with_gaps(rows, "tice")
    swmax24 = _series_with_gaps(rows, "SWmax24")
    swabs = _series_with_gaps(rows, "SWabs_cum")
    bw = _series_with_gaps(rows, "BW_pct")
    qest = _series_with_gaps(rows, "Q_est")

    fig, axes = plt.subplots(3, 1, figsize=(12, 10), sharex=True)

    axes[0].plot(dates, ecddt, label="ECDDT", color="#e6550d", lw=2.0)
    ax0b = axes[0].twinx()
    ax0b.plot(dates, tice, label="Ice thickness", color="#3182bd", lw=2.0)
    axes[0].axhline(ECDD_melt_threshold, color="#969696", ls=":", lw=1.5, label="Melt threshold")
    axes[0].set_ylabel("ECDDT (C-days)")
    ax0b.set_ylabel("Ice thickness (m)")
    axes[0].set_title("Model State Evolution")
    axes[0].grid(alpha=0.25)
    l1, lb1 = axes[0].get_legend_handles_labels()
    l2, lb2 = ax0b.get_legend_handles_labels()
    axes[0].legend(l1 + l2, lb1 + lb2, loc="best")

    axes[1].plot(dates, swmax24, label="SWmax24", color="#31a354", lw=2.0)
    ax1b = axes[1].twinx()
    ax1b.plot(dates, swabs, label="SWabs cumulative", color="#756bb1", lw=2.0)
    axes[1].set_ylabel("SWmax24 (W/m2)")
    ax1b.set_ylabel("SWabs cumulative")
    axes[1].grid(alpha=0.25)
    l1, lb1 = axes[1].get_legend_handles_labels()
    l2, lb2 = ax1b.get_legend_handles_labels()
    axes[1].legend(l1 + l2, lb1 + lb2, loc="best")

    axes[2].plot(dates, bw, label="Backwater", color="#de2d26", lw=2.0)
    ax2b = axes[2].twinx()
    ax2b.plot(dates, qest, label="Estimated discharge", color="#2b8cbe", lw=2.0)
    axes[2].set_ylabel("Backwater (%)")
    ax2b.set_ylabel("Qest (m3/s)")
    axes[2].set_xlabel("Date")
    axes[2].grid(alpha=0.25)
    l1, lb1 = axes[2].get_legend_handles_labels()
    l2, lb2 = ax2b.get_legend_handles_labels()
    axes[2].legend(l1 + l2, lb1 + lb2, loc="best")

    x0, x1 = _full_plot_limits()
    for ax in axes:
        ax.set_xlim(x0, x1)

    no_data_start = _first_no_data_date(rows)
    if no_data_start is not None:
        axes[0].axvspan(no_data_start, x1, color="#d9d9d9", alpha=0.15, label="No input data")
        axes[0].legend(loc="best")

    fig.autofmt_xdate()
    fig.tight_layout()
    if save_path:
        fig.savefig(save_path, dpi=180, bbox_inches="tight")
    if show:
        plt.show()
    return fig, axes


def plot_forces(rows=None, show=True, save_path=None):
    """
    Plot breakup forcing vs resisting force and identify first breakup day.
    """
    plt = _require_matplotlib()
    rows = run_model() if rows is None else rows
    dates = [r["date"] for r in rows]

    fr_mn = _series_with_gaps(rows, "Fr_MN")
    fd_mn = _series_with_gaps(rows, "Fd_MN")
    breakup_flags = [bool(r.get("breakup", False)) for r in rows]
    bw = _series_with_gaps(rows, "BW_pct")

    fig, axes = plt.subplots(2, 1, figsize=(12, 8), sharex=True)

    axes[0].plot(dates, fr_mn, label="Resisting force Fr", color="#1b9e77", lw=2.5)
    axes[0].plot(dates, fd_mn, label="Driving force Fd", color="#d95f02", lw=2.5)
    axes[0].fill_between(dates, fr_mn, fd_mn, where=[fd > fr for fd, fr in zip(fd_mn, fr_mn)],
                         color="#fc8d62", alpha=0.25, label="Fd > Fr")

    first_break = _breakup_start_date(rows)
    if first_break is not None:
        axes[0].axvline(first_break, color="#636363", ls="--", lw=1.7,
                        label=f"First breakup: {first_break}")

    axes[0].set_ylabel("Force (MN)")
    axes[0].set_title("Breakup Condition: Driving Force vs Resisting Force")
    axes[0].grid(alpha=0.25)
    axes[0].legend(loc="best")

    axes[1].plot(dates, bw, color="#3182bd", lw=2.0, label="Backwater (%)")
    axes[1].axhline(BW_threshold_pct, color="#969696", ls=":", lw=1.5,
                    label="Driving-force threshold")
    axes[1].scatter(
        [d for d, b in zip(dates, breakup_flags) if b],
        [v for v, b in zip(bw, breakup_flags) if b],
        color="#e31a1c",
        s=18,
        label="Breakup flagged",
        zorder=3,
    )
    axes[1].set_ylabel("Backwater (%)")
    axes[1].set_xlabel("Date")
    axes[1].grid(alpha=0.25)
    axes[1].legend(loc="best")

    x0, x1 = _full_plot_limits()
    for ax in axes:
        ax.set_xlim(x0, x1)

    no_data_start = _first_no_data_date(rows)
    if no_data_start is not None:
        axes[0].axvspan(no_data_start, x1, color="#d9d9d9", alpha=0.15, label="No input data")
        axes[0].legend(loc="best")

    fig.autofmt_xdate()
    fig.tight_layout()
    if save_path:
        fig.savefig(save_path, dpi=180, bbox_inches="tight")
    if show:
        plt.show()
    return fig, axes


def plot_all(rows=None, show=True):
    """Create all three summary figures and optionally show them."""
    rows = run_model() if rows is None else rows
    figs = []
    figs.append(plot_inputs(rows=rows, show=False)[0])
    figs.append(plot_state(rows=rows, show=False)[0])
    figs.append(plot_forces(rows=rows, show=False)[0])

    if show:
        plt = _require_matplotlib()
        plt.show()
    return figs


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Run Yukon River breakup model and optionally generate plots."
    )
    parser.add_argument(
        "--plot",
        action="store_true",
        help="Generate model plots (interactive window unless --no-show is used).",
    )
    parser.add_argument(
        "--save-plots-dir",
        default=None,
        help="Directory to save plots as PNG files (plot_inputs.png, plot_state.png, plot_forces.png).",
    )
    parser.add_argument(
        "--no-show",
        action="store_true",
        help="Do not open interactive plot windows (useful in headless terminals).",
    )
    args = parser.parse_args()

    rows = run_model()
    header = (
        f"{'Date':<12} {'Tair':>6} {'ECDDT':>7} {'tice':>6} {'SWmax24':>8} "
        f"{'Fr_kPa':>8} {'Fr_MN':>7} {'BW%':>6} {'Qest':>7} {'Fd_kPa':>8} "
        f"{'Fd_MN':>7} {'Break':>6}"
    )
    print(header)
    print("-" * len(header))
    for r in rows:
        if "note" in r:
            print(f"{str(r['date']):<12}  {r['note']}")
            continue
        print(
            f"{str(r['date']):<12} "
            f"{r['tair']:>6.1f} "
            f"{r['ECDDT']:>7.1f} "
            f"{r['tice']:>6.3f} "
            f"{r['SWmax24']:>8.1f} "
            f"{r['Fr_kPa']:>8.1f} "
            f"{r['Fr_MN']:>7.2f} "
            f"{r['BW_pct']:>6.1f} "
            f"{r['Q_est']:>7.1f} "
            f"{r['Fd_kPa']:>8.4f} "
            f"{r['Fd_MN']:>7.2f} "
            f"{'YES' if r['breakup'] else 'no':>6}"
        )

    should_show = (not args.no_show) and (args.plot or args.save_plots_dir is None)
    if args.plot or args.save_plots_dir:
        inputs_path = None
        state_path = None
        forces_path = None
        if args.save_plots_dir:
            import os

            os.makedirs(args.save_plots_dir, exist_ok=True)
            inputs_path = os.path.join(args.save_plots_dir, "plot_inputs.png")
            state_path = os.path.join(args.save_plots_dir, "plot_state.png")
            forces_path = os.path.join(args.save_plots_dir, "plot_forces.png")

        plot_inputs(rows=rows, show=False, save_path=inputs_path)
        plot_state(rows=rows, show=False, save_path=state_path)
        plot_forces(rows=rows, show=False, save_path=forces_path)

        if should_show:
            plt = _require_matplotlib()
            plt.show()
