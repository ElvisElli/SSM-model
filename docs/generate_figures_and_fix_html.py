"""
Generate illustrative function plots for SSM Soybean documentation
and produce a self-contained HTML by embedding all images as base64.
"""

import os, base64, re, math
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.gridspec import GridSpec

FIGURES_DIR = os.path.join(os.path.dirname(__file__), "figures")
os.makedirs(FIGURES_DIR, exist_ok=True)

PLOTS_DIR   = os.path.join(os.path.dirname(__file__), "..", "r-model", "outputs", "plots")
HTML_IN     = os.path.join(os.path.dirname(__file__), "SSM_Soybean_Documentation.html")
HTML_OUT    = os.path.join(os.path.dirname(__file__), "SSM_Soybean_Documentation.html")

# ── colour palette consistent with the HTML ──────────────────────────────────
C_BLUE   = "#1a5276"
C_ACCENT = "#2e86c1"
C_GREEN  = "#1e8449"
C_ORANGE = "#d35400"
C_RED    = "#c0392b"
C_GRAY   = "#7f8c8d"

STYLE = {
    "figure.facecolor":  "white",
    "axes.facecolor":    "#f8f9fa",
    "axes.edgecolor":    "#dee2e6",
    "axes.grid":         True,
    "grid.color":        "#dee2e6",
    "grid.linewidth":    0.6,
    "font.family":       "sans-serif",
    "font.size":         10,
    "axes.titlesize":    11,
    "axes.titleweight":  "bold",
    "axes.labelsize":    10,
    "xtick.labelsize":   9,
    "ytick.labelsize":   9,
    "legend.fontsize":   9,
    "legend.framealpha": 0.85,
}


def savefig(fig, name):
    path = os.path.join(FIGURES_DIR, name)
    fig.savefig(path, dpi=150, bbox_inches="tight")
    plt.close(fig)
    print(f"  saved {path}")
    return path


# =============================================================================
# 1. Temperature response (trapezoidal)
# =============================================================================
def plot_temperature_response():
    with plt.rc_context(STYLE):
        fig, axes = plt.subplots(1, 2, figsize=(10, 4), sharey=True)

        def trapezoid(T, tb, tp1, tp2, tc):
            out = np.zeros_like(T, dtype=float)
            for i, t in enumerate(T):
                if t <= tb or t >= tc:
                    out[i] = 0
                elif t < tp1:
                    out[i] = (t - tb) / (tp1 - tb)
                elif t <= tp2:
                    out[i] = 1.0
                else:
                    out[i] = (tc - t) / (tc - tp2)
            return out

        T = np.linspace(0, 50, 500)

        # Phenology (BD accumulation)
        ax = axes[0]
        y = trapezoid(T, 7, 27, 34, 45)
        ax.plot(T, y, color=C_BLUE, lw=2.2, label="Phenology (BD)")
        ax.axvline(7,  color=C_GRAY,   lw=1, ls="--", alpha=0.7)
        ax.axvline(27, color=C_GREEN,  lw=1, ls="--", alpha=0.7)
        ax.axvline(34, color=C_GREEN,  lw=1, ls="--", alpha=0.7)
        ax.axvline(45, color=C_GRAY,   lw=1, ls="--", alpha=0.7)
        ax.set_xlabel("Mean daily temperature (°C)")
        ax.set_ylabel("Normalized response  f_T  (0–1)")
        ax.set_title("Phenology temperature response\n(TBD=7, TP1D=27, TP2D=34, TCD=45)")
        ax.set_xlim(0, 50);  ax.set_ylim(-0.05, 1.15)
        for x, lbl, ha in [(7,"TB",  "right"),(27,"TP1","left"),
                            (34,"TP2","right"),(45,"TC", "left")]:
            ax.text(x, 1.05, lbl, ha=ha, va="bottom", fontsize=8, color=C_GRAY)

        # RUE
        ax = axes[1]
        y = trapezoid(T, 10, 20, 30, 40)
        ax.plot(T, y, color=C_ORANGE, lw=2.2, label="RUE (TCFRUE)")
        ax.axvline(10, color=C_GRAY,   lw=1, ls="--", alpha=0.7)
        ax.axvline(20, color=C_GREEN,  lw=1, ls="--", alpha=0.7)
        ax.axvline(30, color=C_GREEN,  lw=1, ls="--", alpha=0.7)
        ax.axvline(40, color=C_GRAY,   lw=1, ls="--", alpha=0.7)
        ax.set_xlabel("Mean daily temperature (°C)")
        ax.set_title("RUE temperature response\n(TBRUE=10, TP1RUE=20, TP2RUE=30, TCRUE=40)")
        ax.set_xlim(0, 50);  ax.set_ylim(-0.05, 1.15)
        for x, lbl, ha in [(10,"TB","right"),(20,"TP1","left"),
                            (30,"TP2","right"),(40,"TC","left")]:
            ax.text(x, 1.05, lbl, ha=ha, va="bottom", fontsize=8, color=C_GRAY)

        fig.suptitle("Trapezoidal Temperature Response Functions", fontsize=12,
                     fontweight="bold", color=C_BLUE, y=1.02)
        fig.tight_layout()
    return savefig(fig, "func_temperature_response.png")


# =============================================================================
# 2. Photoperiod response (soybean = short-day plant)
# =============================================================================
def plot_photoperiod_response():
    with plt.rc_context(STYLE):
        fig, ax = plt.subplots(figsize=(7, 4))

        cpp   = 13.09    # critical photoperiod (h)
        ppsen = -0.294   # negative → short-day plant

        pp = np.linspace(8, 18, 500)
        # ppfun = max(0, 1 + ppsen*(pp - cpp))  clipped to [0,1]
        ppfun = np.clip(1 + ppsen * (pp - cpp), 0, 1)

        ax.plot(pp, ppfun, color=C_BLUE, lw=2.5)
        ax.axvline(cpp, color=C_GREEN, lw=1.2, ls="--", label=f"cpp = {cpp} h (critical photoperiod)")
        ax.fill_between(pp, ppfun, alpha=0.12, color=C_BLUE)

        ax.set_xlabel("Effective daylength, pp  (h)  [DAYL + 0.9]")
        ax.set_ylabel("Photoperiod response  f_pp  (0–1)")
        ax.set_title("Soybean Photoperiod Response Function\n"
                     f"(short-day plant: ppsen = {ppsen}, cpp = {cpp} h)\n"
                     "Active between bdBRP (emergence) and bdTRP (flowering)")
        ax.set_xlim(8, 18);  ax.set_ylim(-0.05, 1.15)
        ax.axhline(0, color="black", lw=0.6)
        ax.axhline(1, color=C_GRAY,  lw=0.6, ls=":")
        ax.legend(loc="upper right")

        # annotate regions
        ax.annotate("Short days\n→ fast development", xy=(9.5, 0.9),
                    fontsize=9, color=C_GREEN,
                    bbox=dict(boxstyle="round,pad=0.3", fc="white", ec=C_GREEN, alpha=0.8))
        ax.annotate("Long days\n→ slow development", xy=(14.5, 0.1),
                    fontsize=9, color=C_RED,
                    bbox=dict(boxstyle="round,pad=0.3", fc="white", ec=C_RED, alpha=0.8))

        fig.tight_layout()
    return savefig(fig, "func_photoperiod_response.png")


# =============================================================================
# 3. Beer's law light interception
# =============================================================================
def plot_beer_law():
    with plt.rc_context(STYLE):
        fig, axes = plt.subplots(1, 2, figsize=(10, 4))

        LAI = np.linspace(0, 6, 300)

        # Left: FINT vs LAI for different KPAR
        ax = axes[0]
        for kpar, col, lbl in [(0.5, C_BLUE,   "KPAR = 0.5"),
                                (0.65, C_ACCENT, "KPAR = 0.65 (soybean)"),
                                (0.8, C_ORANGE, "KPAR = 0.8")]:
            fint = 1 - np.exp(-kpar * LAI)
            ax.plot(LAI, fint, color=col, lw=2.2, label=lbl)
        ax.axhline(0.95, color=C_GRAY, lw=1, ls=":", label="95 % interception")
        ax.set_xlabel("Leaf Area Index (LAI,  m²/m²)")
        ax.set_ylabel("Fraction of PAR intercepted  (FINT)")
        ax.set_title("Beer's Law PAR Interception\nFINT = 1 − exp(−KPAR × LAI)")
        ax.set_xlim(0, 6);  ax.set_ylim(0, 1.05)
        ax.legend(loc="lower right")

        # Right: daily DM production vs LAI for typical SRAD
        ax = axes[1]
        SRAD = 15  # MJ/m²/d typical summer
        RUE  = 2.0
        KPAR = 0.65
        fint = 1 - np.exp(-KPAR * LAI)
        ddmp = SRAD * 0.48 * fint * RUE
        ax.plot(LAI, ddmp, color=C_GREEN, lw=2.5)
        ax.fill_between(LAI, ddmp, alpha=0.15, color=C_GREEN)
        ax.axvline(3.5, color=C_GRAY, lw=1, ls="--")
        ax.text(3.6, 1, "LAI = 3.5\n(~90 % interception)", fontsize=8, color=C_GRAY)
        ax.set_xlabel("Leaf Area Index (LAI,  m²/m²)")
        ax.set_ylabel("Daily DM production  (g/m²/d)")
        ax.set_title(f"Typical DM Production vs LAI\n(SRAD={SRAD} MJ/m²/d, RUE={RUE}, KPAR={KPAR})")
        ax.set_xlim(0, 6)

        fig.suptitle("Beer's Law: PAR Interception and DM Production", fontsize=12,
                     fontweight="bold", color=C_BLUE, y=1.02)
        fig.tight_layout()
    return savefig(fig, "func_beer_law.png")


# =============================================================================
# 4. Hourly radiation / temperature / VPD curves (LT trait illustration)
# =============================================================================
def plot_hourly_curves():
    with plt.rc_context(STYLE):
        fig = plt.figure(figsize=(12, 9))
        gs  = GridSpec(3, 2, figure=fig, hspace=0.48, wspace=0.35)

        # ------ simulation parameters ------
        LAT   = 35.7
        DOY   = 200  # ~19 July, midsummer
        TMIN  = 20.0; TMAX = 33.0; TMINA = 18.0
        SRAD_d = 22.0  # MJ/m²/d
        VPDF   = 0.75

        Pi = math.pi
        RDN = Pi / 180
        DEC = math.sin(23.45*RDN) * math.cos(2*Pi*(DOY+10)/365)
        DEC = math.atan(DEC / math.sqrt(max(1e-9, 1 - DEC**2))) * (-1)
        SINLD = math.sin(RDN*LAT) * math.sin(DEC)
        COSLD = math.cos(RDN*LAT) * math.cos(DEC)
        AOB   = SINLD / COSLD
        AOBs  = max(-0.9999, min(0.9999, AOB))
        AOB2  = math.atan(AOBs / math.sqrt(max(1e-9, 1 - AOBs**2)))
        DAYL  = 12 * (1 + 2*AOB2/Pi)
        DSINBE = 3600*(DAYL*(SINLD + 0.4*(SINLD**2 + COSLD**2*0.5)) +
                       12*COSLD*(2 + 3*0.4*SINLD)*math.sqrt(max(0, 1-AOBs**2))/Pi)
        P      = 1.5
        SUNRIS = 12 - DAYL/2
        SUNSET = 12 + DAYL/2
        DTR    = SRAD_d * 1e6
        VPTMIN = 0.6108 * math.exp(17.27*TMIN/(237.3+TMIN))

        Hv = np.arange(1, 25)
        daylight = (Hv > SUNRIS) & (Hv < SUNSET)

        angle   = np.sin(Pi * (Hv - SUNRIS) / (DAYL + 2*P))
        TEMP1v  = np.where(Hv < 13.5,
                           TMIN  + (TMAX  - TMIN)  * angle,
                           TMINA + (TMAX  - TMINA) * angle)

        SINBv  = np.maximum(SINLD + COSLD*np.cos(2*Pi*(Hv+12)/24), 0)
        SRAD1v = DTR * SINBv * (1 + 0.4*SINBv) / DSINBE * 3600 / 1e6

        VPTEMP = 0.6108 * np.exp(17.27 * TEMP1v / (237.3 + TEMP1v))
        VPD1v  = np.maximum((VPTEMP - VPTMIN) * (VPDF/0.75), 0)

        KPAR = 0.65; LAI = 3.5; RUE = 2.0; TEC = 9.0; IRUE = 2.0
        FINT   = 1 - math.exp(-KPAR * LAI)
        DDMP1v = SRAD1v * 0.48 * FINT * RUE

        # ── panel 1: hourly radiation ─────────────────────────
        ax = fig.add_subplot(gs[0, 0])
        ax.fill_between(Hv, SRAD1v, where=daylight, alpha=0.3, color=C_ORANGE)
        ax.plot(Hv[daylight], SRAD1v[daylight], color=C_ORANGE, lw=2)
        ax.axvline(SUNRIS, color=C_GRAY, lw=1, ls="--", alpha=0.7)
        ax.axvline(SUNSET, color=C_GRAY, lw=1, ls="--", alpha=0.7)
        ax.set_title(f"Hourly Solar Radiation\n(DOY={DOY}, LAT={LAT}°N, SRAD={SRAD_d} MJ/m²/d)")
        ax.set_xlabel("Hour of day");  ax.set_ylabel("SRAD (MJ/m²/h)")
        ax.set_xlim(1, 24);  ax.set_xticks([0,4,8,12,16,20,24])
        ax.text(SUNRIS+0.2, ax.get_ylim()[1]*0.9, f"Sunrise\n{SUNRIS:.1f}h",
                fontsize=8, color=C_GRAY)
        ax.text(SUNSET-0.2, ax.get_ylim()[1]*0.9, f"Sunset\n{SUNSET:.1f}h",
                fontsize=8, color=C_GRAY, ha="right")

        # ── panel 2: hourly temperature ───────────────────────
        ax = fig.add_subplot(gs[0, 1])
        ax.plot(Hv, TEMP1v, color=C_RED, lw=2)
        ax.axhline(TMIN,  color=C_BLUE,   lw=1, ls="--", alpha=0.7, label=f"Tmin = {TMIN} °C")
        ax.axhline(TMAX,  color=C_RED,    lw=1, ls="--", alpha=0.7, label=f"Tmax = {TMAX} °C")
        ax.axhline(TMINA, color=C_ACCENT, lw=1, ls=":",  alpha=0.7, label=f"TminA (next day) = {TMINA} °C")
        ax.axvline(13.5,  color=C_GRAY,   lw=1, ls="--", alpha=0.5, label="Pivot hour 13.5")
        ax.set_title("Hourly Temperature\n(asymmetric sinusoidal, next-day Tmin for afternoon)")
        ax.set_xlabel("Hour of day");  ax.set_ylabel("Temperature (°C)")
        ax.set_xlim(1, 24);  ax.set_xticks([0,4,8,12,16,20,24])
        ax.legend(loc="lower center", fontsize=8)

        # ── panel 3: hourly VPD + LT thresholds ──────────────
        ax = fig.add_subplot(gs[1, 0])
        ax.fill_between(Hv, VPD1v, where=daylight, alpha=0.2, color=C_BLUE)
        ax.plot(Hv[daylight], VPD1v[daylight], color=C_BLUE, lw=2, label="Hourly VPD")
        colors_lt = [C_GREEN, C_ORANGE, C_RED]
        for vpdcr, col, lbl in zip([1.5, 2.0, 2.5], colors_lt,
                                    ["VPDcr=1.5 (LT1.5)", "VPDcr=2.0 (LT2)", "VPDcr=2.5 (LT2.5)"]):
            ax.axhline(vpdcr, color=col, lw=1.4, ls="--", label=lbl)
        ax.set_title("Hourly VPD and LT Critical Thresholds\n(VPD1v > VPDcr triggers LT reduction)")
        ax.set_xlabel("Hour of day");  ax.set_ylabel("VPD (kPa)")
        ax.set_xlim(1, 24);  ax.set_xticks([0,4,8,12,16,20,24])
        ax.legend(loc="upper left", fontsize=8)

        # ── panel 4: hourly DM production with/without LT ────
        ax = fig.add_subplot(gs[1, 1])
        ax.fill_between(Hv, DDMP1v, where=daylight, alpha=0.2, color=C_GREEN, label="No LT (check)")
        ax.plot(Hv[daylight], DDMP1v[daylight], color=C_GREEN, lw=2)

        for vpdcr, col, lbl in zip([1.5, 2.0, 2.5], colors_lt,
                                    ["LT1.5", "LT2", "LT2.5"]):
            ddmp_lt = DDMP1v.copy()
            for i in range(len(Hv)):
                if daylight[i] and VPD1v[i] > vpdcr:
                    t1 = ddmp_lt[i] * vpdcr / TEC
                    d1 = t1 * TEC / VPD1v[i]
                    t1 = d1 * vpdcr / TEC
                    d1 = t1 * TEC / VPD1v[i]
                    ddmp_lt[i] = d1
            ax.plot(Hv[daylight], ddmp_lt[daylight], color=col, lw=1.6, ls="--", label=lbl)

        ax.set_title("Hourly DM Production with/without LT\n"
                     f"(LAI={LAI}, RUE={RUE}, KPAR={KPAR})")
        ax.set_xlabel("Hour of day");  ax.set_ylabel("Hourly DDMP (g/m²/h)")
        ax.set_xlim(1, 24);  ax.set_xticks([0,4,8,12,16,20,24])
        ax.legend(loc="upper left", fontsize=8)

        # ── panel 5: daily totals comparison across VPDcr ────
        ax = fig.add_subplot(gs[2, :])
        vpdcr_vals = [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0, 10.0]
        daily_ddmp = []
        for vpdcr in vpdcr_vals:
            ddmp_lt = DDMP1v.copy()
            for i in range(len(Hv)):
                if daylight[i] and VPD1v[i] > vpdcr:
                    t1 = ddmp_lt[i] * vpdcr / TEC
                    d1 = t1 * TEC / VPD1v[i]
                    t1 = d1 * vpdcr / TEC
                    d1 = t1 * TEC / VPD1v[i]
                    ddmp_lt[i] = d1
            daily_ddmp.append(sum(ddmp_lt[daylight]))

        base = daily_ddmp[-1]  # check cultivar value
        pct  = [d/base*100 for d in daily_ddmp]

        colors_bar = plt.cm.RdYlGn(np.linspace(0.2, 0.9, len(vpdcr_vals)))
        bars = ax.bar([f"{v}" for v in vpdcr_vals], pct, color=colors_bar, edgecolor="white")
        ax.axhline(100, color=C_GRAY, lw=1, ls="--", label="Check cultivar (no LT)")
        ax.set_xlabel("Critical VPD threshold  VPDcr  (kPa)")
        ax.set_ylabel("Daily DDMP relative to check (%)")
        ax.set_title(f"Effect of VPDcr on Daily DM Production (this day: peak VPD = {VPD1v[daylight].max():.1f} kPa)\n"
                     "Lower VPDcr = stronger LT trait = more reduction on high-VPD days")
        ax.set_ylim(50, 105)
        ax.legend()

        fig.suptitle("Hourly Integration: Radiation, Temperature, VPD, and Limited Transpiration",
                     fontsize=12, fontweight="bold", color=C_BLUE, y=1.01)
    return savefig(fig, "func_hourly_curves.png")


# =============================================================================
# 5. Leaf partitioning (FLF vs WTOP)
# =============================================================================
def plot_leaf_partitioning():
    with plt.rc_context(STYLE):
        fig, axes = plt.subplots(1, 2, figsize=(10, 4))

        # Parameters from scenarios (Jonesboro check)
        FLF1A = 0.65; FLF1B = 0.40; WTOPL = 150; FLF2 = 0.05

        # Left: FLF vs WTOP (vegetative phase)
        ax = axes[0]
        WTOP = np.linspace(0, 400, 500)
        FLF  = np.where(WTOP < WTOPL, FLF1A, FLF1B)

        ax.step(WTOP, FLF, color=C_GREEN, lw=2.5, where="post")
        ax.axvline(WTOPL, color=C_ORANGE, lw=1.5, ls="--",
                   label=f"WTOPL = {WTOPL} g/m²")
        ax.axhline(FLF1A, color=C_BLUE,   lw=1, ls=":", alpha=0.7,
                   label=f"FLF1A = {FLF1A} (young canopy)")
        ax.axhline(FLF1B, color=C_RED,    lw=1, ls=":", alpha=0.7,
                   label=f"FLF1B = {FLF1B} (established canopy)")
        ax.fill_between(WTOP, FLF, alpha=0.1, color=C_GREEN, step="post")
        ax.set_xlabel("Total above-ground biomass  WTOP  (g/m²)")
        ax.set_ylabel("Leaf fraction of new DM  (FLF)")
        ax.set_title("Leaf Partitioning Function\n(vegetative period, EMR → bdTLM)")
        ax.set_xlim(0, 400);  ax.set_ylim(0, 0.8)
        ax.legend(fontsize=9)
        ax.annotate("More DM → leaves\n(thin early canopy)", xy=(50, 0.65+0.02),
                    fontsize=8, color=C_BLUE, ha="center")
        ax.annotate("More DM → stems\n(dense established canopy)", xy=(250, 0.40+0.04),
                    fontsize=8, color=C_RED, ha="center")

        # Right: FLF after TLM (FLF2) + schematic partitioning pie
        ax = axes[1]
        stages = ["EMR → bdTLM\n(early veg.)", "bdTLM → BSG\n(late veg.)", "BSG → TSG\n(grain fill)"]
        leaf_f  = [0.525, FLF2, 0.0]  # average of FLF1A/FLF1B period
        stem_f  = [0.475, 1-FLF2, 0.15]
        grain_f = [0.0,   0.0,    0.85]

        x = np.arange(len(stages))
        w = 0.5
        ax.bar(x, leaf_f,  w, label="Leaves (GLF)",    color=C_GREEN, alpha=0.85)
        ax.bar(x, stem_f,  w, bottom=leaf_f,           label="Stems (GST)",    color=C_ACCENT, alpha=0.85)
        ax.bar(x, grain_f, w, bottom=[l+s for l,s in zip(leaf_f,stem_f)],
               label="Grain (SGR)", color=C_ORANGE, alpha=0.85)
        ax.set_xticks(x);  ax.set_xticklabels(stages, fontsize=8)
        ax.set_ylabel("Fraction of daily DM (DDMP)")
        ax.set_title("DM Partitioning by Growth Stage\n"
                     "(illustrative; late-veg FLF2 = 0.05)")
        ax.set_ylim(0, 1.05)
        ax.legend(loc="upper right", fontsize=8)

        fig.suptitle("Dry Matter Partitioning Functions", fontsize=12,
                     fontweight="bold", color=C_BLUE, y=1.02)
        fig.tight_layout()
    return savefig(fig, "func_leaf_partitioning.png")


# =============================================================================
# 6. DHI modifier (DHIDMF vs BSGDM)
# =============================================================================
def plot_dhi_modifier():
    with plt.rc_context(STYLE):
        fig, axes = plt.subplots(1, 2, figsize=(10, 4))

        # Thresholds from scenarios
        WDHI1 = 0; WDHI2 = 0; WDHI3 = 9999; WDHI4 = 9999
        # Use more illustrative values typical for soybean
        WDHI1 = 50;  WDHI2 = 150;  WDHI3 = 600;  WDHI4 = 900
        PDHI  = 0.01

        BSGDM = np.linspace(0, 1000, 500)

        def dhidmf(bsg):
            if bsg <= WDHI1 or bsg >= WDHI4:
                return 0.0
            elif bsg < WDHI2:
                return (bsg - WDHI1) / (WDHI2 - WDHI1)
            elif bsg <= WDHI3:
                return 1.0
            else:
                return (WDHI4 - bsg) / (WDHI4 - WDHI3)

        DMF = np.array([dhidmf(b) for b in BSGDM])
        DHI = PDHI * DMF

        ax = axes[0]
        ax.plot(BSGDM, DMF, color=C_BLUE, lw=2.5)
        ax.fill_between(BSGDM, DMF, alpha=0.12, color=C_BLUE)
        for x, lbl in [(WDHI1,"WDHI1"),(WDHI2,"WDHI2"),
                       (WDHI3,"WDHI3"),(WDHI4,"WDHI4")]:
            ax.axvline(x, color=C_GRAY, lw=1, ls="--", alpha=0.7)
            ax.text(x, 1.05, lbl, ha="center", va="bottom", fontsize=8, color=C_GRAY)
        ax.set_xlabel("Biomass at beginning of seed growth  BSGDM  (g/m²)")
        ax.set_ylabel("DHI modifier  DHIDMF  (0–1)")
        ax.set_title(f"Dynamic HI Modifier\n(WDHI1={WDHI1}, WDHI2={WDHI2}, WDHI3={WDHI3}, WDHI4={WDHI4})")
        ax.set_xlim(0, 1000);  ax.set_ylim(-0.05, 1.15)
        ax.annotate("Optimal biomass\nat BSG", xy=(375, 1.02), ha="center", fontsize=9,
                    color=C_GREEN, bbox=dict(boxstyle="round,pad=0.2", fc="white", ec=C_GREEN))

        # Right: grain growth rate over DHI cycle
        ax = axes[1]
        # Simulate a simple grain filling scenario
        days_fill = np.arange(0, 35)
        WTOP_0 = 350  # g/m² at BSG
        HI_sim = np.zeros(35)
        WGRN_sim = np.zeros(35)
        WTOP_sim = np.full(35, WTOP_0)
        BSGDM_val = WTOP_0
        dhiv = dhidmf(BSGDM_val)
        hi = 0
        wtop = WTOP_0
        DDMP_daily = 15  # constant DM production

        for d in range(35):
            dhi_rate = PDHI * dhiv
            sgr  = dhi_rate * (wtop + DDMP_daily) + DDMP_daily * hi
            sgr  = max(0, sgr)
            wgrn = min(WGRN_sim[d-1] + sgr if d>0 else sgr, wtop)
            wtop = wtop + DDMP_daily
            hi   = wgrn / max(wtop, 1)
            HI_sim[d]   = hi
            WGRN_sim[d] = wgrn
            WTOP_sim[d] = wtop

        ax.plot(days_fill, WTOP_sim, color=C_BLUE,   lw=2, label="WTOP (total biomass)")
        ax.plot(days_fill, WGRN_sim, color=C_ORANGE, lw=2, label="WGRN (grain)")
        ax2 = ax.twinx()
        ax2.plot(days_fill, HI_sim, color=C_GREEN, lw=1.5, ls="--", label="HI")
        ax2.set_ylabel("Harvest Index (HI)", color=C_GREEN)
        ax2.tick_params(axis="y", colors=C_GREEN)
        ax2.set_ylim(0, 0.7)
        ax.set_xlabel("Days after beginning of seed growth (BSG)")
        ax.set_ylabel("Biomass (g/m²)")
        ax.set_title(f"Simulated Grain Filling\n(BSGDM={WTOP_0} g/m², PDHI={PDHI})")
        lines1, labels1 = ax.get_legend_handles_labels()
        lines2, labels2 = ax2.get_legend_handles_labels()
        ax.legend(lines1+lines2, labels1+labels2, loc="upper left", fontsize=8)

        fig.suptitle("Dynamic Harvest Index (DHI) Partitioning", fontsize=12,
                     fontweight="bold", color=C_BLUE, y=1.02)
        fig.tight_layout()
    return savefig(fig, "func_dhi_modifier.png")


# =============================================================================
# 7. Typical seasonal progression curves
# =============================================================================
def plot_seasonal_curves():
    with plt.rc_context(STYLE):
        fig, axes = plt.subplots(3, 2, figsize=(12, 11))

        # Simulate a realistic soybean season (120 days)
        n = 120
        days = np.arange(n)

        # -- Radiation: seasonal variation (DOY 130 = May planting)
        doy_start = 130
        doys = doy_start + days
        srad = 15 + 7*np.sin(np.pi*(doys - 100)/180) + np.random.RandomState(42).normal(0, 2, n)
        srad = np.clip(srad, 3, 30)

        # -- Temperature: seasonal bell
        tmax = 26 + 8*np.sin(np.pi*(days - 10)/120) + np.random.RandomState(7).normal(0, 1.5, n)
        tmin = tmax - 10 - np.random.RandomState(99).normal(0, 1, n)
        tmean = (tmax + tmin)/2

        # -- VPD: correlated with temp
        vptmax = 0.6108 * np.exp(17.27*tmax/(237.3+tmax))
        vptmin = 0.6108 * np.exp(17.27*tmin/(237.3+tmin))
        vpd    = 0.75 * (vptmax - vptmin)

        # -- Phenological stages (days after sowing)
        emr = 7; r1 = 30; r3 = 50; r5 = 65; r7 = 100; r8 = 115

        # -- LAI: rise, plateau, decline
        def lai_curve(d):
            if d < emr:     return 0.0
            elif d < r5:    return min(6.0, 6*(1-np.exp(-0.12*(d-emr))))
            elif d < r7:    return 6.0 * np.exp(-0.02*(d-r5))
            else:           return max(0, 6*np.exp(-0.02*(r7-r5)) * (1-(d-r7)/(r8-r7)))
        LAI_d = np.array([lai_curve(d) for d in days])

        # -- FINT, DDMP (simplified)
        KPAR = 0.65; RUE = 2.0
        FINT_d = 1 - np.exp(-KPAR * LAI_d)
        temp_f = np.clip((tmean - 10)/(20 - 10), 0, 1)  # simple temp factor
        wsfg   = np.ones(n)
        wsfg[70:85] = 0.5  # a dry period
        DDMP_d = srad * 0.48 * FINT_d * RUE * temp_f * wsfg

        # -- Biomass
        WTOP = np.zeros(n); WGRN = np.zeros(n)
        for d in range(1, n):
            WTOP[d] = WTOP[d-1] + max(0, DDMP_d[d])
            if d >= r5:
                grain_rate = 0.012 * WTOP[d]
                WGRN[d] = min(WTOP[d]*0.5, WGRN[d-1] + grain_rate)

        # -- Transpiration
        TEC = 9.0
        TR_d = DDMP_d * vpd / TEC
        TR_d = np.where(LAI_d > 0.1, TR_d, 0)

        # -- FTSW (soil water fraction)
        rain = np.zeros(n)
        rain_days = [10, 22, 35, 48, 78, 90, 108]
        for rd in rain_days:
            if rd < n: rain[rd] = np.random.RandomState(rd).uniform(15, 40)
        FTSW = np.zeros(n); FTSW[0] = 1.0
        for d in range(1, n):
            FTSW[d] = np.clip(FTSW[d-1] - TR_d[d]/100 + rain[d]/100, 0, 1)

        # ── stage shading helper ──────────────────────────────
        def add_stages(ax):
            for d, lbl in [(emr,"EMR"),(r1,"R1"),(r3,"R3"),(r5,"R5"),(r7,"R7"),(r8,"R8")]:
                ax.axvline(d, color=C_GRAY, lw=0.8, ls="--", alpha=0.5)
            ylim = ax.get_ylim()
            ypos = ylim[0] + (ylim[1]-ylim[0])*0.02
            for d, lbl in [(emr,"EMR"),(r1,"R1"),(r3,"R3"),(r5,"R5"),(r7,"R7"),(r8,"R8")]:
                ax.text(d, ypos, lbl, fontsize=7, color=C_GRAY, ha="center",
                        bbox=dict(fc="white", ec="none", pad=1))

        # -- Plot 1: Radiation and temperature
        ax = axes[0, 0]
        ax.fill_between(days, srad, alpha=0.25, color=C_ORANGE)
        ax.plot(days, srad, color=C_ORANGE, lw=1.5, label="SRAD (MJ/m²/d)")
        ax2 = ax.twinx()
        ax2.plot(days, tmax, color=C_RED,   lw=1.5, label="Tmax")
        ax2.plot(days, tmin, color=C_BLUE,  lw=1.5, label="Tmin", ls="--")
        ax2.plot(days, tmean,color=C_GRAY,  lw=1,   label="Tmean", ls=":")
        ax2.set_ylabel("Temperature (°C)", color=C_RED)
        ax2.tick_params(axis="y", colors=C_RED)
        ax.set_ylabel("Solar radiation (MJ/m²/d)", color=C_ORANGE)
        ax.set_title("Daily Radiation and Temperature")
        ax.set_xlabel("Days after sowing")
        lines1, lab1 = ax.get_legend_handles_labels()
        lines2, lab2 = ax2.get_legend_handles_labels()
        ax.legend(lines1+lines2, lab1+lab2, fontsize=8, loc="upper right")
        add_stages(ax)

        # -- Plot 2: VPD
        ax = axes[0, 1]
        ax.fill_between(days, vpd, alpha=0.2, color=C_BLUE)
        ax.plot(days, vpd, color=C_BLUE, lw=1.5, label="Daily VPD")
        for v, col, lbl in [(1.5,C_GREEN,"VPDcr=1.5"),(2.0,C_ORANGE,"VPDcr=2.0"),(2.5,C_RED,"VPDcr=2.5")]:
            ax.axhline(v, color=col, lw=1.2, ls="--", label=lbl)
        ax.set_ylabel("VPD (kPa)");  ax.set_xlabel("Days after sowing")
        ax.set_title("Daily VPD with LT Critical Thresholds")
        ax.legend(fontsize=8, loc="upper right")
        add_stages(ax)

        # -- Plot 3: LAI
        ax = axes[1, 0]
        ax.fill_between(days, LAI_d, alpha=0.2, color=C_GREEN)
        ax.plot(days, LAI_d, color=C_GREEN, lw=2, label="LAI")
        ax2 = ax.twinx()
        ax2.plot(days, FINT_d, color=C_ORANGE, lw=1.5, ls="--", label="FINT (PAR intercepted)")
        ax2.set_ylabel("FINT (fraction)", color=C_ORANGE)
        ax2.tick_params(axis="y", colors=C_ORANGE)
        ax2.set_ylim(0, 1.1)
        ax.set_ylabel("Leaf Area Index (m²/m²)", color=C_GREEN)
        ax.set_title("LAI Dynamics and PAR Interception")
        ax.set_xlabel("Days after sowing")
        lines1, lab1 = ax.get_legend_handles_labels()
        lines2, lab2 = ax2.get_legend_handles_labels()
        ax.legend(lines1+lines2, lab1+lab2, fontsize=8, loc="lower right")
        add_stages(ax)

        # -- Plot 4: DM production
        ax = axes[1, 1]
        ax.fill_between(days, DDMP_d, where=(DDMP_d > 0), alpha=0.2, color=C_BLUE)
        ax.plot(days, DDMP_d, color=C_BLUE, lw=1.5, label="Daily DDMP (g/m²/d)")
        ax2 = ax.twinx()
        ax2.plot(days, WTOP, color=C_ACCENT, lw=2, label="WTOP (cum. biomass)")
        ax2.plot(days, WGRN, color=C_ORANGE, lw=2, label="WGRN (grain)")
        ax2.set_ylabel("Cumulative biomass (g/m²)", color=C_ACCENT)
        ax2.tick_params(axis="y", colors=C_ACCENT)
        ax.set_ylabel("Daily DM production (g/m²/d)", color=C_BLUE)
        ax.set_title("DM Production and Biomass Accumulation")
        ax.set_xlabel("Days after sowing")
        # annotate water stress
        ax.axvspan(70, 85, alpha=0.1, color=C_RED, label="Water stress period")
        lines1, lab1 = ax.get_legend_handles_labels()
        lines2, lab2 = ax2.get_legend_handles_labels()
        ax.legend(lines1+lines2, lab1+lab2, fontsize=8, loc="upper left")
        add_stages(ax)

        # -- Plot 5: Transpiration
        ax = axes[2, 0]
        ax.bar(days, TR_d, color=C_BLUE, alpha=0.6, label="Daily transpiration (TR)")
        ax.plot(days, np.cumsum(TR_d)/10, color=C_RED, lw=1.5, label="Cum. TR / 10")
        ax.set_ylabel("Transpiration (mm/d)");  ax.set_xlabel("Days after sowing")
        ax.set_title("Daily and Cumulative Transpiration")
        ax.legend(fontsize=8)
        add_stages(ax)

        # -- Plot 6: Soil water (FTSW)
        ax = axes[2, 1]
        ax.fill_between(days, FTSW, alpha=0.2, color=C_ACCENT)
        ax.plot(days, FTSW, color=C_ACCENT, lw=2, label="FTSW (soil water fraction)")
        rain_h = rain.copy(); rain_h[rain_h == 0] = np.nan
        ax.bar(days, rain_h/np.nanmax(rain)*0.3, bottom=0.7, color=C_BLUE, alpha=0.5,
               width=1, label="Rainfall events (scaled)")
        ax.axhline(0.5, color=C_ORANGE, lw=1.2, ls="--", label="FTSW = 0.5 (stress onset)")
        ax.axhline(0.1, color=C_RED,    lw=1.2, ls="--", label="FTSW = 0.1 (severe stress)")
        ax.set_ylabel("Fraction of transpirable soil water");  ax.set_xlabel("Days after sowing")
        ax.set_title("Soil Water Dynamics (FTSW)")
        ax.set_ylim(0, 1.05)
        ax.legend(fontsize=8, loc="lower left")
        add_stages(ax)

        fig.suptitle("Typical Soybean Season: Daily Model Variables\n"
                     "(Illustrative simulation, mid-South US, MG4, rainfed)",
                     fontsize=12, fontweight="bold", color=C_BLUE, y=1.01)
        fig.tight_layout()
    return savefig(fig, "func_seasonal_curves.png")


# =============================================================================
# 8. LAI expansion function
# =============================================================================
def plot_lai_expansion():
    with plt.rc_context(STYLE):
        fig, axes = plt.subplots(1, 2, figsize=(10, 4))

        # Parameters from scenario
        PHYL = 45; SLA = 0.032; PDEN = 32
        PLAPOW = 2.5; a_pla_den = 1.016; b_pla_den = -0.0005
        plapow_eff = PLAPOW * (b_pla_den * PDEN + a_pla_den)

        CBD = np.linspace(0, 25, 200)

        ax = axes[0]
        for sla, col, lbl in [(0.028, C_BLUE,   "SLA = 0.028"),
                               (0.032, C_GREEN,  "SLA = 0.032 (default)"),
                               (0.038, C_ORANGE, "SLA = 0.038")]:
            pla = (CBD / PHYL) ** plapow_eff
            lai = pla * sla * PDEN / 10000
            ax.plot(CBD, lai, color=col, lw=2, label=lbl)
        ax.set_xlabel("Cumulative biological days (CBD)")
        ax.set_ylabel("Leaf Area Index (LAI,  m²/m²)")
        ax.set_title("LAI Expansion: Allometric Function\n"
                     f"LAI = (CBD/PHYL)^PLAPOW × SLA × PDEN / 10000\n"
                     f"(PHYL={PHYL}, PDEN={PDEN})")
        ax.set_xlim(0, 25);  ax.set_ylim(0, 8)
        ax.legend(fontsize=9)

        ax = axes[1]
        for pden, col, lbl in [(20, C_BLUE,   "20 pl/m²"),
                                (32, C_GREEN,  "32 pl/m² (default)"),
                                (45, C_ORANGE, "45 pl/m²")]:
            plapow_e = PLAPOW * (b_pla_den * pden + a_pla_den)
            pla = (CBD / PHYL) ** plapow_e
            lai = pla * SLA * pden / 10000
            ax.plot(CBD, lai, color=col, lw=2, label=lbl)
        ax.set_xlabel("Cumulative biological days (CBD)")
        ax.set_ylabel("Leaf Area Index (LAI,  m²/m²)")
        ax.set_title("Effect of Plant Density on LAI\n"
                     f"(PLAPOW_eff = PLAPOW × (b × PDEN + a))")
        ax.set_xlim(0, 25);  ax.set_ylim(0, 8)
        ax.legend(fontsize=9)

        fig.suptitle("Leaf Area Index Expansion Functions", fontsize=12,
                     fontweight="bold", color=C_BLUE, y=1.02)
        fig.tight_layout()
    return savefig(fig, "func_lai_expansion.png")


# =============================================================================
# Generate all figures
# =============================================================================
print("Generating illustrative function plots...")
paths = {}
np.random.seed(0)
paths["temperature_response"] = plot_temperature_response()
paths["photoperiod_response"]  = plot_photoperiod_response()
paths["beer_law"]              = plot_beer_law()
paths["hourly_curves"]         = plot_hourly_curves()
paths["leaf_partitioning"]     = plot_leaf_partitioning()
paths["dhi_modifier"]          = plot_dhi_modifier()
paths["seasonal_curves"]       = plot_seasonal_curves()
paths["lai_expansion"]         = plot_lai_expansion()
print(f"Generated {len(paths)} figures in {FIGURES_DIR}/")


# =============================================================================
# Embed all images as base64 in the HTML
# =============================================================================
def img_to_b64(path):
    with open(path, "rb") as f:
        return base64.b64encode(f.read()).decode()

def make_data_uri(path):
    ext = os.path.splitext(path)[1].lower().lstrip(".")
    mime = {"png": "image/png", "jpg": "image/jpeg", "jpeg": "image/jpeg",
            "svg": "image/svg+xml", "gif": "image/gif"}.get(ext, "image/png")
    return f"data:{mime};base64,{img_to_b64(path)}"

print("\nReading HTML and embedding images...")
with open(HTML_IN, "r", encoding="utf-8") as f:
    html = f.read()

# ── Replace existing relative image src paths ────────────────────────────────
def replace_src(html, src_pattern, abs_path):
    if not os.path.exists(abs_path):
        print(f"  WARNING: image not found: {abs_path}")
        return html
    data_uri = make_data_uri(abs_path)
    escaped  = re.escape(src_pattern)
    html     = re.sub(escaped, data_uri, html)
    print(f"  embedded {os.path.basename(abs_path)}")
    return html

docs_dir  = os.path.dirname(HTML_IN)
plots_dir = os.path.join(docs_dir, "..", "r-model", "outputs", "plots")

existing_srcs = re.findall(r'src="([^"]+\.png)"', html)
for src in existing_srcs:
    # src is relative to the docs/ folder
    abs_path = os.path.normpath(os.path.join(docs_dir, src))
    html = replace_src(html, src, abs_path)


# =============================================================================
# Insert new figures into the HTML at appropriate anchors
# =============================================================================

def make_figure(fig_path, alt, caption):
    b64 = make_data_uri(fig_path)
    return (
        f'\n<figure>\n'
        f'  <img src="{b64}" alt="{alt}">\n'
        f'  <figcaption>{caption}</figcaption>\n'
        f'</figure>\n'
    )

FIG = {k: os.path.join(FIGURES_DIR, f"func_{k}.png") for k in
       ["temperature_response","photoperiod_response","beer_law",
        "hourly_curves","leaf_partitioning","dhi_modifier",
        "seasonal_curves","lai_expansion"]}

# -- (A) After phenology temperature function description (before photoperiod section)
anchor_A = "<!-- ── 3. PHENOLOGY"
insert_A = (
    make_figure(FIG["temperature_response"],
                "Trapezoidal temperature response functions",
                "Figure 3.1 — Trapezoidal temperature response f<sub>T</sub>(T) for "
                "phenology (BD accumulation, left) and RUE (right). Both use the same "
                "four-parameter piecewise linear form: zero below T<sub>B</sub>, linear "
                "rise to the optimum plateau (T<sub>P1</sub>–T<sub>P2</sub>), and linear "
                "decline to zero at T<sub>C</sub>.")
    + make_figure(FIG["photoperiod_response"],
                  "Photoperiod response function for soybean",
                  "Figure 3.2 — Photoperiod response f<sub>pp</sub>(DAYL) for soybean "
                  "(short-day plant). Development is fastest under short days (high f<sub>pp</sub>) "
                  "and progressively delayed as daylength exceeds the critical photoperiod "
                  "(cpp = 13.09 h). The slope is set by ppsen = −0.294.")
)

# -- (B) After Beer's law equation (Eq. 5.5)
anchor_B = '<h3 id="daily-mode">5.2 Daily mode'
insert_B = (
    make_figure(FIG["beer_law"],
                "Beer's law PAR interception",
                "Figure 5.1 — Beer's law: FINT = 1 − exp(−KPAR × LAI). "
                "Left: interception fraction vs LAI for three extinction coefficients; "
                "the soybean default KPAR = 0.65 reaches ~95 % interception near LAI = 4.5. "
                "Right: corresponding daily DM production for a typical summer day "
                "(SRAD = 15 MJ/m²/d, RUE = 2.0).")
    + make_figure(FIG["lai_expansion"],
                  "LAI expansion allometric function",
                  "Figure 5.2 — LAI expansion as an allometric power function of cumulative "
                  "biological days. Left: effect of specific leaf area (SLA); Right: effect of "
                  "plant density (PDEN). The exponent PLAPOW<sub>eff</sub> is density-adjusted "
                  "via the linear correction factor (a_pla_den + b_pla_den × PDEN).")
)

# -- (C) After hourly temperature description (before section 5.3.3)
anchor_C = '<h4 id="hourly-vpd">5.3.3 Hourly VPD'
insert_C = make_figure(FIG["hourly_curves"],
                "Hourly radiation, temperature, VPD and LT trait illustration",
                "Figure 5.3 — Hourly sub-daily curves computed in the hourly integration "
                "loop (vpdtp=1). Top row: solar radiation distribution (Spitters 1986) and "
                "asymmetric temperature curve. Middle row: hourly VPD with LT critical "
                "thresholds and the resulting hourly DM production for each cultivar type. "
                "Bottom: daily DDMP relative to the check cultivar as a function of "
                "VPD<sub>cr</sub> — lower thresholds reduce production more on high-VPD days "
                f"(peak VPD shown).")

# -- (D) After DM distribution equations (before translocation section)
anchor_D = '<h3>DM translocation</h3>'
insert_D = (
    make_figure(FIG["leaf_partitioning"],
                "Leaf partitioning function",
                "Figure 6.1 — Left: leaf fraction FLF of new DM as a step function of "
                "total biomass WTOP. Young thin canopies (WTOP &lt; WTOPL = 150 g/m²) allocate "
                "FLF1A = 0.65 to leaves; denser canopies switch to FLF1B = 0.40. After bdTLM "
                "only FLF2 = 0.05 goes to leaves. Right: schematic DM allocation across "
                "three growth phases — leaves dominate early, stems mid-season, grain during "
                "the seed-fill period.")
    + make_figure(FIG["dhi_modifier"],
                  "DHI modifier and simulated grain filling",
                  "Figure 6.2 — Left: the DHIDMF modifier (0–1) as a trapezoid function "
                  "of biomass at beginning of seed growth (BSGDM). Very low or very high "
                  "biomass reduces the potential DHI rate. Right: example grain filling "
                  "trajectory showing WTOP, WGRN, and HI over the BSG–TSG period "
                  "(BSGDM = 350 g/m², PDHI = 0.01).")
)

# -- (E) After Model Integration section header (section 8)
anchor_E = '<!-- ── 8. MODEL INTEGRATION'
insert_E = make_figure(FIG["seasonal_curves"],
                "Typical seasonal daily curves",
                "Figure 8.1 — Illustrative seasonal progression of key daily model "
                "variables for a typical mid-South US rainfed soybean crop (MG4, ~120-day "
                "season). Phenological stages (EMR through R8) are marked with dashed "
                "vertical lines. A mid-season drought stress period (days 70–85) is visible "
                "as reduced FTSW and DDMP. Daily radiation and temperature drive the "
                "characteristic bell-shaped LAI and biomass accumulation curves.")

# Apply insertions — each must be inserted BEFORE its anchor
for anchor, insertion in [
    (anchor_A, insert_A),
    (anchor_B, insert_B),
    (anchor_C, insert_C),
    (anchor_D, insert_D),
    (anchor_E, insert_E),
]:
    if anchor in html:
        html = html.replace(anchor, insertion + anchor, 1)
        print(f"  inserted figures before: {anchor[:60]!r}")
    else:
        print(f"  WARNING: anchor not found: {anchor[:60]!r}")

# Write output
with open(HTML_OUT, "w", encoding="utf-8") as f:
    f.write(html)

size_mb = os.path.getsize(HTML_OUT) / 1e6
print(f"\nWrote {HTML_OUT} ({size_mb:.1f} MB) — fully self-contained.")
