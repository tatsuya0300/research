from __future__ import annotations

import argparse
import sys
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from scipy import stats


TIMEPOINT_ORDER = [
    "admission",
    "day14",
    "day30",
    "day60",
    "discharge",
]

TIMEPOINT_LABELS = {
    "admission": "Admission",
    "day14": "Day 14",
    "day30": "Day 30",
    "day60": "Day 60",
    "discharge": "Discharge",
}

NUMERIC_COLUMNS = [
    "eligible",
    "active_medication_rows",
    "medication_count",
    "section_A",
    "section_B",
    "section_C",
    "MRCI_auto",
    "unmapped_A",
    "unmapped_B",
    "review_required",
    "review_count",
]


# ============================================================
# General utilities
# ============================================================

def read_csv_auto_encoding(
    path: str,
) -> pd.DataFrame:
    """
    UTF-8、CP932などを順に試してCSVを読み込む。
    """
    encodings = [
        "utf-8-sig",
        "utf-8",
        "cp932",
        "shift_jis",
    ]

    errors = []

    for encoding in encodings:
        try:
            return pd.read_csv(
                path,
                encoding=encoding,
                dtype=str,
                keep_default_na=False,
            )

        except UnicodeDecodeError as exc:
            errors.append(
                f"{encoding}: {exc}"
            )

    raise RuntimeError(
        "CSVを読み込めませんでした。\n"
        + "\n".join(errors)
    )


def configure_plot_style():
    """
    グラフの表示形式を設定する。
    """
    sns.set_theme(
        style="whitegrid",
        context="talk",
    )

    plt.rcParams[
        "font.family"
    ] = [
        "Hiragino Sans",
        "Yu Gothic",
        "Arial Unicode MS",
        "DejaVu Sans",
    ]

    plt.rcParams[
        "axes.unicode_minus"
    ] = False

    plt.rcParams[
        "figure.dpi"
    ] = 120

    plt.rcParams[
        "savefig.dpi"
    ] = 300

    plt.rcParams[
        "savefig.bbox"
    ] = "tight"


def save_figure(
    output_dir: Path,
    filename: str,
):
    """
    PNGとPDFの両方でグラフを保存する。
    """
    png_file = (
        output_dir
        / f"{filename}.png"
    )

    pdf_file = (
        output_dir
        / f"{filename}.pdf"
    )

    plt.savefig(
        png_file,
        dpi=300,
        bbox_inches="tight",
    )

    plt.savefig(
        pdf_file,
        bbox_inches="tight",
    )

    plt.close()


def add_episode_key(
    df: pd.DataFrame,
) -> pd.DataFrame:
    """
    入院エピソードを一意に識別する内部キーを作る。

    episode_idが重複する可能性を考慮し、
    患者ID・入院日も結合する。
    """
    result = df.copy()

    episode_id = result[
        "episode_id"
    ].astype(str)

    patient_id = result[
        "患者ID"
    ].astype(str)

    admission_date = result[
        "admission_date"
    ].astype(str)

    result["_episode_key"] = (
        patient_id
        + "||"
        + episode_id
        + "||"
        + admission_date
    )

    return result


def prepare_longitudinal_data(
    input_csv: str,
) -> pd.DataFrame:
    """
    MRCI縦断CSVを解析用に整形する。
    """
    df = read_csv_auto_encoding(
        input_csv
    )

    required_columns = [
        "episode_id",
        "患者ID",
        "timepoint",
        "eligible",
        "MRCI_auto",
        "section_A",
        "section_B",
        "section_C",
        "review_required",
        "score_status",
        "admission_date",
    ]

    missing_columns = [
        column
        for column in required_columns
        if column not in df.columns
    ]

    if missing_columns:
        raise ValueError(
            "必要な列がありません: "
            + ", ".join(
                missing_columns
            )
        )

    for column in NUMERIC_COLUMNS:
        if column in df.columns:
            df[column] = pd.to_numeric(
                df[column],
                errors="coerce",
            )

    df["timepoint"] = pd.Categorical(
        df["timepoint"],
        categories=TIMEPOINT_ORDER,
        ordered=True,
    )

    df = add_episode_key(
        df
    )

    return df


def analysis_subset(
    df: pd.DataFrame,
    analysis_set: str,
) -> pd.DataFrame:
    """
    解析対象を定義する。

    eligible:
        eligible=1かつMRCIが数値の全時点。
        レビュー対象の暫定値・下限値を含む。

    auto_complete:
        自動採点が完了し、
        review flagがない時点だけを使用。
    """
    base_mask = (
        (df["eligible"] == 1)
        & df["MRCI_auto"].notna()
    )

    if analysis_set == "eligible":
        mask = base_mask

    elif analysis_set == "auto_complete":
        mask = (
            base_mask
            & (
                df["score_status"]
                == "auto_complete_no_review_flag"
            )
        )

    else:
        raise ValueError(
            f"不明なanalysis_set: {analysis_set}"
        )

    return df.loc[
        mask
    ].copy()


# ============================================================
# Descriptive statistics
# ============================================================

def make_descriptive_statistics(
    df: pd.DataFrame,
    analysis_set: str,
) -> pd.DataFrame:
    """
    時点別の記述統計を計算する。
    """
    data = analysis_subset(
        df,
        analysis_set,
    )

    rows = []

    for timepoint in TIMEPOINT_ORDER:
        values = data.loc[
            data["timepoint"] == timepoint,
            "MRCI_auto",
        ].dropna()

        if values.empty:
            rows.append({
                "analysis_set": analysis_set,
                "timepoint": timepoint,
                "n": 0,
                "mean": np.nan,
                "sd": np.nan,
                "median": np.nan,
                "q1": np.nan,
                "q3": np.nan,
                "min": np.nan,
                "max": np.nan,
            })

            continue

        rows.append({
            "analysis_set": analysis_set,
            "timepoint": timepoint,
            "n": int(values.size),
            "mean": float(
                values.mean()
            ),
            "sd": float(
                values.std(
                    ddof=1
                )
            )
            if values.size > 1
            else np.nan,
            "median": float(
                values.median()
            ),
            "q1": float(
                values.quantile(
                    0.25
                )
            ),
            "q3": float(
                values.quantile(
                    0.75
                )
            ),
            "min": float(
                values.min()
            ),
            "max": float(
                values.max()
            ),
        })

    return pd.DataFrame(
        rows
    )


def make_component_statistics(
    df: pd.DataFrame,
    analysis_set: str,
) -> pd.DataFrame:
    """
    Section A/B/Cの時点別統計を計算する。
    """
    data = analysis_subset(
        df,
        analysis_set,
    )

    rows = []

    for timepoint in TIMEPOINT_ORDER:
        timepoint_data = data.loc[
            data["timepoint"] == timepoint
        ]

        row = {
            "analysis_set": analysis_set,
            "timepoint": timepoint,
            "n": int(
                len(timepoint_data)
            ),
        }

        for column in [
            "section_A",
            "section_B",
            "section_C",
            "medication_count",
        ]:
            if column not in timepoint_data:
                row[
                    f"{column}_mean"
                ] = np.nan

                row[
                    f"{column}_median"
                ] = np.nan

                continue

            values = timepoint_data[
                column
            ].dropna()

            row[
                f"{column}_mean"
            ] = (
                float(values.mean())
                if not values.empty
                else np.nan
            )

            row[
                f"{column}_median"
            ] = (
                float(values.median())
                if not values.empty
                else np.nan
            )

        rows.append(row)

    return pd.DataFrame(
        rows
    )


def make_availability_table(
    df: pd.DataFrame,
) -> pd.DataFrame:
    """
    各時点の評価可能数、採点数、レビュー数を集計する。
    """
    rows = []

    for timepoint in TIMEPOINT_ORDER:
        group = df.loc[
            df["timepoint"] == timepoint
        ]

        total = len(group)

        eligible = int(
            (group["eligible"] == 1).sum()
        )

        scored = int(
            group["MRCI_auto"]
            .notna()
            .sum()
        )

        auto_complete = int(
            (
                group["score_status"]
                == "auto_complete_no_review_flag"
            ).sum()
        )

        review_required = int(
            (
                group["review_required"]
                == 1
            ).sum()
        )

        not_reached = int(
            (
                group["score_status"]
                == "not_reached_before_discharge"
            ).sum()
        )

        discharge_missing = int(
            (
                group["score_status"]
                == "discharge_date_missing"
            ).sum()
        )

        no_active_medication = int(
            (
                group["score_status"]
                == (
                    "no_active_medication_"
                    "review_required"
                )
            ).sum()
        )

        rows.append({
            "timepoint": timepoint,
            "total_episodes": total,
            "eligible": eligible,
            "scored": scored,
            "auto_complete": auto_complete,
            "review_required": review_required,
            "not_reached_before_discharge": (
                not_reached
            ),
            "discharge_date_missing": (
                discharge_missing
            ),
            "no_active_medication": (
                no_active_medication
            ),
            "eligible_rate": (
                eligible / total
                if total > 0
                else np.nan
            ),
            "review_rate": (
                review_required / eligible
                if eligible > 0
                else np.nan
            ),
        })

    return pd.DataFrame(
        rows
    )


def make_status_table(
    df: pd.DataFrame,
) -> pd.DataFrame:
    """
    score_statusの件数を時点別に集計する。
    """
    status = (
        df.groupby(
            [
                "timepoint",
                "score_status",
            ],
            observed=False,
        )
        .size()
        .reset_index(
            name="n"
        )
    )

    status = status.loc[
        status["n"] > 0
    ].copy()

    return status


# ============================================================
# Paired comparisons
# ============================================================

def holm_adjust(
    p_values: list[float],
) -> list[float]:
    """
    Holm法で多重比較補正する。
    """
    p_array = np.asarray(
        p_values,
        dtype=float,
    )

    adjusted = np.full(
        len(p_array),
        np.nan,
        dtype=float,
    )

    valid_indices = np.where(
        np.isfinite(p_array)
    )[0]

    if valid_indices.size == 0:
        return adjusted.tolist()

    valid_p = p_array[
        valid_indices
    ]

    order = np.argsort(
        valid_p
    )

    sorted_p = valid_p[
        order
    ]

    number_of_tests = len(
        sorted_p
    )

    sorted_adjusted = np.empty(
        number_of_tests,
        dtype=float,
    )

    previous = 0.0

    for position, p_value in enumerate(
        sorted_p
    ):
        multiplier = (
            number_of_tests
            - position
        )

        current = min(
            1.0,
            p_value * multiplier,
        )

        current = max(
            current,
            previous,
        )

        sorted_adjusted[
            position
        ] = current

        previous = current

    inverse_order = np.empty_like(
        order
    )

    inverse_order[
        order
    ] = np.arange(
        number_of_tests
    )

    adjusted_valid = (
        sorted_adjusted[
            inverse_order
        ]
    )

    adjusted[
        valid_indices
    ] = adjusted_valid

    return adjusted.tolist()


def rank_biserial_from_differences(
    differences: np.ndarray,
) -> float:
    """
    対応差に対するrank-biserial correlationを計算する。

    正値:
        追跡時MRCIが入院時より高い傾向。

    負値:
        追跡時MRCIが入院時より低い傾向。
    """
    differences = np.asarray(
        differences,
        dtype=float,
    )

    differences = differences[
        differences != 0
    ]

    if differences.size == 0:
        return 0.0

    ranks = stats.rankdata(
        np.abs(differences)
    )

    positive_rank = ranks[
        differences > 0
    ].sum()

    negative_rank = ranks[
        differences < 0
    ].sum()

    total_rank = (
        positive_rank
        + negative_rank
    )

    if total_rank == 0:
        return 0.0

    return float(
        (
            positive_rank
            - negative_rank
        )
        / total_rank
    )


def paired_comparisons(
    df: pd.DataFrame,
    analysis_set: str,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """
    入院時と各時点の対応比較を行う。

    主要検定:
        Wilcoxon符号付順位検定

    併記:
        平均変化量、中央値変化量、
        平均変化量の95%信頼区間、
        rank-biserial correlation
    """
    data = analysis_subset(
        df,
        analysis_set,
    )

    wide = data.pivot_table(
        index="_episode_key",
        columns="timepoint",
        values="MRCI_auto",
        aggfunc="first",
        observed=False,
    )

    rows = []
    change_rows = []

    for followup in [
        "day14",
        "day30",
        "day60",
        "discharge",
    ]:
        if (
            "admission" not in wide.columns
            or followup not in wide.columns
        ):
            continue

        pair = wide[
            [
                "admission",
                followup,
            ]
        ].dropna()

        if pair.empty:
            rows.append({
                "analysis_set": analysis_set,
                "comparison": (
                    f"{followup}_vs_admission"
                ),
                "n_pairs": 0,
                "admission_mean": np.nan,
                "followup_mean": np.nan,
                "mean_change": np.nan,
                "mean_change_ci95_low": np.nan,
                "mean_change_ci95_high": np.nan,
                "median_change": np.nan,
                "wilcoxon_statistic": np.nan,
                "p_value": np.nan,
                "rank_biserial": np.nan,
            })

            continue

        differences = (
            pair[followup]
            - pair["admission"]
        )

        for episode_key, difference in (
            differences.items()
        ):
            change_rows.append({
                "analysis_set": analysis_set,
                "_episode_key": episode_key,
                "followup": followup,
                "change_from_admission": (
                    float(difference)
                ),
            })

        n_pairs = len(
            differences
        )

        mean_change = float(
            differences.mean()
        )

        median_change = float(
            differences.median()
        )

        if n_pairs > 1:
            standard_error = (
                differences.std(
                    ddof=1
                )
                / np.sqrt(
                    n_pairs
                )
            )

            critical_value = (
                stats.t.ppf(
                    0.975,
                    df=n_pairs - 1,
                )
            )

            ci_low = (
                mean_change
                - critical_value
                * standard_error
            )

            ci_high = (
                mean_change
                + critical_value
                * standard_error
            )
        else:
            ci_low = np.nan
            ci_high = np.nan

        if np.allclose(
            differences.to_numpy(),
            0.0,
        ):
            wilcoxon_statistic = 0.0
            p_value = 1.0
        else:
            test_result = stats.wilcoxon(
                differences.to_numpy(),
                alternative="two-sided",
                zero_method="wilcox",
                method="auto",
            )

            wilcoxon_statistic = float(
                test_result.statistic
            )

            p_value = float(
                test_result.pvalue
            )

        rows.append({
            "analysis_set": analysis_set,
            "comparison": (
                f"{followup}_vs_admission"
            ),
            "n_pairs": n_pairs,
            "admission_mean": float(
                pair["admission"].mean()
            ),
            "followup_mean": float(
                pair[followup].mean()
            ),
            "mean_change": mean_change,
            "mean_change_ci95_low": (
                ci_low
            ),
            "mean_change_ci95_high": (
                ci_high
            ),
            "median_change": (
                median_change
            ),
            "wilcoxon_statistic": (
                wilcoxon_statistic
            ),
            "p_value": p_value,
            "rank_biserial": (
                rank_biserial_from_differences(
                    differences.to_numpy()
                )
            ),
        })

    results = pd.DataFrame(
        rows
    )

    if not results.empty:
        results[
            "p_value_holm"
        ] = holm_adjust(
            results[
                "p_value"
            ].tolist()
        )

    changes = pd.DataFrame(
        change_rows
    )

    return results, changes


# ============================================================
# Mixed-effects model
# ============================================================

def fit_mixed_effects_model(
    df: pd.DataFrame,
    analysis_set: str,
    output_dir: Path,
) -> pd.DataFrame:
    """
    ランダム切片線形混合モデルを適合する。

    MRCI ~ timepoint + (1 | episode)

    注意:
        day14/day30/day60の欠測は、
        早期退院による構造的欠測を含む。
        したがって因果効果ではなく探索的解析とする。
    """
    try:
        import statsmodels.formula.api as smf

    except ImportError:
        return pd.DataFrame({
            "error": [
                "statsmodelsがインストールされていません"
            ]
        })

    data = analysis_subset(
        df,
        analysis_set,
    )

    data = data.loc[
        data["timepoint"].isin(
            TIMEPOINT_ORDER
        )
    ].copy()

    data["timepoint"] = (
        data["timepoint"]
        .astype(str)
    )

    repeated_counts = (
        data.groupby(
            "_episode_key"
        )
        .size()
    )

    valid_episode_keys = (
        repeated_counts.loc[
            repeated_counts >= 2
        ].index
    )

    data = data.loc[
        data["_episode_key"].isin(
            valid_episode_keys
        )
    ].copy()

    if (
        len(data) < 10
        or data["_episode_key"].nunique()
        < 3
    ):
        return pd.DataFrame({
            "error": [
                "混合モデルに必要な反復データが不足しています"
            ]
        })

    formula = (
        "MRCI_auto ~ "
        "C(timepoint, Treatment(reference='admission'))"
    )

    try:
        model = smf.mixedlm(
            formula=formula,
            data=data,
            groups=data[
                "_episode_key"
            ],
        )

        result = model.fit(
            reml=False,
            method="lbfgs",
            maxiter=1000,
            disp=False,
        )

        confidence_intervals = (
            result.conf_int()
        )

        parameter_table = pd.DataFrame({
            "analysis_set": analysis_set,
            "term": result.params.index,
            "estimate": (
                result.params.values
            ),
            "standard_error": (
                result.bse.reindex(
                    result.params.index
                ).values
            ),
            "ci95_low": (
                confidence_intervals
                .reindex(
                    result.params.index
                )[0]
                .values
            ),
            "ci95_high": (
                confidence_intervals
                .reindex(
                    result.params.index
                )[1]
                .values
            ),
            "p_value": (
                result.pvalues.reindex(
                    result.params.index
                ).values
            ),
        })

        summary_file = (
            output_dir
            / (
                "mixed_model_"
                f"{analysis_set}_summary.txt"
            )
        )

        summary_file.write_text(
            result.summary().as_text(),
            encoding="utf-8",
        )

        return parameter_table

    except Exception as exc:
        return pd.DataFrame({
            "error": [
                (
                    f"{type(exc).__name__}: "
                    f"{exc}"
                )
            ]
        })


# ============================================================
# Graphs
# ============================================================

def plot_availability(
    availability: pd.DataFrame,
    output_dir: Path,
):
    """
    各時点の評価可能数を表示する。
    """
    plot_data = availability.copy()

    plot_data["label"] = (
        plot_data["timepoint"]
        .map(TIMEPOINT_LABELS)
    )

    positions = np.arange(
        len(plot_data)
    )

    plt.figure(
        figsize=(11, 7)
    )

    plt.bar(
        positions,
        plot_data[
            "total_episodes"
        ],
        color="#D9D9D9",
        label="All episodes",
    )

    plt.bar(
        positions,
        plot_data["eligible"],
        color="#4C78A8",
        label="Eligible",
    )

    plt.bar(
        positions,
        plot_data["auto_complete"],
        color="#59A14F",
        label="Auto-complete",
    )

    plt.xticks(
        positions,
        plot_data["label"],
    )

    plt.ylabel(
        "Number of episodes"
    )

    plt.xlabel(
        "Timepoint"
    )

    plt.title(
        "Availability of MRCI measurements"
    )

    plt.legend()

    save_figure(
        output_dir,
        "01_availability",
    )


def plot_mrci_distribution(
    df: pd.DataFrame,
    output_dir: Path,
    analysis_set: str,
):
    """
    MRCIの時点別箱ひげ図を作成する。
    """
    data = analysis_subset(
        df,
        analysis_set,
    )

    data["timepoint_label"] = (
        data["timepoint"]
        .astype(str)
        .map(TIMEPOINT_LABELS)
    )

    label_order = [
        TIMEPOINT_LABELS[item]
        for item in TIMEPOINT_ORDER
    ]

    plt.figure(
        figsize=(12, 7)
    )

    sns.boxplot(
        data=data,
        x="timepoint_label",
        y="MRCI_auto",
        order=label_order,
        color="#9ECAE1",
        showfliers=False,
    )

    sample_size = min(
        1000,
        len(data),
    )

    if sample_size > 0:
        sample = data.sample(
            n=sample_size,
            random_state=20260720,
        )

        sns.stripplot(
            data=sample,
            x="timepoint_label",
            y="MRCI_auto",
            order=label_order,
            color="black",
            alpha=0.18,
            size=2.5,
            jitter=0.2,
        )

    plt.xlabel(
        "Timepoint"
    )

    plt.ylabel(
        "MRCI"
    )

    plt.title(
        f"MRCI distribution: {analysis_set}"
    )

    save_figure(
        output_dir,
        (
            "02_mrci_distribution_"
            f"{analysis_set}"
        ),
    )


def plot_summary_trajectory(
    descriptive: pd.DataFrame,
    output_dir: Path,
    analysis_set: str,
):
    """
    平均値・中央値の経時変化を表示する。
    """
    data = descriptive.copy()

    data["label"] = (
        data["timepoint"]
        .map(TIMEPOINT_LABELS)
    )

    positions = np.arange(
        len(data)
    )

    plt.figure(
        figsize=(11, 7)
    )

    plt.plot(
        positions,
        data["mean"],
        marker="o",
        linewidth=2.5,
        label="Mean",
        color="#4C78A8",
    )

    plt.plot(
        positions,
        data["median"],
        marker="s",
        linewidth=2.5,
        label="Median",
        color="#E45756",
    )

    plt.fill_between(
        positions,
        data["q1"].astype(float),
        data["q3"].astype(float),
        alpha=0.15,
        color="#E45756",
        label="IQR",
    )

    plt.xticks(
        positions,
        data["label"],
    )

    plt.xlabel(
        "Timepoint"
    )

    plt.ylabel(
        "MRCI"
    )

    plt.title(
        f"Longitudinal MRCI summary: {analysis_set}"
    )

    plt.legend()

    save_figure(
        output_dir,
        (
            "03_mrci_summary_trajectory_"
            f"{analysis_set}"
        ),
    )


def plot_individual_trajectories(
    df: pd.DataFrame,
    output_dir: Path,
    analysis_set: str,
    max_episodes: int = 100,
):
    """
    無作為抽出した入院エピソードの個別軌跡を表示する。
    """
    data = analysis_subset(
        df,
        analysis_set,
    )

    episode_keys = (
        data["_episode_key"]
        .drop_duplicates()
    )

    if len(episode_keys) > max_episodes:
        rng = np.random.default_rng(
            20260720
        )

        selected = rng.choice(
            episode_keys.to_numpy(),
            size=max_episodes,
            replace=False,
        )
    else:
        selected = episode_keys.to_numpy()

    sample = data.loc[
        data["_episode_key"].isin(
            selected
        )
    ].copy()

    sample["time_number"] = (
        sample["timepoint"]
        .astype(str)
        .map({
            timepoint: index
            for index, timepoint
            in enumerate(
                TIMEPOINT_ORDER
            )
        })
    )

    plt.figure(
        figsize=(12, 8)
    )

    for _, group in sample.groupby(
        "_episode_key"
    ):
        group = group.sort_values(
            "time_number"
        )

        plt.plot(
            group["time_number"],
            group["MRCI_auto"],
            color="#4C78A8",
            alpha=0.15,
            linewidth=1,
        )

    median_values = (
        sample.groupby(
            "time_number"
        )["MRCI_auto"]
        .median()
        .sort_index()
    )

    plt.plot(
        median_values.index,
        median_values.values,
        color="black",
        marker="o",
        linewidth=3,
        label="Median",
    )

    plt.xticks(
        range(
            len(TIMEPOINT_ORDER)
        ),
        [
            TIMEPOINT_LABELS[item]
            for item in TIMEPOINT_ORDER
        ],
    )

    plt.xlabel(
        "Timepoint"
    )

    plt.ylabel(
        "MRCI"
    )

    plt.title(
        (
            "Individual MRCI trajectories "
            f"(maximum {max_episodes} episodes)"
        )
    )

    plt.legend()

    save_figure(
        output_dir,
        (
            "04_individual_trajectories_"
            f"{analysis_set}"
        ),
    )


def plot_section_components(
    component_statistics: pd.DataFrame,
    output_dir: Path,
    analysis_set: str,
):
    """
    Section A/B/C平均値の積み上げ棒グラフを作成する。
    """
    data = component_statistics.copy()

    positions = np.arange(
        len(data)
    )

    section_a = data[
        "section_A_mean"
    ].fillna(0)

    section_b = data[
        "section_B_mean"
    ].fillna(0)

    section_c = data[
        "section_C_mean"
    ].fillna(0)

    plt.figure(
        figsize=(11, 7)
    )

    plt.bar(
        positions,
        section_a,
        label="Section A",
        color="#4C78A8",
    )

    plt.bar(
        positions,
        section_b,
        bottom=section_a,
        label="Section B",
        color="#F28E2B",
    )

    plt.bar(
        positions,
        section_c,
        bottom=section_a + section_b,
        label="Section C",
        color="#59A14F",
    )

    plt.xticks(
        positions,
        [
            TIMEPOINT_LABELS[item]
            for item in data[
                "timepoint"
            ]
        ],
    )

    plt.xlabel(
        "Timepoint"
    )

    plt.ylabel(
        "Mean score"
    )

    plt.title(
        f"Mean MRCI components: {analysis_set}"
    )

    plt.legend()

    save_figure(
        output_dir,
        (
            "05_section_components_"
            f"{analysis_set}"
        ),
    )


def plot_change_from_admission(
    changes: pd.DataFrame,
    output_dir: Path,
    analysis_set: str,
):
    """
    入院時からの変化量を箱ひげ図で表示する。
    """
    if changes.empty:
        return

    changes = changes.copy()

    changes["followup_label"] = (
        changes["followup"]
        .map(TIMEPOINT_LABELS)
    )

    order = [
        TIMEPOINT_LABELS[item]
        for item in [
            "day14",
            "day30",
            "day60",
            "discharge",
        ]
    ]

    plt.figure(
        figsize=(11, 7)
    )

    sns.boxplot(
        data=changes,
        x="followup_label",
        y="change_from_admission",
        order=order,
        color="#B2DF8A",
        showfliers=False,
    )

    plt.axhline(
        0,
        color="black",
        linestyle="--",
        linewidth=1.5,
    )

    plt.xlabel(
        "Follow-up timepoint"
    )

    plt.ylabel(
        "Change in MRCI from admission"
    )

    plt.title(
        f"Paired change from admission: {analysis_set}"
    )

    save_figure(
        output_dir,
        (
            "06_change_from_admission_"
            f"{analysis_set}"
        ),
    )


def plot_review_rate(
    availability: pd.DataFrame,
    output_dir: Path,
):
    """
    各時点のレビュー必要率を表示する。
    """
    data = availability.copy()

    data["review_percent"] = (
        data["review_rate"]
        * 100
    )

    positions = np.arange(
        len(data)
    )

    plt.figure(
        figsize=(11, 7)
    )

    bars = plt.bar(
        positions,
        data["review_percent"],
        color="#E15759",
    )

    plt.xticks(
        positions,
        [
            TIMEPOINT_LABELS[item]
            for item in data[
                "timepoint"
            ]
        ],
    )

    plt.xlabel(
        "Timepoint"
    )

    plt.ylabel(
        "Review-required rate (%)"
    )

    plt.title(
        "Manual review requirement"
    )

    for bar, value in zip(
        bars,
        data["review_percent"],
    ):
        if pd.notna(value):
            plt.text(
                bar.get_x()
                + bar.get_width() / 2,
                bar.get_height(),
                f"{value:.1f}%",
                ha="center",
                va="bottom",
                fontsize=10,
            )

    save_figure(
        output_dir,
        "07_review_rate",
    )


# ============================================================
# Excel output
# ============================================================

def write_excel_report(
    output_file: Path,
    tables: dict[str, pd.DataFrame],
):
    """
    複数の統計表を1つのExcelファイルへ出力する。
    """
    try:
        with pd.ExcelWriter(
            output_file,
            engine="openpyxl",
        ) as writer:
            for sheet_name, table in (
                tables.items()
            ):
                safe_sheet_name = (
                    sheet_name[:31]
                )

                table.to_excel(
                    writer,
                    sheet_name=safe_sheet_name,
                    index=False,
                )

    except ImportError:
        print(
            "WARNING: openpyxlがないため"
            "Excelファイルを作成できませんでした。",
            file=sys.stderr,
        )


# ============================================================
# Main analysis
# ============================================================

def analyze_mrci_results(
    input_csv: str,
    output_dir: str,
    max_spaghetti_episodes: int = 100,
):
    """
    縦断MRCIの統計解析とグラフ作成を実行する。
    """
    output_path = Path(
        output_dir
    )

    output_path.mkdir(
        parents=True,
        exist_ok=True,
    )

    configure_plot_style()

    df = prepare_longitudinal_data(
        input_csv
    )

    availability = make_availability_table(
        df
    )

    status_table = make_status_table(
        df
    )

    descriptive_eligible = (
        make_descriptive_statistics(
            df,
            "eligible",
        )
    )

    descriptive_complete = (
        make_descriptive_statistics(
            df,
            "auto_complete",
        )
    )

    components_eligible = (
        make_component_statistics(
            df,
            "eligible",
        )
    )

    components_complete = (
        make_component_statistics(
            df,
            "auto_complete",
        )
    )

    (
        paired_eligible,
        changes_eligible,
    ) = paired_comparisons(
        df,
        "eligible",
    )

    (
        paired_complete,
        changes_complete,
    ) = paired_comparisons(
        df,
        "auto_complete",
    )

    mixed_eligible = (
        fit_mixed_effects_model(
            df,
            "eligible",
            output_path,
        )
    )

    mixed_complete = (
        fit_mixed_effects_model(
            df,
            "auto_complete",
            output_path,
        )
    )

    # CSV outputs
    tables = {
        "availability": availability,
        "score_status": status_table,
        "descriptive_eligible": (
            descriptive_eligible
        ),
        "descriptive_auto_complete": (
            descriptive_complete
        ),
        "components_eligible": (
            components_eligible
        ),
        "components_auto_complete": (
            components_complete
        ),
        "paired_eligible": (
            paired_eligible
        ),
        "paired_auto_complete": (
            paired_complete
        ),
        "mixed_eligible": (
            mixed_eligible
        ),
        "mixed_auto_complete": (
            mixed_complete
        ),
    }

    for name, table in tables.items():
        table.to_csv(
            output_path
            / f"{name}.csv",
            index=False,
            encoding="utf-8-sig",
        )

    write_excel_report(
        output_path
        / "mrci_statistical_report.xlsx",
        tables,
    )

    # Graphs
    plot_availability(
        availability,
        output_path,
    )

    plot_review_rate(
        availability,
        output_path,
    )

    for analysis_set in [
        "eligible",
        "auto_complete",
    ]:
        if analysis_set == "eligible":
            descriptive = (
                descriptive_eligible
            )

            components = (
                components_eligible
            )

            changes = (
                changes_eligible
            )

        else:
            descriptive = (
                descriptive_complete
            )

            components = (
                components_complete
            )

            changes = (
                changes_complete
            )

        plot_mrci_distribution(
            df,
            output_path,
            analysis_set,
        )

        plot_summary_trajectory(
            descriptive,
            output_path,
            analysis_set,
        )

        plot_individual_trajectories(
            df,
            output_path,
            analysis_set,
            max_episodes=(
                max_spaghetti_episodes
            ),
        )

        plot_section_components(
            components,
            output_path,
            analysis_set,
        )

        plot_change_from_admission(
            changes,
            output_path,
            analysis_set,
        )

    log_lines = [
        f"input_csv={input_csv}",
        f"rows={len(df)}",
        (
            "episodes="
            f"{df['_episode_key'].nunique()}"
        ),
        (
            "patients="
            f"{df['患者ID'].nunique()}"
        ),
        (
            "max_spaghetti_episodes="
            f"{max_spaghetti_episodes}"
        ),
        "",
        "解析セット:",
        (
            "eligible = eligible=1でMRCI値がある"
            "全時点。レビュー対象の暫定値を含む。"
        ),
        (
            "auto_complete = 自動採点完了かつ"
            "レビュー不要の時点だけ。"
        ),
        "",
        "統計:",
        (
            "入院時との対応比較には"
            "Wilcoxon符号付順位検定を使用。"
        ),
        (
            "day14、day30、day60、退院時の"
            "4比較についてHolm補正を実施。"
        ),
        (
            "混合モデルはランダム切片モデルであり、"
            "探索的解析として出力。"
        ),
        "",
        "重要:",
        (
            "day14、day30、day60の欠測には、"
            "評価日前退院による構造的欠測が含まれる。"
        ),
        (
            "したがって各時点の単純比較は、"
            "入院期間が長い患者を選択する可能性がある。"
        ),
        (
            "MRCI_autoが暫定値または下限値の場合、"
            "主要解析前に手作業レビューが必要。"
        ),
    ]

    (
        output_path
        / "analysis_log.txt"
    ).write_text(
        "\n".join(
            log_lines
        ),
        encoding="utf-8",
    )

    print(
        "MRCI visualization and statistical analysis completed."
    )

    print(
        f"出力先: {output_path}"
    )

    print(
        "主要統計: "
        f"{output_path / 'mrci_statistical_report.xlsx'}"
    )

    print(
        "グラフ: PNGおよびPDF"
    )

    return tables


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description=(
            "縦断MRCI結果を可視化し、"
            "記述統計・対応比較・混合モデルを実行する"
        )
    )

    parser.add_argument(
        "input_csv",
        help=(
            "mrci_longitudinal_long.csv"
        ),
    )

    parser.add_argument(
        "--output-dir",
        default="mrci_analysis",
        help=(
            "解析結果の出力ディレクトリ"
        ),
    )

    parser.add_argument(
        "--max-spaghetti-episodes",
        type=int,
        default=100,
        help=(
            "個別軌跡グラフに表示する"
            "最大入院エピソード数"
        ),
    )

    args = parser.parse_args()

    if (
        args.max_spaghetti_episodes
        < 1
    ):
        parser.error(
            "--max-spaghetti-episodesは"
            "1以上が必要です"
        )

    analyze_mrci_results(
        input_csv=args.input_csv,
        output_dir=args.output_dir,
        max_spaghetti_episodes=(
            args.max_spaghetti_episodes
        ),
    )


if __name__ == "__main__":
    try:
        main()

    except Exception as exc:
        print(
            f"ERROR: {type(exc).__name__}: {exc}",
            file=sys.stderr,
        )

        sys.exit(1)
