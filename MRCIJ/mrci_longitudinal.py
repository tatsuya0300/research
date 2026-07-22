from __future__ import annotations

import argparse
import sys
from pathlib import Path

import pandas as pd

from mrci_auto import (
    SECTION_C_WEIGHTS,
    canonical_drug_name_hint,
    classify_additional_directions,
    classify_dosage_form,
    classify_exclusion,
    classify_frequency,
    find_column,
    normalize_headers,
    normalize_text,
    normalize_unit,
    parse_float,
    read_csv_auto_encoding,
)


# ============================================================
# Date utilities
# ============================================================

def parse_date_value(value) -> pd.Timestamp:
    """
    1つの日付をTimestampへ変換する。

    変換できない場合はNaTを返す。
    時刻が含まれる場合は日付単位へ正規化する。
    """
    text = normalize_text(value)

    if not text:
        return pd.NaT

    parsed = pd.to_datetime(
        text,
        errors="coerce",
    )

    if pd.isna(parsed):
        return pd.NaT

    return pd.Timestamp(parsed).normalize()


def parse_date_column(
    series: pd.Series,
) -> pd.Series:
    """
    日付列をTimestampへ変換する。

    時刻が含まれる場合は日付単位へ正規化する。
    """
    cleaned = series.map(
        normalize_text
    ).replace("", pd.NA)

    parsed = pd.to_datetime(
        cleaned,
        errors="coerce",
    )

    return parsed.dt.normalize()


def date_to_string(value) -> str:
    """
    TimestampをYYYY-MM-DDへ変換する。
    """
    if value is None or pd.isna(value):
        return ""

    return pd.Timestamp(value).strftime(
        "%Y-%m-%d"
    )


# ============================================================
# General utilities
# ============================================================

def append_unique(
    items: list[str],
    value: str,
):
    """
    リストに重複なく文字列を追加する。
    """
    if value and value not in items:
        items.append(value)


def first_nonempty(
    values: pd.Series,
) -> str:
    """
    Series内で最初の空欄でない値を返す。
    """
    for value in values:
        text = normalize_text(value)

        if text:
            return text

    return ""


def add_review_reason(
    detail: pd.DataFrame,
    indices: list,
    reason: str,
):
    """
    複数のdetail行へレビュー理由を追加する。
    """
    for index in indices:
        old_reason = normalize_text(
            detail.at[
                index,
                "review_reason",
            ]
        )

        reasons = [
            item.strip()
            for item in old_reason.split(";")
            if item.strip()
        ]

        if reason not in reasons:
            reasons.append(reason)

        detail.at[
            index,
            "review_required",
        ] = 1

        detail.at[
            index,
            "review_reason",
        ] = "; ".join(reasons)


# ============================================================
# Column detection
# ============================================================

def detect_medication_columns(
    df: pd.DataFrame,
) -> dict:
    """
    薬歴CSVの列を同定する。
    """
    return {
        "patient": find_column(
            df,
            [
                "患者ID",
                "患者ＩＤ",
                "患者Id",
            ],
            required=True,
        ),

        "patient_name": find_column(
            df,
            [
                "患者氏名",
                "氏名",
            ],
        ),

        "episode": find_column(
            df,
            [
                "入院ID",
                "入院ＩＤ",
                "入院番号",
                "入院エピソードID",
                "入院エピソード",
                "synthetic_episode_id",
            ],
        ),

        "drug": find_column(
            df,
            [
                "薬剤名",
                "医薬品名",
                "薬品名",
            ],
            required=True,
        ),

        "sig": find_column(
            df,
            [
                "用法",
                "用法名称",
                "服用方法",
            ],
            required=True,
        ),

        "amount": find_column(
            df,
            [
                "1日量",
                "一日量",
                "投与量",
            ],
        ),

        "unit": find_column(
            df,
            [
                "単位",
                "1日量単位",
                "投与量単位",
            ],
        ),

        "ingredient": find_column(
            df,
            [
                "成分コード",
                "一般名コード",
                "一般名",
                "成分名",
                "薬剤キー",
            ],
        ),

        "ward": find_column(
            df,
            ["病棟"],
        ),

        "team": find_column(
            df,
            ["チーム"],
        ),

        "start": find_column(
            df,
            [
                "服用開始日",
                "使用開始日",
                "開始日",
            ],
        ),

        "end": find_column(
            df,
            [
                "使用終了日",
                "服用終了日",
                "終了日",
            ],
        ),

        "order_date": find_column(
            df,
            [
                "オーダ指示日",
                "オーダー指示日",
                "処方日",
            ],
        ),

        "days": find_column(
            df,
            [
                "処方日数・回数",
                "処方日数",
                "投与日数",
            ],
        ),
    }


def detect_admission_columns(
    df: pd.DataFrame,
) -> dict:
    """
    入退院CSVの列を同定する。
    """
    return {
        "patient": find_column(
            df,
            [
                "患者ID",
                "患者ＩＤ",
                "患者Id",
            ],
            required=True,
        ),

        "patient_name": find_column(
            df,
            [
                "患者氏名",
                "氏名",
            ],
        ),

        "admission": find_column(
            df,
            [
                "入院日",
                "入院年月日",
                "入院日時",
            ],
            required=True,
        ),

        "discharge": find_column(
            df,
            [
                "退院日",
                "退院年月日",
                "退院日時",
            ],
            required=True,
        ),

        "episode": find_column(
            df,
            [
                "入院ID",
                "入院ＩＤ",
                "入院番号",
                "入院エピソードID",
                "入院エピソード",
            ],
        ),

        "ward": find_column(
            df,
            ["病棟"],
        ),

        "team": find_column(
            df,
            ["チーム"],
        ),
    }


# ============================================================
# Medication preprocessing
# ============================================================

def preprocess_medications(
    medications: pd.DataFrame,
    columns: dict,
) -> pd.DataFrame:
    """
    薬歴CSVを正規化し、日付列・薬剤キーを追加する。
    """
    df = medications.copy()

    patient_col = columns["patient"]
    episode_col = columns["episode"]
    drug_col = columns["drug"]
    sig_col = columns["sig"]

    df["_patient_id"] = (
        df[patient_col].map(
            normalize_text
        )
    )

    if episode_col is not None:
        df["_episode_id"] = (
            df[episode_col].map(
                normalize_text
            )
        )
    else:
        df["_episode_id"] = ""

    df["_drug_name"] = (
        df[drug_col].map(
            normalize_text
        )
    )

    df["_sig"] = (
        df[sig_col].map(
            normalize_text
        )
    )

    df = df.loc[
        (df["_patient_id"] != "")
        & (df["_drug_name"] != "")
    ].copy()

    start_col = columns["start"]
    end_col = columns["end"]
    order_date_col = columns["order_date"]

    if start_col is not None:
        df["_start_date"] = (
            parse_date_column(
                df[start_col]
            )
        )
    else:
        df["_start_date"] = pd.NaT

    if order_date_col is not None:
        df["_order_date"] = (
            parse_date_column(
                df[order_date_col]
            )
        )
    else:
        df["_order_date"] = pd.NaT

    if end_col is not None:
        df["_end_date"] = (
            parse_date_column(
                df[end_col]
            )
        )
    else:
        df["_end_date"] = pd.NaT

    # 服用開始日が欠損している場合は、
    # オーダ指示日を補助的に使用する。
    df["_effective_start_date"] = (
        df["_start_date"].fillna(
            df["_order_date"]
        )
    )

    ingredient_col = columns["ingredient"]

    medication_keys = []
    medication_key_sources = []

    for _, row in df.iterrows():
        ingredient_value = ""

        if ingredient_col is not None:
            ingredient_value = normalize_text(
                row.get(
                    ingredient_col,
                    "",
                )
            )

        if ingredient_value:
            medication_keys.append(
                ingredient_value
            )

            medication_key_sources.append(
                "ingredient_column"
            )
        else:
            medication_keys.append(
                canonical_drug_name_hint(
                    row.get(
                        drug_col,
                        "",
                    )
                )
            )

            medication_key_sources.append(
                "drug_name_heuristic"
            )

    df["_medication_key"] = medication_keys

    df["_medication_key_source"] = (
        medication_key_sources
    )

    exclusion_results = df.apply(
        lambda row: classify_exclusion(
            row.get(drug_col, ""),
            row.get(sig_col, ""),
        ),
        axis=1,
    )

    df["_excluded"] = [
        int(result[0])
        for result in exclusion_results
    ]

    df["_exclusion_reason"] = [
        result[1]
        for result in exclusion_results
    ]

    return df


# ============================================================
# Active medication extraction
# ============================================================

def extract_active_medications(
    medications: pd.DataFrame,
    patient_id: str,
    episode_id: str,
    evaluation_date: pd.Timestamp,
    medication_episode_available: bool,
) -> pd.DataFrame:
    """
    指定患者・入院エピソード・評価日に有効な薬剤を抽出する。

    日付判定:
        開始日 <= 評価日 <= 終了日

    開始日・終了日の空欄は制限なしとする。

    薬歴CSVに入院ID列がある場合は、
    同一入院IDの薬剤だけを抽出する。
    """
    patient_id = normalize_text(
        patient_id
    )

    episode_id = normalize_text(
        episode_id
    )

    patient_medications = medications.loc[
        medications["_patient_id"]
        == patient_id
    ].copy()

    if medication_episode_available:
        patient_medications = (
            patient_medications.loc[
                patient_medications[
                    "_episode_id"
                ] == episode_id
            ].copy()
        )

    active_mask = (
        (
            patient_medications[
                "_effective_start_date"
            ].isna()
            |
            (
                patient_medications[
                    "_effective_start_date"
                ]
                <= evaluation_date
            )
        )
        &
        (
            patient_medications[
                "_end_date"
            ].isna()
            |
            (
                patient_medications[
                    "_end_date"
                ]
                >= evaluation_date
            )
        )
    )

    active = patient_medications.loc[
        active_mask
    ].copy()

    active = active.loc[
        active["_excluded"] == 0
    ].copy()

    return active


# ============================================================
# Snapshot scoring
# ============================================================

def score_snapshot(
    active_medications: pd.DataFrame,
    medication_columns: dict,
    episode_row_key: str,
    episode_id: str,
    patient_id: str,
    patient_name: str,
    ward: str,
    team: str,
    timepoint: str,
    evaluation_date: pd.Timestamp,
    admission_date: pd.Timestamp,
    discharge_date: pd.Timestamp,
) -> tuple[dict, pd.DataFrame]:
    """
    1患者・1入院エピソード・1評価日のMRCIを算出する。
    """
    drug_col = medication_columns["drug"]
    sig_col = medication_columns["sig"]
    amount_col = medication_columns["amount"]
    unit_col = medication_columns["unit"]
    medication_episode_col = medication_columns[
        "episode"
    ]
    ward_col = medication_columns["ward"]
    team_col = medication_columns["team"]
    patient_name_col = medication_columns[
        "patient_name"
    ]

    active = active_medications.copy()

    # 完全に同一の処方行だけを除外する。
    dedup_columns = [
        "_patient_id",
        "_episode_id",
        "_drug_name",
        "_sig",
        "_effective_start_date",
        "_end_date",
    ]

    for optional_col in [
        amount_col,
        unit_col,
    ]:
        if (
            optional_col is not None
            and optional_col not in dedup_columns
        ):
            dedup_columns.append(
                optional_col
            )

    active = active.drop_duplicates(
        subset=dedup_columns,
        keep="first",
    ).copy()

    if active.empty:
        summary = {
            "_episode_row_key": episode_row_key,
            "episode_id": episode_id,
            "患者ID": patient_id,
            "患者氏名": patient_name,
            "病棟": ward,
            "チーム": team,
            "timepoint": timepoint,
            "evaluation_date": (
                date_to_string(
                    evaluation_date
                )
            ),
            "admission_date": (
                date_to_string(
                    admission_date
                )
            ),
            "discharge_date": (
                date_to_string(
                    discharge_date
                )
            ),
            "eligible": 1,
            "active_medication_rows": 0,
            "medication_count": 0,
            "section_A": 0.0,
            "section_B": 0.0,
            "section_C": 0.0,
            "MRCI_auto": 0.0,
            "unmapped_A": 0,
            "unmapped_B": 0,
            "review_required": 1,
            "review_count": 1,
            "score_status": (
                "no_active_medication_review_required"
            ),
        }

        return summary, pd.DataFrame()

    detail_rows = []

    for source_index, row in active.iterrows():
        drug_name = normalize_text(
            row.get(drug_col, "")
        )

        sig = normalize_text(
            row.get(sig_col, "")
        )

        daily_amount_raw = (
            row.get(amount_col, "")
            if amount_col is not None
            else ""
        )

        amount_unit_raw = (
            row.get(unit_col, "")
            if unit_col is not None
            else ""
        )

        daily_amount = parse_float(
            daily_amount_raw
        )

        normalized_unit = normalize_unit(
            amount_unit_raw
        )

        (
            form_code,
            form_weight,
            form_confidence,
            form_note,
        ) = classify_dosage_form(
            drug_name=drug_name,
            sig=sig,
            amount_unit=normalized_unit,
        )

        (
            frequency_code,
            frequency_weight,
            daily_frequency,
            frequency_confidence,
            frequency_note,
        ) = classify_frequency(
            sig=sig,
            form_category=form_code,
        )

        (
            direction_codes,
            direction_weight,
            direction_evidence,
            direction_review_notes,
        ) = classify_additional_directions(
            sig=sig,
            drug_name=drug_name,
            form_category=form_code,
            daily_amount=daily_amount,
            amount_unit=normalized_unit,
            daily_frequency=daily_frequency,
        )

        review_reasons = []

        if form_code is None:
            append_unique(
                review_reasons,
                "Section A未分類",
            )

        if frequency_code is None:
            append_unique(
                review_reasons,
                "Section B未分類",
            )

        if form_confidence == "low":
            append_unique(
                review_reasons,
                "Section A判定の信頼度が低い",
            )

        if frequency_confidence == "low":
            append_unique(
                review_reasons,
                "Section B判定の信頼度が低い",
            )

        if pd.isna(
            row.get(
                "_effective_start_date",
                pd.NaT,
            )
        ):
            append_unique(
                review_reasons,
                "服用開始日・オーダ指示日がともに欠損",
            )

        if (
            medication_episode_col is not None
            and not normalize_text(
                row.get(
                    "_episode_id",
                    "",
                )
            )
        ):
            append_unique(
                review_reasons,
                "薬歴側の入院IDが欠損",
            )

        for note in direction_review_notes:
            append_unique(
                review_reasons,
                note,
            )

        row_patient_name = (
            normalize_text(
                row.get(
                    patient_name_col,
                    "",
                )
            )
            if patient_name_col is not None
            else ""
        )

        row_ward = (
            normalize_text(
                row.get(
                    ward_col,
                    "",
                )
            )
            if ward_col is not None
            else ""
        )

        row_team = (
            normalize_text(
                row.get(
                    team_col,
                    "",
                )
            )
            if team_col is not None
            else ""
        )

        detail_rows.append({
            "_episode_row_key": episode_row_key,
            "episode_id": episode_id,
            "患者ID": patient_id,
            "患者氏名": (
                row_patient_name
                or patient_name
            ),
            "病棟": row_ward or ward,
            "チーム": row_team or team,
            "timepoint": timepoint,
            "evaluation_date": (
                date_to_string(
                    evaluation_date
                )
            ),
            "admission_date": (
                date_to_string(
                    admission_date
                )
            ),
            "discharge_date": (
                date_to_string(
                    discharge_date
                )
            ),
            "source_index": source_index,
            "薬剤名": drug_name,
            "medication_key": normalize_text(
                row.get(
                    "_medication_key",
                    "",
                )
            ),
            "medication_key_source": (
                normalize_text(
                    row.get(
                        "_medication_key_source",
                        "",
                    )
                )
            ),
            "服用開始日": date_to_string(
                row.get(
                    "_effective_start_date",
                    pd.NaT,
                )
            ),
            "使用終了日": date_to_string(
                row.get(
                    "_end_date",
                    pd.NaT,
                )
            ),
            "1日量_original": normalize_text(
                daily_amount_raw
            ),
            "単位_original": normalize_text(
                amount_unit_raw
            ),
            "単位_normalized": normalized_unit,
            "用法": sig,
            "A_form_code": form_code or "",
            "A_original_weight": (
                form_weight
                if form_weight is not None
                else ""
            ),
            "A_confidence": form_confidence,
            "A_note": form_note,
            "B_frequency_code": (
                frequency_code or ""
            ),
            "B_original_weight": (
                frequency_weight
                if frequency_weight is not None
                else ""
            ),
            "B_daily_frequency": (
                daily_frequency
                if daily_frequency is not None
                else ""
            ),
            "B_confidence": (
                frequency_confidence
            ),
            "B_note": frequency_note,
            "C_direction_codes": "|".join(
                direction_codes
            ),
            "C_original_weight": (
                direction_weight
            ),
            "C_evidence": (
                direction_evidence
            ),
            "review_required": int(
                bool(review_reasons)
            ),
            "review_reason": "; ".join(
                review_reasons
            ),
        })

    detail = pd.DataFrame(
        detail_rows
    )

    detail["_A_numeric"] = pd.to_numeric(
        detail["A_original_weight"],
        errors="coerce",
    ).fillna(0.0)

    detail["_B_numeric"] = pd.to_numeric(
        detail["B_original_weight"],
        errors="coerce",
    ).fillna(0.0)

    detail["_C_numeric"] = pd.to_numeric(
        detail["C_original_weight"],
        errors="coerce",
    ).fillna(0.0)

    detail["A_counted_weight"] = 0.0
    detail["B_counted_weight"] = 0.0
    detail["C_counted_weight"] = 0.0
    detail["A_duplicate_form"] = 0
    detail["same_medication_group"] = 0

    # --------------------------------------------------------
    # Section A
    # 同じ剤形カテゴリーは患者・評価時点内で1回だけ加点
    # --------------------------------------------------------

    counted_forms = set()

    for index in detail.index:
        form_code = detail.at[
            index,
            "A_form_code",
        ]

        if not form_code:
            continue

        if form_code in counted_forms:
            detail.at[
                index,
                "A_duplicate_form",
            ] = 1
        else:
            detail.at[
                index,
                "A_counted_weight",
            ] = detail.at[
                index,
                "_A_numeric",
            ]

            counted_forms.add(
                form_code
            )

    # --------------------------------------------------------
    # Section B/C
    # 同一薬剤＋同一剤形単位で集計
    # --------------------------------------------------------

    detail["_aggregation_key"] = (
        detail["medication_key"]
        + "||"
        + detail["A_form_code"]
    )

    for _, group in detail.groupby(
        "_aggregation_key",
        sort=False,
        dropna=False,
    ):
        indices = list(
            group.index
        )

        if len(indices) > 1:
            detail.loc[
                indices,
                "same_medication_group",
            ] = 1

            heuristic_used = bool(
                (
                    group[
                        "medication_key_source"
                    ]
                    == "drug_name_heuristic"
                ).any()
            )

            if heuristic_used:
                add_review_reason(
                    detail,
                    indices,
                    "薬剤名推定キーによる同一薬剤候補。"
                    "成分コードがないため統合結果を要確認",
                )

        # Section B：
        # 同一薬剤について1つの頻度カテゴリーを加点
        valid_b_indices = [
            index
            for index in indices
            if detail.at[
                index,
                "_B_numeric",
            ] > 0
        ]

        if valid_b_indices:
            selected_index = min(
                valid_b_indices,
                key=lambda index: detail.at[
                    index,
                    "_B_numeric",
                ],
            )

            detail.at[
                selected_index,
                "B_counted_weight",
            ] = detail.at[
                selected_index,
                "_B_numeric",
            ]

        frequency_codes = {
            detail.at[
                index,
                "B_frequency_code",
            ]
            for index in indices
            if detail.at[
                index,
                "B_frequency_code",
            ]
        }

        if len(frequency_codes) > 1:
            add_review_reason(
                detail,
                indices,
                "同一薬剤候補に異なる頻度がある。"
                "Bは複雑性の低いカテゴリーを暫定採用",
            )

        # Section C：
        # 同一薬剤内で各カテゴリーは1回だけ加点
        counted_c_codes = set()

        for index in indices:
            row_codes = [
                code
                for code in normalize_text(
                    detail.at[
                        index,
                        "C_direction_codes",
                    ]
                ).split("|")
                if code
            ]

            new_codes = [
                code
                for code in row_codes
                if code not in counted_c_codes
            ]

            detail.at[
                index,
                "C_counted_weight",
            ] = sum(
                SECTION_C_WEIGHTS[code]
                for code in new_codes
            )

            counted_c_codes.update(
                new_codes
            )

    section_a = float(
        detail["A_counted_weight"].sum()
    )

    section_b = float(
        detail["B_counted_weight"].sum()
    )

    section_c = float(
        detail["C_counted_weight"].sum()
    )

    unmapped_a = int(
        (detail["A_form_code"] == "").sum()
    )

    unmapped_b = int(
        (
            detail["B_frequency_code"]
            == ""
        ).sum()
    )

    review_required = int(
        detail["review_required"].max()
    )

    review_count = int(
        detail["review_required"].sum()
    )

    if (
        unmapped_a == 0
        and unmapped_b == 0
        and review_required == 0
    ):
        score_status = (
            "auto_complete_no_review_flag"
        )
    else:
        score_status = (
            "lower_bound_or_manual_review_required"
        )

    summary = {
        "_episode_row_key": episode_row_key,
        "episode_id": episode_id,
        "患者ID": patient_id,
        "患者氏名": (
            first_nonempty(
                detail["患者氏名"]
            )
            or patient_name
        ),
        "病棟": (
            first_nonempty(
                detail["病棟"]
            )
            or ward
        ),
        "チーム": (
            first_nonempty(
                detail["チーム"]
            )
            or team
        ),
        "timepoint": timepoint,
        "evaluation_date": (
            date_to_string(
                evaluation_date
            )
        ),
        "admission_date": (
            date_to_string(
                admission_date
            )
        ),
        "discharge_date": (
            date_to_string(
                discharge_date
            )
        ),
        "eligible": 1,
        "active_medication_rows": int(
            len(detail)
        ),
        "medication_count": int(
            detail[
                "_aggregation_key"
            ].nunique()
        ),
        "section_A": section_a,
        "section_B": section_b,
        "section_C": section_c,
        "MRCI_auto": (
            section_a
            + section_b
            + section_c
        ),
        "unmapped_A": unmapped_a,
        "unmapped_B": unmapped_b,
        "review_required": (
            review_required
        ),
        "review_count": (
            review_count
        ),
        "score_status": score_status,
    }

    return summary, detail


# ============================================================
# Wide-format construction
# ============================================================

def build_wide_output(
    longitudinal: pd.DataFrame,
) -> pd.DataFrame:
    """
    縦長データから横長データを作成する。

    pandas.pivotは、
    ・退院日欠損
    ・入院ID重複
    の場合に行消失またはエラーの原因になり得るため、
    入院行ごとの内部キーを用いて明示的に変換する。
    """
    timepoint_order = [
        "admission",
        "day14",
        "day30",
        "day60",
        "discharge",
    ]

    value_columns = [
        "evaluation_date",
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
        "score_status",
    ]

    base_columns = [
        "episode_id",
        "患者ID",
        "患者氏名",
        "病棟",
        "チーム",
        "admission_date",
        "discharge_date",
    ]

    if longitudinal.empty:
        output_columns = list(
            base_columns
        )

        for timepoint in timepoint_order:
            for value_name in value_columns:
                output_columns.append(
                    f"{timepoint}_{value_name}"
                )

        return pd.DataFrame(
            columns=output_columns
        )

    wide_rows = []

    for _, episode_group in longitudinal.groupby(
        "_episode_row_key",
        sort=False,
        dropna=False,
    ):
        first_row = episode_group.iloc[0]

        wide_row = {
            column: first_row.get(
                column,
                "",
            )
            for column in base_columns
        }

        for timepoint in timepoint_order:
            timepoint_rows = episode_group.loc[
                episode_group["timepoint"]
                == timepoint
            ]

            if timepoint_rows.empty:
                for value_name in value_columns:
                    wide_row[
                        f"{timepoint}_{value_name}"
                    ] = ""
            else:
                timepoint_row = (
                    timepoint_rows.iloc[0]
                )

                for value_name in value_columns:
                    wide_row[
                        f"{timepoint}_{value_name}"
                    ] = timepoint_row.get(
                        value_name,
                        "",
                    )

        wide_rows.append(
            wide_row
        )

    return pd.DataFrame(
        wide_rows
    )


# ============================================================
# Longitudinal calculation
# ============================================================

def calculate_longitudinal_mrci(
    medication_csv: str,
    admission_csv: str,
    output_prefix: str,
    day14_offset: int = 14,
    day30_offset: int = 30,
    day60_offset: int = 60,
):
    """
    入院日、入院+14日、入院+30日、
    入院+60日、退院日のMRCIを
    患者・入院エピソードごとに算出する。
    """
    medications = read_csv_auto_encoding(
        medication_csv
    )

    admissions = read_csv_auto_encoding(
        admission_csv
    )

    medications = normalize_headers(
        medications
    )

    admissions = normalize_headers(
        admissions
    )

    medication_columns = (
        detect_medication_columns(
            medications
        )
    )

    admission_columns = (
        detect_admission_columns(
            admissions
        )
    )

    medications = preprocess_medications(
        medications,
        medication_columns,
    )

    admission_patient_col = (
        admission_columns["patient"]
    )

    admission_patient_name_col = (
        admission_columns["patient_name"]
    )

    admission_date_col = (
        admission_columns["admission"]
    )

    discharge_date_col = (
        admission_columns["discharge"]
    )

    episode_col = (
        admission_columns["episode"]
    )

    admission_ward_col = (
        admission_columns["ward"]
    )

    admission_team_col = (
        admission_columns["team"]
    )

    medication_episode_available = (
        medication_columns["episode"]
        is not None
    )

    admissions["_patient_id"] = (
        admissions[
            admission_patient_col
        ].map(normalize_text)
    )

    admissions["_admission_date"] = (
        parse_date_column(
            admissions[
                admission_date_col
            ]
        )
    )

    admissions["_discharge_date"] = (
        parse_date_column(
            admissions[
                discharge_date_col
            ]
        )
    )

    admissions["_invalid_reason"] = ""

    missing_patient_mask = (
        admissions["_patient_id"] == ""
    )

    missing_admission_mask = (
        admissions[
            "_admission_date"
        ].isna()
    )

    reversed_date_mask = (
        admissions[
            "_admission_date"
        ].notna()
        &
        admissions[
            "_discharge_date"
        ].notna()
        &
        (
            admissions[
                "_discharge_date"
            ]
            <
            admissions[
                "_admission_date"
            ]
        )
    )

    admissions.loc[
        missing_patient_mask,
        "_invalid_reason",
    ] = "患者ID欠損"

    admissions.loc[
        missing_admission_mask,
        "_invalid_reason",
    ] = admissions.loc[
        missing_admission_mask,
        "_invalid_reason",
    ].apply(
        lambda value: (
            f"{value}; 入院日欠損または不正"
            if value
            else "入院日欠損または不正"
        )
    )

    admissions.loc[
        reversed_date_mask,
        "_invalid_reason",
    ] = admissions.loc[
        reversed_date_mask,
        "_invalid_reason",
    ].apply(
        lambda value: (
            f"{value}; 退院日が入院日より前"
            if value
            else "退院日が入院日より前"
        )
    )

    invalid_mask = (
        missing_patient_mask
        | missing_admission_mask
        | reversed_date_mask
    )

    invalid_admissions = admissions.loc[
        invalid_mask
    ].copy()

    valid_admissions = admissions.loc[
        ~invalid_mask
    ].copy()

    summary_rows = []
    all_detail_frames = []

    for admission_index, admission_row in (
        valid_admissions.iterrows()
    ):
        episode_row_key = (
            f"admission_row_{admission_index}"
        )

        patient_id = normalize_text(
            admission_row.get(
                admission_patient_col,
                "",
            )
        )

        patient_name = (
            normalize_text(
                admission_row.get(
                    admission_patient_name_col,
                    "",
                )
            )
            if admission_patient_name_col
            is not None
            else ""
        )

        ward = (
            normalize_text(
                admission_row.get(
                    admission_ward_col,
                    "",
                )
            )
            if admission_ward_col is not None
            else ""
        )

        team = (
            normalize_text(
                admission_row.get(
                    admission_team_col,
                    "",
                )
            )
            if admission_team_col is not None
            else ""
        )

        admission_date = pd.Timestamp(
            admission_row[
                "_admission_date"
            ]
        ).normalize()

        discharge_raw = admission_row[
            "_discharge_date"
        ]

        discharge_date = (
            pd.Timestamp(
                discharge_raw
            ).normalize()
            if pd.notna(discharge_raw)
            else pd.NaT
        )

        if episode_col is not None:
            episode_id = normalize_text(
                admission_row.get(
                    episode_col,
                    "",
                )
            )
        else:
            episode_id = ""

        if not episode_id:
            episode_id = (
                f"{patient_id}_"
                f"{admission_date.strftime('%Y%m%d')}"
            )

        timepoints = [
            (
                "admission",
                admission_date,
            ),
            (
                "day14",
                admission_date
                + pd.Timedelta(
                    days=day14_offset
                ),
            ),
            (
                "day30",
                admission_date
                + pd.Timedelta(
                    days=day30_offset
                ),
            ),
            (
                "day60",
                admission_date
                + pd.Timedelta(
                    days=day60_offset
                ),
            ),
            (
                "discharge",
                discharge_date,
            ),
        ]

        for timepoint, evaluation_date in timepoints:
            # --------------------------------------------
            # 退院日欠損
            # --------------------------------------------
            if (
                timepoint == "discharge"
                and pd.isna(evaluation_date)
            ):
                summary_rows.append({
                    "_episode_row_key": (
                        episode_row_key
                    ),
                    "episode_id": episode_id,
                    "患者ID": patient_id,
                    "患者氏名": patient_name,
                    "病棟": ward,
                    "チーム": team,
                    "timepoint": timepoint,
                    "evaluation_date": "",
                    "admission_date": (
                        date_to_string(
                            admission_date
                        )
                    ),
                    "discharge_date": "",
                    "eligible": 0,
                    "active_medication_rows": "",
                    "medication_count": "",
                    "section_A": "",
                    "section_B": "",
                    "section_C": "",
                    "MRCI_auto": "",
                    "unmapped_A": "",
                    "unmapped_B": "",
                    "review_required": 1,
                    "review_count": 1,
                    "score_status": (
                        "discharge_date_missing"
                    ),
                })

                continue

            # --------------------------------------------
            # 14日、30日、60日前に退院
            # --------------------------------------------
            if (
                timepoint in {
                    "day14",
                    "day30",
                    "day60",
                }
                and pd.notna(discharge_date)
                and evaluation_date
                > discharge_date
            ):
                summary_rows.append({
                    "_episode_row_key": (
                        episode_row_key
                    ),
                    "episode_id": episode_id,
                    "患者ID": patient_id,
                    "患者氏名": patient_name,
                    "病棟": ward,
                    "チーム": team,
                    "timepoint": timepoint,
                    "evaluation_date": (
                        date_to_string(
                            evaluation_date
                        )
                    ),
                    "admission_date": (
                        date_to_string(
                            admission_date
                        )
                    ),
                    "discharge_date": (
                        date_to_string(
                            discharge_date
                        )
                    ),
                    "eligible": 0,
                    "active_medication_rows": "",
                    "medication_count": "",
                    "section_A": "",
                    "section_B": "",
                    "section_C": "",
                    "MRCI_auto": "",
                    "unmapped_A": "",
                    "unmapped_B": "",
                    "review_required": 0,
                    "review_count": 0,
                    "score_status": (
                        "not_reached_before_discharge"
                    ),
                })

                continue

            active = extract_active_medications(
                medications=medications,
                patient_id=patient_id,
                episode_id=episode_id,
                evaluation_date=evaluation_date,
                medication_episode_available=(
                    medication_episode_available
                ),
            )

            summary, detail = score_snapshot(
                active_medications=active,
                medication_columns=(
                    medication_columns
                ),
                episode_row_key=(
                    episode_row_key
                ),
                episode_id=episode_id,
                patient_id=patient_id,
                patient_name=patient_name,
                ward=ward,
                team=team,
                timepoint=timepoint,
                evaluation_date=evaluation_date,
                admission_date=admission_date,
                discharge_date=discharge_date,
            )

            summary_rows.append(
                summary
            )

            if not detail.empty:
                all_detail_frames.append(
                    detail
                )

    longitudinal_internal = pd.DataFrame(
        summary_rows
    )

    if all_detail_frames:
        detail_internal = pd.concat(
            all_detail_frames,
            ignore_index=True,
        )
    else:
        detail_internal = pd.DataFrame()

    if not detail_internal.empty:
        review_internal = (
            detail_internal.loc[
                detail_internal[
                    "review_required"
                ] == 1
            ].copy()
        )
    else:
        review_internal = pd.DataFrame()

    # 時点単位のレビュー一覧
    if not longitudinal_internal.empty:
        summary_review_internal = (
            longitudinal_internal.loc[
                longitudinal_internal[
                    "review_required"
                ] == 1
            ].copy()
        )
    else:
        summary_review_internal = (
            pd.DataFrame()
        )

    # 横長形式を作成
    wide = build_wide_output(
        longitudinal_internal
    )

    # 内部キーは最終出力から除く
    longitudinal = (
        longitudinal_internal.drop(
            columns=["_episode_row_key"],
            errors="ignore",
        )
    )

    detail = detail_internal.drop(
        columns=["_episode_row_key"],
        errors="ignore",
    )

    review = review_internal.drop(
        columns=["_episode_row_key"],
        errors="ignore",
    )

    summary_review = (
        summary_review_internal.drop(
            columns=["_episode_row_key"],
            errors="ignore",
        )
    )

    # --------------------------------------------------------
    # Output files
    # --------------------------------------------------------

    prefix = Path(
        output_prefix
    )

    prefix.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    long_file = (
        prefix.parent
        / f"{prefix.name}_long.csv"
    )

    wide_file = (
        prefix.parent
        / f"{prefix.name}_wide.csv"
    )

    detail_file = (
        prefix.parent
        / f"{prefix.name}_detail.csv"
    )

    review_file = (
        prefix.parent
        / f"{prefix.name}_review.csv"
    )

    summary_review_file = (
        prefix.parent
        / f"{prefix.name}_summary_review.csv"
    )

    invalid_admission_file = (
        prefix.parent
        / f"{prefix.name}_invalid_admissions.csv"
    )

    excluded_medication_file = (
        prefix.parent
        / f"{prefix.name}_excluded_medications.csv"
    )

    log_file = (
        prefix.parent
        / f"{prefix.name}_log.txt"
    )

    longitudinal.to_csv(
        long_file,
        index=False,
        encoding="utf-8-sig",
    )

    wide.to_csv(
        wide_file,
        index=False,
        encoding="utf-8-sig",
    )

    detail.to_csv(
        detail_file,
        index=False,
        encoding="utf-8-sig",
    )

    review.to_csv(
        review_file,
        index=False,
        encoding="utf-8-sig",
    )

    summary_review.to_csv(
        summary_review_file,
        index=False,
        encoding="utf-8-sig",
    )

    invalid_admissions.to_csv(
        invalid_admission_file,
        index=False,
        encoding="utf-8-sig",
    )

    medications.loc[
        medications["_excluded"] == 1
    ].to_csv(
        excluded_medication_file,
        index=False,
        encoding="utf-8-sig",
    )

    log_lines = [
        f"medication_csv={medication_csv}",
        f"admission_csv={admission_csv}",
        f"day14_offset={day14_offset}",
        f"day30_offset={day30_offset}",
        f"day60_offset={day60_offset}",
        (
            "day14_definition="
            f"admission_date_plus_{day14_offset}_days"
        ),
        (
            "day30_definition="
            f"admission_date_plus_{day30_offset}_days"
        ),
        (
            "day60_definition="
            f"admission_date_plus_{day60_offset}_days"
        ),
        (
            "medication_episode_column="
            f"{medication_columns['episode'] or 'not_available'}"
        ),
        (
            "admission_episode_column="
            f"{admission_columns['episode'] or 'not_available'}"
        ),
        (
            "medication_episode_matching="
            f"{int(medication_episode_available)}"
        ),
        (
            "medication_rows="
            f"{len(medications)}"
        ),
        (
            "admission_rows="
            f"{len(admissions)}"
        ),
        (
            "valid_admission_rows="
            f"{len(valid_admissions)}"
        ),
        (
            "invalid_admission_rows="
            f"{len(invalid_admissions)}"
        ),
        (
            "longitudinal_rows="
            f"{len(longitudinal)}"
        ),
        (
            "wide_rows="
            f"{len(wide)}"
        ),
        (
            "detail_rows="
            f"{len(detail)}"
        ),
        (
            "review_rows="
            f"{len(review)}"
        ),
        (
            "summary_review_rows="
            f"{len(summary_review)}"
        ),
        "",
        "注意:",
        (
            "入院日+14日、+30日、+60日は、"
            "入院日を0日として暦日加算しています。"
        ),
        (
            "入院14日目を意味する場合は"
            "--day14-offset 13を使用してください。"
        ),
        (
            "入院30日目を意味する場合は"
            "--day30-offset 29を使用してください。"
        ),
        (
            "入院60日目を意味する場合は"
            "--day60-offset 59を使用してください。"
        ),
        (
            "各評価日の前に退院している場合は"
            "not_reached_before_dischargeとしています。"
        ),
        (
            "服用開始日と終了日は包含関係です。"
            "開始日<=評価日<=終了日として判定します。"
        ),
        (
            "薬歴CSVに入院ID列がある場合は、"
            "患者IDと入院IDの両方を一致させます。"
        ),
        (
            "薬歴CSVに入院ID列がない場合は、"
            "患者IDと処方期間だけで判定するため、"
            "複数入院間の処方混入に注意が必要です。"
        ),
        (
            "MRCI_autoはルールベースの自動算出値です。"
        ),
        (
            "研究利用前にreview.csvおよび"
            "summary_review.csvを確認してください。"
        ),
        (
            "手作業採点との妥当性検証が必要です。"
        ),
    ]

    log_file.write_text(
        "\n".join(log_lines),
        encoding="utf-8",
    )

    print(
        "Longitudinal MRCI calculation completed."
    )

    print(
        f"縦長患者時点データ: {long_file}"
    )

    print(
        f"横長患者データ:     {wide_file}"
    )

    print(
        f"薬剤別詳細:         {detail_file}"
    )

    print(
        f"薬剤単位の要確認:   {review_file}"
    )

    print(
        f"時点単位の要確認:   {summary_review_file}"
    )

    print(
        f"不正な入退院データ: {invalid_admission_file}"
    )

    print(
        f"除外薬剤:           {excluded_medication_file}"
    )

    print(
        f"ログ:               {log_file}"
    )

    return (
        longitudinal,
        wide,
        detail,
        review,
    )


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description=(
            "薬歴CSVと入退院CSVから、"
            "入院時・14日後・30日後・60日後・"
            "退院時のMRCIを患者・入院ごとに算出する"
        )
    )

    parser.add_argument(
        "medication_csv",
        help="薬歴CSVファイル",
    )

    parser.add_argument(
        "admission_csv",
        help="入退院CSVファイル",
    )

    parser.add_argument(
        "--output-prefix",
        default=(
            "output/mrci_longitudinal"
        ),
        help="出力ファイルの接頭辞",
    )

    parser.add_argument(
        "--day14-offset",
        type=int,
        default=14,
        help=(
            "入院日から14日後までの暦日差。"
            "既定値14。入院14日目なら13"
        ),
    )

    parser.add_argument(
        "--day30-offset",
        type=int,
        default=30,
        help=(
            "入院日から30日後までの暦日差。"
            "既定値30。入院30日目なら29"
        ),
    )

    parser.add_argument(
        "--day60-offset",
        type=int,
        default=60,
        help=(
            "入院日から60日後までの暦日差。"
            "既定値60。入院60日目なら59"
        ),
    )

    args = parser.parse_args()

    if args.day14_offset < 0:
        parser.error(
            "--day14-offsetは0以上が必要です"
        )

    if args.day30_offset < 0:
        parser.error(
            "--day30-offsetは0以上が必要です"
        )

    if args.day60_offset < 0:
        parser.error(
            "--day60-offsetは0以上が必要です"
        )

    calculate_longitudinal_mrci(
        medication_csv=(
            args.medication_csv
        ),
        admission_csv=(
            args.admission_csv
        ),
        output_prefix=(
            args.output_prefix
        ),
        day14_offset=(
            args.day14_offset
        ),
        day30_offset=(
            args.day30_offset
        ),
        day60_offset=(
            args.day60_offset
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
