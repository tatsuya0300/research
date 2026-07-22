from __future__ import annotations

import argparse
import re
import sys
import unicodedata
from pathlib import Path
from typing import Optional

import pandas as pd

from jars_auto import (
    classify_jars_scope,
    file_sha256,
    find_column,
    first_nonempty,
    load_jars_master,
    load_name_map,
    match_drug,
    normalize_drug_name,
    normalize_headers,
    normalize_text,
    read_csv_auto_encoding,
)


# ============================================================
# Constants
# ============================================================

TIMEPOINT_ORDER = [
    "admission",
    "day14",
    "day30",
    "discharge",
]


# 全身作用目的の経皮薬として扱えるJARS成分。
#
# フェンタニルテープ、ロチゴチンパッチ、
# オキシブチニンテープ、ブロナンセリンテープを想定。
#
# 院内採用品に応じて追加・修正すること。
SYSTEMIC_TRANSDERMAL_JARS_DRUGS = {
    "フェンタニル",
    "ロチゴチン",
    "オキシブチニン",
    "ブロナンセリン",
}


# 英語成分コードに付加される可能性のある剤形・経路接尾辞。
INGREDIENT_CODE_SUFFIXES = {
    "patch",
    "tablet",
    "tablets",
    "tab",
    "capsule",
    "capsules",
    "cap",
    "cream",
    "ointment",
    "gel",
    "eye",
    "eyedrop",
    "eyedrops",
    "inhaler",
    "inhalation",
    "injection",
    "syringe",
    "solution",
}


# ============================================================
# Date utilities
# ============================================================

def parse_date_column(
    series: pd.Series,
) -> pd.Series:
    """
    日付列をTimestampへ変換する。
    """
    cleaned = (
        series.map(normalize_text)
        .replace("", pd.NA)
    )

    return pd.to_datetime(
        cleaned,
        errors="coerce",
    )


def date_to_string(value) -> str:
    """
    日付をYYYY-MM-DDへ変換する。
    """
    if value is None or pd.isna(value):
        return ""

    return pd.Timestamp(value).strftime(
        "%Y-%m-%d"
    )


# ============================================================
# Ingredient-code utilities
# ============================================================

def normalize_english_ingredient(
    value,
) -> str:
    """
    英語成分名・英語成分コードの照合用正規化。

    例:
        METFORMIN        -> metformin
        FENTANYL_PATCH   -> fentanylpatch
        Lansoprazole     -> lansoprazole
    """
    text = normalize_text(value).lower()

    text = unicodedata.normalize(
        "NFKC",
        text,
    )

    text = re.sub(
        r"[^a-z0-9]+",
        "",
        text,
    )

    return text


def ingredient_code_candidates(
    value,
) -> list[str]:
    """
    ローカル成分コードから照合候補を作成する。

    例:
        FENTANYL_PATCH
        -> fentanylpatch
        -> fentanyl
    """
    raw_text = normalize_text(value)

    if not raw_text:
        return []

    normalized = normalize_english_ingredient(
        raw_text
    )

    candidates = []

    if normalized:
        candidates.append(normalized)

    # 元文字列を区切り文字で分割し、
    # PATCH等の接尾辞を除いた候補を作る。
    parts = [
        part.lower()
        for part in re.split(
            r"[_\-/\s]+",
            raw_text,
        )
        if part
    ]

    while (
        parts
        and parts[-1].lower()
        in INGREDIENT_CODE_SUFFIXES
    ):
        parts.pop()

    without_suffix = (
        normalize_english_ingredient(
            "".join(parts)
        )
    )

    if (
        without_suffix
        and without_suffix not in candidates
    ):
        candidates.append(without_suffix)

    return candidates


def prepare_jars_master(
    jars_master: pd.DataFrame,
) -> pd.DataFrame:
    """
    JARSマスターへ英語照合用列を追加する。
    """
    master = jars_master.copy()

    if "medication_en" not in master.columns:
        master["medication_en"] = ""

    master["_english_name_normalized"] = (
        master["medication_en"]
        .fillna("")
        .map(normalize_english_ingredient)
    )

    return master


def match_by_local_ingredient(
    ingredient_value: str,
    jars_master: pd.DataFrame,
) -> pd.DataFrame:
    """
    成分コード・成分名をJARSマスターへ照合する。

    優先順位:
        1. JARS日本語薬物名
        2. JARS Medication英語名
    """
    ingredient_value = normalize_text(
        ingredient_value
    )

    if not ingredient_value:
        return pd.DataFrame()

    # --------------------------------------------------------
    # 日本語一般名として照合
    # --------------------------------------------------------

    japanese_result = match_drug(
        drug_name=ingredient_value,
        jars_master=jars_master,
        name_map=pd.DataFrame(
            columns=[
                "alias",
                "jars_drug",
                "normalized_alias",
            ]
        ),
    )

    if not japanese_result.empty:
        japanese_result = (
            japanese_result.copy()
        )

        japanese_result[
            "match_method"
        ] = "local_ingredient_" + (
            japanese_result[
                "match_method"
            ].astype(str)
        )

        return japanese_result

    # --------------------------------------------------------
    # 英語成分コードとして照合
    # --------------------------------------------------------

    candidates = ingredient_code_candidates(
        ingredient_value
    )

    if not candidates:
        return pd.DataFrame()

    matched_rows = []

    for candidate in candidates:
        exact = jars_master.loc[
            jars_master[
                "_english_name_normalized"
            ] == candidate
        ].copy()

        if not exact.empty:
            matched_rows.append(exact)

    if not matched_rows:
        return pd.DataFrame()

    matches = pd.concat(
        matched_rows,
        ignore_index=True,
    ).drop_duplicates(
        subset=[
            "normalized_jars_drug",
        ]
    )

    if len(matches) > 1:
        return pd.DataFrame([{
            "match_status": "ambiguous",
            "match_method": (
                "local_ingredient_english"
            ),
            "matched_alias": (
                ingredient_value
            ),
            "jars_drug": "|".join(
                matches[
                    "jars_drug"
                ].astype(str)
            ),
            "jars_score": pd.NA,
            "therapeutic_class": "",
        }])

    match = matches.iloc[0]

    return pd.DataFrame([{
        "match_status": "matched",
        "match_method": (
            "local_ingredient_english"
        ),
        "matched_alias": ingredient_value,
        "jars_drug": match["jars_drug"],
        "jars_score": int(
            match["jars_score"]
        ),
        "therapeutic_class": (
            match.get(
                "therapeutic_class",
                "",
            )
        ),
    }])


def match_medication_row(
    drug_name: str,
    ingredient_value: str,
    jars_master: pd.DataFrame,
    name_map: pd.DataFrame,
) -> pd.DataFrame:
    """
    1薬剤行をJARS成分へ照合する。

    優先順位:
        1. 成分コード・一般名列
        2. 商品名対応表
        3. 薬剤名中のJARS一般名
    """
    ingredient_match = (
        match_by_local_ingredient(
            ingredient_value=(
                ingredient_value
            ),
            jars_master=jars_master,
        )
    )

    if not ingredient_match.empty:
        return ingredient_match

    return match_drug(
        drug_name=drug_name,
        jars_master=jars_master,
        name_map=name_map,
    )


# ============================================================
# Column detection
# ============================================================

def detect_medication_columns(
    df: pd.DataFrame,
) -> dict:
    return {
        "patient": find_column(
            df,
            [
                "患者ID",
                "患者ＩＤ",
                "患者Id",
                "patient_id",
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

        "drug": find_column(
            df,
            [
                "薬剤名",
                "医薬品名",
                "薬品名",
                "drug_name",
            ],
            required=True,
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

        "sig": find_column(
            df,
            [
                "用法",
                "用法名称",
                "服用方法",
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
    }


def detect_admission_columns(
    df: pd.DataFrame,
) -> dict:
    return {
        "patient": find_column(
            df,
            [
                "患者ID",
                "患者ＩＤ",
                "患者Id",
                "patient_id",
            ],
            required=True,
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
    }


# ============================================================
# Medication preprocessing
# ============================================================

def preprocess_medications(
    medications: pd.DataFrame,
    columns: dict,
) -> pd.DataFrame:
    df = medications.copy()

    patient_col = columns["patient"]
    drug_col = columns["drug"]

    df["_patient_id"] = (
        df[patient_col].map(
            normalize_text
        )
    )

    df["_drug_name"] = (
        df[drug_col].map(
            normalize_text
        )
    )

    ingredient_col = columns["ingredient"]

    if ingredient_col is not None:
        df["_ingredient_value"] = (
            df[ingredient_col].map(
                normalize_text
            )
        )
    else:
        df["_ingredient_value"] = ""

    start_col = columns["start"]
    end_col = columns["end"]
    order_col = columns["order_date"]

    if start_col is not None:
        df["_start_date"] = (
            parse_date_column(
                df[start_col]
            )
        )
    else:
        df["_start_date"] = pd.NaT

    if order_col is not None:
        df["_order_date"] = (
            parse_date_column(
                df[order_col]
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

    # 開始日欠損時はオーダ指示日を補助的に使用。
    df["_effective_start_date"] = (
        df["_start_date"].fillna(
            df["_order_date"]
        )
    )

    df = df.loc[
        (df["_patient_id"] != "")
        & (df["_drug_name"] != "")
    ].copy()

    return df


def extract_active_medications(
    medications: pd.DataFrame,
    patient_id: str,
    evaluation_date: pd.Timestamp,
) -> pd.DataFrame:
    """
    指定した評価日に有効な薬剤を抽出する。

    開始日・終了日は包含関係:
        開始日 <= 評価日 <= 終了日
    """
    patient_id = normalize_text(
        patient_id
    )

    patient_medications = medications.loc[
        medications["_patient_id"]
        == patient_id
    ].copy()

    active_mask = (
        (
            patient_medications[
                "_effective_start_date"
            ].isna()
            |
            (
                patient_medications[
                    "_effective_start_date"
                ] <= evaluation_date
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
                ] >= evaluation_date
            )
        )
    )

    active = patient_medications.loc[
        active_mask
    ].copy()

    # 完全に同一の薬歴行のみ除外。
    dedup_columns = [
        "_patient_id",
        "_drug_name",
        "_ingredient_value",
        "_effective_start_date",
        "_end_date",
    ]

    sig_col = None

    if "_sig" in active.columns:
        sig_col = "_sig"

    if sig_col is not None:
        dedup_columns.append(sig_col)

    return active.drop_duplicates(
        subset=dedup_columns,
        keep="first",
    ).copy()


# ============================================================
# Scope resolution
# ============================================================

def resolve_scope(
    drug_name: str,
    matched_jars_drug: str,
) -> tuple[str, str, str]:
    """
    薬剤名と照合済みJARS成分から対象範囲を判定する。
    """
    scope_status, scope_type, note = (
        classify_jars_scope(
            drug_name
        )
    )

    if scope_status != "unknown":
        return (
            scope_status,
            scope_type,
            note,
        )

    # テープ・パッチで、JARS成分が全身作用目的と
    # 明確に判断できる場合。
    if (
        re.search(
            r"テープ|パッチ|貼付",
            normalize_text(drug_name),
        )
        and matched_jars_drug
        in SYSTEMIC_TRANSDERMAL_JARS_DRUGS
    ):
        return (
            "in_scope",
            "systemic_transdermal",
            (
                "JARS成分と剤形から"
                "全身作用目的の経皮薬と判定"
            ),
        )

    return (
        scope_status,
        scope_type,
        note,
    )


# ============================================================
# Snapshot scoring
# ============================================================

def score_jars_snapshot(
    active_medications: pd.DataFrame,
    medication_columns: dict,
    jars_master: pd.DataFrame,
    name_map: pd.DataFrame,
    episode_id: str,
    patient_id: str,
    timepoint: str,
    evaluation_date: pd.Timestamp,
    admission_date: pd.Timestamp,
    discharge_date: pd.Timestamp,
    ingredient_code_is_english: bool = False,
) -> tuple[dict, pd.DataFrame]:
    """
    1患者・1評価日のJARSを算出する。
    """
    if active_medications.empty:
        summary = {
            "episode_id": episode_id,
            "患者ID": patient_id,
            "患者氏名": "",
            "病棟": "",
            "チーム": "",
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
            "jars_drug_count": 0,
            "jars_total": 0,
            "score_3_count": 0,
            "score_2_count": 0,
            "score_1_count": 0,
            "out_of_scope_count": 0,
            "non_jars_count": 0,
            "unresolved_count": 0,
            "review_required": 1,
            "review_count": 1,
            "score_status": (
                "no_active_medication_review_required"
            ),
        }

        return summary, pd.DataFrame()

    drug_col = medication_columns["drug"]
    ingredient_col = medication_columns[
        "ingredient"
    ]
    sig_col = medication_columns["sig"]
    ward_col = medication_columns["ward"]
    team_col = medication_columns["team"]
    patient_name_col = medication_columns[
        "patient_name"
    ]

    detail_rows = []

    for source_index, row in (
        active_medications.iterrows()
    ):
        drug_name = normalize_text(
            row.get(drug_col, "")
        )

        ingredient_value = (
            normalize_text(
                row.get(
                    ingredient_col,
                    "",
                )
            )
            if ingredient_col is not None
            else ""
        )

        matches = match_medication_row(
            drug_name=drug_name,
            ingredient_value=(
                ingredient_value
            ),
            jars_master=jars_master,
            name_map=name_map,
        )

        # 薬剤名のみから先に投与経路を評価する。
        initial_scope = classify_jars_scope(
            drug_name
        )

        # ----------------------------------------------------
        # 未照合
        # ----------------------------------------------------

        if matches.empty:
            scope_status, scope_type, scope_note = (
                initial_scope
            )

            review_reasons = []

            if scope_status == "out_of_scope":
                match_status = (
                    "not_scored_out_of_scope"
                )

            elif (
                ingredient_code_is_english
                and ingredient_value
            ):
                # 成分コードが英語一般名であることを
                # 明示した場合に限り、マスター非掲載を
                # JARS非該当として扱う。
                match_status = (
                    "not_listed_in_jars"
                )

            else:
                match_status = "unresolved"

                review_reasons.append(
                    "内服薬または投与経路不明薬が"
                    "JARSマスターへ照合できない"
                )

            if scope_status == "unknown":
                review_reasons.append(
                    "JARS対象投与経路か確認が必要"
                )

            detail_rows.append({
                "episode_id": episode_id,
                "患者ID": patient_id,

                "患者氏名": (
                    normalize_text(
                        row.get(
                            patient_name_col,
                            "",
                        )
                    )
                    if patient_name_col is not None
                    else ""
                ),

                "病棟": (
                    normalize_text(
                        row.get(ward_col, "")
                    )
                    if ward_col is not None
                    else ""
                ),

                "チーム": (
                    normalize_text(
                        row.get(team_col, "")
                    )
                    if team_col is not None
                    else ""
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

                "source_index": source_index,
                "薬剤名": drug_name,
                "成分コード": ingredient_value,

                "用法": (
                    normalize_text(
                        row.get(sig_col, "")
                    )
                    if sig_col is not None
                    else ""
                ),

                "服用開始日": (
                    date_to_string(
                        row.get(
                            "_effective_start_date",
                            pd.NaT,
                        )
                    )
                ),

                "使用終了日": (
                    date_to_string(
                        row.get(
                            "_end_date",
                            pd.NaT,
                        )
                    )
                ),

                "match_status": match_status,
                "match_method": "",
                "jars_drug": "",
                "jars_score": "",
                "scope_status": scope_status,
                "scope_type": scope_type,
                "scope_note": scope_note,
                "preliminary_counted": 0,
                "duplicate_ingredient": 0,
                "final_counted": 0,
                "final_counted_score": 0,
                "review_required": int(
                    bool(review_reasons)
                ),
                "review_reason": "; ".join(
                    dict.fromkeys(
                        review_reasons
                    )
                ),
            })

            continue

        # ----------------------------------------------------
        # 照合あり
        # ----------------------------------------------------

        for _, match in matches.iterrows():
            match_status = normalize_text(
                match.get(
                    "match_status",
                    "",
                )
            )

            matched_jars_drug = normalize_text(
                match.get(
                    "jars_drug",
                    "",
                )
            )

            scope_status, scope_type, scope_note = (
                resolve_scope(
                    drug_name=drug_name,
                    matched_jars_drug=(
                        matched_jars_drug
                    ),
                )
            )

            review_reasons = []

            if match_status == "ambiguous":
                review_reasons.append(
                    "複数のJARS成分候補があり"
                    "照合を確定できない"
                )

            if scope_status == "unknown":
                review_reasons.append(
                    "JARS対象投与経路か確認が必要"
                )

            preliminary_counted = int(
                match_status == "matched"
                and scope_status == "in_scope"
            )

            jars_score = (
                int(match["jars_score"])
                if pd.notna(
                    match.get(
                        "jars_score",
                        pd.NA,
                    )
                )
                else ""
            )

            detail_rows.append({
                "episode_id": episode_id,
                "患者ID": patient_id,

                "患者氏名": (
                    normalize_text(
                        row.get(
                            patient_name_col,
                            "",
                        )
                    )
                    if patient_name_col is not None
                    else ""
                ),

                "病棟": (
                    normalize_text(
                        row.get(ward_col, "")
                    )
                    if ward_col is not None
                    else ""
                ),

                "チーム": (
                    normalize_text(
                        row.get(team_col, "")
                    )
                    if team_col is not None
                    else ""
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

                "source_index": source_index,
                "薬剤名": drug_name,
                "成分コード": ingredient_value,

                "用法": (
                    normalize_text(
                        row.get(sig_col, "")
                    )
                    if sig_col is not None
                    else ""
                ),

                "服用開始日": (
                    date_to_string(
                        row.get(
                            "_effective_start_date",
                            pd.NaT,
                        )
                    )
                ),

                "使用終了日": (
                    date_to_string(
                        row.get(
                            "_end_date",
                            pd.NaT,
                        )
                    )
                ),

                "match_status": match_status,

                "match_method": (
                    normalize_text(
                        match.get(
                            "match_method",
                            "",
                        )
                    )
                ),

                "jars_drug": (
                    matched_jars_drug
                ),

                "jars_score": jars_score,

                "scope_status": scope_status,
                "scope_type": scope_type,
                "scope_note": scope_note,

                "preliminary_counted": (
                    preliminary_counted
                ),

                "duplicate_ingredient": 0,
                "final_counted": 0,
                "final_counted_score": 0,

                "review_required": int(
                    bool(review_reasons)
                ),

                "review_reason": "; ".join(
                    dict.fromkeys(
                        review_reasons
                    )
                ),
            })

    detail = pd.DataFrame(
        detail_rows
    )

    # --------------------------------------------------------
    # 同一評価時点の同一JARS成分は1回だけ加算
    # --------------------------------------------------------

    counted_ingredients = set()

    for index in detail.index:
        if (
            detail.at[
                index,
                "preliminary_counted",
            ] != 1
        ):
            continue

        ingredient = normalize_text(
            detail.at[
                index,
                "jars_drug",
            ]
        )

        if not ingredient:
            continue

        if ingredient in counted_ingredients:
            detail.at[
                index,
                "duplicate_ingredient",
            ] = 1

            continue

        score = pd.to_numeric(
            detail.at[
                index,
                "jars_score",
            ],
            errors="coerce",
        )

        if pd.isna(score):
            detail.at[
                index,
                "review_required",
            ] = 1

            old_reason = normalize_text(
                detail.at[
                    index,
                    "review_reason",
                ]
            )

            new_reason = (
                "JARSスコアが数値として取得できない"
            )

            detail.at[
                index,
                "review_reason",
            ] = (
                f"{old_reason}; {new_reason}"
                if old_reason
                else new_reason
            )

            continue

        detail.at[
            index,
            "final_counted",
        ] = 1

        detail.at[
            index,
            "final_counted_score",
        ] = int(score)

        counted_ingredients.add(
            ingredient
        )

    # --------------------------------------------------------
    # Summary
    # --------------------------------------------------------

    jars_total = int(
        detail[
            "final_counted_score"
        ].sum()
    )

    jars_drug_count = int(
        detail[
            "final_counted"
        ].sum()
    )

    review_count = int(
        detail[
            "review_required"
        ].sum()
    )

    review_required = int(
        review_count > 0
    )

    unresolved_count = int(
        (
            detail["match_status"]
            == "unresolved"
        ).sum()
        +
        (
            detail["match_status"]
            == "ambiguous"
        ).sum()
    )

    out_of_scope_count = int(
        (
            detail["scope_status"]
            == "out_of_scope"
        ).sum()
    )

    non_jars_count = int(
        (
            detail["match_status"]
            == "not_listed_in_jars"
        ).sum()
    )

    if review_required == 0:
        score_status = "complete"
    else:
        score_status = (
            "partial_lower_bound_or_review_required"
        )

    summary = {
        "episode_id": episode_id,
        "患者ID": patient_id,

        "患者氏名": first_nonempty(
            detail["患者氏名"]
        ),

        "病棟": first_nonempty(
            detail["病棟"]
        ),

        "チーム": first_nonempty(
            detail["チーム"]
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
            detail["source_index"].nunique()
        ),

        "jars_drug_count": jars_drug_count,
        "jars_total": jars_total,

        "score_3_count": int(
            (
                detail[
                    "final_counted_score"
                ] == 3
            ).sum()
        ),

        "score_2_count": int(
            (
                detail[
                    "final_counted_score"
                ] == 2
            ).sum()
        ),

        "score_1_count": int(
            (
                detail[
                    "final_counted_score"
                ] == 1
            ).sum()
        ),

        "out_of_scope_count": (
            out_of_scope_count
        ),

        "non_jars_count": non_jars_count,

        "unresolved_count": (
            unresolved_count
        ),

        "review_required": (
            review_required
        ),

        "review_count": review_count,

        "score_status": score_status,
    }

    return summary, detail


# ============================================================
# Longitudinal calculation
# ============================================================

def calculate_longitudinal_jars(
    medication_csv: str,
    admission_csv: str,
    jars_master_path: str,
    output_prefix: str,
    name_map_path: Optional[str] = None,
    day14_offset: int = 14,
    day30_offset: int = 30,
    ingredient_code_is_english: bool = False,
):
    """
    入院時、入院+14日、入院+30日、退院時の
    JARSを入院エピソードごとに算出する。
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
        medications=medications,
        columns=medication_columns,
    )

    # --------------------------------------------------------
    # JARS master
    # --------------------------------------------------------

    jars_master = load_jars_master(
        jars_master_path
    )

    jars_master = prepare_jars_master(
        jars_master
    )

    name_map = load_name_map(
        name_map_path=name_map_path,
        jars_master=jars_master,
    )

    master_hash = file_sha256(
        jars_master_path
    )

    # --------------------------------------------------------
    # Admissions
    # --------------------------------------------------------

    admission_patient_col = (
        admission_columns["patient"]
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

    invalid_admissions = admissions.loc[
        (
            admissions["_patient_id"]
            == ""
        )
        |
        admissions[
            "_admission_date"
        ].isna()
        |
        (
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
    ].copy()

    valid_admissions = admissions.loc[
        ~admissions.index.isin(
            invalid_admissions.index
        )
    ].copy()

    summary_rows = []
    detail_frames = []

    for _, admission_row in (
        valid_admissions.iterrows()
    ):
        patient_id = normalize_text(
            admission_row[
                "_patient_id"
            ]
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
                f"{admission_date:%Y%m%d}"
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
                "discharge",
                discharge_date,
            ),
        ]

        for timepoint, evaluation_date in timepoints:
            # 退院日欠損
            if (
                timepoint == "discharge"
                and pd.isna(
                    evaluation_date
                )
            ):
                summary_rows.append({
                    "episode_id": episode_id,
                    "患者ID": patient_id,
                    "患者氏名": "",
                    "病棟": "",
                    "チーム": "",
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
                    "jars_drug_count": "",
                    "jars_total": "",
                    "score_3_count": "",
                    "score_2_count": "",
                    "score_1_count": "",
                    "out_of_scope_count": "",
                    "non_jars_count": "",
                    "unresolved_count": "",
                    "review_required": 1,
                    "review_count": 1,
                    "score_status": (
                        "discharge_date_missing"
                    ),
                })

                continue

            # 評価日前に退院
            if (
                timepoint in {
                    "day14",
                    "day30",
                }
                and pd.notna(
                    discharge_date
                )
                and evaluation_date
                > discharge_date
            ):
                summary_rows.append({
                    "episode_id": episode_id,
                    "患者ID": patient_id,
                    "患者氏名": "",
                    "病棟": "",
                    "チーム": "",
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
                    "jars_drug_count": "",
                    "jars_total": "",
                    "score_3_count": "",
                    "score_2_count": "",
                    "score_1_count": "",
                    "out_of_scope_count": "",
                    "non_jars_count": "",
                    "unresolved_count": "",
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
                evaluation_date=(
                    evaluation_date
                ),
            )

            summary, detail = (
                score_jars_snapshot(
                    active_medications=active,
                    medication_columns=(
                        medication_columns
                    ),
                    jars_master=jars_master,
                    name_map=name_map,
                    episode_id=episode_id,
                    patient_id=patient_id,
                    timepoint=timepoint,
                    evaluation_date=(
                        evaluation_date
                    ),
                    admission_date=(
                        admission_date
                    ),
                    discharge_date=(
                        discharge_date
                    ),
                    ingredient_code_is_english=(
                        ingredient_code_is_english
                    ),
                )
            )

            summary_rows.append(summary)

            if not detail.empty:
                detail_frames.append(
                    detail
                )

    longitudinal = pd.DataFrame(
        summary_rows
    )

    if detail_frames:
        detail = pd.concat(
            detail_frames,
            ignore_index=True,
        )
    else:
        detail = pd.DataFrame()

    if not detail.empty:
        review = detail.loc[
            detail[
                "review_required"
            ] == 1
        ].copy()
    else:
        review = pd.DataFrame()

    # --------------------------------------------------------
    # Wide format
    # --------------------------------------------------------

    if longitudinal.duplicated(
        subset=[
            "episode_id",
            "timepoint",
        ]
    ).any():
        duplicates = longitudinal.loc[
            longitudinal.duplicated(
                subset=[
                    "episode_id",
                    "timepoint",
                ],
                keep=False,
            ),
            [
                "episode_id",
                "患者ID",
                "timepoint",
            ],
        ]

        raise ValueError(
            "同一episode_id・timepointが重複しています。\n"
            + duplicates.to_string(
                index=False
            )
        )

    wide_value_columns = [
        "evaluation_date",
        "eligible",
        "active_medication_rows",
        "jars_drug_count",
        "jars_total",
        "score_3_count",
        "score_2_count",
        "score_1_count",
        "out_of_scope_count",
        "non_jars_count",
        "unresolved_count",
        "review_required",
        "score_status",
    ]

    wide = longitudinal.pivot(
        index=[
            "episode_id",
            "患者ID",
            "admission_date",
            "discharge_date",
        ],
        columns="timepoint",
        values=wide_value_columns,
    )

    wide.columns = [
        f"{timepoint}_{value_name}"
        for value_name, timepoint
        in wide.columns
    ]

    wide = wide.reset_index()

    preferred_columns = [
        "episode_id",
        "患者ID",
        "admission_date",
        "discharge_date",
    ]

    for timepoint in TIMEPOINT_ORDER:
        for value_name in wide_value_columns:
            candidate = (
                f"{timepoint}_{value_name}"
            )

            if candidate in wide.columns:
                preferred_columns.append(
                    candidate
                )

    remaining_columns = [
        column
        for column in wide.columns
        if column not in preferred_columns
    ]

    wide = wide[
        preferred_columns
        + remaining_columns
    ]

    # --------------------------------------------------------
    # Output
    # --------------------------------------------------------

    prefix = Path(output_prefix)

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

    invalid_file = (
        prefix.parent
        / f"{prefix.name}_invalid_admissions.csv"
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

    invalid_admissions.to_csv(
        invalid_file,
        index=False,
        encoding="utf-8-sig",
    )

    log_lines = [
        f"medication_csv={medication_csv}",
        f"admission_csv={admission_csv}",
        f"jars_master={jars_master_path}",
        f"jars_master_sha256={master_hash}",
        f"name_map={name_map_path or 'not specified'}",
        (
            "ingredient_code_is_english="
            f"{ingredient_code_is_english}"
        ),
        f"day14_offset={day14_offset}",
        f"day30_offset={day30_offset}",
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
            "detail_rows="
            f"{len(detail)}"
        ),
        (
            "review_rows="
            f"{len(review)}"
        ),
        "",
        "定義:",
        (
            "active medication = "
            "effective_start_date <= evaluation_date "
            "<= end_date"
        ),
        (
            "day14 = admission_date + "
            f"{day14_offset} calendar days"
        ),
        (
            "day30 = admission_date + "
            f"{day30_offset} calendar days"
        ),
        (
            "同一患者・同一評価時点・同一JARS成分は"
            "1回のみ加算した。"
        ),
        (
            "局所外用、眼科、耳科、経鼻、吸入などは"
            "原則としてJARS集計対象外とした。"
        ),
        (
            "review_required=1の場合、jars_totalは"
            "部分合計または暫定値である。"
        ),
        (
            "JARSには確立した標準カットオフ値はない。"
        ),
    ]

    log_file.write_text(
        "\n".join(log_lines),
        encoding="utf-8",
    )

    print(
        "Longitudinal JARS calculation completed."
    )
    print(
        f"縦長データ:       {long_file}"
    )
    print(
        f"横長データ:       {wide_file}"
    )
    print(
        f"薬剤別詳細:       {detail_file}"
    )
    print(
        f"要確認:           {review_file}"
    )
    print(
        f"不正な入退院情報: {invalid_file}"
    )
    print(
        f"ログ:             {log_file}"
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
            "入院時・14日後・30日後・退院時の"
            "JARSを入院エピソードごとに算出する"
        )
    )

    parser.add_argument(
        "medication_csv",
        help="薬歴CSV",
    )

    parser.add_argument(
        "admission_csv",
        help="入退院CSV",
    )

    parser.add_argument(
        "--jars-master",
        default=(
            "jsgp-jars-datebase-"
            "1st-release-20240528.xlsx"
        ),
        help="日本老年薬学会の公式JARS Excel",
    )

    parser.add_argument(
        "--name-map",
        default=None,
        help=(
            "商品名・別名―JARS薬物名対応CSV"
        ),
    )

    parser.add_argument(
        "--output-prefix",
        default=(
            "output/jars_longitudinal"
        ),
        help="出力ファイルの接頭辞",
    )

    parser.add_argument(
        "--day14-offset",
        type=int,
        default=14,
        help=(
            "入院日からの暦日差。"
            "入院14日目なら13を指定"
        ),
    )

    parser.add_argument(
        "--day30-offset",
        type=int,
        default=30,
        help=(
            "入院日からの暦日差。"
            "入院30日目なら29を指定"
        ),
    )

    parser.add_argument(
        "--ingredient-code-is-english",
        action="store_true",
        help=(
            "成分コード列がMETFORMIN等の"
            "英語一般名である場合に指定。"
            "この場合、マスター非掲載成分を"
            "JARS非該当として扱う"
        ),
    )

    args = parser.parse_args()

    calculate_longitudinal_jars(
        medication_csv=(
            args.medication_csv
        ),
        admission_csv=(
            args.admission_csv
        ),
        jars_master_path=(
            args.jars_master
        ),
        output_prefix=(
            args.output_prefix
        ),
        name_map_path=(
            args.name_map
        ),
        day14_offset=(
            args.day14_offset
        ),
        day30_offset=(
            args.day30_offset
        ),
        ingredient_code_is_english=(
            args.ingredient_code_is_english
        ),
    )


if __name__ == "__main__":
    try:
        main()
    except Exception as exc:
        print(
            f"ERROR: {exc}",
            file=sys.stderr,
        )
        sys.exit(1)
