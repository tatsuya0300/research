from __future__ import annotations

import argparse
import hashlib
import re
import sys
import unicodedata
from pathlib import Path
from typing import Optional

import pandas as pd


# ============================================================
# Constants
# ============================================================

TIMEPOINT_ORDER = [
    "admission",
    "day14",
    "day30",
    "discharge",
]

PRN_PATTERNS = [
    r"頓服",
    r"頓用",
    r"必要時",
    r"疼痛時",
    r"発熱時",
    r"不眠時",
    r"眠れない時",
    r"嘔気時",
    r"悪心時",
    r"便秘時",
    r"発作時",
    r"症状時",
    r"適宜",
]


# ============================================================
# General utilities
# ============================================================

def normalize_text(value) -> str:
    if value is None:
        return ""

    try:
        if pd.isna(value):
            return ""
    except (TypeError, ValueError):
        pass

    text = unicodedata.normalize(
        "NFKC",
        str(value),
    )

    replacements = {
        "　": " ",
        "，": ",",
        "、": ",",
        "−": "-",
        "–": "-",
        "—": "-",
        "―": "-",
        "〜": "~",
        "～": "~",
    }

    for old, new in replacements.items():
        text = text.replace(old, new)

    text = re.sub(r"\s+", " ", text)

    return text.strip()


def normalize_code(value) -> str:
    text = normalize_text(value).lower()

    return re.sub(
        r"[^a-z0-9ぁ-んァ-ヶ一-龥]+",
        "",
        text,
    )


def normalize_unit(value) -> str:
    text = normalize_text(value).lower()

    replacements = {
        "ｍｇ": "mg",
        "μｇ": "μg",
        "mcg": "μg",
        "ug": "μg",
        "／": "/",
        "日": "day",
    }

    for old, new in replacements.items():
        text = text.replace(old, new)

    text = re.sub(r"\s+", "", text)

    return text


def parse_bool(value) -> bool:
    text = normalize_text(value).lower()

    return text in {
        "1",
        "true",
        "yes",
        "y",
        "include",
        "included",
        "対象",
    }


def first_nonempty(values: pd.Series) -> str:
    for value in values:
        text = normalize_text(value)

        if text:
            return text

    return ""


def file_sha256(path: str | Path) -> str:
    sha256 = hashlib.sha256()

    with Path(path).open("rb") as file:
        for chunk in iter(
            lambda: file.read(1024 * 1024),
            b"",
        ):
            sha256.update(chunk)

    return sha256.hexdigest()


def read_csv_auto_encoding(
    path: str | Path,
) -> pd.DataFrame:
    encodings = [
        "utf-8-sig",
        "cp932",
        "shift_jis",
        "utf-8",
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
        "CSVの文字コードを判定できません。\n"
        + "\n".join(errors)
    )


def normalize_headers(
    df: pd.DataFrame,
) -> pd.DataFrame:
    return df.rename(
        columns={
            column: normalize_text(column)
            .replace("\r", "")
            .replace("\n", "")
            for column in df.columns
        }
    )


def find_column(
    df: pd.DataFrame,
    candidates: list[str],
    required: bool = False,
) -> Optional[str]:
    for candidate in candidates:
        if candidate in df.columns:
            return candidate

    if required:
        raise ValueError(
            "必要な列がありません。"
            f"候補={candidates}, "
            f"実際の列={list(df.columns)}"
        )

    return None


# ============================================================
# Date utilities
# ============================================================

def parse_date_column(
    series: pd.Series,
) -> pd.Series:
    cleaned = (
        series.map(normalize_text)
        .replace("", pd.NA)
    )

    return pd.to_datetime(
        cleaned,
        errors="coerce",
    ).dt.normalize()


def date_to_string(value) -> str:
    if value is None or pd.isna(value):
        return ""

    return pd.Timestamp(value).strftime(
        "%Y-%m-%d"
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

        "ward": find_column(
            df,
            ["病棟"],
        ),

        "team": find_column(
            df,
            ["チーム"],
        ),

        "ingredient": find_column(
            df,
            [
                "成分コード",
                "一般名コード",
                "成分名",
                "一般名",
                "薬剤キー",
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

        "daily_quantity": find_column(
            df,
            [
                "1日量",
                "一日量",
                "daily_quantity",
            ],
        ),

        "quantity_unit": find_column(
            df,
            [
                "単位",
                "1日量単位",
                "daily_quantity_unit",
            ],
        ),

        # この列が存在する場合は、製剤量からの換算より優先する。
        "actual_daily_dose": find_column(
            df,
            [
                "実成分1日量",
                "実投与量",
                "actual_daily_dose",
            ],
        ),

        "actual_daily_dose_unit": find_column(
            df,
            [
                "実成分1日量単位",
                "実投与量単位",
                "actual_daily_dose_unit",
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
            ],
        ),
    }


# ============================================================
# DBI master
# ============================================================

def load_dbi_master(
    path: str | Path,
) -> pd.DataFrame:
    master = read_csv_auto_encoding(path)
    master = normalize_headers(master)

    required_columns = [
        "ingredient_id",
        "ingredient_name_jp",
        "local_ingredient_codes",
        "dbi_category",
        "burden_type",
        "route",
        "include_main",
        "delta",
        "delta_unit",
        "source_dose_unit",
        "dose_factor_to_delta_unit",
    ]

    missing = [
        column
        for column in required_columns
        if column not in master.columns
    ]

    if missing:
        raise ValueError(
            "DBIマスターに必要な列がありません："
            f"{missing}"
        )

    optional_defaults = {
        "product_name_regex": "",
        "profile": "",
        "source": "",
        "notes": "",
    }

    for column, default_value in (
        optional_defaults.items()
    ):
        if column not in master.columns:
            master[column] = default_value

    text_columns = [
        "ingredient_id",
        "ingredient_name_jp",
        "local_ingredient_codes",
        "product_name_regex",
        "dbi_category",
        "burden_type",
        "route",
        "delta_unit",
        "source_dose_unit",
        "profile",
        "source",
        "notes",
    ]

    for column in text_columns:
        master[column] = (
            master[column]
            .map(normalize_text)
        )

    master["include_main"] = (
        master["include_main"]
        .map(parse_bool)
    )

    master["delta"] = pd.to_numeric(
        master["delta"],
        errors="coerce",
    )

    master["dose_factor_to_delta_unit"] = (
        pd.to_numeric(
            master[
                "dose_factor_to_delta_unit"
            ],
            errors="coerce",
        )
    )

    invalid_delta = master.loc[
        master["include_main"]
        & (
            master["delta"].isna()
            | (master["delta"] <= 0)
        )
    ]

    if not invalid_delta.empty:
        raise ValueError(
            "主解析対象行に有効なdeltaがありません。\n"
            + invalid_delta[
                [
                    "ingredient_id",
                    "ingredient_name_jp",
                    "delta",
                ]
            ].to_string(index=False)
        )

    invalid_burden_type = master.loc[
        ~master["burden_type"].isin(
            ["AC", "S"]
        )
    ]

    if not invalid_burden_type.empty:
        raise ValueError(
            "burden_typeはACまたはSにしてください。\n"
            + invalid_burden_type[
                [
                    "ingredient_id",
                    "burden_type",
                ]
            ].to_string(index=False)
        )

    # 正規表現の事前検証
    for index, value in (
        master["product_name_regex"].items()
    ):
        if not value:
            continue

        try:
            re.compile(value)
        except re.error as exc:
            raise ValueError(
                "product_name_regexが不正です。"
                f"index={index}, regex={value!r}, "
                f"error={exc}"
            ) from exc

    master["_code_set"] = (
        master["local_ingredient_codes"]
        .map(
            lambda value: {
                normalize_code(item)
                for item in value.split("|")
                if normalize_code(item)
            }
        )
    )

    master["_delta_unit_normalized"] = (
        master["delta_unit"]
        .map(normalize_unit)
    )

    master["_source_unit_normalized"] = (
        master["source_dose_unit"]
        .map(normalize_unit)
    )

    if master["ingredient_id"].eq("").any():
        raise ValueError(
            "ingredient_idが空欄の行があります。"
        )

    return master.reset_index(drop=True)


def match_master_row(
    ingredient_value: str,
    drug_name: str,
    master: pd.DataFrame,
) -> tuple[Optional[pd.Series], str]:
    normalized_code = normalize_code(
        ingredient_value
    )

    if not normalized_code:
        return None, "ingredient_code_missing"

    candidates = master.loc[
        master["_code_set"].map(
            lambda values:
                normalized_code in values
        )
    ].copy()

    if candidates.empty:
        return None, "not_in_master"

    if len(candidates) == 1:
        return candidates.iloc[0], "ingredient_code"

    product_specific = candidates.loc[
        candidates["product_name_regex"].map(
            lambda pattern:
                bool(pattern)
                and bool(
                    re.search(
                        pattern,
                        drug_name,
                        flags=re.IGNORECASE,
                    )
                )
        )
    ].copy()

    if len(product_specific) == 1:
        return (
            product_specific.iloc[0],
            "ingredient_code_product_regex",
        )

    if len(product_specific) > 1:
        return None, "ambiguous_product_regex"

    generic = candidates.loc[
        candidates["product_name_regex"] == ""
    ].copy()

    if len(generic) == 1:
        return (
            generic.iloc[0],
            "ingredient_code_generic",
        )

    return None, "ambiguous_master_match"


# ============================================================
# Medication preprocessing
# ============================================================

def is_prn(sig: str) -> bool:
    text = normalize_text(sig)

    return any(
        re.search(pattern, text)
        for pattern in PRN_PATTERNS
    )


def preprocess_medications(
    medications: pd.DataFrame,
    columns: dict,
) -> pd.DataFrame:
    df = medications.copy()

    df["_patient_id"] = (
        df[columns["patient"]]
        .map(normalize_text)
    )

    df["_drug_name"] = (
        df[columns["drug"]]
        .map(normalize_text)
    )

    if columns["ingredient"] is not None:
        df["_ingredient_value"] = (
            df[columns["ingredient"]]
            .map(normalize_text)
        )
    else:
        df["_ingredient_value"] = ""

    if columns["sig"] is not None:
        df["_sig"] = (
            df[columns["sig"]]
            .map(normalize_text)
        )
    else:
        df["_sig"] = ""

    df["_is_prn"] = (
        df["_sig"].map(is_prn)
    )

    if columns["start"] is not None:
        df["_start_date"] = parse_date_column(
            df[columns["start"]]
        )
    else:
        df["_start_date"] = pd.NaT

    if columns["order_date"] is not None:
        df["_order_date"] = parse_date_column(
            df[columns["order_date"]]
        )
    else:
        df["_order_date"] = pd.NaT

    if columns["end"] is not None:
        df["_end_date"] = parse_date_column(
            df[columns["end"]]
        )
    else:
        df["_end_date"] = pd.NaT

    df["_effective_start_date"] = (
        df["_start_date"]
        .fillna(df["_order_date"])
    )

    if columns["daily_quantity"] is not None:
        df["_daily_quantity"] = pd.to_numeric(
            df[columns["daily_quantity"]]
            .map(normalize_text)
            .str.replace(",", "", regex=False),
            errors="coerce",
        )
    else:
        df["_daily_quantity"] = pd.NA

    if columns["quantity_unit"] is not None:
        df["_quantity_unit"] = (
            df[columns["quantity_unit"]]
            .map(normalize_text)
        )
    else:
        df["_quantity_unit"] = ""

    if columns["actual_daily_dose"] is not None:
        df["_actual_daily_dose"] = pd.to_numeric(
            df[columns["actual_daily_dose"]]
            .map(normalize_text)
            .str.replace(",", "", regex=False),
            errors="coerce",
        )
    else:
        df["_actual_daily_dose"] = pd.NA

    if (
        columns["actual_daily_dose_unit"]
        is not None
    ):
        df["_actual_daily_dose_unit"] = (
            df[
                columns[
                    "actual_daily_dose_unit"
                ]
            ].map(normalize_text)
        )
    else:
        df["_actual_daily_dose_unit"] = ""

    df = df.loc[
        (df["_patient_id"] != "")
        & (df["_drug_name"] != "")
    ].copy()

    dedup_columns = [
        "_patient_id",
        "_ingredient_value",
        "_drug_name",
        "_daily_quantity",
        "_quantity_unit",
        "_actual_daily_dose",
        "_actual_daily_dose_unit",
        "_sig",
        "_effective_start_date",
        "_end_date",
    ]

    return df.drop_duplicates(
        subset=dedup_columns,
        keep="first",
    ).copy()


def extract_active_medications(
    medications: pd.DataFrame,
    patient_id: str,
    evaluation_date: pd.Timestamp,
) -> pd.DataFrame:
    patient_id = normalize_text(patient_id)
    evaluation_date = pd.Timestamp(
        evaluation_date
    ).normalize()

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

    return patient_medications.loc[
        active_mask
    ].copy()


# ============================================================
# Dose conversion
# ============================================================

def calculate_actual_daily_dose(
    medication_row: pd.Series,
    master_row: pd.Series,
) -> tuple[Optional[float], str]:
    """
    Returns
    -------
    dose:
        deltaと同一単位の実成分1日量
    status:
        計算方法またはエラー理由
    """
    delta_unit = normalize_unit(
        master_row["delta_unit"]
    )

    explicit_dose = pd.to_numeric(
        medication_row.get(
            "_actual_daily_dose",
            pd.NA,
        ),
        errors="coerce",
    )

    if pd.notna(explicit_dose):
        explicit_unit = normalize_unit(
            medication_row.get(
                "_actual_daily_dose_unit",
                "",
            )
        )

        if not explicit_unit:
            return (
                None,
                "actual_daily_dose_unit_missing",
            )

        if explicit_unit != delta_unit:
            return (
                None,
                "actual_daily_dose_unit_mismatch",
            )

        if explicit_dose < 0:
            return (
                None,
                "actual_daily_dose_negative",
            )

        return (
            float(explicit_dose),
            "explicit_actual_daily_dose",
        )

    daily_quantity = pd.to_numeric(
        medication_row.get(
            "_daily_quantity",
            pd.NA,
        ),
        errors="coerce",
    )

    if pd.isna(daily_quantity):
        return None, "daily_quantity_missing"

    if daily_quantity < 0:
        return None, "daily_quantity_negative"

    source_unit = normalize_unit(
        medication_row.get(
            "_quantity_unit",
            "",
        )
    )

    expected_source_unit = normalize_unit(
        master_row.get(
            "source_dose_unit",
            "",
        )
    )

    if not source_unit:
        return None, "source_dose_unit_missing"

    if source_unit != expected_source_unit:
        return None, "source_dose_unit_mismatch"

    factor = pd.to_numeric(
        master_row.get(
            "dose_factor_to_delta_unit",
            pd.NA,
        ),
        errors="coerce",
    )

    if pd.isna(factor) or factor <= 0:
        return None, "dose_conversion_factor_missing"

    return (
        float(daily_quantity) * float(factor),
        "converted_from_daily_quantity",
    )


# ============================================================
# Snapshot calculation
# ============================================================

def calculate_snapshot(
    active_medications: pd.DataFrame,
    master: pd.DataFrame,
    episode_id: str,
    patient_id: str,
    evaluation_date: pd.Timestamp,
    timepoint: str,
    include_prn: bool,
    ingredient_universe_complete: bool,
) -> tuple[dict, pd.DataFrame, pd.DataFrame]:
    source_rows = []
    computable_rows = []

    for source_index, row in (
        active_medications.iterrows()
    ):
        ingredient_value = normalize_text(
            row.get(
                "_ingredient_value",
                "",
            )
        )

        drug_name = normalize_text(
            row.get(
                "_drug_name",
                "",
            )
        )

        sig = normalize_text(
            row.get("_sig", "")
        )

        base = {
            "episode_id": episode_id,
            "患者ID": patient_id,
            "timepoint": timepoint,
            "evaluation_date": date_to_string(
                evaluation_date
            ),
            "source_index": source_index,
            "成分コード": ingredient_value,
            "薬剤名": drug_name,
            "用法": sig,
            "source_daily_quantity": (
                row.get(
                    "_daily_quantity",
                    pd.NA,
                )
            ),
            "source_dose_unit": (
                row.get(
                    "_quantity_unit",
                    "",
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
        }

        if row.get("_is_prn", False):
            if not include_prn:
                source_rows.append({
                    **base,
                    "status": "excluded_prn",
                    "match_method": "",
                    "ingredient_id": "",
                    "ingredient_name_jp": "",
                    "burden_type": "",
                    "dbi_category": "",
                    "route": "",
                    "actual_daily_dose": "",
                    "delta": "",
                    "delta_unit": "",
                    "review_required": 0,
                    "review_reason": "",
                })

                continue

        master_row, match_method = (
            match_master_row(
                ingredient_value=(
                    ingredient_value
                ),
                drug_name=drug_name,
                master=master,
            )
        )

        if master_row is None:
            if (
                match_method == "not_in_master"
                and ingredient_universe_complete
            ):
                status = "not_dbi_drug"
                review_required = 0
                review_reason = ""
            else:
                status = "unresolved"
                review_required = 1
                review_reason = match_method

            source_rows.append({
                **base,
                "status": status,
                "match_method": match_method,
                "ingredient_id": "",
                "ingredient_name_jp": "",
                "burden_type": "",
                "dbi_category": "",
                "route": "",
                "actual_daily_dose": "",
                "delta": "",
                "delta_unit": "",
                "review_required": (
                    review_required
                ),
                "review_reason": review_reason,
            })

            continue

        if not bool(
            master_row["include_main"]
        ):
            source_rows.append({
                **base,
                "status": "excluded_by_master",
                "match_method": match_method,
                "ingredient_id": (
                    master_row["ingredient_id"]
                ),
                "ingredient_name_jp": (
                    master_row[
                        "ingredient_name_jp"
                    ]
                ),
                "burden_type": (
                    master_row["burden_type"]
                ),
                "dbi_category": (
                    master_row["dbi_category"]
                ),
                "route": master_row["route"],
                "actual_daily_dose": "",
                "delta": master_row["delta"],
                "delta_unit": (
                    master_row["delta_unit"]
                ),
                "review_required": 0,
                "review_reason": "",
            })

            continue

        actual_dose, dose_status = (
            calculate_actual_daily_dose(
                medication_row=row,
                master_row=master_row,
            )
        )

        if actual_dose is None:
            source_rows.append({
                **base,
                "status": "dose_unresolved",
                "match_method": match_method,
                "ingredient_id": (
                    master_row["ingredient_id"]
                ),
                "ingredient_name_jp": (
                    master_row[
                        "ingredient_name_jp"
                    ]
                ),
                "burden_type": (
                    master_row["burden_type"]
                ),
                "dbi_category": (
                    master_row["dbi_category"]
                ),
                "route": master_row["route"],
                "actual_daily_dose": "",
                "delta": master_row["delta"],
                "delta_unit": (
                    master_row["delta_unit"]
                ),
                "review_required": 1,
                "review_reason": dose_status,
            })

            continue

        source_rows.append({
            **base,
            "status": "computable",
            "match_method": match_method,
            "ingredient_id": (
                master_row["ingredient_id"]
            ),
            "ingredient_name_jp": (
                master_row[
                    "ingredient_name_jp"
                ]
            ),
            "burden_type": (
                master_row["burden_type"]
            ),
            "dbi_category": (
                master_row["dbi_category"]
            ),
            "route": master_row["route"],
            "actual_daily_dose": actual_dose,
            "delta": float(
                master_row["delta"]
            ),
            "delta_unit": (
                master_row["delta_unit"]
            ),
            "review_required": 0,
            "review_reason": "",
        })

        computable_rows.append({
            "episode_id": episode_id,
            "患者ID": patient_id,
            "timepoint": timepoint,
            "evaluation_date": date_to_string(
                evaluation_date
            ),
            "ingredient_id": (
                master_row["ingredient_id"]
            ),
            "ingredient_name_jp": (
                master_row[
                    "ingredient_name_jp"
                ]
            ),
            "burden_type": (
                master_row["burden_type"]
            ),
            "dbi_category": (
                master_row["dbi_category"]
            ),
            "route": master_row["route"],
            "actual_daily_dose": actual_dose,
            "delta": float(
                master_row["delta"]
            ),
            "delta_unit": (
                master_row["delta_unit"]
            ),
        })

    source_detail = pd.DataFrame(
        source_rows
    )

    if computable_rows:
        computable = pd.DataFrame(
            computable_rows
        )

        # 同一成分の複数製剤は投与量を合算する。
        ingredient_detail = (
            computable.groupby(
                [
                    "episode_id",
                    "患者ID",
                    "timepoint",
                    "evaluation_date",
                    "ingredient_id",
                    "ingredient_name_jp",
                    "burden_type",
                    "dbi_category",
                    "route",
                    "delta",
                    "delta_unit",
                ],
                as_index=False,
                dropna=False,
            )
            .agg(
                actual_daily_dose=(
                    "actual_daily_dose",
                    "sum",
                ),
                source_row_count=(
                    "actual_daily_dose",
                    "size",
                ),
            )
        )

        ingredient_detail[
            "individual_dbi"
        ] = (
            ingredient_detail[
                "actual_daily_dose"
            ]
            /
            (
                ingredient_detail[
                    "actual_daily_dose"
                ]
                +
                ingredient_detail["delta"]
            )
        )
    else:
        ingredient_detail = pd.DataFrame(
            columns=[
                "episode_id",
                "患者ID",
                "timepoint",
                "evaluation_date",
                "ingredient_id",
                "ingredient_name_jp",
                "burden_type",
                "dbi_category",
                "route",
                "delta",
                "delta_unit",
                "actual_daily_dose",
                "source_row_count",
                "individual_dbi",
            ]
        )

    if source_detail.empty:
        review_count = 0
        unresolved_count = 0
        active_rows = 0
    else:
        review_count = int(
            source_detail[
                "review_required"
            ].sum()
        )

        unresolved_count = int(
            source_detail["status"].isin(
                [
                    "unresolved",
                    "dose_unresolved",
                ]
            ).sum()
        )

        active_rows = int(
            source_detail[
                "source_index"
            ].nunique()
        )

    total_dbi = float(
        ingredient_detail[
            "individual_dbi"
        ].sum()
    )

    ac_dbi = float(
        ingredient_detail.loc[
            ingredient_detail[
                "burden_type"
            ] == "AC",
            "individual_dbi",
        ].sum()
    )

    sedative_dbi = float(
        ingredient_detail.loc[
            ingredient_detail[
                "burden_type"
            ] == "S",
            "individual_dbi",
        ].sum()
    )

    dbi_drug_count = int(
        len(ingredient_detail)
    )

    summary = {
        "episode_id": episode_id,
        "患者ID": patient_id,
        "timepoint": timepoint,
        "evaluation_date": date_to_string(
            evaluation_date
        ),
        "eligible": 1,
        "active_medication_rows": active_rows,
        "dbi_drug_count": dbi_drug_count,
        "dbi_total": total_dbi,
        "dbi_anticholinergic": ac_dbi,
        "dbi_sedative": sedative_dbi,
        "unresolved_count": unresolved_count,
        "review_required": int(
            review_count > 0
        ),
        "review_count": review_count,
        "score_status": (
            "complete"
            if review_count == 0
            else "partial_lower_bound_or_review_required"
        ),
    }

    return (
        summary,
        source_detail,
        ingredient_detail,
    )


# ============================================================
# Exact prescription-interval DBI
# ============================================================

def build_episode_boundaries(
    patient_medications: pd.DataFrame,
    admission_date: pd.Timestamp,
    discharge_date: pd.Timestamp,
) -> list[pd.Timestamp]:
    """
    開始日と終了日+1日から、DBIが一定となる区間境界を作る。

    入退院日を両方含むため、最終境界は退院日+1日。
    """
    end_exclusive = (
        discharge_date
        + pd.Timedelta(days=1)
    )

    boundaries = {
        admission_date,
        end_exclusive,
    }

    for _, row in patient_medications.iterrows():
        start = row.get(
            "_effective_start_date",
            pd.NaT,
        )

        end = row.get(
            "_end_date",
            pd.NaT,
        )

        if pd.notna(start):
            start = max(
                pd.Timestamp(start).normalize(),
                admission_date,
            )

            if start < end_exclusive:
                boundaries.add(start)

        if pd.notna(end):
            end_plus_one = (
                pd.Timestamp(end).normalize()
                + pd.Timedelta(days=1)
            )

            end_plus_one = min(
                max(
                    end_plus_one,
                    admission_date,
                ),
                end_exclusive,
            )

            boundaries.add(end_plus_one)

    return sorted(boundaries)


def calculate_interval_dbi(
    medications: pd.DataFrame,
    master: pd.DataFrame,
    episode_id: str,
    patient_id: str,
    admission_date: pd.Timestamp,
    discharge_date: pd.Timestamp,
    include_prn: bool,
    ingredient_universe_complete: bool,
) -> tuple[dict, pd.DataFrame]:
    patient_medications = medications.loc[
        medications["_patient_id"]
        == patient_id
    ].copy()

    boundaries = build_episode_boundaries(
        patient_medications=(
            patient_medications
        ),
        admission_date=admission_date,
        discharge_date=discharge_date,
    )

    interval_rows = []

    for start, end_exclusive in zip(
        boundaries[:-1],
        boundaries[1:],
    ):
        interval_days = int(
            (
                end_exclusive - start
            ).days
        )

        if interval_days <= 0:
            continue

        active = extract_active_medications(
            medications=medications,
            patient_id=patient_id,
            evaluation_date=start,
        )

        snapshot, _, _ = calculate_snapshot(
            active_medications=active,
            master=master,
            episode_id=episode_id,
            patient_id=patient_id,
            evaluation_date=start,
            timepoint="interval",
            include_prn=include_prn,
            ingredient_universe_complete=(
                ingredient_universe_complete
            ),
        )

        dbi_total = float(
            snapshot["dbi_total"]
        )

        interval_rows.append({
            "episode_id": episode_id,
            "患者ID": patient_id,
            "interval_start": (
                date_to_string(start)
            ),
            "interval_end": (
                date_to_string(
                    end_exclusive
                    - pd.Timedelta(days=1)
                )
            ),
            "interval_days": interval_days,
            "dbi_total": dbi_total,
            "dbi_anticholinergic": float(
                snapshot[
                    "dbi_anticholinergic"
                ]
            ),
            "dbi_sedative": float(
                snapshot["dbi_sedative"]
            ),
            "dbi_drug_count": int(
                snapshot["dbi_drug_count"]
            ),
            "review_required": int(
                snapshot["review_required"]
            ),
            "review_count": int(
                snapshot["review_count"]
            ),
            "dbi_burden_days": (
                dbi_total * interval_days
            ),
        })

    intervals = pd.DataFrame(
        interval_rows
    )

    hospital_days = int(
        (
            discharge_date
            - admission_date
        ).days
        + 1
    )

    if intervals.empty:
        dbi_auc = 0.0
        mean_daily_dbi = 0.0
        ac_auc = 0.0
        sedative_auc = 0.0
        review_required = 0
        review_interval_count = 0
    else:
        dbi_auc = float(
            intervals[
                "dbi_burden_days"
            ].sum()
        )

        ac_auc = float(
            (
                intervals[
                    "dbi_anticholinergic"
                ]
                * intervals["interval_days"]
            ).sum()
        )

        sedative_auc = float(
            (
                intervals[
                    "dbi_sedative"
                ]
                * intervals["interval_days"]
            ).sum()
        )

        mean_daily_dbi = (
            dbi_auc / hospital_days
        )

        review_interval_count = int(
            intervals[
                "review_required"
            ].sum()
        )

        review_required = int(
            review_interval_count > 0
        )

    summary = {
        "episode_id": episode_id,
        "患者ID": patient_id,
        "admission_date": date_to_string(
            admission_date
        ),
        "discharge_date": date_to_string(
            discharge_date
        ),
        "hospital_days_inclusive": (
            hospital_days
        ),
        "dbi_burden_days": dbi_auc,
        "anticholinergic_burden_days": (
            ac_auc
        ),
        "sedative_burden_days": (
            sedative_auc
        ),
        "mean_daily_dbi": mean_daily_dbi,
        "mean_daily_anticholinergic_dbi": (
            ac_auc / hospital_days
        ),
        "mean_daily_sedative_dbi": (
            sedative_auc / hospital_days
        ),
        "review_required": review_required,
        "review_interval_count": (
            review_interval_count
        ),
        "score_status": (
            "complete"
            if review_required == 0
            else "partial_lower_bound_or_review_required"
        ),
    }

    return summary, intervals


# ============================================================
# Longitudinal calculation
# ============================================================

def calculate_longitudinal_dbi(
    medication_csv: str,
    admission_csv: str,
    dbi_master_path: str,
    output_prefix: str,
    day14_offset: int = 14,
    day30_offset: int = 30,
    include_prn: bool = False,
    ingredient_universe_complete: bool = False,
):
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

    master = load_dbi_master(
        dbi_master_path
    )

    master_hash = file_sha256(
        dbi_master_path
    )

    patient_col = admission_columns[
        "patient"
    ]

    admission_col = admission_columns[
        "admission"
    ]

    discharge_col = admission_columns[
        "discharge"
    ]

    episode_col = admission_columns[
        "episode"
    ]

    admissions["_patient_id"] = (
        admissions[patient_col]
        .map(normalize_text)
    )

    admissions["_admission_date"] = (
        parse_date_column(
            admissions[admission_col]
        )
    )

    admissions["_discharge_date"] = (
        parse_date_column(
            admissions[discharge_col]
        )
    )

    invalid_mask = (
        (admissions["_patient_id"] == "")
        | admissions[
            "_admission_date"
        ].isna()
        | admissions[
            "_discharge_date"
        ].isna()
        | (
            admissions[
                "_discharge_date"
            ]
            <
            admissions[
                "_admission_date"
            ]
        )
    )

    invalid_admissions = admissions.loc[
        invalid_mask
    ].copy()

    valid_admissions = admissions.loc[
        ~invalid_mask
    ].copy()

    snapshot_summaries = []
    source_detail_frames = []
    ingredient_detail_frames = []
    episode_summaries = []
    interval_frames = []

    for _, admission_row in (
        valid_admissions.iterrows()
    ):
        patient_id = normalize_text(
            admission_row["_patient_id"]
        )

        admission_date = pd.Timestamp(
            admission_row[
                "_admission_date"
            ]
        ).normalize()

        discharge_date = pd.Timestamp(
            admission_row[
                "_discharge_date"
            ]
        ).normalize()

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

        for timepoint, evaluation_date in (
            timepoints
        ):
            if evaluation_date > discharge_date:
                snapshot_summaries.append({
                    "episode_id": episode_id,
                    "患者ID": patient_id,
                    "timepoint": timepoint,
                    "evaluation_date": (
                        date_to_string(
                            evaluation_date
                        )
                    ),
                    "eligible": 0,
                    "active_medication_rows": "",
                    "dbi_drug_count": "",
                    "dbi_total": "",
                    "dbi_anticholinergic": "",
                    "dbi_sedative": "",
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

            (
                snapshot,
                source_detail,
                ingredient_detail,
            ) = calculate_snapshot(
                active_medications=active,
                master=master,
                episode_id=episode_id,
                patient_id=patient_id,
                evaluation_date=evaluation_date,
                timepoint=timepoint,
                include_prn=include_prn,
                ingredient_universe_complete=(
                    ingredient_universe_complete
                ),
            )

            snapshot_summaries.append(
                snapshot
            )

            if not source_detail.empty:
                source_detail_frames.append(
                    source_detail
                )

            if not ingredient_detail.empty:
                ingredient_detail_frames.append(
                    ingredient_detail
                )

        (
            episode_summary,
            intervals,
        ) = calculate_interval_dbi(
            medications=medications,
            master=master,
            episode_id=episode_id,
            patient_id=patient_id,
            admission_date=admission_date,
            discharge_date=discharge_date,
            include_prn=include_prn,
            ingredient_universe_complete=(
                ingredient_universe_complete
            ),
        )

        episode_summaries.append(
            episode_summary
        )

        if not intervals.empty:
            interval_frames.append(
                intervals
            )

    snapshot_long = pd.DataFrame(
        snapshot_summaries
    )

    episode_summary = pd.DataFrame(
        episode_summaries
    )

    source_detail = (
        pd.concat(
            source_detail_frames,
            ignore_index=True,
        )
        if source_detail_frames
        else pd.DataFrame()
    )

    ingredient_detail = (
        pd.concat(
            ingredient_detail_frames,
            ignore_index=True,
        )
        if ingredient_detail_frames
        else pd.DataFrame()
    )

    intervals = (
        pd.concat(
            interval_frames,
            ignore_index=True,
        )
        if interval_frames
        else pd.DataFrame()
    )

    if not source_detail.empty:
        review = source_detail.loc[
            source_detail[
                "review_required"
            ] == 1
        ].copy()
    else:
        review = pd.DataFrame()

    # --------------------------------------------------------
    # Wide snapshots
    # --------------------------------------------------------

    if snapshot_long.duplicated(
        subset=[
            "episode_id",
            "timepoint",
        ]
    ).any():
        raise ValueError(
            "同一episode_id・timepointが"
            "重複しています。"
        )

    wide_values = [
        "evaluation_date",
        "eligible",
        "dbi_drug_count",
        "dbi_total",
        "dbi_anticholinergic",
        "dbi_sedative",
        "unresolved_count",
        "review_required",
        "score_status",
    ]

    snapshot_wide = snapshot_long.pivot(
        index=[
            "episode_id",
            "患者ID",
        ],
        columns="timepoint",
        values=wide_values,
    )

    snapshot_wide.columns = [
        f"{timepoint}_{value}"
        for value, timepoint
        in snapshot_wide.columns
    ]

    snapshot_wide = (
        snapshot_wide.reset_index()
    )

    snapshot_wide = snapshot_wide.merge(
        episode_summary,
        on=[
            "episode_id",
            "患者ID",
        ],
        how="left",
        suffixes=(
            "",
            "_episode",
        ),
    )

    preferred = [
        "episode_id",
        "患者ID",
        "admission_date",
        "discharge_date",
        "hospital_days_inclusive",
    ]

    for timepoint in TIMEPOINT_ORDER:
        for value in wide_values:
            column = (
                f"{timepoint}_{value}"
            )

            if column in snapshot_wide.columns:
                preferred.append(column)

    preferred.extend([
        "dbi_burden_days",
        "mean_daily_dbi",
        "anticholinergic_burden_days",
        "mean_daily_anticholinergic_dbi",
        "sedative_burden_days",
        "mean_daily_sedative_dbi",
        "review_required_episode",
        "score_status_episode",
    ])

    preferred = [
        column
        for column in preferred
        if column in snapshot_wide.columns
    ]

    remaining = [
        column
        for column in snapshot_wide.columns
        if column not in preferred
    ]

    snapshot_wide = snapshot_wide[
        preferred + remaining
    ]

    # --------------------------------------------------------
    # Output
    # --------------------------------------------------------

    prefix = Path(output_prefix)

    prefix.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    files = {
        "snapshot_long": (
            prefix.parent
            / f"{prefix.name}_snapshot_long.csv"
        ),
        "snapshot_wide": (
            prefix.parent
            / f"{prefix.name}_snapshot_wide.csv"
        ),
        "episode": (
            prefix.parent
            / f"{prefix.name}_episode.csv"
        ),
        "intervals": (
            prefix.parent
            / f"{prefix.name}_intervals.csv"
        ),
        "source_detail": (
            prefix.parent
            / f"{prefix.name}_source_detail.csv"
        ),
        "ingredient_detail": (
            prefix.parent
            / f"{prefix.name}_ingredient_detail.csv"
        ),
        "review": (
            prefix.parent
            / f"{prefix.name}_review.csv"
        ),
        "invalid": (
            prefix.parent
            / f"{prefix.name}_invalid_admissions.csv"
        ),
        "log": (
            prefix.parent
            / f"{prefix.name}_log.txt"
        ),
    }

    snapshot_long.to_csv(
        files["snapshot_long"],
        index=False,
        encoding="utf-8-sig",
    )

    snapshot_wide.to_csv(
        files["snapshot_wide"],
        index=False,
        encoding="utf-8-sig",
    )

    episode_summary.to_csv(
        files["episode"],
        index=False,
        encoding="utf-8-sig",
    )

    intervals.to_csv(
        files["intervals"],
        index=False,
        encoding="utf-8-sig",
    )

    source_detail.to_csv(
        files["source_detail"],
        index=False,
        encoding="utf-8-sig",
    )

    ingredient_detail.to_csv(
        files["ingredient_detail"],
        index=False,
        encoding="utf-8-sig",
    )

    review.to_csv(
        files["review"],
        index=False,
        encoding="utf-8-sig",
    )

    invalid_admissions.to_csv(
        files["invalid"],
        index=False,
        encoding="utf-8-sig",
    )

    log_lines = [
        f"medication_csv={medication_csv}",
        f"admission_csv={admission_csv}",
        f"dbi_master={dbi_master_path}",
        (
            "dbi_master_sha256="
            f"{master_hash}"
        ),
        f"day14_offset={day14_offset}",
        f"day30_offset={day30_offset}",
        f"include_prn={include_prn}",
        (
            "ingredient_universe_complete="
            f"{ingredient_universe_complete}"
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
            "snapshot_rows="
            f"{len(snapshot_long)}"
        ),
        (
            "episode_rows="
            f"{len(episode_summary)}"
        ),
        (
            "interval_rows="
            f"{len(intervals)}"
        ),
        (
            "review_rows="
            f"{len(review)}"
        ),
        "",
        "計算定義:",
        "individual_DBI = D / (D + delta)",
        (
            "同一評価時点の同一ingredient_idは"
            "実投与量を合算して1回だけ計算した。"
        ),
        (
            "処方開始日・終了日は包含関係とした。"
        ),
        (
            "hospital_days_inclusive = "
            "discharge_date - admission_date + 1"
        ),
        (
            "dbi_burden_days = "
            "sum(interval_DBI * interval_days)"
        ),
        (
            "mean_daily_dbi = "
            "dbi_burden_days / hospital_days_inclusive"
        ),
        (
            "review_required=1の場合、DBIは"
            "部分合計または暫定的下限値である。"
        ),
        (
            "薬剤名から規格またはdeltaを"
            "自動推定していない。"
        ),
    ]

    files["log"].write_text(
        "\n".join(log_lines),
        encoding="utf-8",
    )

    print(
        "Longitudinal DBI calculation completed."
    )

    for label, path in files.items():
        print(f"{label}: {path}")

    return {
        "snapshot_long": snapshot_long,
        "snapshot_wide": snapshot_wide,
        "episode_summary": episode_summary,
        "intervals": intervals,
        "source_detail": source_detail,
        "ingredient_detail": ingredient_detail,
        "review": review,
    }


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description=(
            "薬歴CSVと入退院CSVから、"
            "時点DBIおよび入院中平均日次DBIを"
            "算出する"
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
        "--dbi-master",
        required=True,
        help="検証済みDBI薬剤マスターCSV",
    )

    parser.add_argument(
        "--output-prefix",
        default="output/dbi_longitudinal",
        help="出力ファイル接頭辞",
    )

    parser.add_argument(
        "--day14-offset",
        type=int,
        default=14,
        help=(
            "入院日からの暦日差。"
            "入院14日目なら13"
        ),
    )

    parser.add_argument(
        "--day30-offset",
        type=int,
        default=30,
        help=(
            "入院日からの暦日差。"
            "入院30日目なら29"
        ),
    )

    parser.add_argument(
        "--include-prn",
        action="store_true",
        help=(
            "頓服を含める。"
            "ただし入力1日量が実使用量でない場合、"
            "DBIは実曝露を反映しない"
        ),
    )

    parser.add_argument(
        "--ingredient-universe-complete",
        action="store_true",
        help=(
            "院内成分コードとDBIマスターの対応が"
            "完全である場合に指定。"
            "マスター非掲載コードを非DBI薬として扱う"
        ),
    )

    args = parser.parse_args()

    calculate_longitudinal_dbi(
        medication_csv=(
            args.medication_csv
        ),
        admission_csv=(
            args.admission_csv
        ),
        dbi_master_path=(
            args.dbi_master
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
        include_prn=(
            args.include_prn
        ),
        ingredient_universe_complete=(
            args.ingredient_universe_complete
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
