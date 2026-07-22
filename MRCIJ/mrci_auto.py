from __future__ import annotations

import argparse
import re
import sys
import unicodedata
from pathlib import Path
from typing import Optional

import pandas as pd


# ============================================================
# MRCI weight masters
# ============================================================

# 原版Section A：32項目
SECTION_A_WEIGHTS = {
    # Oral: 6
    "ORAL_TABLET_CAPSULE": 1.0,
    "ORAL_GARGLE": 2.0,
    "ORAL_LOZENGE_GUM": 2.0,
    "ORAL_LIQUID": 2.0,
    "ORAL_POWDER_GRANULE": 2.0,
    "SUBLINGUAL": 2.0,

    # Topical: 6
    "TOPICAL_CREAM_GEL_OINTMENT": 2.0,
    "TOPICAL_DRESSING": 3.0,
    "TOPICAL_SOLUTION": 2.0,
    "TOPICAL_PASTE": 3.0,
    "TOPICAL_PATCH": 2.0,
    "TOPICAL_SPRAY": 1.0,

    # Ear: 1
    "EAR_DROP_CREAM_OINTMENT": 3.0,

    # Eye: 2
    "EYE_DROP": 3.0,
    "EYE_GEL_OINTMENT": 3.0,

    # Nose: 2
    "NASAL_DROP_CREAM_OINTMENT": 3.0,
    "NASAL_SPRAY": 2.0,

    # Inhalation and oxygen: 7
    "INHALER_ACCUHALER_DISKUS": 3.0,
    "INHALER_AEROLIZER": 3.0,
    "INHALER_MDI": 4.0,
    "INHALER_NEBULIZER": 5.0,
    "OXYGEN": 3.0,
    "INHALER_TURBUHALER": 3.0,
    "INHALER_OTHER_DPI": 3.0,

    # Other: 8
    "DIALYSATE": 5.0,
    "ENEMA": 2.0,
    "INJECTION_PREFILLED": 3.0,
    "INJECTION_VIAL_AMPULE": 4.0,
    "PESSARY": 3.0,
    "PCA": 2.0,
    "SUPPOSITORY": 2.0,
    "VAGINAL_CREAM": 2.0,
}

assert len(SECTION_A_WEIGHTS) == 32


# 原版Section B：23項目
SECTION_B_WEIGHTS = {
    "ONCE_DAILY": 1.0,
    "ONCE_DAILY_PRN": 0.5,

    "TWICE_DAILY": 2.0,
    "TWICE_DAILY_PRN": 1.0,

    "THREE_TIMES_DAILY": 3.0,
    "THREE_TIMES_DAILY_PRN": 1.5,

    "FOUR_TIMES_DAILY": 4.0,
    "FOUR_TIMES_DAILY_PRN": 2.0,

    "Q12H": 2.5,
    "Q12H_PRN": 1.5,

    "Q8H": 3.5,
    "Q8H_PRN": 2.0,

    "Q6H": 4.5,
    "Q6H_PRN": 2.5,

    "Q4H": 6.5,
    "Q4H_PRN": 3.5,

    "Q2H": 12.5,
    "Q2H_PRN": 6.5,

    "PRN": 0.5,
    "LESS_THAN_DAILY": 2.0,

    "OXYGEN_PRN": 1.0,
    "OXYGEN_LT15H": 2.0,
    "OXYGEN_GE15H": 3.0,
}

assert len(SECTION_B_WEIGHTS) == 23


# 原版Section C：10項目
SECTION_C_WEIGHTS = {
    "BREAK_CRUSH": 1.0,
    "DISSOLVE_MIX": 1.0,
    "MULTIPLE_UNITS": 1.0,
    "VARIABLE_DOSE": 1.0,
    "SPECIFIC_TIME": 1.0,
    "RELATION_TO_FOOD": 1.0,
    "SPECIFIC_FLUID": 1.0,
    "AS_DIRECTED": 2.0,
    "TAPERING": 2.0,
    "ALTERNATING_DOSE": 2.0,
}

assert len(SECTION_C_WEIGHTS) == 10


# ============================================================
# Unit definitions
# ============================================================

UNIT_NORMALIZATION = {
    "cap": "カプセル",
    "caps": "カプセル",
    "capsule": "カプセル",
    "カプセル": "カプセル",

    "ml": "mL",
    "g": "g",
    "mg": "mg",
    "mcg": "μg",
    "ug": "μg",
    "μg": "μg",

    "錠": "錠",
    "包": "包",
    "枚": "枚",
    "滴": "滴",
    "本": "本",
    "単位": "単位",

    "キット": "キット",
    "管": "管",
    "袋": "袋",
    "筒": "筒",
    "瓶": "瓶",
    "個": "個",
}


COUNTABLE_ADMINISTRATION_UNITS = {
    "錠",
    "カプセル",
    "包",
    "枚",
}


NON_COUNTABLE_QUANTITY_UNITS = {
    "g",
    "mL",
    "mg",
    "μg",
    "単位",
}


PACKAGE_UNITS = {
    "キット",
    "管",
    "袋",
    "筒",
    "瓶",
}


# ============================================================
# General utilities
# ============================================================

def normalize_text(value) -> str:
    """
    文字、空白、全角・半角を正規化する。
    """
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
        "：": ":",
        "，": ",",
        "、": ",",
        "−": "-",
        "–": "-",
        "—": "-",
        "―": "-",
        "〜": "~",
        "～": "~",
        "　": " ",
    }

    for old, new in replacements.items():
        text = text.replace(old, new)

    text = re.sub(r"\s+", " ", text)

    return text.strip()


def normalize_unit(value) -> str:
    text = normalize_text(value)

    if not text:
        return ""

    lower = text.lower()

    if lower in UNIT_NORMALIZATION:
        return UNIT_NORMALIZATION[lower]

    if text in UNIT_NORMALIZATION:
        return UNIT_NORMALIZATION[text]

    return text


def parse_float(value) -> Optional[float]:
    """
    文字列内の最初の数値をfloatへ変換する。
    """
    text = normalize_text(value)

    if not text:
        return None

    match = re.search(
        r"[-+]?\d+(?:\.\d+)?",
        text,
    )

    if not match:
        return None

    try:
        return float(match.group(0))
    except ValueError:
        return None


def append_unique(
    items: list[str],
    value: str,
):
    if value and value not in items:
        items.append(value)


def is_numeric_only_unit(unit: str) -> bool:
    normalized = normalize_unit(unit)

    return bool(
        re.fullmatch(
            r"\d+(?:\.\d+)?",
            normalized,
        )
    )


def first_nonempty(
    values: pd.Series,
) -> str:
    for value in values:
        text = normalize_text(value)

        if text:
            return text

    return ""


def read_csv_auto_encoding(
    path: str,
) -> pd.DataFrame:
    """
    院内CSVで想定される文字コードを順番に試す。
    """
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
        "CSVの文字コードを判定できませんでした。\n"
        + "\n".join(errors)
    )


def normalize_headers(
    df: pd.DataFrame,
) -> pd.DataFrame:
    renamed = {}

    for column in df.columns:
        new_name = normalize_text(column)
        new_name = new_name.replace("\r", "")
        new_name = new_name.replace("\n", "")
        renamed[column] = new_name

    return df.rename(columns=renamed)


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
            f"必要な列が見つかりません。"
            f"候補={candidates}\n"
            f"実際の列={list(df.columns)}"
        )

    return None


# ============================================================
# Date handling
# ============================================================

def parse_date_series(
    series: pd.Series,
) -> pd.Series:
    cleaned = series.map(
        normalize_text
    )

    return pd.to_datetime(
        cleaned.replace("", pd.NA),
        errors="coerce",
    )


def filter_by_evaluation_date(
    df: pd.DataFrame,
    evaluation_date: Optional[str],
    start_col: Optional[str],
    end_col: Optional[str],
) -> pd.DataFrame:
    """
    評価日に有効な処方だけを抽出する。

    開始日・終了日の空欄は制限なしとする。
    """
    if evaluation_date is None:
        return df.copy()

    eval_date = pd.to_datetime(
        evaluation_date,
        errors="raise",
    )

    if start_col is not None:
        start_dates = parse_date_series(
            df[start_col]
        )
    else:
        start_dates = pd.Series(
            pd.NaT,
            index=df.index,
            dtype="datetime64[ns]",
        )

    if end_col is not None:
        end_dates = parse_date_series(
            df[end_col]
        )
    else:
        end_dates = pd.Series(
            pd.NaT,
            index=df.index,
            dtype="datetime64[ns]",
        )

    active_mask = (
        (
            start_dates.isna()
            | (start_dates <= eval_date)
        )
        & (
            end_dates.isna()
            | (end_dates >= eval_date)
        )
    )

    return df.loc[
        active_mask
    ].copy()


# ============================================================
# Drug key and exclusion
# ============================================================

def canonical_drug_name_hint(
    drug_name: str,
) -> str:
    """
    薬剤名から規格等を除いた推定キー。

    成分コードではないため、成分列がない場合は
    重複薬候補の検出にのみ使用する。
    """
    text = normalize_text(
        drug_name
    ).lower()

    text = re.sub(
        r"[「『][^」』]+[」』]",
        "",
        text,
    )

    text = re.sub(
        r"\([^()]*\)",
        "",
        text,
    )

    text = re.sub(
        r"\d+(?:\.\d+)?\s*"
        r"(?:mg|μg|mcg|g|mL|ml|%|単位)",
        "",
        text,
        flags=re.IGNORECASE,
    )

    text = re.sub(
        r"\d+(?:\.\d+)?\s*[-/]\s*"
        r"\d+(?:\.\d+)?",
        "",
        text,
    )

    text = re.sub(
        r"\s+",
        "",
        text,
    )

    return text


def classify_exclusion(
    drug_name: str,
    sig: str = "",
):
    """
    明らかなMRCI採点対象外を検出する。
    """
    text = (
        f"{normalize_text(drug_name)} "
        f"{normalize_text(sig)}"
    )

    if re.search(
        r"ワクチン|予防接種|"
        r"コミナティ|スパイクバックス",
        text,
        flags=re.IGNORECASE,
    ):
        return True, "ワクチン", "high"

    if re.search(
        r"注射針|ペンニードル|"
        r"ランセット|穿刺針|"
        r"血糖.*(?:センサー|測定器)|"
        r"試験紙|テストストリップ|"
        r"ガーゼ|カテーテル|"
        r"シリンジのみ",
        text,
        flags=re.IGNORECASE,
    ):
        return True, "医療材料", "high"

    return False, "", ""


# ============================================================
# Dose sequence extraction
# ============================================================

def extract_dose_sequence(
    sig: str,
) -> list[float]:
    """
    括弧内の時点別投与量配列を抽出する。

    例:
        (2-1)   -> [2.0, 1.0]
        (1-0.5) -> [1.0, 0.5]
    """
    text = normalize_text(sig)

    matches = re.findall(
        r"\(([^()]*)\)",
        text,
    )

    for inner in reversed(matches):
        inner = normalize_text(inner)

        if not re.fullmatch(
            r"\d+(?:\.\d+)?"
            r"(?:\s*-\s*\d+(?:\.\d+)?)+",
            inner,
        ):
            continue

        try:
            return [
                float(value.strip())
                for value in inner.split("-")
            ]
        except ValueError:
            continue

    return []


# ============================================================
# Section A: dosage form
# ============================================================

def classify_dosage_form(
    drug_name: str,
    sig: str = "",
    amount_unit: str = "",
):
    name = normalize_text(drug_name)
    instruction = normalize_text(sig)
    normalized_unit = normalize_unit(
        amount_unit
    )

    combined = f"{name} {instruction}"

    def result(
        code: str,
        confidence: str = "high",
        note: str = "",
    ):
        return (
            code,
            SECTION_A_WEIGHTS[code],
            confidence,
            note,
        )

    # Oxygen, dialysis, PCA
    if re.search(
        r"酸素|\bO2\b",
        combined,
        flags=re.IGNORECASE,
    ):
        return result("OXYGEN")

    if re.search(
        r"透析液|ダイアライセート",
        combined,
    ):
        return result("DIALYSATE")

    if re.search(
        r"\bPCA\b|自己調節鎮痛",
        combined,
        flags=re.IGNORECASE,
    ):
        return result("PCA")

    # Inhalation
    if re.search(
        r"吸入|吸入用|inhal",
        combined,
        flags=re.IGNORECASE,
    ):
        if re.search(
            r"吸入液|ネブライザ|"
            r"吸入用アンプル|nebul",
            combined,
            flags=re.IGNORECASE,
        ):
            return result(
                "INHALER_NEBULIZER"
            )

        if re.search(
            r"エアゾール|pMDI|\bMDI\b|"
            r"定量噴霧|HFA",
            combined,
            flags=re.IGNORECASE,
        ):
            return result(
                "INHALER_MDI"
            )

        if re.search(
            r"アキュヘラー|ディスカス|"
            r"Accuhaler|Diskus",
            combined,
            flags=re.IGNORECASE,
        ):
            return result(
                "INHALER_ACCUHALER_DISKUS"
            )

        if re.search(
            r"エアロライザー|Aerolizer",
            combined,
            flags=re.IGNORECASE,
        ):
            return result(
                "INHALER_AEROLIZER"
            )

        if re.search(
            r"タービュヘイラー|Turbuhaler",
            combined,
            flags=re.IGNORECASE,
        ):
            return result(
                "INHALER_TURBUHALER"
            )

        if re.search(
            r"レスピマット|Respimat|"
            r"ソフトミスト",
            combined,
            flags=re.IGNORECASE,
        ):
            return (
                None,
                None,
                "unmapped",
                "ソフトミスト吸入器は"
                "事前の割付規則が必要",
            )

        if re.search(
            r"ブリーズヘラー|ハンディヘラー|"
            r"エリプタ|スイングヘラー|"
            r"クリックヘラー|ロタディスク|"
            r"吸入用カプセル|\bDPI\b|"
            r"dry powder",
            combined,
            flags=re.IGNORECASE,
        ):
            return result(
                "INHALER_OTHER_DPI"
            )

        return (
            None,
            None,
            "unmapped",
            "吸入デバイスを確定できない",
        )

    # Eye
    if re.search(
        r"眼軟膏|点眼ゲル|眼用ゲル",
        name,
    ):
        return result(
            "EYE_GEL_OINTMENT"
        )

    if re.search(
        r"点眼|眼科用液|眼用液",
        name,
    ):
        return result("EYE_DROP")

    # Ear
    if re.search(
        r"点耳|耳科用|耳用液|"
        r"耳用クリーム|耳用軟膏",
        name,
    ):
        return result(
            "EAR_DROP_CREAM_OINTMENT"
        )

    # Nose
    if re.search(
        r"点鼻スプレー|鼻噴霧|"
        r"噴霧用点鼻",
        name,
    ):
        return result("NASAL_SPRAY")

    if re.search(
        r"鼻用クリーム|鼻用軟膏|"
        r"点鼻液|鼻用液",
        name,
    ):
        return result(
            "NASAL_DROP_CREAM_OINTMENT"
        )

    if re.search(r"点鼻", name):
        return (
            None,
            None,
            "unmapped",
            "点鼻液とスプレーを区別できない",
        )

    # Injection
    if re.search(
        r"注射|\b注\b|皮下注|静注|筋注|"
        r"静脈注|点滴",
        combined,
    ):
        if re.search(
            r"シリンジ|プレフィルド|"
            r"ペン型|注入器|"
            r"オートインジェクター",
            combined,
        ):
            return result(
                "INJECTION_PREFILLED"
            )

        if normalized_unit in {
            "キット",
            "筒",
        }:
            return result(
                "INJECTION_PREFILLED",
                "medium",
                "単位からプレフィルドと暫定分類",
            )

        return result(
            "INJECTION_VIAL_AMPULE",
            "medium",
            "アンプル・バイアル等として暫定分類",
        )

    # Rectal and vaginal
    if re.search(r"浣腸", name):
        return result("ENEMA")

    if re.search(
        r"坐剤|坐薬|坐用",
        name,
    ):
        return result("SUPPOSITORY")

    if re.search(
        r"腟錠|膣錠|ペッサリー",
        name,
    ):
        return result("PESSARY")

    if re.search(
        r"腟クリーム|膣クリーム",
        name,
    ):
        return result("VAGINAL_CREAM")

    # Oral cavity
    if re.search(r"舌下", combined):
        return result("SUBLINGUAL")

    if re.search(
        r"含嗽|うがい|洗口|"
        r"マウスウォッシュ",
        combined,
    ):
        return result("ORAL_GARGLE")

    if re.search(
        r"トローチ|ガム",
        name,
    ):
        return result(
            "ORAL_LOZENGE_GUM"
        )

    # Topical
    if re.search(
        r"ドレッシング|被覆材|創傷被覆",
        name,
    ):
        return result(
            "TOPICAL_DRESSING"
        )

    if re.search(
        r"テープ|パップ|貼付|パッチ",
        name,
    ):
        return result("TOPICAL_PATCH")

    if re.search(
        r"外用スプレー|噴霧剤",
        name,
    ):
        return result("TOPICAL_SPRAY")

    if re.search(r"ペースト", name):
        return result("TOPICAL_PASTE")

    if re.search(
        r"ローション|外用液|リニメント",
        name,
    ):
        return result(
            "TOPICAL_SOLUTION"
        )

    if re.search(
        r"軟膏|クリーム|ゲル|外用ゼリー",
        name,
    ):
        return result(
            "TOPICAL_CREAM_GEL_OINTMENT"
        )

    # Oral powder
    if re.search(
        r"細粒|顆粒|散剤|"
        r"ドライシロップ|エキス散|"
        r"\bDS\b",
        name,
        flags=re.IGNORECASE,
    ):
        return result(
            "ORAL_POWDER_GRANULE"
        )

    # Oral liquid
    if re.search(
        r"シロップ|内用液|経口液|"
        r"経口懸濁液|エリキシル",
        name,
    ):
        return result("ORAL_LIQUID")

    # Oral tablet/capsule
    if re.search(
        r"錠|カプセル|\bcap\b|\btablet\b",
        name,
        flags=re.IGNORECASE,
    ):
        return result(
            "ORAL_TABLET_CAPSULE"
        )

    # Unit fallback
    if normalized_unit in {
        "錠",
        "カプセル",
    }:
        return result(
            "ORAL_TABLET_CAPSULE",
            "medium",
            "単位から錠剤・カプセルと推定",
        )

    if normalized_unit == "包":
        return result(
            "ORAL_POWDER_GRANULE",
            "low",
            "単位から散剤・顆粒と暫定分類",
        )

    if normalized_unit == "枚":
        return (
            None,
            None,
            "unmapped",
            "貼付剤と被覆材を区別できない",
        )

    if normalized_unit in (
        NON_COUNTABLE_QUANTITY_UNITS
        | PACKAGE_UNITS
    ):
        return (
            None,
            None,
            "unmapped",
            f"単位={normalized_unit}だけでは"
            "剤形を判定できない",
        )

    if is_numeric_only_unit(
        normalized_unit
    ):
        return (
            None,
            None,
            "unmapped",
            f"数値のみの単位={normalized_unit}",
        )

    return (
        None,
        None,
        "unmapped",
        "剤形を判定できない",
    )


# ============================================================
# Section B preparation
# ============================================================

def is_prn(
    sig: str,
) -> bool:
    text = normalize_text(sig)

    return bool(
        re.search(
            r"頓服|頓用|必要時|疼痛時|発熱時|"
            r"不眠時|便秘時|嘔気時|発作時|"
            r"症状時|苦痛時|適宜",
            text,
        )
    )
# ============================================================
# Section B: frequency
# ============================================================

def classify_frequency(
    sig: str,
    form_category: Optional[str] = None,
):
    text = normalize_text(sig)
    prn = is_prn(text)

    if not text:
        return (
            None,
            None,
            None,
            "unmapped",
            "用法が空欄",
        )

    if re.fullmatch(
        r"mL|ml|g|mg|μg|"
        r"フリーコメント|外用",
        text,
        flags=re.IGNORECASE,
    ):
        return (
            None,
            None,
            None,
            "unmapped",
            "頻度情報を含まない用法",
        )

    if "…" in text or "..." in text:
        return (
            None,
            None,
            None,
            "low",
            "用法が省略・切断されている可能性",
        )

    # Oxygen
    if form_category == "OXYGEN":
        if prn:
            code = "OXYGEN_PRN"

            return (
                code,
                SECTION_B_WEIGHTS[code],
                None,
                "high",
                "",
            )

        hours_match = re.search(
            r"1日\s*(\d+(?:\.\d+)?)\s*時間",
            text,
        )

        if hours_match:
            hours = float(
                hours_match.group(1)
            )

            code = (
                "OXYGEN_LT15H"
                if hours < 15
                else "OXYGEN_GE15H"
            )

            return (
                code,
                SECTION_B_WEIGHTS[code],
                None,
                "high",
                "",
            )

        if re.search(
            r"持続|連続|常時|終日|24時間",
            text,
        ):
            code = "OXYGEN_GE15H"

            return (
                code,
                SECTION_B_WEIGHTS[code],
                None,
                "medium",
                "常時使用を15時間以上として"
                "暫定分類",
            )

        return (
            None,
            None,
            None,
            "unmapped",
            "酸素療法の使用時間を判定できない",
        )

    # 隔日またはそれより低頻度
    if re.search(
        r"隔日|1日おき|"
        r"[2-9]\s*日おき|"
        r"[2-9]\s*日ごと|"
        r"毎週|週\s*\d+\s*回|"
        r"週に\s*\d+\s*回|"
        r"毎月|月\s*\d+\s*回|"
        r"月に\s*\d+\s*回|"
        r"年\s*\d+\s*回|"
        r"月水金|火木土|"
        r"透析日|透析後|曜日|"
        r"(?:48|72|96|120|168)\s*時間",
        text,
    ):
        code = "LESS_THAN_DAILY"

        return (
            code,
            SECTION_B_WEIGHTS[code],
            None,
            "high",
            "隔日または毎日未満の頻度",
        )

    # 4～6時間ごと等
    interval_range_match = re.search(
        r"(2|4|6|8|12)\s*"
        r"(?:~|-)\s*"
        r"(2|4|6|8|12)\s*時間\s*"
        r"(?:おき|毎|ごと|"
        r"以上あけ|以上空け)?",
        text,
    )

    if interval_range_match:
        hours = max(
            int(
                interval_range_match.group(1)
            ),
            int(
                interval_range_match.group(2)
            ),
        )

        base_code = f"Q{hours}H"

        code = (
            f"{base_code}_PRN"
            if prn
            else base_code
        )

        return (
            code,
            SECTION_B_WEIGHTS[code],
            None if prn else 24 / hours,
            "medium",
            f"間隔幅から複雑性の低い"
            f"{hours}時間間隔を採用",
        )

    # 単一の時間間隔
    interval_match = re.search(
        r"(2|4|6|8|12|24)\s*時間\s*"
        r"(?:おき|毎|ごと|"
        r"以上あけ|以上空け)",
        text,
    )

    if interval_match:
        hours = int(
            interval_match.group(1)
        )

        if hours == 24:
            code = (
                "ONCE_DAILY_PRN"
                if prn
                else "ONCE_DAILY"
            )

            daily_frequency = (
                None if prn else 1.0
            )
        else:
            base_code = f"Q{hours}H"

            code = (
                f"{base_code}_PRN"
                if prn
                else base_code
            )

            daily_frequency = (
                None
                if prn
                else 24 / hours
            )

        confidence = "high"
        note = ""

        if re.search(
            r"以上あけ|以上空け",
            text,
        ):
            confidence = "medium"
            note = (
                "最小投与間隔を時間間隔投与として"
                "暫定分類"
            )

        return (
            code,
            SECTION_B_WEIGHTS[code],
            daily_frequency,
            confidence,
            note,
        )

    # 明示された1日回数
    frequency_match = re.search(
        r"1日\s*([1-9])\s*回",
        text,
    )

    if frequency_match:
        count = int(
            frequency_match.group(1)
        )

        code_map = {
            1: "ONCE_DAILY",
            2: "TWICE_DAILY",
            3: "THREE_TIMES_DAILY",
            4: "FOUR_TIMES_DAILY",
        }

        if count in code_map:
            code = code_map[count]

            if prn:
                code += "_PRN"

            confidence = "high"
            note = ""

            dose_sequence = (
                extract_dose_sequence(text)
            )

            if (
                dose_sequence
                and len(dose_sequence) != count
            ):
                confidence = "low"
                note = (
                    f"1日{count}回だが投与量配列は"
                    f"{len(dose_sequence)}時点分: "
                    f"{dose_sequence}"
                )

            return (
                code,
                SECTION_B_WEIGHTS[code],
                None if prn else float(count),
                confidence,
                note,
            )

        # Colorado追加規則
        if count == 5:
            code = (
                "Q6H_PRN"
                if prn
                else "Q6H"
            )

            return (
                code,
                SECTION_B_WEIGHTS[code],
                None if prn else 4.0,
                "medium",
                "1日5回は原版にないため"
                "Q6Hへ割付",
            )

        return (
            None,
            None,
            float(count),
            "unmapped",
            f"1日{count}回は標準カテゴリー外",
        )

    # 毎食
    if re.search(
        r"毎食後|毎食前|毎食直前|"
        r"毎食直後|毎食時",
        text,
    ):
        code = (
            "THREE_TIMES_DAILY_PRN"
            if prn
            else "THREE_TIMES_DAILY"
        )

        return (
            code,
            SECTION_B_WEIGHTS[code],
            None if prn else 3.0,
            "medium",
            "毎食の記載から1日3回と判定",
        )

    # 時刻だけの表現
    compact = re.sub(
        r"[\s・,]",
        "",
        text,
    )

    exact_time_frequency = {
        "朝": 1,
        "昼": 1,
        "夕": 1,
        "晩": 1,
        "就寝前": 1,
        "眠前": 1,
        "起床時": 1,
        "朝夕": 2,
        "朝昼": 2,
        "昼夕": 2,
        "朝昼夕": 3,
    }

    if compact in exact_time_frequency:
        count = (
            exact_time_frequency[compact]
        )

        code_map = {
            1: "ONCE_DAILY",
            2: "TWICE_DAILY",
            3: "THREE_TIMES_DAILY",
        }

        code = code_map[count]

        if prn:
            code += "_PRN"

        return (
            code,
            SECTION_B_WEIGHTS[code],
            None if prn else float(count),
            "medium",
            "時刻表現から1日回数を推定",
        )

    if prn:
        code = "PRN"

        return (
            code,
            SECTION_B_WEIGHTS[code],
            None,
            "high",
            "",
        )

    return (
        None,
        None,
        None,
        "unmapped",
        f"投与頻度を判定できない: {text}",
    )


# ============================================================
# Section C: additional directions
# ============================================================

def is_countable_administration(
    drug_name: str,
    form_category: Optional[str],
    amount_unit: str,
) -> bool:
    name = normalize_text(drug_name)
    normalized_unit = normalize_unit(
        amount_unit
    )

    if normalized_unit in (
        COUNTABLE_ADMINISTRATION_UNITS
    ):
        return True

    if re.search(
        r"錠|カプセル|顆粒|細粒|散剤|"
        r"テープ|パップ|貼付",
        name,
    ):
        return True

    return form_category in {
        "INHALER_ACCUHALER_DISKUS",
        "INHALER_AEROLIZER",
        "INHALER_MDI",
        "INHALER_TURBUHALER",
        "INHALER_OTHER_DPI",
        "TOPICAL_PATCH",
        "ORAL_TABLET_CAPSULE",
        "ORAL_POWDER_GRANULE",
    }


def classify_additional_directions(
    sig: str,
    drug_name: str,
    form_category: Optional[str],
    daily_amount: Optional[float] = None,
    amount_unit: str = "",
    daily_frequency: Optional[float] = None,
):
    text = normalize_text(sig)
    name = normalize_text(drug_name)
    normalized_unit = normalize_unit(
        amount_unit
    )

    codes: set[str] = set()
    evidence: list[str] = []
    review_notes: list[str] = []

    if not text:
        return [], 0.0, "", []

    if re.fullmatch(
        r"mL|ml|g|mg|μg|外用",
        text,
        flags=re.IGNORECASE,
    ):
        return [], 0.0, "", []

    if text == "フリーコメント":
        return (
            [],
            0.0,
            "",
            [
                "フリーコメントの実内容が"
                "CSVに含まれていない"
            ],
        )

    if "…" in text or "..." in text:
        review_notes.append(
            "用法が省略・切断されている可能性"
        )

    dose_sequence = (
        extract_dose_sequence(text)
    )

    # --------------------------------------------------------
    # 分割・粉砕
    #
    # 0.5錠等の数値だけでは加点しない。
    # 行為が明示された場合だけ加点する。
    # --------------------------------------------------------

    crush_prohibited = bool(
        re.search(
            r"粉砕不可|粉砕禁止|"
            r"分割不可|割らない|砕かない",
            text,
        )
    )

    explicit_break_or_crush = bool(
        re.search(
            r"半分に割|半錠に割|"
            r"1/2\s*錠に割|"
            r"1/4\s*錠に割|"
            r"粉砕して|粉砕後|砕いて|"
            r"分割して|割って服用|"
            r"錠剤を割",
            text,
        )
    )

    if (
        explicit_break_or_crush
        and not crush_prohibited
    ):
        codes.add("BREAK_CRUSH")
        append_unique(
            evidence,
            "明示的な分割・粉砕指示",
        )

    # 溶解・混和
    if re.search(
        r"溶かして|溶解して|"
        r"懸濁して|混ぜて|"
        r"混和して|簡易懸濁",
        text,
    ):
        codes.add("DISSOLVE_MIX")
        append_unique(
            evidence,
            "明示的な溶解・混和指示",
        )

    insulin_context = bool(
        re.search(
            r"インスリン|insulin",
            f"{name} {text}",
            flags=re.IGNORECASE,
        )
    )

    bilateral_drop_context = bool(
        re.search(
            r"両眼|各眼|左右の眼|"
            r"両耳|各耳|左右の耳",
            text,
        )
    )

    # 複数単位：明示的な1回量
    explicit_multiple_match = re.search(
        r"(?:1回|一回)\s*"
        r"([1-9]\d*(?:\.\d+)?)\s*"
        r"(錠|包|カプセル|Cap|枚|"
        r"噴霧|吸入|プッシュ|本)",
        text,
        flags=re.IGNORECASE,
    )

    if explicit_multiple_match:
        value = float(
            explicit_multiple_match.group(1)
        )

        if value > 1.0:
            codes.add("MULTIPLE_UNITS")
            append_unique(
                evidence,
                "明示的な複数単位: "
                + explicit_multiple_match.group(0),
            )

    # 投与量配列
    if (
        dose_sequence
        and any(
            value > 1.0
            for value in dose_sequence
        )
        and is_countable_administration(
            drug_name,
            form_category,
            normalized_unit,
        )
        and not insulin_context
        and not bilateral_drop_context
    ):
        codes.add("MULTIPLE_UNITS")
        append_unique(
            evidence,
            f"投与量配列に1単位超あり: "
            f"{dose_sequence}",
        )

    # 1日量÷投与回数
    if (
        "MULTIPLE_UNITS" not in codes
        and daily_amount is not None
        and daily_frequency is not None
        and daily_frequency > 0
        and is_countable_administration(
            drug_name,
            form_category,
            normalized_unit,
        )
        and not insulin_context
        and not bilateral_drop_context
    ):
        amount_per_dose = (
            daily_amount / daily_frequency
        )

        if amount_per_dose > 1.0:
            codes.add("MULTIPLE_UNITS")
            append_unique(
                evidence,
                f"1日量÷投与回数から"
                f"1回{amount_per_dose:g}"
                f"{normalized_unit}と推定",
            )

    # インスリンの単位と両眼・両耳は除外
    if (
        insulin_context
        or bilateral_drop_context
    ):
        codes.discard("MULTIPLE_UNITS")

    # --------------------------------------------------------
    # 交互投与・時点別異用量
    # --------------------------------------------------------

    unequal_fixed_sequence = (
        len(dose_sequence) >= 2
        and len(set(dose_sequence)) >= 2
    )

    explicit_alternating = bool(
        re.search(
            r"交互に|"
            r"奇数日.*偶数日|"
            r"偶数日.*奇数日|"
            r"月水金.*(?:他|それ以外)|"
            r"(?:他|それ以外).*月水金|"
            r"曜日によって.*(?:量|用量)|"
            r"日によって.*(?:量|用量)",
            text,
        )
    )

    if (
        unequal_fixed_sequence
        or explicit_alternating
    ):
        codes.add("ALTERNATING_DOSE")

        if unequal_fixed_sequence:
            append_unique(
                evidence,
                f"時点別に異なる固定用量: "
                f"{dose_sequence}",
            )
        else:
            append_unique(
                evidence,
                "交互投与指示",
            )

    # --------------------------------------------------------
    # 可変用量
    # --------------------------------------------------------

    text_without_sequence = re.sub(
        r"\(\s*\d+(?:\.\d+)?"
        r"(?:\s*-\s*\d+(?:\.\d+)?)+\s*\)",
        "",
        text,
    )

    variable_dose = bool(
        re.search(
            r"\d+(?:\.\d+)?\s*(?:~|-)\s*"
            r"\d+(?:\.\d+)?\s*"
            r"(錠|包|カプセル|Cap|回|"
            r"噴霧|吸入|滴|mg|μg|"
            r"g|mL|単位)"
            r"|症状に応じて.*"
            r"(?:増減|調節|変更)"
            r"|適宜増減"
            r"|量を調節"
            r"|血糖値に応じて"
            r"|スライディングスケール"
            r"|体重に応じて"
            r"|検査値に応じて",
            text_without_sequence,
            flags=re.IGNORECASE,
        )
    )

    if variable_dose:
        codes.add("VARIABLE_DOSE")
        append_unique(
            evidence,
            "可変用量",
        )

    # 同一複雑性の二重加点を避ける
    if "ALTERNATING_DOSE" in codes:
        codes.discard("VARIABLE_DOSE")

    # 特定時刻
    if re.search(
        r"起床時|就寝前|眠前|"
        r"朝|昼|夕|晩|"
        r"午前|午後|"
        r"\d{1,2}\s*時",
        text,
    ):
        codes.add("SPECIFIC_TIME")
        append_unique(
            evidence,
            "指定された時刻・時間帯",
        )

    # 食事との関係
    if re.search(
        r"食直前|食直後|食前|食後|"
        r"食間|食事中|空腹時|"
        r"朝食|昼食|夕食|"
        r"食事とともに|食事と一緒",
        text,
    ):
        codes.add("RELATION_TO_FOOD")
        append_unique(
            evidence,
            "食事との関係",
        )

    # 特定の液体
    if re.search(
        r"コップ\s*1杯の水|"
        r"\d+\s*mLの水|"
        r"多めの水|十分な水|"
        r"牛乳と|ジュースに|"
        r"オレンジジュース|水以外",
        text,
        flags=re.IGNORECASE,
    ):
        codes.add("SPECIFIC_FLUID")
        append_unique(
            evidence,
            "特定の液体・飲料",
        )

    # 指示どおり
    as_directed_present = bool(
        re.search(
            r"医師の指示|指示通り|"
            r"指示どおり|指示に従",
            text,
        )
    )

    substantive_as_directed = bool(
        re.search(
            r"INRに応じ|"
            r"検査値に応じ|"
            r"血糖値に応じ|"
            r"体重増加時|"
            r"一定条件で.*休薬|"
            r"一定条件で.*追加|"
            r"発作時.*(?:追加|繰り返)",
            text,
            flags=re.IGNORECASE,
        )
    )

    compact_text = re.sub(
        r"[\s:;,]",
        "",
        text,
    )

    only_as_directed = bool(
        re.fullmatch(
            r"(?:医師の)?指示"
            r"(?:通り|どおり|に従う)?",
            compact_text,
        )
    )

    detailed_dose_present = bool(
        re.search(
            r"1日\s*[1-9]\s*回|"
            r"\d+\s*時間|"
            r"\d+(?:\.\d+)?\s*"
            r"(錠|包|カプセル|"
            r"mL|mg|μg|噴霧|吸入)",
            text,
        )
    )

    if (
        substantive_as_directed
        or only_as_directed
    ):
        codes.add("AS_DIRECTED")
        append_unique(
            evidence,
            "実質的な指示どおり使用",
        )

    elif (
        as_directed_present
        and detailed_dose_present
    ):
        review_notes.append(
            "具体的用法に形式的な"
            "「指示どおり」が付記されているため"
            "AS_DIRECTEDは加点しなかった"
        )

    # 漸減・漸増
    if re.search(
        r"漸減|漸増|テーパ|"
        r"徐々に減|徐々に増|"
        r"\d+\s*日.*減量|"
        r"\d+\s*日.*増量|"
        r"\d+\s*日間.*その後",
        text,
    ):
        codes.add("TAPERING")
        append_unique(
            evidence,
            "漸減・漸増",
        )

        taper_doses = [
            float(value)
            for value in re.findall(
                r"(\d+(?:\.\d+)?)\s*錠",
                text,
            )
        ]

        if any(
            value > 1.0
            for value in taper_doses
        ):
            codes.add("MULTIPLE_UNITS")
            append_unique(
                evidence,
                "漸減・漸増指示中に複数錠あり",
            )

    sorted_codes = sorted(codes)

    score = sum(
        SECTION_C_WEIGHTS[code]
        for code in sorted_codes
    )

    return (
        sorted_codes,
        score,
        "; ".join(evidence),
        review_notes,
    )


# ============================================================
# MRCI calculation
# ============================================================

def calculate_mrci(
    input_csv: str,
    output_prefix: str,
    evaluation_date: Optional[str] = None,
):
    df = read_csv_auto_encoding(
        input_csv
    )
    df = normalize_headers(df)

    # Required columns
    patient_col = find_column(
        df,
        ["患者ID", "患者ＩＤ", "患者Id"],
        required=True,
    )

    drug_col = find_column(
        df,
        ["薬剤名", "医薬品名", "薬品名"],
        required=True,
    )

    sig_col = find_column(
        df,
        ["用法", "用法名称", "服用方法"],
        required=True,
    )

    # Optional columns
    daily_amount_col = find_column(
        df,
        ["1日量", "一日量", "投与量"],
    )

    dose_unit_col = find_column(
        df,
        ["単位", "1日量単位", "投与量単位"],
    )

    ingredient_col = find_column(
        df,
        [
            "成分コード",
            "一般名コード",
            "一般名",
            "成分名",
            "薬剤キー",
        ],
    )

    ward_col = find_column(
        df,
        ["病棟"],
    )

    team_col = find_column(
        df,
        ["チーム"],
    )

    start_col = find_column(
        df,
        ["服用開始日", "使用開始日", "開始日"],
    )

    end_col = find_column(
        df,
        ["使用終了日", "服用終了日", "終了日"],
    )

    order_date_col = find_column(
        df,
        ["オーダ指示日", "オーダー指示日", "処方日"],
    )

    days_col = find_column(
        df,
        ["処方日数", "投与日数"],
    )

    original_row_count = len(df)

    # Date filtering
    df = filter_by_evaluation_date(
        df=df,
        evaluation_date=evaluation_date,
        start_col=start_col,
        end_col=end_col,
    )

    rows_after_date_filter = len(df)

    # Normalize core columns
    df["_patient_id"] = (
        df[patient_col].map(normalize_text)
    )

    df["_drug_name"] = (
        df[drug_col].map(normalize_text)
    )

    df["_sig"] = (
        df[sig_col].map(normalize_text)
    )

    df = df.loc[
        (df["_patient_id"] != "")
        & (df["_drug_name"] != "")
    ].copy()

    # Exclusion
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

    excluded = df.loc[
        df["_excluded"] == 1
    ].copy()

    df = df.loc[
        df["_excluded"] == 0
    ].copy()

    # Exact duplicate removal
    dedup_columns = [
        "_patient_id",
        "_drug_name",
        "_sig",
    ]

    for optional_col in [
        daily_amount_col,
        dose_unit_col,
        start_col,
        end_col,
        order_date_col,
    ]:
        if (
            optional_col is not None
            and optional_col not in dedup_columns
        ):
            dedup_columns.append(
                optional_col
            )

    before_dedup = len(df)

    df = df.drop_duplicates(
        subset=dedup_columns,
        keep="first",
    ).copy()

    duplicate_rows_removed = (
        before_dedup - len(df)
    )

    # Medication key
    authoritative_medication_key = (
        ingredient_col is not None
    )

    if ingredient_col is not None:
        df["_medication_key"] = df.apply(
            lambda row: (
                normalize_text(
                    row.get(ingredient_col, "")
                )
                or canonical_drug_name_hint(
                    row.get(drug_col, "")
                )
            ),
            axis=1,
        )
    else:
        df["_medication_key"] = (
            df[drug_col].map(
                canonical_drug_name_hint
            )
        )

    duplicate_candidate_mask = (
        df.duplicated(
            subset=[
                "_patient_id",
                "_medication_key",
            ],
            keep=False,
        )
    )

    df["_duplicate_candidate"] = (
        duplicate_candidate_mask.astype(int)
    )

    # Row-level analysis
    detail_rows = []

    for source_index, row in df.iterrows():
        patient_id = normalize_text(
            row.get(patient_col, "")
        )

        drug_name = normalize_text(
            row.get(drug_col, "")
        )

        sig = normalize_text(
            row.get(sig_col, "")
        )

        daily_amount_raw = (
            row.get(daily_amount_col, "")
            if daily_amount_col is not None
            else ""
        )

        amount_unit_raw = (
            row.get(dose_unit_col, "")
            if dose_unit_col is not None
            else ""
        )

        daily_amount = parse_float(
            daily_amount_raw
        )

        amount_unit = normalize_unit(
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
            amount_unit=amount_unit,
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
            direction_score,
            direction_evidence,
            direction_review_notes,
        ) = classify_additional_directions(
            sig=sig,
            drug_name=drug_name,
            form_category=form_code,
            daily_amount=daily_amount,
            amount_unit=amount_unit,
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

        if is_numeric_only_unit(
            amount_unit
        ):
            append_unique(
                review_reasons,
                f"数値のみの単位={amount_unit}",
            )

        duplicate_candidate = int(
            row.get(
                "_duplicate_candidate",
                0,
            )
        )

        if (
            duplicate_candidate == 1
            and not authoritative_medication_key
        ):
            append_unique(
                review_reasons,
                "同一成分・異規格薬の可能性。"
                "成分コードがないため"
                "自動統合していない",
            )

        for note in direction_review_notes:
            append_unique(
                review_reasons,
                note,
            )

        detail_rows.append({
            "source_index": source_index,
            "患者ID": patient_id,

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

            "薬剤名": drug_name,
            "medication_key": normalize_text(
                row.get(
                    "_medication_key",
                    "",
                )
            ),

            "medication_key_authoritative": int(
                authoritative_medication_key
            ),

            "duplicate_candidate": (
                duplicate_candidate
            ),

            "1日量_original": normalize_text(
                daily_amount_raw
            ),

            "1日量_numeric": (
                daily_amount
                if daily_amount is not None
                else ""
            ),

            "単位_original": normalize_text(
                amount_unit_raw
            ),

            "単位_normalized": amount_unit,
            "用法": sig,

            "服用開始日": (
                normalize_text(
                    row.get(start_col, "")
                )
                if start_col is not None
                else ""
            ),

            "使用終了日": (
                normalize_text(
                    row.get(end_col, "")
                )
                if end_col is not None
                else ""
            ),

            "オーダ指示日": (
                normalize_text(
                    row.get(order_date_col, "")
                )
                if order_date_col is not None
                else ""
            ),

            "処方日数": (
                normalize_text(
                    row.get(days_col, "")
                )
                if days_col is not None
                else ""
            ),

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

            "dose_sequence": "|".join(
                str(value)
                for value in extract_dose_sequence(
                    sig
                )
            ),

            "C_direction_codes": "|".join(
                direction_codes
            ),

            "C_original_weight": (
                direction_score
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

    if not detail_rows:
        raise ValueError(
            "評価対象となる薬剤行がありません"
        )

    detail = pd.DataFrame(
        detail_rows
    )

    # Numeric scoring columns
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
    detail["BC_same_medication"] = 0

    # Section A:
    # 同じ剤形は患者内で1回だけ加点
    for _, patient_group in detail.groupby(
        "患者ID",
        sort=False,
    ):
        counted_forms = set()

        for row_index in patient_group.index:
            form_code = detail.at[
                row_index,
                "A_form_code",
            ]

            if not form_code:
                continue

            if form_code in counted_forms:
                detail.at[
                    row_index,
                    "A_duplicate_form",
                ] = 1
            else:
                detail.at[
                    row_index,
                    "A_counted_weight",
                ] = detail.at[
                    row_index,
                    "_A_numeric",
                ]

                counted_forms.add(
                    form_code
                )

    # Section B/C:
    # 成分キーがある場合は同一成分・同一剤形を
    # 原則として1薬剤として集計する。
    if authoritative_medication_key:
        detail["_aggregation_key"] = (
            detail["medication_key"]
            + "||"
            + detail["A_form_code"]
        )
    else:
        detail["_aggregation_key"] = (
            detail.index.astype(str)
        )

    for _, medication_group in detail.groupby(
        [
            "患者ID",
            "_aggregation_key",
        ],
        sort=False,
        dropna=False,
    ):
        indices = list(
            medication_group.index
        )

        if len(indices) > 1:
            detail.loc[
                indices,
                "BC_same_medication",
            ] = 1

        # Section B:
        # 同一薬剤に複数頻度があれば、
        # 暫定的に低い複雑性を採用しレビュー対象
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
            for index in indices:
                old_reason = normalize_text(
                    detail.at[
                        index,
                        "review_reason",
                    ]
                )

                new_reason = (
                    "同一成分に異なる頻度があるため、"
                    "Bは低い複雑性を暫定採用"
                )

                detail.at[
                    index,
                    "review_required",
                ] = 1

                detail.at[
                    index,
                    "review_reason",
                ] = (
                    f"{old_reason}; {new_reason}"
                    if old_reason
                    else new_reason
                )

        # Section C:
        # 同一薬剤内で同じカテゴリーは1回だけ
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

    # Patient summary
    patient_summary = (
        detail.groupby(
            "患者ID",
            as_index=False,
        )
        .agg(
            病棟=(
                "病棟",
                first_nonempty,
            ),

            チーム=(
                "チーム",
                first_nonempty,
            ),

            medication_rows=(
                "薬剤名",
                "size",
            ),

            medication_names=(
                "薬剤名",
                "nunique",
            ),

            medication_keys=(
                "medication_key",
                "nunique",
            ),

            section_A=(
                "A_counted_weight",
                "sum",
            ),

            section_B=(
                "B_counted_weight",
                "sum",
            ),

            section_C=(
                "C_counted_weight",
                "sum",
            ),

            unmapped_A=(
                "A_form_code",
                lambda values: int(
                    (values == "").sum()
                ),
            ),

            unmapped_B=(
                "B_frequency_code",
                lambda values: int(
                    (values == "").sum()
                ),
            ),

            review_required=(
                "review_required",
                "max",
            ),

            review_count=(
                "review_required",
                "sum",
            ),
        )
    )

    patient_summary["MRCI_auto"] = (
        patient_summary["section_A"]
        + patient_summary["section_B"]
        + patient_summary["section_C"]
    )

    patient_summary["AB_mapping_complete"] = (
        (
            patient_summary["unmapped_A"]
            == 0
        )
        & (
            patient_summary["unmapped_B"]
            == 0
        )
    ).astype(int)

    patient_summary["auto_complete"] = (
        (
            patient_summary[
                "AB_mapping_complete"
            ]
            == 1
        )
        & (
            patient_summary[
                "review_required"
            ]
            == 0
        )
    ).astype(int)

    patient_summary["score_status"] = (
        patient_summary[
            "auto_complete"
        ].map({
            1: (
                "auto_complete_"
                "no_review_flag"
            ),
            0: (
                "lower_bound_or_"
                "manual_review_required"
            ),
        })
    )

    patient_summary[
        "MRCI_reportable_auto"
    ] = patient_summary[
        "MRCI_auto"
    ].where(
        patient_summary[
            "auto_complete"
        ]
        == 1
    )

    patient_summary[
        "evaluation_date"
    ] = evaluation_date or ""

    # Review list
    review_columns = [
        "患者ID",
        "病棟",
        "チーム",
        "薬剤名",
        "medication_key",
        "duplicate_candidate",
        "1日量_original",
        "単位_original",
        "単位_normalized",
        "用法",
        "A_form_code",
        "A_confidence",
        "A_note",
        "B_frequency_code",
        "B_confidence",
        "B_note",
        "C_direction_codes",
        "C_evidence",
        "review_reason",
    ]

    review = detail.loc[
        detail["review_required"] == 1,
        review_columns,
    ].copy()

    # Output
    prefix = Path(
        output_prefix
    )

    prefix.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    patient_file = (
        prefix.parent
        / f"{prefix.name}_patient.csv"
    )

    detail_file = (
        prefix.parent
        / f"{prefix.name}_detail.csv"
    )

    review_file = (
        prefix.parent
        / f"{prefix.name}_review.csv"
    )

    excluded_file = (
        prefix.parent
        / f"{prefix.name}_excluded.csv"
    )

    log_file = (
        prefix.parent
        / f"{prefix.name}_log.txt"
    )

    patient_summary.to_csv(
        patient_file,
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

    excluded.to_csv(
        excluded_file,
        index=False,
        encoding="utf-8-sig",
    )

    log_lines = [
        f"input_file={input_csv}",
        (
            "evaluation_date="
            f"{evaluation_date or 'not specified'}"
        ),
        f"original_rows={original_row_count}",
        (
            "rows_after_date_filter="
            f"{rows_after_date_filter}"
        ),
        f"excluded_rows={len(excluded)}",
        (
            "exact_duplicate_rows_removed="
            f"{duplicate_rows_removed}"
        ),
        f"analyzed_rows={len(detail)}",
        f"patients={len(patient_summary)}",
        f"review_rows={len(review)}",
        (
            "ingredient_column="
            f"{ingredient_col or 'not available'}"
        ),
        "",
        "注意:",
        "MRCI_autoはルールベースの自動算出値です。",
        (
            "review_required=1の患者・薬剤は"
            "手作業確認が必要です。"
        ),
        (
            "auto_complete=0のMRCI_autoは"
            "下限値または暫定値です。"
        ),
        (
            "研究利用前に手作業採点との"
            "妥当性検証が必要です。"
        ),
    ]

    log_file.write_text(
        "\n".join(log_lines),
        encoding="utf-8",
    )

    print(
        "MRCI-auto calculation completed."
    )
    print(
        f"患者単位: {patient_file}"
    )
    print(
        f"薬剤単位: {detail_file}"
    )
    print(
        f"要確認:   {review_file}"
    )
    print(
        f"除外薬:   {excluded_file}"
    )
    print(
        f"ログ:     {log_file}"
    )

    return (
        patient_summary,
        detail,
        review,
    )


# ============================================================
# Self tests
# ============================================================

def run_self_tests():
    frequency_cases = {
        "12時間おき": (
            "Q12H",
            2.5,
        ),

        "8時間おき": (
            "Q8H",
            3.5,
        ),

        "疼痛時 4～6時間ごと": (
            "Q6H_PRN",
            2.5,
        ),

        "1日5回": (
            "Q6H",
            4.5,
        ),

        "72時間ごとに貼り替え": (
            "LESS_THAN_DAILY",
            2.0,
        ),

        "毎食後": (
            "THREE_TIMES_DAILY",
            3.0,
        ),

        "隔日 1日1回朝食後": (
            "LESS_THAN_DAILY",
            2.0,
        ),

        "朝": (
            "ONCE_DAILY",
            1.0,
        ),

        "朝・夕": (
            "TWICE_DAILY",
            2.0,
        ),

        "頓服": (
            "PRN",
            0.5,
        ),
    }

    for sig, expected in (
        frequency_cases.items()
    ):
        (
            code,
            weight,
            _,
            _,
            note,
        ) = classify_frequency(sig)

        assert code == expected[0], (
            f"Frequency code error: "
            f"sig={sig}, actual={code}, "
            f"expected={expected[0]}, "
            f"note={note}"
        )

        assert weight == expected[1], (
            f"Frequency weight error: "
            f"sig={sig}, actual={weight}, "
            f"expected={expected[1]}"
        )

    assert extract_dose_sequence(
        "1日2回:朝・夕食後(1-0.5)"
    ) == [1.0, 0.5]

    # 0.5錠だけではBREAK_CRUSHにしない
    codes, score, _, _ = (
        classify_additional_directions(
            sig=(
                "1日2回:"
                "朝・夕食後(1-0.5)"
            ),
            drug_name="テスト錠10mg",
            form_category=(
                "ORAL_TABLET_CAPSULE"
            ),
            daily_amount=1.5,
            amount_unit="錠",
            daily_frequency=2,
        )
    )

    assert "BREAK_CRUSH" not in codes
    assert "ALTERNATING_DOSE" in codes
    assert "SPECIFIC_TIME" in codes
    assert "RELATION_TO_FOOD" in codes
    assert score == 4.0

    # 分割行為が明示されれば加点
    codes, _, _, _ = (
        classify_additional_directions(
            sig=(
                "錠剤を半分に割って"
                "1日1回服用"
            ),
            drug_name="テスト錠10mg",
            form_category=(
                "ORAL_TABLET_CAPSULE"
            ),
            daily_amount=None,
            amount_unit="錠",
            daily_frequency=1,
        )
    )

    assert "BREAK_CRUSH" in codes

    # インスリン10単位は複数単位ではない
    codes, _, _, _ = (
        classify_additional_directions(
            sig=(
                "朝食前 "
                "インスリン10単位皮下注"
            ),
            drug_name="インスリン注",
            form_category=(
                "INJECTION_PREFILLED"
            ),
            daily_amount=10,
            amount_unit="単位",
            daily_frequency=1,
        )
    )

    assert "MULTIPLE_UNITS" not in codes
    assert "SPECIFIC_TIME" in codes
    assert "RELATION_TO_FOOD" in codes

    # 両眼1滴は複数単位ではない
    codes, _, _, _ = (
        classify_additional_directions(
            sig="1日2回 両眼に1滴",
            drug_name="テスト点眼液",
            form_category="EYE_DROP",
            daily_amount=None,
            amount_unit="滴",
            daily_frequency=2,
        )
    )

    assert "MULTIPLE_UNITS" not in codes

    # Section Aは32項目
    assert len(
        SECTION_A_WEIGHTS
    ) == 32

    # ディスカスとタービュヘイラーは
    # 別カテゴリー
    diskus = classify_dosage_form(
        "テストディスカス吸入剤"
    )

    turbuhaler = classify_dosage_form(
        "テストタービュヘイラー吸入剤"
    )

    assert (
        diskus[0]
        == "INHALER_ACCUHALER_DISKUS"
    )

    assert (
        turbuhaler[0]
        == "INHALER_TURBUHALER"
    )

    print(
        "All self-tests passed."
    )


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description=(
            "院内薬歴CSVから"
            "MRCI-autoを算出する"
        )
    )

    parser.add_argument(
        "input_csv",
        nargs="?",
        help="入力CSVファイル",
    )

    parser.add_argument(
        "--output-prefix",
        default="mrci_result",
        help=(
            "出力ファイルの接頭辞。"
            "既定値: mrci_result"
        ),
    )

    parser.add_argument(
        "--date",
        default=None,
        help=(
            "評価日。YYYY-MM-DD。"
            "省略時はCSV内の全行を対象"
        ),
    )

    parser.add_argument(
        "--self-test",
        action="store_true",
        help="内蔵テストを実行する",
    )

    args = parser.parse_args()

    if args.self_test:
        run_self_tests()
        return

    if not args.input_csv:
        parser.error(
            "input_csvを指定するか、"
            "--self-testを指定してください"
        )

    calculate_mrci(
        input_csv=args.input_csv,
        output_prefix=args.output_prefix,
        evaluation_date=args.date,
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
