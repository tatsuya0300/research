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
# General utilities
# ============================================================

def first_nonempty(
    values: pd.Series,
) -> str:
    for value in values:
        text = normalize_text(value)

        if text:
            return text

    return ""


def normalize_text(value) -> str:
    """
    全角・半角、記号、空白を正規化する。
    """
    if value is None:
        return ""

    try:
        if pd.isna(value):
            return ""
    except (TypeError, ValueError):
        pass

    text = unicodedata.normalize("NFKC", str(value))

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


def normalize_drug_name(value) -> str:
    """
    薬剤名照合用の正規化。

    ・Unicode NFKC正規化
    ・英字を小文字化
    ・空白、括弧、句読点などを除去

    強度を示す数字は残すが、成分名照合では前方一致を使用する。
    """
    text = normalize_text(value).lower()

    # 一般名処方に付く接頭辞
    text = re.sub(
        r"^(?:【?般】?|一般名処方[:：]?)",
        "",
        text,
    )

    text = re.sub(
        r"""[
            \s
            "'“”‘’
            「」『』
            【】

            \[\]［］
            \(\)（）
            ・
            /／\\
            ,，、。.
            :：;
        ]""",
        "",
        text,
        flags=re.VERBOSE,
    )

    return text.strip()


def read_csv_auto_encoding(path: str | Path) -> pd.DataFrame:
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
            errors.append(f"{encoding}: {exc}")

    raise RuntimeError(
        "CSVの文字コードを判定できませんでした。\n"
        + "\n".join(errors)
    )


def normalize_headers(df: pd.DataFrame) -> pd.DataFrame:
    """
    CSVまたはExcelの列名を正規化する。
    """
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
    """
    候補名から実際の列名を検索する。
    """
    for candidate in candidates:
        if candidate in df.columns:
            return candidate

    if required:
        raise ValueError(
            f"必要な列が見つかりません。候補={candidates}\n"
            f"実際の列={list(df.columns)}"
        )

    return None


def append_unique(items: list[str], value: str):
    if value and value not in items:
        items.append(value)


def file_sha256(path: str | Path) -> str:
    """
    使用したJARSマスターのSHA-256を計算する。
    """
    sha256 = hashlib.sha256()

    with Path(path).open("rb") as file:
        for chunk in iter(
            lambda: file.read(1024 * 1024),
            b"",
        ):
            sha256.update(chunk)

    return sha256.hexdigest()


# ============================================================
# Date handling
# ============================================================

def parse_date_series(series: pd.Series) -> pd.Series:
    """
    日付列をdatetimeへ変換する。
    """
    cleaned = series.map(normalize_text)

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

    服用開始日 <= 評価日 <= 使用終了日

    開始日・終了日の空欄は制限なしとして扱う。
    """
    if evaluation_date is None:
        return df.copy()

    eval_date = pd.to_datetime(
        evaluation_date,
        errors="raise",
    )

    if start_col is not None:
        start_dates = parse_date_series(df[start_col])
    else:
        start_dates = pd.Series(
            pd.NaT,
            index=df.index,
            dtype="datetime64[ns]",
        )

    if end_col is not None:
        end_dates = parse_date_series(df[end_col])
    else:
        end_dates = pd.Series(
            pd.NaT,
            index=df.index,
            dtype="datetime64[ns]",
        )

    active_mask = (
        (start_dates.isna() | (start_dates <= eval_date))
        & (end_dates.isna() | (end_dates >= eval_date))
    )

    return df.loc[active_mask].copy()


# ============================================================
# JARS master handling
# ============================================================

def find_jars_sheet(
    excel_path: str | Path,
) -> tuple[str, int]:
    """
    「薬物」列と「スコア」列を含むシートと
    ヘッダー行を探索する。

    提示されたExcelのシート名：
        158医薬品（薬効） 
    の末尾空白にも対応する。
    """
    excel = pd.ExcelFile(
        excel_path,
        engine="openpyxl",
    )

    for sheet_name in excel.sheet_names:
        preview = pd.read_excel(
            excel_path,
            sheet_name=sheet_name,
            header=None,
            dtype=str,
            engine="openpyxl",
        )

        max_rows = min(30, len(preview))

        for row_number in range(max_rows):
            values = {
                normalize_text(value)
                for value in preview.iloc[
                    row_number
                ].dropna()
            }

            if "薬物" in values and "スコア" in values:
                return sheet_name, row_number

    raise RuntimeError(
        "「薬物」列と「スコア」列を持つシートを"
        "検出できませんでした。"
    )


def load_jars_master(
    excel_path: str | Path,
) -> pd.DataFrame:
    """
    公式JARS Excelから158成分を読み込む。
    """
    sheet_name, header_row = find_jars_sheet(
        excel_path
    )

    master = pd.read_excel(
        excel_path,
        sheet_name=sheet_name,
        header=header_row,
        engine="openpyxl",
    )

    master = normalize_headers(master)

    required_columns = {
        "薬物",
        "スコア",
    }

    missing_columns = (
        required_columns - set(master.columns)
    )

    if missing_columns:
        raise RuntimeError(
            "JARSマスターに必要な列がありません："
            f"{sorted(missing_columns)}"
        )

    rename_map = {
        "薬物": "jars_drug",
        "スコア": "jars_score",
    }

    optional_columns = {
        "薬効群": "therapeutic_class",
        "薬効群中分類": "therapeutic_subclass",
        "薬効分類": "therapeutic_code",
        "Medication": "medication_en",
        "ATCコード": "atc_code",
    }

    for original, renamed in optional_columns.items():
        if original in master.columns:
            rename_map[original] = renamed

    master = master.rename(
        columns=rename_map
    )

    master["jars_drug"] = (
        master["jars_drug"]
        .fillna("")
        .astype(str)
        .str.strip()
    )

    master["jars_score"] = pd.to_numeric(
        master["jars_score"],
        errors="coerce",
    )

    master = master.loc[
        (master["jars_drug"] != "")
        & master["jars_score"].isin([1, 2, 3])
    ].copy()

    master["jars_score"] = (
        master["jars_score"].astype(int)
    )

    master["normalized_jars_drug"] = (
        master["jars_drug"].map(
            normalize_drug_name
        )
    )

    # 同一薬物に異なるスコアがないか検証
    conflicts = (
        master.groupby(
            "normalized_jars_drug"
        )["jars_score"]
        .nunique()
        .loc[lambda values: values > 1]
    )

    if not conflicts.empty:
        raise RuntimeError(
            "同一薬物名に異なるスコアがあります："
            f"{conflicts.index.tolist()}"
        )

    master = master.drop_duplicates(
        subset=["normalized_jars_drug"],
        keep="first",
    ).copy()

    if len(master) != 158:
        print(
            "WARNING: JARS薬物数が158ではありません。"
            f"抽出数={len(master)}",
            file=sys.stderr,
        )

    print(
        f"JARS sheet={sheet_name!r}, "
        f"header_row={header_row + 1}, "
        f"drugs={len(master)}"
    )

    return master.reset_index(drop=True)


# ============================================================
# Optional product-name/alias master
# ============================================================

def load_name_map(
    name_map_path: Optional[str | Path],
    jars_master: pd.DataFrame,
) -> pd.DataFrame:
    """
    任意の商品名・別名―JARS薬物名対応表を読み込む。

    CSV列：
        alias,jars_drug

    例：
        アーテン,トリヘキシフェニジル
        ポララミン,クロルフェニラミン

    配合剤で複数成分を指定する場合：
        商品名,成分1|成分2
    """
    columns = [
        "alias",
        "jars_drug",
        "normalized_alias",
    ]

    if name_map_path is None:
        return pd.DataFrame(columns=columns)

    name_map = read_csv_auto_encoding(
        name_map_path
    )

    name_map = normalize_headers(name_map)

    alias_col = find_column(
        name_map,
        [
            "alias",
            "商品名",
            "別名",
            "薬剤名",
            "医薬品名",
        ],
        required=True,
    )

    canonical_col = find_column(
        name_map,
        [
            "jars_drug",
            "薬物",
            "成分名",
            "一般名",
        ],
        required=True,
    )

    name_map = name_map.rename(
        columns={
            alias_col: "alias",
            canonical_col: "jars_drug",
        }
    )

    name_map["alias"] = (
        name_map["alias"]
        .fillna("")
        .astype(str)
        .str.strip()
    )

    name_map["jars_drug"] = (
        name_map["jars_drug"]
        .fillna("")
        .astype(str)
        .str.strip()
    )

    name_map = name_map.loc[
        (name_map["alias"] != "")
        & (name_map["jars_drug"] != "")
    ].copy()

    name_map["normalized_alias"] = (
        name_map["alias"].map(
            normalize_drug_name
        )
    )

    valid_drugs = {
        value: drug_name
        for value, drug_name in zip(
            jars_master["normalized_jars_drug"],
            jars_master["jars_drug"],
        )
    }

    invalid_canonical_names = []

    for canonical_text in name_map[
        "jars_drug"
    ]:
        for canonical_name in canonical_text.split("|"):
            normalized = normalize_drug_name(
                canonical_name
            )

            if normalized not in valid_drugs:
                invalid_canonical_names.append(
                    canonical_name
                )

    if invalid_canonical_names:
        raise ValueError(
            "商品名対応表の薬物名がJARSマスターに"
            "存在しません："
            f"{sorted(set(invalid_canonical_names))}"
        )

    # 同じaliasが異なる成分へ割り当てられていないか
    alias_conflicts = (
        name_map.groupby(
            "normalized_alias"
        )["jars_drug"]
        .nunique()
        .loc[lambda values: values > 1]
    )

    if not alias_conflicts.empty:
        raise ValueError(
            "同一の商品名・別名に複数の対応があります："
            f"{alias_conflicts.index.tolist()}"
        )

    return name_map[
        columns
    ].drop_duplicates().reset_index(drop=True)


# ============================================================
# Route and dosage-form scope
# ============================================================

OUT_OF_SCOPE_PATTERNS = [
    (
        "eye",
        r"点眼|眼軟膏|眼科用|眼用液|点眼ゲル",
    ),
    (
        "ear",
        r"点耳|耳科用|耳用液",
    ),
    (
        "nasal",
        r"点鼻|鼻噴霧|鼻用液|鼻用スプレー",
    ),
    (
        "inhalation",
        r"吸入|ネブライザ|ネブライザー|"
        r"エアゾール|ディスカス|エリプタ|"
        r"タービュヘイラー|レスピマット",
    ),
    (
        "injection",
        r"注射|静注|筋注|皮下注|点滴|"
        r"静脈注|シリンジ|注入",
    ),
    (
        "local_topical",
        r"軟膏|クリーム|ローション|"
        r"外用液|ゲル|リニメント|含嗽|"
        r"うがい|洗口",
    ),
    (
        "rectal_vaginal",
        r"坐剤|坐薬|浣腸|腟錠|膣錠|"
        r"腟クリーム|膣クリーム",
    ),
]


IN_SCOPE_ORAL_PATTERNS = [
    r"錠",
    r"カプセル",
    r"細粒",
    r"顆粒",
    r"散剤",
    r"ドライシロップ",
    r"シロップ",
    r"内用液",
    r"経口液",
    r"エリキシル",
    r"トローチ",
    r"内服",
    r"経口",
]


SYSTEMIC_TRANSDERMAL_PATTERNS = [
    r"経皮吸収",
    r"全身用.*テープ",
    r"全身用.*パッチ",
]


def classify_jars_scope(
    drug_name: str,
) -> tuple[str, str, str]:
    """
    JARSの剤形・投与経路範囲を判定する。

    Returns
    -------
    scope_status:
        in_scope
        out_of_scope
        unknown

    scope_type:
        oral
        systemic_transdermal
        eye 等

    note:
        判定理由
    """
    text = normalize_text(drug_name)

    # 全身作用目的の経皮薬を先に判定
    for pattern in SYSTEMIC_TRANSDERMAL_PATTERNS:
        if re.search(pattern, text):
            return (
                "in_scope",
                "systemic_transdermal",
                "全身作用目的の経皮薬と判定",
            )

    # 明確な非経口・局所薬
    for scope_type, pattern in OUT_OF_SCOPE_PATTERNS:
        if re.search(pattern, text):
            return (
                "out_of_scope",
                scope_type,
                f"対象外剤形・投与経路を検出: {scope_type}",
            )

    # 明確な内服薬
    for pattern in IN_SCOPE_ORAL_PATTERNS:
        if re.search(pattern, text):
            return (
                "in_scope",
                "oral",
                "内服薬と判定",
            )

    # 「テープ」「パッチ」だけでは全身作用か局所作用か
    # 判断できない
    if re.search(
        r"テープ|パッチ|貼付",
        text,
    ):
        return (
            "unknown",
            "transdermal_unknown",
            "経皮薬だが全身作用目的か確認が必要",
        )

    return (
        "unknown",
        "route_unknown",
        "薬剤名から投与経路を判定できない",
    )


# ============================================================
# Ingredient matching
# ============================================================

ALLOWED_AFTER_INGREDIENT_PATTERN = re.compile(
    r"^(?:"
    r"塩酸塩|臭化物|酒石酸塩|クエン酸塩|"
    r"マレイン酸塩|フマル酸塩|メシル酸塩|"
    r"コハク酸塩|硫酸塩|リン酸塩|硝酸塩|"
    r"酢酸塩|乳酸塩|ベシル酸塩|水和物|"
    r"ナトリウム|カリウム|カルシウム|"
    r"塩"
    r")*"
    r"(?:"
    r"錠|od錠|口腔内崩壊錠|カプセル|"
    r"細粒|顆粒|散|散剤|ドライシロップ|"
    r"シロップ|内用液|経口液|"
    r"徐放錠|徐放カプセル|腸溶錠|"
    r"配合錠|配合カプセル"
    r")?"
    r".*$",
    flags=re.IGNORECASE,
)


def is_safe_ingredient_match(
    normalized_input: str,
    normalized_ingredient: str,
) -> bool:
    """
    成分名の安全な前方一致を判定する。

    例：
        トリヘキシフェニジル塩酸塩錠
        → トリヘキシフェニジル

    一方、
        ジヒドロコデイン
        → コデイン
    のような文字列内部の誤一致は許可しない。
    """
    if not normalized_input:
        return False

    if normalized_input == normalized_ingredient:
        return True

    if not normalized_input.startswith(
        normalized_ingredient
    ):
        return False

    remainder = normalized_input[
        len(normalized_ingredient):
    ]

    if not remainder:
        return True

    return bool(
        ALLOWED_AFTER_INGREDIENT_PATTERN.fullmatch(
            remainder
        )
    )


def remove_nested_candidates(
    candidates: pd.DataFrame,
) -> pd.DataFrame:
    """
    長い薬物名に内包される短い薬物名を除外する。

    例：
        レボセチリジン → セチリジンを除外
        デスロラタジン → ロラタジンを除外
        ブチルスコポラミン → スコポラミンを除外
        ヒドロコルチゾン → コルチゾンを除外
    """
    if candidates.empty:
        return candidates

    candidates = candidates.copy()

    candidates["_name_length"] = (
        candidates["normalized_jars_drug"]
        .str.len()
    )

    candidates = candidates.sort_values(
        "_name_length",
        ascending=False,
    )

    selected_indices = []
    selected_names = []

    for index, row in candidates.iterrows():
        current_name = row[
            "normalized_jars_drug"
        ]

        nested = any(
            current_name in selected_name
            for selected_name in selected_names
        )

        if not nested:
            selected_indices.append(index)
            selected_names.append(current_name)

    return candidates.loc[
        selected_indices
    ].drop(
        columns=["_name_length"]
    )


def match_by_name_map(
    drug_name: str,
    name_map: pd.DataFrame,
    jars_master: pd.DataFrame,
) -> pd.DataFrame:
    """
    商品名・別名対応表で照合する。

    完全一致を優先し、次に最長の包含一致を使用する。
    """
    if name_map.empty:
        return pd.DataFrame()

    normalized_input = normalize_drug_name(
        drug_name
    )

    exact = name_map.loc[
        name_map["normalized_alias"]
        == normalized_input
    ].copy()

    if not exact.empty:
        matched_aliases = exact
        match_method = "exact_alias"
    else:
        contained = name_map.loc[
            name_map["normalized_alias"].map(
                lambda alias:
                    len(alias) >= 3
                    and normalized_input.startswith(alias)
            )
        ].copy()

        if contained.empty:
            return pd.DataFrame()

        contained["_alias_length"] = (
            contained["normalized_alias"].str.len()
        )

        max_length = contained[
            "_alias_length"
        ].max()

        matched_aliases = contained.loc[
            contained["_alias_length"]
            == max_length
        ].copy()

        match_method = "prefix_alias"

    distinct_mappings = (
        matched_aliases["jars_drug"]
        .drop_duplicates()
        .tolist()
    )

    if len(distinct_mappings) > 1:
        return pd.DataFrame(
            [{
                "match_status": "ambiguous",
                "match_method": match_method,
                "matched_alias": "|".join(
                    matched_aliases["alias"].tolist()
                ),
                "jars_drug": "",
                "jars_score": pd.NA,
                "therapeutic_class": "",
            }]
        )

    records = []

    for _, alias_row in matched_aliases.iterrows():
        canonical_drugs = alias_row[
            "jars_drug"
        ].split("|")

        for canonical_drug in canonical_drugs:
            normalized_canonical = (
                normalize_drug_name(
                    canonical_drug
                )
            )

            master_matches = jars_master.loc[
                jars_master[
                    "normalized_jars_drug"
                ] == normalized_canonical
            ]

            for _, master_row in master_matches.iterrows():
                records.append({
                    "match_status": "matched",
                    "match_method": match_method,
                    "matched_alias": alias_row["alias"],
                    "jars_drug": master_row[
                        "jars_drug"
                    ],
                    "jars_score": int(
                        master_row["jars_score"]
                    ),
                    "therapeutic_class": (
                        master_row.get(
                            "therapeutic_class",
                            "",
                        )
                    ),
                })

    return pd.DataFrame(records)


def match_by_ingredient(
    drug_name: str,
    jars_master: pd.DataFrame,
) -> pd.DataFrame:
    """
    JARS薬物名と薬剤名を照合する。

    商品名ではなく一般名・成分名を対象とする。
    """
    normalized_input = normalize_drug_name(
        drug_name
    )

    mask = jars_master.apply(
        lambda row: is_safe_ingredient_match(
            normalized_input,
            row["normalized_jars_drug"],
        ),
        axis=1,
    )

    candidates = jars_master.loc[
        mask
    ].copy()

    if candidates.empty:
        return pd.DataFrame()

    candidates = remove_nested_candidates(
        candidates
    )

    # 前方一致では通常1成分になる。
    # 複数残った場合は、安全のため曖昧扱いにする。
    if len(candidates) > 1:
        return pd.DataFrame(
            [{
                "match_status": "ambiguous",
                "match_method": "ingredient_prefix",
                "matched_alias": "",
                "jars_drug": "|".join(
                    candidates["jars_drug"].tolist()
                ),
                "jars_score": pd.NA,
                "therapeutic_class": "",
            }]
        )

    candidate = candidates.iloc[0]

    exact = (
        normalized_input
        == candidate["normalized_jars_drug"]
    )

    return pd.DataFrame(
        [{
            "match_status": "matched",
            "match_method": (
                "exact_ingredient"
                if exact
                else "ingredient_prefix"
            ),
            "matched_alias": "",
            "jars_drug": candidate[
                "jars_drug"
            ],
            "jars_score": int(
                candidate["jars_score"]
            ),
            "therapeutic_class": (
                candidate.get(
                    "therapeutic_class",
                    "",
                )
            ),
        }]
    )


def match_drug(
    drug_name: str,
    jars_master: pd.DataFrame,
    name_map: pd.DataFrame,
) -> pd.DataFrame:
    """
    1薬剤をJARSマスターへ照合する。

    優先順位：
        1. 商品名・別名対応表
        2. JARS一般名・成分名
    """
    alias_matches = match_by_name_map(
        drug_name=drug_name,
        name_map=name_map,
        jars_master=jars_master,
    )

    if not alias_matches.empty:
        return alias_matches

    return match_by_ingredient(
        drug_name=drug_name,
        jars_master=jars_master,
    )


# ============================================================
# JARS calculation
# ============================================================

def calculate_jars(
    input_csv: str,
    jars_master_path: str,
    output_prefix: str,
    evaluation_date: Optional[str] = None,
    name_map_path: Optional[str] = None,
):
    """
    院内薬歴CSVから患者単位JARSを算出する。
    """
    df = read_csv_auto_encoding(
        input_csv
    )

    df = normalize_headers(df)

    # --------------------------------------------------------
    # CSV列同定
    # --------------------------------------------------------

    patient_col = find_column(
        df,
        [
            "患者ID",
            "患者ＩＤ",
            "患者Id",
            "patient_id",
        ],
        required=True,
    )

    drug_col = find_column(
        df,
        [
            "薬剤名",
            "医薬品名",
            "薬品名",
            "drug_name",
        ],
        required=True,
    )

    ward_col = find_column(
        df,
        ["病棟"],
        required=False,
    )

    team_col = find_column(
        df,
        ["チーム"],
        required=False,
    )

    sig_col = find_column(
        df,
        [
            "用法",
            "用法名称",
            "服用方法",
        ],
        required=False,
    )

    start_col = find_column(
        df,
        [
            "服用開始日",
            "使用開始日",
            "開始日",
        ],
        required=False,
    )

    end_col = find_column(
        df,
        [
            "使用終了日",
            "服用終了日",
            "終了日",
        ],
        required=False,
    )

    order_date_col = find_column(
        df,
        [
            "オーダ指示日",
            "オーダー指示日",
            "処方日",
        ],
        required=False,
    )

    # --------------------------------------------------------
    # JARSマスター
    # --------------------------------------------------------

    jars_master = load_jars_master(
        jars_master_path
    )

    name_map = load_name_map(
        name_map_path=name_map_path,
        jars_master=jars_master,
    )

    master_hash = file_sha256(
        jars_master_path
    )

    # --------------------------------------------------------
    # 評価日フィルタ
    # --------------------------------------------------------

    original_row_count = len(df)

    df = filter_by_evaluation_date(
        df=df,
        evaluation_date=evaluation_date,
        start_col=start_col,
        end_col=end_col,
    )

    rows_after_date_filter = len(df)

    # --------------------------------------------------------
    # 必須情報の正規化
    # --------------------------------------------------------

    df["_patient_id"] = df[
        patient_col
    ].map(normalize_text)

    df["_drug_name"] = df[
        drug_col
    ].map(normalize_text)

    df = df.loc[
        (df["_patient_id"] != "")
        & (df["_drug_name"] != "")
    ].copy()

    # --------------------------------------------------------
    # 完全に同一の薬歴行を除外
    # --------------------------------------------------------

    dedup_columns = [
        "_patient_id",
        "_drug_name",
    ]

    for optional_col in [
        sig_col,
        start_col,
        end_col,
        order_date_col,
    ]:
        if (
            optional_col is not None
            and optional_col not in dedup_columns
        ):
            dedup_columns.append(optional_col)

    before_dedup = len(df)

    df = df.drop_duplicates(
        subset=dedup_columns,
        keep="first",
    ).copy()

    duplicate_rows_removed = (
        before_dedup - len(df)
    )

    # --------------------------------------------------------
    # 薬剤単位の照合
    # --------------------------------------------------------

    detail_rows = []

    for source_index, row in df.iterrows():
        patient_id = normalize_text(
            row.get(patient_col, "")
        )

        drug_name = normalize_text(
            row.get(drug_col, "")
        )

        scope_status, scope_type, scope_note = (
            classify_jars_scope(
                drug_name
            )
        )

        matches = match_drug(
            drug_name=drug_name,
            jars_master=jars_master,
            name_map=name_map,
        )

        base_information = {
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
            "normalized_drug_name": (
                normalize_drug_name(
                    drug_name
                )
            ),
            "用法": (
                normalize_text(
                    row.get(sig_col, "")
                )
                if sig_col is not None
                else ""
            ),
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
            "scope_status": scope_status,
            "scope_type": scope_type,
            "scope_note": scope_note,
        }

        # JARS成分として照合されなかった
        if matches.empty:
            detail_rows.append({
                **base_information,
                "match_status": "not_found",
                "match_method": "",
                "matched_alias": "",
                "jars_drug": "",
                "jars_score": "",
                "therapeutic_class": "",
                "counted": 0,
                "counted_score": 0,
                "review_required": 1,
                "review_reason": (
                    "JARS非該当薬または商品名未登録。"
                    "商品名―成分対応を確認"
                ),
            })

            continue

        for _, match in matches.iterrows():
            match_status = match[
                "match_status"
            ]

            review_reasons = []

            if match_status == "ambiguous":
                append_unique(
                    review_reasons,
                    "複数候補があり照合を確定できない",
                )

            if scope_status == "unknown":
                append_unique(
                    review_reasons,
                    "JARS対象投与経路か確認が必要",
                )

            if scope_status == "out_of_scope":
                counted = 0
                counted_score = 0
            elif (
                scope_status == "in_scope"
                and match_status == "matched"
            ):
                counted = 1
                counted_score = int(
                    match["jars_score"]
                )
            else:
                counted = 0
                counted_score = 0

            detail_rows.append({
                **base_information,
                "match_status": match_status,
                "match_method": match[
                    "match_method"
                ],
                "matched_alias": match[
                    "matched_alias"
                ],
                "jars_drug": match[
                    "jars_drug"
                ],
                "jars_score": (
                    match["jars_score"]
                    if pd.notna(
                        match["jars_score"]
                    )
                    else ""
                ),
                "therapeutic_class": (
                    match.get(
                        "therapeutic_class",
                        "",
                    )
                ),
                "counted": counted,
                "counted_score": counted_score,
                "review_required": int(
                    bool(review_reasons)
                ),
                "review_reason": "; ".join(
                    review_reasons
                ),
            })

    if not detail_rows:
        raise ValueError(
            "評価対象となる薬剤行がありません。"
        )

    detail = pd.DataFrame(
        detail_rows
    )

    # --------------------------------------------------------
    # 同一患者・同一成分の重複除外
    # --------------------------------------------------------

    detail["duplicate_ingredient"] = 0
    detail["final_counted"] = 0
    detail["final_counted_score"] = 0

    for patient_id, patient_group in detail.groupby(
        "患者ID",
        sort=False,
    ):
        counted_ingredients = set()

        for row_index in patient_group.index:
            preliminary_counted = detail.at[
                row_index,
                "counted",
            ]

            ingredient = detail.at[
                row_index,
                "jars_drug",
            ]

            if preliminary_counted != 1:
                continue

            if not ingredient:
                continue

            if ingredient in counted_ingredients:
                detail.at[
                    row_index,
                    "duplicate_ingredient",
                ] = 1

                detail.at[
                    row_index,
                    "final_counted",
                ] = 0

                detail.at[
                    row_index,
                    "final_counted_score",
                ] = 0
            else:
                detail.at[
                    row_index,
                    "final_counted",
                ] = 1

                detail.at[
                    row_index,
                    "final_counted_score",
                ] = int(
                    detail.at[
                        row_index,
                        "counted_score",
                    ]
                )

                counted_ingredients.add(
                    ingredient
                )

    detail["jars_score_numeric"] = pd.to_numeric(
        detail["jars_score"],
        errors="coerce",
    )

    # --------------------------------------------------------
    # 患者単位集計
    # --------------------------------------------------------

    patient_summary = (
        detail.groupby(
            "患者ID",
            as_index=False,
        )
        .agg(
            病棟=("病棟", "first"),
            チーム=("チーム", "first"),

            medication_rows=(
                "source_index",
                "nunique",
            ),

            jars_total=(
                "final_counted_score",
                "sum",
            ),

            jars_drug_count=(
                "final_counted",
                "sum",
            ),

            score_3_count=(
                "final_counted_score",
                lambda values: int(
                    (values == 3).sum()
                ),
            ),

            score_2_count=(
                "final_counted_score",
                lambda values: int(
                    (values == 2).sum()
                ),
            ),

            score_1_count=(
                "final_counted_score",
                lambda values: int(
                    (values == 1).sum()
                ),
            ),

            unmatched_count=(
                "match_status",
                lambda values: int(
                    (values == "not_found").sum()
                ),
            ),

            ambiguous_count=(
                "match_status",
                lambda values: int(
                    (values == "ambiguous").sum()
                ),
            ),

            unknown_scope_count=(
                "scope_status",
                lambda values: int(
                    (values == "unknown").sum()
                ),
            ),

            excluded_scope_count=(
                "scope_status",
                lambda values: int(
                    (values == "out_of_scope").sum()
                ),
            ),

            duplicate_ingredient_count=(
                "duplicate_ingredient",
                "sum",
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

    # not_foundはJARS非該当薬である可能性と
    # 商品名未登録である可能性を区別できない。
    patient_summary["mapping_complete"] = (
        (
            patient_summary[
                "unmatched_count"
            ] == 0
        )
        & (
            patient_summary[
                "ambiguous_count"
            ] == 0
        )
        & (
            patient_summary[
                "unknown_scope_count"
            ] == 0
        )
    ).astype(int)

    patient_summary["score_status"] = (
        patient_summary[
            "mapping_complete"
        ].map({
            1: "complete",
            0: "partial_lower_bound",
        })
    )

    # 完全照合症例のみ確定値として別列に表示
    patient_summary[
        "jars_total_complete_only"
    ] = patient_summary[
        "jars_total"
    ].where(
        patient_summary[
            "mapping_complete"
        ] == 1
    )

    patient_summary["evaluation_date"] = (
        evaluation_date
        if evaluation_date is not None
        else ""
    )

    patient_summary["jars_master_sha256"] = (
        master_hash
    )

    # --------------------------------------------------------
    # 要確認一覧
    # --------------------------------------------------------

    review_mask = (
        (detail["match_status"] != "matched")
        | (detail["scope_status"] == "unknown")
        | (detail["review_required"] == 1)
    )

    review_columns = [
        "患者ID",
        "病棟",
        "チーム",
        "薬剤名",
        "用法",
        "match_status",
        "match_method",
        "matched_alias",
        "jars_drug",
        "jars_score",
        "scope_status",
        "scope_type",
        "scope_note",
        "review_reason",
    ]

    review = detail.loc[
        review_mask,
        review_columns,
    ].copy()

    # --------------------------------------------------------
    # 出力
    # --------------------------------------------------------

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

    log_lines = [
        f"input_file={input_csv}",
        f"jars_master={jars_master_path}",
        f"jars_master_sha256={master_hash}",
        f"name_map={name_map_path or 'not specified'}",
        f"evaluation_date={evaluation_date or 'not specified'}",
        f"original_rows={original_row_count}",
        f"rows_after_date_filter={rows_after_date_filter}",
        f"rows_after_empty_and_duplicate_filter={len(df)}",
        f"exact_duplicate_rows_removed={duplicate_rows_removed}",
        f"patients={len(patient_summary)}",
        f"review_rows={len(review)}",
        "",
        "注意:",
        "JARSは内服薬および全身作用目的の経皮薬を対象とする。",
        "同一患者・同一JARS成分は1回のみ加算した。",
        "未照合薬はJARS非該当薬と商品名未登録薬を区別できない。",
        "score_status=partial_lower_boundの場合、jars_totalは部分合計である。",
        "JARS合計点に確立した標準カットオフはない。",
    ]

    log_file.write_text(
        "\n".join(log_lines),
        encoding="utf-8",
    )

    print("JARS calculation completed.")
    print(f"患者単位: {patient_file}")
    print(f"薬剤単位: {detail_file}")
    print(f"要確認:   {review_file}")
    print(f"ログ:     {log_file}")

    return (
        patient_summary,
        detail,
        review,
    )


# ============================================================
# Self tests
# ============================================================

def run_self_tests():
    """
    薬剤名正規化、包含誤照合、投与経路の基本テスト。
    """
    test_master = pd.DataFrame({
        "jars_drug": [
            "セチリジン",
            "レボセチリジン",
            "ロラタジン",
            "デスロラタジン",
            "スコポラミン",
            "ブチルスコポラミン",
            "コデイン",
            "トリヘキシフェニジル",
        ],
        "jars_score": [
            2,
            1,
            1,
            1,
            3,
            3,
            1,
            3,
        ],
    })

    test_master[
        "normalized_jars_drug"
    ] = test_master[
        "jars_drug"
    ].map(
        normalize_drug_name
    )

    test_name_map = pd.DataFrame(
        columns=[
            "alias",
            "jars_drug",
            "normalized_alias",
        ]
    )

    cases = {
        "レボセチリジン塩酸塩錠5mg": (
            "レボセチリジン",
            1,
        ),
        "デスロラタジン錠5mg": (
            "デスロラタジン",
            1,
        ),
        "ブチルスコポラミン臭化物錠10mg": (
            "ブチルスコポラミン",
            3,
        ),
        "トリヘキシフェニジル塩酸塩錠2mg": (
            "トリヘキシフェニジル",
            3,
        ),
    }

    for drug_name, expected in cases.items():
        result = match_drug(
            drug_name=drug_name,
            jars_master=test_master,
            name_map=test_name_map,
        )

        assert len(result) == 1, (
            f"Expected one match: {drug_name}"
        )

        assert (
            result.iloc[0]["jars_drug"]
            == expected[0]
        ), (
            f"Ingredient error: {drug_name}, "
            f"actual={result.iloc[0]['jars_drug']}, "
            f"expected={expected[0]}"
        )

        assert (
            int(result.iloc[0]["jars_score"])
            == expected[1]
        ), (
            f"Score error: {drug_name}"
        )

    # ジヒドロコデインをコデインに誤照合しない
    result = match_drug(
        drug_name="ジヒドロコデインリン酸塩散1%",
        jars_master=test_master,
        name_map=test_name_map,
    )

    assert result.empty, (
        "ジヒドロコデインをコデインに"
        "誤照合しています"
    )

    # 投与経路テスト
    assert classify_jars_scope(
        "オロパタジン塩酸塩錠5mg"
    )[0] == "in_scope"

    assert classify_jars_scope(
        "オロパタジン点眼液0.1%"
    )[0] == "out_of_scope"

    assert classify_jars_scope(
        "アマンタジン塩酸塩錠50mg"
    )[0] == "in_scope"

    print("All self-tests passed.")


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description=(
            "院内薬歴CSVから日本版抗コリン薬"
            "リスクスケールを算出する"
        )
    )

    parser.add_argument(
        "input_csv",
        nargs="?",
        help="入力薬歴CSV",
    )

    parser.add_argument(
        "--jars-master",
        default=(
            "jsgp-jars-datebase-"
            "1st-release-20240528.xlsx"
        ),
        help="公式JARS Excelファイル",
    )

    parser.add_argument(
        "--name-map",
        default=None,
        help=(
            "任意の商品名―JARS薬物名対応CSV"
        ),
    )

    parser.add_argument(
        "--output-prefix",
        default="jars_result",
        help=(
            "出力ファイルの接頭辞。"
            "既定値: jars_result"
        ),
    )

    parser.add_argument(
        "--date",
        default=None,
        help=(
            "評価日。YYYY-MM-DD。"
            "省略時はCSV内の全行を対象とする"
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
            "--self-testを指定してください。"
        )

    calculate_jars(
        input_csv=args.input_csv,
        jars_master_path=args.jars_master,
        output_prefix=args.output_prefix,
        evaluation_date=args.date,
        name_map_path=args.name_map,
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
