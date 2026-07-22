from __future__ import annotations

from pathlib import Path

import pandas as pd


# ============================================================
# Configuration
# ============================================================

OUTPUT_DIR = Path("sample_data")


# ============================================================
# Sample JARS master
# ============================================================

def create_sample_jars_master(
    output_path: Path,
) -> None:
    """
    動作確認用の模擬JARSマスターを作成する。

    注意:
        公式JARSマスターではない。
        スコアはプログラム動作確認用。
    """
    master = pd.DataFrame(
        [
            {
                "薬物": "トリヘキシフェニジル",
                "スコア": 3,
                "薬効群": "抗パーキンソン病薬",
                "薬効群中分類": "抗コリン薬",
                "薬効分類": "TEST001",
                "Medication": "Trihexyphenidyl",
                "ATCコード": "N04AA01",
            },
            {
                "薬物": "レボセチリジン",
                "スコア": 1,
                "薬効群": "抗アレルギー薬",
                "薬効群中分類": "抗ヒスタミン薬",
                "薬効分類": "TEST002",
                "Medication": "Levocetirizine",
                "ATCコード": "R06AE09",
            },
            {
                "薬物": "オロパタジン",
                "スコア": 1,
                "薬効群": "抗アレルギー薬",
                "薬効群中分類": "抗ヒスタミン薬",
                "薬効分類": "TEST003",
                "Medication": "Olopatadine",
                "ATCコード": "S01GX09",
            },
            {
                "薬物": "ロチゴチン",
                "スコア": 1,
                "薬効群": "抗パーキンソン病薬",
                "薬効群中分類": "ドパミン作動薬",
                "薬効分類": "TEST004",
                "Medication": "Rotigotine",
                "ATCコード": "N04BC09",
            },
            {
                "薬物": "オキシブチニン",
                "スコア": 3,
                "薬効群": "泌尿器系薬",
                "薬効群中分類": "過活動膀胱治療薬",
                "薬効分類": "TEST005",
                "Medication": "Oxybutynin",
                "ATCコード": "G04BD04",
            },
            {
                "薬物": "ブロナンセリン",
                "スコア": 1,
                "薬効群": "精神神経用薬",
                "薬効群中分類": "抗精神病薬",
                "薬効分類": "TEST006",
                "Medication": "Blonanserin",
                "ATCコード": "N05AX",
            },
            {
                "薬物": "フェンタニル",
                "スコア": 1,
                "薬効群": "鎮痛薬",
                "薬効群中分類": "オピオイド",
                "薬効分類": "TEST007",
                "Medication": "Fentanyl",
                "ATCコード": "N02AB03",
            },
            {
                "薬物": "セチリジン",
                "スコア": 2,
                "薬効群": "抗アレルギー薬",
                "薬効群中分類": "抗ヒスタミン薬",
                "薬効分類": "TEST008",
                "Medication": "Cetirizine",
                "ATCコード": "R06AE07",
            },
            {
                "薬物": "デスロラタジン",
                "スコア": 1,
                "薬効群": "抗アレルギー薬",
                "薬効群中分類": "抗ヒスタミン薬",
                "薬効分類": "TEST009",
                "Medication": "Desloratadine",
                "ATCコード": "R06AX27",
            },
            {
                "薬物": "ブチルスコポラミン",
                "スコア": 3,
                "薬効群": "鎮痙薬",
                "薬効群中分類": "抗コリン薬",
                "薬効分類": "TEST010",
                "Medication": "Butylscopolamine",
                "ATCコード": "A03BB01",
            },
            {
                "薬物": "スコポラミン",
                "スコア": 3,
                "薬効群": "鎮暈薬",
                "薬効群中分類": "抗コリン薬",
                "薬効分類": "TEST011",
                "Medication": "Scopolamine",
                "ATCコード": "A04AD01",
            },
            {
                "薬物": "コデイン",
                "スコア": 1,
                "薬効群": "鎮咳薬",
                "薬効群中分類": "オピオイド",
                "薬効分類": "TEST012",
                "Medication": "Codeine",
                "ATCコード": "R05DA04",
            },
        ]
    )

    output_path.parent.mkdir(
        parents=True,
        exist_ok=True,
    )

    # find_jars_sheet()がヘッダー行を探索できるか確認するため、
    # Excelの先頭2行を説明行として空ける。
    with pd.ExcelWriter(
        output_path,
        engine="openpyxl",
    ) as writer:
        description = pd.DataFrame(
            [
                ["JARS動作確認用模擬マスター"],
                [
                    "公式マスターではなく、"
                    "プログラムのテスト目的で作成"
                ],
            ]
        )

        description.to_excel(
            writer,
            sheet_name="158医薬品（薬効） ",
            index=False,
            header=False,
            startrow=0,
        )

        master.to_excel(
            writer,
            sheet_name="158医薬品（薬効） ",
            index=False,
            startrow=2,
        )


# ============================================================
# Product-name/alias map
# ============================================================

def create_sample_name_map(
    output_path: Path,
) -> None:
    """
    商品名・別名からJARS一般名への対応表を作成する。
    """
    name_map = pd.DataFrame(
        [
            {
                "alias": "アーテン",
                "jars_drug": "トリヘキシフェニジル",
            },
            {
                "alias": "ザイザル",
                "jars_drug": "レボセチリジン",
            },
            {
                "alias": "アレロック",
                "jars_drug": "オロパタジン",
            },
            {
                "alias": "ニュープロ",
                "jars_drug": "ロチゴチン",
            },
            {
                "alias": "ネオキシ",
                "jars_drug": "オキシブチニン",
            },
            {
                "alias": "ロナセン",
                "jars_drug": "ブロナンセリン",
            },
            {
                "alias": "デュロテップ",
                "jars_drug": "フェンタニル",
            },
            {
                "alias": "ジルテック",
                "jars_drug": "セチリジン",
            },
            {
                "alias": "デザレックス",
                "jars_drug": "デスロラタジン",
            },
            {
                "alias": "ブスコパン",
                "jars_drug": "ブチルスコポラミン",
            },
        ]
    )

    name_map.to_csv(
        output_path,
        index=False,
        encoding="utf-8-sig",
    )


# ============================================================
# Admission sample
# ============================================================

def create_sample_admissions(
    output_path: Path,
) -> None:
    """
    入退院情報のサンプルを作成する。

    E001:
        30日以上入院し、全評価時点が対象。

    E002:
        14日未満で退院するためday14/day30は対象外。

    E003:
        退院日欠損。

    E_BAD:
        退院日が入院日より前の不正データ。
    """
    admissions = pd.DataFrame(
        [
            {
                "患者ID": "P001",
                "入院ID": "E001",
                "入院日": "2026-01-01",
                "退院日": "2026-02-05",
            },
            {
                "患者ID": "P002",
                "入院ID": "E002",
                "入院日": "2026-01-01",
                "退院日": "2026-01-10",
            },
            {
                "患者ID": "P003",
                "入院ID": "E003",
                "入院日": "2026-03-01",
                "退院日": "",
            },
            {
                "患者ID": "P999",
                "入院ID": "E_BAD",
                "入院日": "2026-04-10",
                "退院日": "2026-04-01",
            },
        ]
    )

    admissions.to_csv(
        output_path,
        index=False,
        encoding="utf-8-sig",
    )


# ============================================================
# Medication history sample
# ============================================================

def create_sample_medications(
    output_path: Path,
) -> None:
    """
    薬歴のサンプルを作成する。

    以下を含む:
        ・一般名による照合
        ・商品名対応表による照合
        ・英語成分コードによる照合
        ・JARS非掲載成分
        ・未解決薬
        ・点眼薬の対象外判定
        ・全身作用目的の経皮薬
        ・同一成分の重複処方
        ・開始日／終了日の境界
    """
    medication_rows = [
        # ====================================================
        # P001: すべての評価時点に到達する患者
        # ====================================================

        # 商品名対応：アーテン
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "アーテン錠2mg",
            "成分コード": "TRIHEXYPHENIDYL",
            "用法": "1日3回 毎食後",
            "服用開始日": "2025-12-20",
            "使用終了日": "2026-02-05",
            "オーダ指示日": "2025-12-20",
        },

        # 一般名照合：day14では有効、day30では終了済み
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "レボセチリジン塩酸塩錠5mg",
            "成分コード": "LEVOCETIRIZINE",
            "用法": "1日1回 就寝前",
            "服用開始日": "2026-01-10",
            "使用終了日": "2026-01-20",
            "オーダ指示日": "2026-01-10",
        },

        # 同一成分の別薬剤行。
        # day14時点でレボセチリジンと重複し、
        # 同一成分1回加算の確認に使用。
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "ザイザル錠5mg",
            "成分コード": "",
            "用法": "1日1回 朝食後",
            "服用開始日": "2026-01-12",
            "使用終了日": "2026-01-18",
            "オーダ指示日": "2026-01-12",
        },

        # 点眼薬：成分はJARSにあるが集計対象外
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "オロパタジン点眼液0.1%",
            "成分コード": "OLOPATADINE",
            "用法": "両眼1回1滴 1日2回",
            "服用開始日": "2025-12-25",
            "使用終了日": "2026-02-05",
            "オーダ指示日": "2025-12-25",
        },

        # 全身作用目的の経皮薬
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "ニュープロパッチ4.5mg",
            "成分コード": "ROTIGOTINE_PATCH",
            "用法": "1日1回貼付",
            "服用開始日": "2026-01-20",
            "使用終了日": "2026-02-05",
            "オーダ指示日": "2026-01-20",
        },

        # JARS非掲載成分。
        # --ingredient-code-is-english指定時は
        # not_listed_in_jarsになる。
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "メトホルミン塩酸塩錠500mg",
            "成分コード": "METFORMIN",
            "用法": "1日2回 朝夕食後",
            "服用開始日": "2025-12-01",
            "使用終了日": "2026-02-05",
            "オーダ指示日": "2025-12-01",
        },

        # 成分コードなし・商品名対応なしの未解決薬
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "院内採用サンプル錠10mg",
            "成分コード": "",
            "用法": "1日1回 朝食後",
            "服用開始日": "2026-01-01",
            "使用終了日": "2026-02-05",
            "オーダ指示日": "2026-01-01",
        },

        # 開始日欠損。オーダ指示日で補完される。
        {
            "患者ID": "P001",
            "患者氏名": "患者 一郎",
            "病棟": "回復期A",
            "チーム": "A",
            "薬剤名": "ブスコパン錠10mg",
            "成分コード": "",
            "用法": "疼痛時",
            "服用開始日": "",
            "使用終了日": "2026-01-15",
            "オーダ指示日": "2026-01-01",
        },

        # ====================================================
        # P002: 入院10日で退院
        # ====================================================

        {
            "患者ID": "P002",
            "患者氏名": "患者 二郎",
            "病棟": "回復期B",
            "チーム": "B",
            "薬剤名": "ジルテック錠10mg",
            "成分コード": "CETIRIZINE",
            "用法": "1日1回 就寝前",
            "服用開始日": "2026-01-01",
            "使用終了日": "2026-01-10",
            "オーダ指示日": "2026-01-01",
        },
        {
            "患者ID": "P002",
            "患者氏名": "患者 二郎",
            "病棟": "回復期B",
            "チーム": "B",
            "薬剤名": "デザレックス錠5mg",
            "成分コード": "DESLORATADINE",
            "用法": "1日1回 朝食後",
            "服用開始日": "2026-01-05",
            "使用終了日": "2026-01-10",
            "オーダ指示日": "2026-01-05",
        },

        # ====================================================
        # P003: 退院日欠損
        # ====================================================

        {
            "患者ID": "P003",
            "患者氏名": "患者 三郎",
            "病棟": "療養病棟",
            "チーム": "C",
            "薬剤名": "ネオキシテープ73.5mg",
            "成分コード": "OXYBUTYNIN_PATCH",
            "用法": "1日1回貼付",
            "服用開始日": "2026-03-01",
            "使用終了日": "",
            "オーダ指示日": "2026-03-01",
        },
        {
            "患者ID": "P003",
            "患者氏名": "患者 三郎",
            "病棟": "療養病棟",
            "チーム": "C",
            "薬剤名": "ブロナンセリン錠4mg",
            "成分コード": "BLONANSERIN",
            "用法": "1日2回 朝夕食後",
            "服用開始日": "2026-03-10",
            "使用終了日": "",
            "オーダ指示日": "2026-03-10",
        },

        # ====================================================
        # 誤照合防止確認
        # ====================================================

        # コデインに誤照合してはいけない薬剤
        {
            "患者ID": "P003",
            "患者氏名": "患者 三郎",
            "病棟": "療養病棟",
            "チーム": "C",
            "薬剤名": "ジヒドロコデインリン酸塩散1%",
            "成分コード": "",
            "用法": "1日3回 毎食後",
            "服用開始日": "2026-03-15",
            "使用終了日": "2026-03-20",
            "オーダ指示日": "2026-03-15",
        },
    ]

    medications = pd.DataFrame(
        medication_rows
    )

    medications.to_csv(
        output_path,
        index=False,
        encoding="utf-8-sig",
    )


# ============================================================
# Expected result memo
# ============================================================

def create_readme(
    output_path: Path,
) -> None:
    text = """\
# JARSサンプルデータ

## ファイル

- sample_medications.csv
- sample_admissions.csv
- sample_name_map.csv
- sample_jars_master.xlsx

sample_jars_master.xlsxは公式JARSマスターではありません。
プログラム動作確認専用の模擬データです。

## 縦断解析の実行例

python jars_longitudinal.py \
  sample_data/sample_medications.csv \
  sample_data/sample_admissions.csv \
  --jars-master sample_data/sample_jars_master.xlsx \
  --name-map sample_data/sample_name_map.csv \
  --ingredient-code-is-english \
  --output-prefix output/sample_jars

## 入院日を第1病日とする場合

「入院14日目」を評価する場合はoffset=13、
「入院30日目」を評価する場合はoffset=29とします。

python jars_longitudinal.py \
  sample_data/sample_medications.csv \
  sample_data/sample_admissions.csv \
  --jars-master sample_data/sample_jars_master.xlsx \
  --name-map sample_data/sample_name_map.csv \
  --ingredient-code-is-english \
  --day14-offset 13 \
  --day30-offset 29 \
  --output-prefix output/sample_jars_hospital_day

## 単一評価日の実行例

python jars_auto.py \
  sample_data/sample_medications.csv \
  --jars-master sample_data/sample_jars_master.xlsx \
  --name-map sample_data/sample_name_map.csv \
  --date 2026-01-15 \
  --output-prefix output/sample_single_date

## 想定される確認事項

- 模擬マスターは158薬剤ではないためWARNINGが出る。
- P001では同一レボセチリジン成分の重複除外が確認できる。
- オロパタジン点眼液はout_of_scopeになる。
- ニュープロパッチは全身作用目的の経皮薬として集計される。
- METFORMINは--ingredient-code-is-english指定時にnot_listed_in_jarsになる。
- 院内採用サンプル錠はunresolvedになる。
- P002のday14/day30はnot_reached_before_dischargeになる。
- P003のdischargeはdischarge_date_missingになる。
- E_BADはinvalid_admissionsへ出力される。
- ジヒドロコデインはコデインに誤照合されない。
"""

    output_path.write_text(
        text,
        encoding="utf-8",
    )


# ============================================================
# Main
# ============================================================

def main() -> None:
    OUTPUT_DIR.mkdir(
        parents=True,
        exist_ok=True,
    )

    master_path = (
        OUTPUT_DIR
        / "sample_jars_master.xlsx"
    )

    name_map_path = (
        OUTPUT_DIR
        / "sample_name_map.csv"
    )

    admissions_path = (
        OUTPUT_DIR
        / "sample_admissions.csv"
    )

    medications_path = (
        OUTPUT_DIR
        / "sample_medications.csv"
    )

    readme_path = (
        OUTPUT_DIR
        / "README.md"
    )

    create_sample_jars_master(
        master_path
    )

    create_sample_name_map(
        name_map_path
    )

    create_sample_admissions(
        admissions_path
    )

    create_sample_medications(
        medications_path
    )

    create_readme(
        readme_path
    )

    print("Sample data creation completed.")
    print(f"JARS模擬マスター: {master_path}")
    print(f"商品名対応表:     {name_map_path}")
    print(f"入退院データ:     {admissions_path}")
    print(f"薬歴データ:       {medications_path}")
    print(f"説明:             {readme_path}")


if __name__ == "__main__":
    main()
