from __future__ import annotations

from pathlib import Path

import pandas as pd


def create_sample_medications() -> pd.DataFrame:
    """
    DBI計算コード検証用の架空薬歴を作成する。

    注意
    ----
    ・薬剤名、成分、deltaはテスト専用の架空設定。
    ・臨床判断や実研究には使用しない。
    """
    columns = [
        "病棟",
        "チーム",
        "患者ID",
        "患者氏名",
        "生年月日",
        "年齢",
        "性別",
        "区分",
        "成分コード",
        "薬剤名",
        "1日量",
        "単位",
        "実成分1日量",
        "実成分1日量単位",
        "用法",
        "処方日数・回数",
        "処方単位",
        "オーダ指示日",
        "服用開始日",
        "使用終了日",
        "Dr名",
        "薬価",
    ]

    rows = [
        # ====================================================
        # 患者201
        #
        # 架空鎮静薬A：
        #   D = 1錠/day × 10 mg/錠
        #     = 10 mg/day
        #   delta = 10 mg/day
        #   DBI = 10 / (10 + 10) = 0.5
        #
        # 架空抗コリン薬B：
        #   2026-01-21開始
        #   D = 5 mg/day
        #   delta = 5 mg/day
        #   DBI = 0.5
        # ====================================================

        {
            "病棟": "5階病棟",
            "チーム": "A",
            "患者ID": "201",
            "患者氏名": "架空患者A",
            "生年月日": "1956/4/10",
            "年齢": "69",
            "性別": "男",
            "区分": "処方",
            "成分コード": "TEST_SEDATIVE_A_10",
            "薬剤名": "架空鎮静薬A錠10mg",
            "1日量": "1",
            "単位": "錠",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "1日1回 就寝前",
            "処方日数・回数": "41",
            "処方単位": "日分",
            "オーダ指示日": "2025/12/31",
            "服用開始日": "2026/1/1",
            "使用終了日": "2026/2/10",
            "Dr名": "架空医師A",
            "薬価": "10.0",
        },

        # 非DBI薬として処理する行
        {
            "病棟": "5階病棟",
            "チーム": "A",
            "患者ID": "201",
            "患者氏名": "架空患者A",
            "生年月日": "1956/4/10",
            "年齢": "69",
            "性別": "男",
            "区分": "処方",
            "成分コード": "METFORMIN",
            "薬剤名": "メトホルミン錠500mg",
            "1日量": "4",
            "単位": "錠",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "1日2回 朝・夕食後",
            "処方日数・回数": "20",
            "処方単位": "日分",
            "オーダ指示日": "2025/12/31",
            "服用開始日": "2026/1/1",
            "使用終了日": "2026/1/20",
            "Dr名": "架空医師A",
            "薬価": "10.1",
        },

        # 全身作用目的の架空貼付剤。
        #
        # 貼付剤では「1枚」をmg/dayへ機械的に変換せず、
        # 実成分1日量を明示する設計をテストする。
        {
            "病棟": "5階病棟",
            "チーム": "A",
            "患者ID": "201",
            "患者氏名": "架空患者A",
            "生年月日": "1956/4/10",
            "年齢": "69",
            "性別": "男",
            "区分": "処方",
            "成分コード": "TEST_AC_PATCH_B",
            "薬剤名": "架空抗コリン薬B全身用パッチ",
            "1日量": "1",
            "単位": "枚",
            "実成分1日量": "5",
            "実成分1日量単位": "mg/day",
            "用法": "1日1回貼り替え",
            "処方日数・回数": "21",
            "処方単位": "日分",
            "オーダ指示日": "2026/1/20",
            "服用開始日": "2026/1/21",
            "使用終了日": "2026/2/10",
            "Dr名": "架空医師A",
            "薬価": "100.0",
        },

        # ====================================================
        # 患者202
        #
        # 架空鎮静薬Aだが頓服。
        # 既定の主解析では除外される。
        # ====================================================

        {
            "病棟": "3階病棟",
            "チーム": "B",
            "患者ID": "202",
            "患者氏名": "架空患者B",
            "生年月日": "1948/8/20",
            "年齢": "77",
            "性別": "女",
            "区分": "処方",
            "成分コード": "TEST_SEDATIVE_A_10",
            "薬剤名": "架空鎮静薬A錠10mg",
            "1日量": "1",
            "単位": "錠",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "不眠時 1回1錠",
            "処方日数・回数": "10",
            "処方単位": "回分",
            "オーダ指示日": "2026/2/28",
            "服用開始日": "2026/3/1",
            "使用終了日": "2026/3/10",
            "Dr名": "架空医師B",
            "薬価": "10.0",
        },

        # ====================================================
        # 患者203
        #
        # 局所クリーム。
        # DBIマスターには存在するが、include_main=0。
        # ====================================================

        {
            "病棟": "4階病棟",
            "チーム": "A",
            "患者ID": "203",
            "患者氏名": "架空患者C",
            "生年月日": "1960/1/15",
            "年齢": "66",
            "性別": "女",
            "区分": "処方",
            "成分コード": "TEST_AC_CREAM",
            "薬剤名": "架空抗コリン薬Cクリーム1％",
            "1日量": "10",
            "単位": "g",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "1日2回 局所塗布",
            "処方日数・回数": "10",
            "処方単位": "日分",
            "オーダ指示日": "2026/3/31",
            "服用開始日": "2026/4/1",
            "使用終了日": "2026/4/10",
            "Dr名": "架空医師C",
            "薬価": "2.0",
        },

        # 非DBI薬
        {
            "病棟": "4階病棟",
            "チーム": "A",
            "患者ID": "203",
            "患者氏名": "架空患者C",
            "生年月日": "1960/1/15",
            "年齢": "66",
            "性別": "女",
            "区分": "処方",
            "成分コード": "MAGNESIUM_OXIDE",
            "薬剤名": "酸化マグネシウム錠330mg",
            "1日量": "3",
            "単位": "錠",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "1日3回 朝・昼・夕食後",
            "処方日数・回数": "20",
            "処方単位": "日分",
            "オーダ指示日": "2026/3/31",
            "服用開始日": "2026/4/1",
            "使用終了日": "2026/4/20",
            "Dr名": "架空医師C",
            "薬価": "6.1",
        },

        # ====================================================
        # 患者204
        #
        # 同一成分の規格違いを同時使用。
        #
        # 10 mg錠 × 1錠/day = 10 mg/day
        #  5 mg錠 × 2錠/day = 10 mg/day
        # 合計 D = 20 mg/day
        #
        # delta = 10 mg/day
        # DBI = 20 / (20 + 10)
        #     = 0.666666...
        #
        # 製剤ごとに0.5 + 0.5と計算してはいけない。
        # ====================================================

        {
            "病棟": "6階病棟",
            "チーム": "C",
            "患者ID": "204",
            "患者氏名": "架空患者D",
            "生年月日": "1950/9/1",
            "年齢": "75",
            "性別": "男",
            "区分": "処方",
            "成分コード": "TEST_SEDATIVE_C_10",
            "薬剤名": "架空鎮静薬C錠10mg",
            "1日量": "1",
            "単位": "錠",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "1日1回 朝食後",
            "処方日数・回数": "36",
            "処方単位": "日分",
            "オーダ指示日": "2026/4/30",
            "服用開始日": "2026/5/1",
            "使用終了日": "2026/6/5",
            "Dr名": "架空医師D",
            "薬価": "20.0",
        },

        {
            "病棟": "6階病棟",
            "チーム": "C",
            "患者ID": "204",
            "患者氏名": "架空患者D",
            "生年月日": "1950/9/1",
            "年齢": "75",
            "性別": "男",
            "区分": "処方",
            "成分コード": "TEST_SEDATIVE_C_5",
            "薬剤名": "架空鎮静薬C錠5mg",
            "1日量": "2",
            "単位": "錠",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "1日2回 朝・夕食後",
            "処方日数・回数": "36",
            "処方単位": "日分",
            "オーダ指示日": "2026/4/30",
            "服用開始日": "2026/5/1",
            "使用終了日": "2026/6/5",
            "Dr名": "架空医師D",
            "薬価": "10.0",
        },

        # 吸入薬としてマスター側で除外する例
        {
            "病棟": "6階病棟",
            "チーム": "C",
            "患者ID": "204",
            "患者氏名": "架空患者D",
            "生年月日": "1950/9/1",
            "年齢": "75",
            "性別": "男",
            "区分": "処方",
            "成分コード": "TEST_INHALATION_D",
            "薬剤名": "架空鎮静薬D吸入剤",
            "1日量": "2",
            "単位": "吸入",
            "実成分1日量": "",
            "実成分1日量単位": "",
            "用法": "1日1回 1回2吸入",
            "処方日数・回数": "17",
            "処方単位": "日分",
            "オーダ指示日": "2026/5/19",
            "服用開始日": "2026/5/20",
            "使用終了日": "2026/6/5",
            "Dr名": "架空医師D",
            "薬価": "200.0",
        },
    ]

    return pd.DataFrame(
        rows,
        columns=columns,
    )


def create_sample_admissions() -> pd.DataFrame:
    rows = [
        {
            "入院ID": "E201",
            "患者ID": "201",
            "入院日": "2026/1/1",
            "退院日": "2026/2/10",
        },
        {
            "入院ID": "E202",
            "患者ID": "202",
            "入院日": "2026/3/1",
            "退院日": "2026/3/10",
        },
        {
            "入院ID": "E203",
            "患者ID": "203",
            "入院日": "2026/4/1",
            "退院日": "2026/4/20",
        },
        {
            "入院ID": "E204",
            "患者ID": "204",
            "入院日": "2026/5/1",
            "退院日": "2026/6/5",
        },
    ]

    return pd.DataFrame(rows)


def create_sample_dbi_master() -> pd.DataFrame:
    """
    テスト専用DBIマスター。

    deltaおよび用量変換係数は、コード動作確認用の
    架空値であり、実在薬には使用できない。
    """
    columns = [
        "ingredient_id",
        "ingredient_name_jp",
        "local_ingredient_codes",
        "product_name_regex",
        "dbi_category",
        "burden_type",
        "route",
        "include_main",
        "delta",
        "delta_unit",
        "source_dose_unit",
        "dose_factor_to_delta_unit",
        "profile",
        "source",
        "notes",
    ]

    rows = [
        # ----------------------------------------------------
        # 架空鎮静薬A
        # ----------------------------------------------------
        {
            "ingredient_id": "TEST_SEDATIVE_A",
            "ingredient_name_jp": "架空鎮静薬A",
            "local_ingredient_codes": (
                "TEST_SEDATIVE_A_10"
            ),
            "product_name_regex": "",
            "dbi_category": (
                "benzodiazepine_related_test"
            ),
            "burden_type": "S",
            "route": "oral",
            "include_main": "1",
            "delta": "10",
            "delta_unit": "mg/day",
            "source_dose_unit": "錠",
            "dose_factor_to_delta_unit": "10",
            "profile": "TEST_ONLY",
            "source": "TEST_ONLY_NOT_CLINICAL",
            "notes": (
                "テスト専用架空値。"
                "臨床・研究には使用禁止"
            ),
        },

        # ----------------------------------------------------
        # 架空抗コリン薬B貼付剤
        #
        # 実成分1日量列を使うため、変換係数は実際には
        # 使用されない。
        # ----------------------------------------------------
        {
            "ingredient_id": "TEST_AC_PATCH_B",
            "ingredient_name_jp": (
                "架空抗コリン薬B"
            ),
            "local_ingredient_codes": (
                "TEST_AC_PATCH_B"
            ),
            "product_name_regex": "",
            "dbi_category": (
                "muscarinic_antagonist_test"
            ),
            "burden_type": "AC",
            "route": "systemic_transdermal",
            "include_main": "1",
            "delta": "5",
            "delta_unit": "mg/day",
            "source_dose_unit": "枚",
            "dose_factor_to_delta_unit": "5",
            "profile": "TEST_ONLY",
            "source": "TEST_ONLY_NOT_CLINICAL",
            "notes": (
                "テスト専用架空貼付剤。"
                "実成分1日量列を優先して計算"
            ),
        },

        # ----------------------------------------------------
        # 局所クリーム：主解析対象外
        # ----------------------------------------------------
        {
            "ingredient_id": "TEST_AC_CREAM",
            "ingredient_name_jp": (
                "架空抗コリン薬C"
            ),
            "local_ingredient_codes": (
                "TEST_AC_CREAM"
            ),
            "product_name_regex": "",
            "dbi_category": (
                "antihistamine_test"
            ),
            "burden_type": "AC",
            "route": "local_topical",
            "include_main": "0",
            "delta": "",
            "delta_unit": "",
            "source_dose_unit": "g",
            "dose_factor_to_delta_unit": "",
            "profile": "TEST_ONLY",
            "source": "TEST_ONLY_NOT_CLINICAL",
            "notes": (
                "局所作用のみを想定し主解析除外"
            ),
        },

        # ----------------------------------------------------
        # 架空鎮静薬C 10 mg錠
        # ----------------------------------------------------
        {
            "ingredient_id": "TEST_SEDATIVE_C",
            "ingredient_name_jp": "架空鎮静薬C",
            "local_ingredient_codes": (
                "TEST_SEDATIVE_C_10"
            ),
            "product_name_regex": "",
            "dbi_category": (
                "antiepileptic_test"
            ),
            "burden_type": "S",
            "route": "oral",
            "include_main": "1",
            "delta": "10",
            "delta_unit": "mg/day",
            "source_dose_unit": "錠",
            "dose_factor_to_delta_unit": "10",
            "profile": "TEST_ONLY",
            "source": "TEST_ONLY_NOT_CLINICAL",
            "notes": (
                "テスト専用架空値。"
                "同一成分規格違いの合算確認用"
            ),
        },

        # ----------------------------------------------------
        # 架空鎮静薬C 5 mg錠
        #
        # ingredient_idを10 mg錠と同じにすることで、
        # 同一成分としてDを合算する。
        # ----------------------------------------------------
        {
            "ingredient_id": "TEST_SEDATIVE_C",
            "ingredient_name_jp": "架空鎮静薬C",
            "local_ingredient_codes": (
                "TEST_SEDATIVE_C_5"
            ),
            "product_name_regex": "",
            "dbi_category": (
                "antiepileptic_test"
            ),
            "burden_type": "S",
            "route": "oral",
            "include_main": "1",
            "delta": "10",
            "delta_unit": "mg/day",
            "source_dose_unit": "錠",
            "dose_factor_to_delta_unit": "5",
            "profile": "TEST_ONLY",
            "source": "TEST_ONLY_NOT_CLINICAL",
            "notes": (
                "テスト専用架空値。"
                "同一成分規格違いの合算確認用"
            ),
        },

        # ----------------------------------------------------
        # 吸入剤：主解析対象外
        # ----------------------------------------------------
        {
            "ingredient_id": "TEST_INHALATION_D",
            "ingredient_name_jp": (
                "架空鎮静薬D"
            ),
            "local_ingredient_codes": (
                "TEST_INHALATION_D"
            ),
            "product_name_regex": "",
            "dbi_category": "other_test",
            "burden_type": "S",
            "route": "inhalation",
            "include_main": "0",
            "delta": "",
            "delta_unit": "",
            "source_dose_unit": "吸入",
            "dose_factor_to_delta_unit": "",
            "profile": "TEST_ONLY",
            "source": "TEST_ONLY_NOT_CLINICAL",
            "notes": (
                "吸入薬として主解析除外"
            ),
        },
    ]

    return pd.DataFrame(
        rows,
        columns=columns,
    )


def create_expected_snapshot_results() -> pd.DataFrame:
    """
    既定設定での期待値。

    設定:
        day14_offset = 14
        day30_offset = 30
        include_prn = False
        ingredient_universe_complete = True
    """
    rows = [
        # 患者201
        {
            "episode_id": "E201",
            "患者ID": "201",
            "timepoint": "admission",
            "evaluation_date": "2026-01-01",
            "expected_dbi_total": 0.5,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 0.5,
            "expected_dbi_drug_count": 1,
        },
        {
            "episode_id": "E201",
            "患者ID": "201",
            "timepoint": "day14",
            "evaluation_date": "2026-01-15",
            "expected_dbi_total": 0.5,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 0.5,
            "expected_dbi_drug_count": 1,
        },
        {
            "episode_id": "E201",
            "患者ID": "201",
            "timepoint": "day30",
            "evaluation_date": "2026-01-31",
            "expected_dbi_total": 1.0,
            "expected_dbi_anticholinergic": 0.5,
            "expected_dbi_sedative": 0.5,
            "expected_dbi_drug_count": 2,
        },
        {
            "episode_id": "E201",
            "患者ID": "201",
            "timepoint": "discharge",
            "evaluation_date": "2026-02-10",
            "expected_dbi_total": 1.0,
            "expected_dbi_anticholinergic": 0.5,
            "expected_dbi_sedative": 0.5,
            "expected_dbi_drug_count": 2,
        },

        # 患者202：頓服除外
        {
            "episode_id": "E202",
            "患者ID": "202",
            "timepoint": "admission",
            "evaluation_date": "2026-03-01",
            "expected_dbi_total": 0.0,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 0.0,
            "expected_dbi_drug_count": 0,
        },
        {
            "episode_id": "E202",
            "患者ID": "202",
            "timepoint": "discharge",
            "evaluation_date": "2026-03-10",
            "expected_dbi_total": 0.0,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 0.0,
            "expected_dbi_drug_count": 0,
        },

        # 患者203：局所クリーム除外
        {
            "episode_id": "E203",
            "患者ID": "203",
            "timepoint": "admission",
            "evaluation_date": "2026-04-01",
            "expected_dbi_total": 0.0,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 0.0,
            "expected_dbi_drug_count": 0,
        },
        {
            "episode_id": "E203",
            "患者ID": "203",
            "timepoint": "discharge",
            "evaluation_date": "2026-04-20",
            "expected_dbi_total": 0.0,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 0.0,
            "expected_dbi_drug_count": 0,
        },

        # 患者204：同一成分の規格違いを合算
        {
            "episode_id": "E204",
            "患者ID": "204",
            "timepoint": "admission",
            "evaluation_date": "2026-05-01",
            "expected_dbi_total": 2 / 3,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 2 / 3,
            "expected_dbi_drug_count": 1,
        },
        {
            "episode_id": "E204",
            "患者ID": "204",
            "timepoint": "day14",
            "evaluation_date": "2026-05-15",
            "expected_dbi_total": 2 / 3,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 2 / 3,
            "expected_dbi_drug_count": 1,
        },
        {
            "episode_id": "E204",
            "患者ID": "204",
            "timepoint": "day30",
            "evaluation_date": "2026-05-31",
            "expected_dbi_total": 2 / 3,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 2 / 3,
            "expected_dbi_drug_count": 1,
        },
        {
            "episode_id": "E204",
            "患者ID": "204",
            "timepoint": "discharge",
            "evaluation_date": "2026-06-05",
            "expected_dbi_total": 2 / 3,
            "expected_dbi_anticholinergic": 0.0,
            "expected_dbi_sedative": 2 / 3,
            "expected_dbi_drug_count": 1,
        },
    ]

    return pd.DataFrame(rows)


def create_expected_episode_results() -> pd.DataFrame:
    """
    処方区間ベースの期待値。

    入院日と退院日の両方を含む。
    """
    rows = [
        {
            "episode_id": "E201",
            "患者ID": "201",
            "hospital_days_inclusive": 41,

            # 架空鎮静薬A:
            # 0.5 × 41日 = 20.5
            #
            # 架空抗コリン薬B:
            # 0.5 × 21日 = 10.5
            #
            # 合計 = 31.0
            "expected_dbi_burden_days": 31.0,
            "expected_mean_daily_dbi": (
                31.0 / 41.0
            ),
        },
        {
            "episode_id": "E202",
            "患者ID": "202",
            "hospital_days_inclusive": 10,
            "expected_dbi_burden_days": 0.0,
            "expected_mean_daily_dbi": 0.0,
        },
        {
            "episode_id": "E203",
            "患者ID": "203",
            "hospital_days_inclusive": 20,
            "expected_dbi_burden_days": 0.0,
            "expected_mean_daily_dbi": 0.0,
        },
        {
            "episode_id": "E204",
            "患者ID": "204",
            "hospital_days_inclusive": 36,
            "expected_dbi_burden_days": 24.0,
            "expected_mean_daily_dbi": 2 / 3,
        },
    ]

    return pd.DataFrame(rows)


def main():
    output_dir = Path(
        "sample_dbi_data"
    )

    output_dir.mkdir(
        parents=True,
        exist_ok=True,
    )

    medications = (
        create_sample_medications()
    )

    admissions = (
        create_sample_admissions()
    )

    master = (
        create_sample_dbi_master()
    )

    expected_snapshots = (
        create_expected_snapshot_results()
    )

    expected_episodes = (
        create_expected_episode_results()
    )

    medication_file = (
        output_dir
        / "sample_medications.csv"
    )

    admission_file = (
        output_dir
        / "sample_admissions.csv"
    )

    master_file = (
        output_dir
        / "sample_dbi_master.csv"
    )

    expected_snapshot_file = (
        output_dir
        / "sample_expected_snapshots.csv"
    )

    expected_episode_file = (
        output_dir
        / "sample_expected_episodes.csv"
    )

    medications.to_csv(
        medication_file,
        index=False,
        encoding="utf-8-sig",
    )

    admissions.to_csv(
        admission_file,
        index=False,
        encoding="utf-8-sig",
    )

    master.to_csv(
        master_file,
        index=False,
        encoding="utf-8-sig",
    )

    expected_snapshots.to_csv(
        expected_snapshot_file,
        index=False,
        encoding="utf-8-sig",
    )

    expected_episodes.to_csv(
        expected_episode_file,
        index=False,
        encoding="utf-8-sig",
    )

    print(
        "DBI sample data were created."
    )

    print(
        f"薬歴:             {medication_file}"
    )

    print(
        f"入退院情報:       {admission_file}"
    )

    print(
        f"DBIマスター:      {master_file}"
    )

    print(
        "期待時点結果:     "
        f"{expected_snapshot_file}"
    )

    print(
        "期待エピソード結果: "
        f"{expected_episode_file}"
    )

    print()
    print(
        "WARNING: DBIマスターの薬剤とdeltaは"
        "テスト専用の架空値です。"
    )


if __name__ == "__main__":
    main()
