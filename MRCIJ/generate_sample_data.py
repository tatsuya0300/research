from __future__ import annotations

import argparse
import csv
import json
import random
from collections import Counter
from datetime import date, timedelta
from pathlib import Path
from typing import Dict, List, Optional, Tuple


# ============================================================
# Synthetic medication master
# Section A 32カテゴリーを網羅
# ============================================================

MEDICATION_TEMPLATES = [
    {
        "form": "ORAL_TABLET_CAPSULE",
        "drug": "合成アムロジピン錠5mg",
        "ingredient": "SYN-ING-001",
        "unit": "錠",
        "kind": "standard",
    },
    {
        "form": "ORAL_GARGLE",
        "drug": "合成アズレン含嗽液",
        "ingredient": "SYN-ING-002",
        "unit": "mL",
        "kind": "standard",
    },
    {
        "form": "ORAL_LOZENGE_GUM",
        "drug": "合成口腔用トローチ",
        "ingredient": "SYN-ING-003",
        "unit": "錠",
        "kind": "standard",
    },
    {
        "form": "ORAL_LIQUID",
        "drug": "合成鎮咳シロップ",
        "ingredient": "SYN-ING-004",
        "unit": "mL",
        "kind": "standard",
    },
    {
        "form": "ORAL_POWDER_GRANULE",
        "drug": "合成整腸剤細粒",
        "ingredient": "SYN-ING-005",
        "unit": "包",
        "kind": "standard",
    },
    {
        "form": "SUBLINGUAL",
        "drug": "合成ニトログリセリン舌下錠",
        "ingredient": "SYN-ING-006",
        "unit": "錠",
        "kind": "prn_preferred",
    },
    {
        "form": "TOPICAL_CREAM_GEL_OINTMENT",
        "drug": "合成保湿外用クリーム",
        "ingredient": "SYN-ING-007",
        "unit": "g",
        "kind": "standard",
    },
    {
        "form": "TOPICAL_DRESSING",
        "drug": "合成創傷被覆材ドレッシング",
        "ingredient": "SYN-ING-008",
        "unit": "枚",
        "kind": "standard",
    },
    {
        "form": "TOPICAL_SOLUTION",
        "drug": "合成消毒外用液",
        "ingredient": "SYN-ING-009",
        "unit": "mL",
        "kind": "standard",
    },
    {
        "form": "TOPICAL_PASTE",
        "drug": "合成皮膚保護ペースト",
        "ingredient": "SYN-ING-010",
        "unit": "g",
        "kind": "standard",
    },
    {
        "form": "TOPICAL_PATCH",
        "drug": "合成鎮痛テープ20mg",
        "ingredient": "SYN-ING-011",
        "unit": "枚",
        "kind": "patch",
    },
    {
        "form": "TOPICAL_SPRAY",
        "drug": "合成外用スプレー",
        "ingredient": "SYN-ING-012",
        "unit": "本",
        "kind": "standard",
    },
    {
        "form": "EAR_DROP_CREAM_OINTMENT",
        "drug": "合成抗菌点耳液",
        "ingredient": "SYN-ING-013",
        "unit": "滴",
        "kind": "standard",
    },
    {
        "form": "EYE_DROP",
        "drug": "合成緑内障点眼液",
        "ingredient": "SYN-ING-014",
        "unit": "滴",
        "kind": "standard",
    },
    {
        "form": "EYE_GEL_OINTMENT",
        "drug": "合成抗菌眼軟膏",
        "ingredient": "SYN-ING-015",
        "unit": "g",
        "kind": "standard",
    },
    {
        "form": "NASAL_DROP_CREAM_OINTMENT",
        "drug": "合成鼻用点鼻液",
        "ingredient": "SYN-ING-016",
        "unit": "mL",
        "kind": "standard",
    },
    {
        "form": "NASAL_SPRAY",
        "drug": "合成ステロイド点鼻スプレー",
        "ingredient": "SYN-ING-017",
        "unit": "本",
        "kind": "standard",
    },
    {
        "form": "INHALER_ACCUHALER_DISKUS",
        "drug": "合成配合剤ディスカス吸入剤",
        "ingredient": "SYN-ING-018",
        "unit": "吸入",
        "kind": "inhaler",
    },
    {
        "form": "INHALER_AEROLIZER",
        "drug": "合成気管支拡張薬エアロライザー吸入剤",
        "ingredient": "SYN-ING-019",
        "unit": "吸入",
        "kind": "inhaler",
    },
    {
        "form": "INHALER_MDI",
        "drug": "合成気管支拡張薬エアゾール吸入剤",
        "ingredient": "SYN-ING-020",
        "unit": "吸入",
        "kind": "inhaler",
    },
    {
        "form": "INHALER_NEBULIZER",
        "drug": "合成気管支拡張薬吸入液ネブライザ用",
        "ingredient": "SYN-ING-021",
        "unit": "mL",
        "kind": "inhaler",
    },
    {
        "form": "OXYGEN",
        "drug": "合成在宅酸素療法",
        "ingredient": "SYN-OXYGEN",
        "unit": "L/分",
        "kind": "oxygen",
    },
    {
        "form": "INHALER_TURBUHALER",
        "drug": "合成配合剤タービュヘイラー吸入剤",
        "ingredient": "SYN-ING-023",
        "unit": "吸入",
        "kind": "inhaler",
    },
    {
        "form": "INHALER_OTHER_DPI",
        "drug": "合成配合剤エリプタDPI吸入剤",
        "ingredient": "SYN-ING-024",
        "unit": "吸入",
        "kind": "inhaler",
    },
    {
        "form": "DIALYSATE",
        "drug": "合成腹膜透析液",
        "ingredient": "SYN-ING-025",
        "unit": "袋",
        "kind": "dialysis",
    },
    {
        "form": "ENEMA",
        "drug": "合成グリセリン浣腸液",
        "ingredient": "SYN-ING-026",
        "unit": "個",
        "kind": "prn_preferred",
    },
    {
        "form": "INJECTION_PREFILLED",
        "drug": "合成抗凝固薬プレフィルドシリンジ注射",
        "ingredient": "SYN-ING-027",
        "unit": "キット",
        "kind": "injection",
    },
    {
        "form": "INJECTION_VIAL_AMPULE",
        "drug": "合成抗菌薬静注用バイアル注射",
        "ingredient": "SYN-ING-028",
        "unit": "瓶",
        "kind": "injection",
    },
    {
        "form": "PESSARY",
        "drug": "合成抗真菌腟錠",
        "ingredient": "SYN-ING-029",
        "unit": "錠",
        "kind": "standard",
    },
    {
        "form": "PCA",
        "drug": "合成オピオイドPCA自己調節鎮痛",
        "ingredient": "SYN-ING-030",
        "unit": "キット",
        "kind": "standard",
    },
    {
        "form": "SUPPOSITORY",
        "drug": "合成解熱鎮痛坐剤",
        "ingredient": "SYN-ING-031",
        "unit": "個",
        "kind": "prn_preferred",
    },
    {
        "form": "VAGINAL_CREAM",
        "drug": "合成抗真菌腟クリーム",
        "ingredient": "SYN-ING-032",
        "unit": "g",
        "kind": "standard",
    },
]

assert len(MEDICATION_TEMPLATES) == 32


# ============================================================
# Frequency patterns
# ============================================================

STANDARD_FREQUENCIES = [
    {
        "sig": "1日1回 朝",
        "daily_frequency": 1.0,
        "label": "once_daily",
    },
    {
        "sig": "1日1回 朝食後",
        "daily_frequency": 1.0,
        "label": "once_daily_after_breakfast",
    },
    {
        "sig": "1日2回 朝・夕",
        "daily_frequency": 2.0,
        "label": "twice_daily",
    },
    {
        "sig": "1日2回 朝・夕食後",
        "daily_frequency": 2.0,
        "label": "twice_daily_after_meals",
    },
    {
        "sig": "1日3回 毎食後",
        "daily_frequency": 3.0,
        "label": "three_times_daily",
    },
    {
        "sig": "1日4回 朝・昼・夕・就寝前",
        "daily_frequency": 4.0,
        "label": "four_times_daily",
    },
    {
        "sig": "12時間おき",
        "daily_frequency": 2.0,
        "label": "q12h",
    },
    {
        "sig": "8時間おき",
        "daily_frequency": 3.0,
        "label": "q8h",
    },
    {
        "sig": "6時間おき",
        "daily_frequency": 4.0,
        "label": "q6h",
    },
    {
        "sig": "4時間おき",
        "daily_frequency": 6.0,
        "label": "q4h",
    },
    {
        "sig": "隔日 1日1回朝",
        "daily_frequency": None,
        "label": "less_than_daily",
    },
    {
        "sig": "毎週月曜日",
        "daily_frequency": None,
        "label": "weekly",
    },
    {
        "sig": "疼痛時 頓服",
        "daily_frequency": None,
        "label": "prn",
    },
    {
        "sig": "発熱時 6時間以上あけて必要時",
        "daily_frequency": None,
        "label": "q6h_prn",
    },
]


EXCLUDED_TEMPLATES = [
    {
        "drug": "合成インフルエンザワクチン",
        "ingredient": "SYN-EXC-VACCINE",
        "unit": "本",
        "sig": "予防接種として1回",
        "case": "excluded_vaccine",
    },
    {
        "drug": "合成ペンニードル注射針",
        "ingredient": "SYN-EXC-NEEDLE",
        "unit": "本",
        "sig": "インスリン注射時に使用",
        "case": "excluded_medical_device",
    },
    {
        "drug": "合成血糖測定用テストストリップ",
        "ingredient": "SYN-EXC-STRIP",
        "unit": "枚",
        "sig": "血糖測定時に使用",
        "case": "excluded_test_strip",
    },
]


# ============================================================
# General utilities
# ============================================================

def date_string(
    value: Optional[date],
) -> str:
    if value is None:
        return ""

    return value.isoformat()


def random_date(
    rng: random.Random,
    start: date,
    end: date,
) -> date:
    span = max(
        0,
        (end - start).days,
    )

    return (
        start
        + timedelta(
            days=rng.randint(
                0,
                span,
            )
        )
    )


def random_ward(
    rng: random.Random,
) -> str:
    return rng.choice([
        "回復期リハ病棟A",
        "回復期リハ病棟B",
        "一般病棟3階",
        "一般病棟4階",
        "地域包括ケア病棟",
    ])


def random_team(
    rng: random.Random,
) -> str:
    return rng.choice([
        "脳血管チーム",
        "運動器チーム",
        "廃用症候群チーム",
        "脊髄損傷チーム",
        "呼吸・循環チーム",
    ])


def choose_frequency(
    rng: random.Random,
    kind: str,
) -> Dict:
    if kind == "oxygen":
        return rng.choice([
            {
                "sig": "酸素 1日12時間使用",
                "daily_frequency": None,
                "label": "oxygen_lt15h",
            },
            {
                "sig": "酸素 1日18時間使用",
                "daily_frequency": None,
                "label": "oxygen_ge15h",
            },
            {
                "sig": "呼吸困難時 必要時酸素投与",
                "daily_frequency": None,
                "label": "oxygen_prn",
            },
            {
                "sig": "酸素を24時間持続使用",
                "daily_frequency": None,
                "label": "oxygen_continuous",
            },
        ])

    if kind == "patch":
        return rng.choice([
            {
                "sig": "1日1回 朝に貼付",
                "daily_frequency": 1.0,
                "label": "patch_daily",
            },
            {
                "sig": "72時間ごとに貼り替え",
                "daily_frequency": None,
                "label": "patch_q72h",
            },
        ])

    if kind == "dialysis":
        return rng.choice([
            {
                "sig": "1日4回 透析液を交換",
                "daily_frequency": 4.0,
                "label": "dialysis_four_times",
            },
            {
                "sig": "毎日 就寝前に使用",
                "daily_frequency": 1.0,
                "label": "dialysis_daily",
            },
        ])

    if kind == "inhaler":
        return rng.choice([
            {
                "sig": "1日1回 1吸入 朝",
                "daily_frequency": 1.0,
                "label": "inhaler_once",
            },
            {
                "sig": "1日2回 1回2吸入 朝・夕",
                "daily_frequency": 2.0,
                "label": "inhaler_twice_two_puffs",
            },
            {
                "sig": "発作時 1回2吸入 必要時",
                "daily_frequency": None,
                "label": "inhaler_prn",
            },
        ])

    if kind == "injection":
        return rng.choice([
            {
                "sig": "1日1回 朝 皮下注射",
                "daily_frequency": 1.0,
                "label": "injection_once",
            },
            {
                "sig": "1日2回 朝・夕 皮下注射",
                "daily_frequency": 2.0,
                "label": "injection_twice",
            },
            {
                "sig": "8時間おきに静脈注射",
                "daily_frequency": 3.0,
                "label": "injection_q8h",
            },
        ])

    if kind == "prn_preferred":
        if rng.random() < 0.7:
            return rng.choice([
                {
                    "sig": "疼痛時 頓服",
                    "daily_frequency": None,
                    "label": "prn",
                },
                {
                    "sig": "必要時 6時間以上あけて使用",
                    "daily_frequency": None,
                    "label": "q6h_prn",
                },
            ])

    return rng.choice(
        STANDARD_FREQUENCIES
    )


def make_daily_amount(
    rng: random.Random,
    unit: str,
    daily_frequency: Optional[float],
) -> str:
    countable_units = {
        "錠",
        "カプセル",
        "包",
        "枚",
        "吸入",
    }

    if unit in countable_units:
        units_per_dose = rng.choices(
            [0.5, 1.0, 2.0, 3.0],
            weights=[5, 65, 25, 5],
            k=1,
        )[0]

        if daily_frequency is None:
            return "{:g}".format(
                units_per_dose
            )

        return "{:g}".format(
            units_per_dose
            * daily_frequency
        )

    if unit == "mL":
        return str(
            rng.choice([
                1,
                2,
                5,
                10,
                15,
                20,
                30,
            ])
        )

    if unit == "g":
        return str(
            rng.choice([
                0.5,
                1,
                2,
                5,
                10,
            ])
        )

    if unit == "滴":
        return str(
            rng.choice([
                1,
                2,
                4,
                6,
            ])
        )

    if unit == "L/分":
        return str(
            rng.choice([
                1,
                1.5,
                2,
                3,
            ])
        )

    return "1"


# ============================================================
# Section C sample generation
# ============================================================

def add_section_c_direction(
    rng: random.Random,
    sig: str,
    template: Dict,
) -> Tuple[str, str]:
    form = template["form"]
    unit = template["unit"]

    candidates = [
        "specific_time",
        "food",
        "specific_fluid",
        "as_directed_formal",
        "tapering",
        "none",
        "none",
        "none",
    ]

    if form == "ORAL_TABLET_CAPSULE":
        candidates.extend([
            "break_crush",
            "alternating",
            "variable_dose",
        ])

    if form == "ORAL_POWDER_GRANULE":
        candidates.extend([
            "dissolve",
            "specific_fluid",
        ])

    if unit in {
        "錠",
        "カプセル",
        "包",
        "枚",
        "吸入",
    }:
        candidates.append(
            "multiple_units"
        )

    selected = rng.choice(
        candidates
    )

    if selected == "specific_time":
        return (
            "{} 就寝前".format(sig),
            "C_specific_time",
        )

    if selected == "food":
        return (
            "{} 食後".format(sig),
            "C_relation_to_food",
        )

    if selected == "specific_fluid":
        return (
            "{} コップ1杯の水で服用".format(
                sig
            ),
            "C_specific_fluid",
        )

    if selected == "break_crush":
        return (
            "{} 錠剤を半分に割って服用".format(
                sig
            ),
            "C_break_crush",
        )

    if selected == "dissolve":
        return (
            "{} 水に溶かして服用".format(
                sig
            ),
            "C_dissolve_mix",
        )

    if selected == "multiple_units":
        return (
            "{} 1回2{}使用".format(
                sig,
                unit,
            ),
            "C_multiple_units",
        )

    if selected == "alternating":
        return (
            "1日2回 朝・夕食後(1-0.5)",
            "C_alternating_dose",
        )

    if selected == "variable_dose":
        return (
            "{} 症状に応じて1～2錠に調節".format(
                sig
            ),
            "C_variable_dose",
        )

    if selected == "as_directed_formal":
        return (
            "{} 医師の指示どおり".format(
                sig
            ),
            "C_formal_as_directed_review",
        )

    if selected == "tapering":
        return (
            "{} 3日間2錠、その後1錠に減量".format(
                sig
            ),
            "C_tapering",
        )

    return sig, "standard"


# ============================================================
# Date generation
# ============================================================

def medication_dates(
    rng: random.Random,
    admission_date: date,
    discharge_date: date,
) -> Tuple[
    Optional[date],
    Optional[date],
    Optional[date],
]:
    pattern = rng.choices(
        [
            "baseline",
            "during_stay",
            "short_course",
            "ongoing",
        ],
        weights=[
            45,
            25,
            20,
            10,
        ],
        k=1,
    )[0]

    if pattern == "baseline":
        medication_start = (
            admission_date
            - timedelta(
                days=rng.randint(
                    0,
                    30,
                )
            )
        )

        medication_end = (
            discharge_date
        )

    elif pattern == "during_stay":
        medication_start = random_date(
            rng,
            admission_date,
            discharge_date,
        )

        medication_end = (
            discharge_date
        )

    elif pattern == "short_course":
        medication_start = random_date(
            rng,
            admission_date,
            discharge_date,
        )

        medication_end = min(
            discharge_date,
            medication_start
            + timedelta(
                days=rng.randint(
                    2,
                    14,
                )
            ),
        )

    else:
        medication_start = (
            admission_date
            - timedelta(
                days=rng.randint(
                    0,
                    14,
                )
            )
        )

        medication_end = None

    if medication_start is not None:
        order_date = (
            medication_start
            - timedelta(
                days=rng.randint(
                    0,
                    2,
                )
            )
        )
    else:
        order_date = admission_date

    return (
        medication_start,
        medication_end,
        order_date,
    )


def make_episode_dates(
    rng: random.Random,
    first_start: date,
    episodes: int,
) -> List[Tuple[date, date]]:
    result = []
    admission_date = first_start

    for _ in range(episodes):
        length_of_stay = rng.randint(
            5,
            70,
        )

        discharge_date = (
            admission_date
            + timedelta(
                days=length_of_stay
            )
        )

        result.append(
            (
                admission_date,
                discharge_date,
            )
        )

        admission_date = (
            discharge_date
            + timedelta(
                days=rng.randint(
                    30,
                    240,
                )
            )
        )

    return result


# ============================================================
# Main generator
# ============================================================

def generate_sample_data(
    output_dir: str,
    patients: int = 10000,
    mean_medications: float = 12.0,
    max_episodes: int = 2,
    multiple_episode_rate: float = 0.15,
    missing_discharge_rate: float = 0.02,
    missing_date_rate: float = 0.01,
    excluded_rate: float = 0.02,
    unmapped_rate: float = 0.03,
    duplicate_rate: float = 0.02,
    conflicting_frequency_rate: float = 0.02,
    invalid_admission_rate: float = 0.005,
    omit_ingredient_column: bool = False,
    seed: int = 20260720,
):
    rng = random.Random(seed)

    output_path = Path(
        output_dir
    )

    output_path.mkdir(
        parents=True,
        exist_ok=True,
    )

    medication_file = (
        output_path
        / "sample_medications.csv"
    )

    admission_file = (
        output_path
        / "sample_admissions.csv"
    )

    manifest_file = (
        output_path
        / "sample_manifest.json"
    )

    medication_fields = [
        "患者ID",
        "患者氏名",
        "病棟",
        "チーム",
        "薬剤名",
    ]

    if not omit_ingredient_column:
        medication_fields.append(
            "成分コード"
        )

    medication_fields.extend([
        "1日量",
        "単位",
        "用法",
        "服用開始日",
        "使用終了日",
        "オーダ指示日",
        "処方日数・回数",
        "synthetic_case",
        "synthetic_form_category",
        "synthetic_frequency_label",
        "synthetic_episode_id",
    ])

    admission_fields = [
        "患者ID",
        "患者氏名",
        "入院ID",
        "入院日",
        "退院日",
        "病棟",
        "チーム",
        "synthetic_case",
    ]

    counters = Counter()
    form_counter = Counter()
    frequency_counter = Counter()
    case_counter = Counter()

    template_cursor = 0
    total_valid_episodes = 0

    first_possible_admission = date(
        2023,
        1,
        1,
    )

    last_possible_admission = date(
        2025,
        6,
        30,
    )

    # 古いPythonとの互換性のため、
    # 括弧付きの複数with構文は使用しない。
    with medication_file.open(
        "w",
        encoding="utf-8-sig",
        newline="",
    ) as medication_handle:

        with admission_file.open(
            "w",
            encoding="utf-8-sig",
            newline="",
        ) as admission_handle:

            medication_writer = (
                csv.DictWriter(
                    medication_handle,
                    fieldnames=(
                        medication_fields
                    ),
                    extrasaction="ignore",
                )
            )

            admission_writer = (
                csv.DictWriter(
                    admission_handle,
                    fieldnames=(
                        admission_fields
                    ),
                    extrasaction="ignore",
                )
            )

            medication_writer.writeheader()
            admission_writer.writeheader()

            for patient_number in range(
                1,
                patients + 1,
            ):
                patient_id = (
                    "SYN{:08d}".format(
                        patient_number
                    )
                )

                patient_name = (
                    "架空患者{:08d}".format(
                        patient_number
                    )
                )

                ward = random_ward(rng)
                team = random_team(rng)

                episode_count = 1

                if (
                    max_episodes >= 2
                    and rng.random()
                    < multiple_episode_rate
                ):
                    episode_count = (
                        rng.randint(
                            2,
                            max_episodes,
                        )
                    )

                first_admission = (
                    random_date(
                        rng,
                        first_possible_admission,
                        last_possible_admission,
                    )
                )

                episodes = (
                    make_episode_dates(
                        rng,
                        first_admission,
                        episode_count,
                    )
                )

                for (
                    episode_number,
                    episode_dates,
                ) in enumerate(
                    episodes,
                    start=1,
                ):
                    admission_date = (
                        episode_dates[0]
                    )

                    actual_discharge_date = (
                        episode_dates[1]
                    )

                    episode_id = (
                        "EP-{:08d}-{:02d}".format(
                            patient_number,
                            episode_number,
                        )
                    )

                    discharge_missing = (
                        rng.random()
                        < missing_discharge_rate
                    )

                    if discharge_missing:
                        output_discharge_date = ""
                        admission_case = (
                            "missing_discharge"
                        )
                    else:
                        output_discharge_date = (
                            date_string(
                                actual_discharge_date
                            )
                        )
                        admission_case = (
                            "valid_admission"
                        )

                    admission_row = {
                        "患者ID": patient_id,
                        "患者氏名": patient_name,
                        "入院ID": episode_id,
                        "入院日": date_string(
                            admission_date
                        ),
                        "退院日": (
                            output_discharge_date
                        ),
                        "病棟": ward,
                        "チーム": team,
                        "synthetic_case": (
                            admission_case
                        ),
                    }

                    admission_writer.writerow(
                        admission_row
                    )

                    counters[
                        "admission_rows"
                    ] += 1

                    total_valid_episodes += 1

                    medication_sd = max(
                        1.0,
                        mean_medications
                        * 0.25,
                    )

                    medication_count = max(
                        1,
                        int(
                            round(
                                rng.gauss(
                                    mean_medications,
                                    medication_sd,
                                )
                            )
                        ),
                    )

                    for _ in range(
                        medication_count
                    ):
                        # 最初の32行は各剤形を
                        # 1回ずつ強制的に生成する。
                        if (
                            template_cursor
                            < len(
                                MEDICATION_TEMPLATES
                            )
                        ):
                            template = dict(
                                MEDICATION_TEMPLATES[
                                    template_cursor
                                ]
                            )

                            template_cursor += 1

                        else:
                            template = dict(
                                rng.choice(
                                    MEDICATION_TEMPLATES
                                )
                            )

                        frequency = (
                            choose_frequency(
                                rng,
                                template["kind"],
                            )
                        )

                        sig = frequency["sig"]

                        (
                            sig,
                            direction_case,
                        ) = add_section_c_direction(
                            rng,
                            sig,
                            template,
                        )

                        daily_amount = (
                            make_daily_amount(
                                rng,
                                template["unit"],
                                frequency[
                                    "daily_frequency"
                                ],
                            )
                        )

                        (
                            medication_start,
                            medication_end,
                            order_date,
                        ) = medication_dates(
                            rng,
                            admission_date,
                            actual_discharge_date,
                        )

                        synthetic_case = (
                            direction_case
                        )

                        synthetic_frequency_label = (
                            frequency["label"]
                        )

                        # ------------------------------------
                        # 除外薬剤
                        # ------------------------------------
                        if (
                            rng.random()
                            < excluded_rate
                        ):
                            excluded = dict(
                                rng.choice(
                                    EXCLUDED_TEMPLATES
                                )
                            )

                            template["drug"] = (
                                excluded["drug"]
                            )

                            template[
                                "ingredient"
                            ] = excluded[
                                "ingredient"
                            ]

                            template["unit"] = (
                                excluded["unit"]
                            )

                            sig = excluded["sig"]
                            daily_amount = "1"

                            synthetic_case = (
                                excluded["case"]
                            )

                            synthetic_frequency_label = (
                                "excluded"
                            )

                        # ------------------------------------
                        # 意図的な未分類ケース
                        # ------------------------------------
                        elif (
                            rng.random()
                            < unmapped_rate
                        ):
                            unmapped_type = (
                                rng.choice([
                                    "unknown_form",
                                    "free_comment",
                                    "truncated_sig",
                                    "numeric_unit",
                                    "unknown_inhaler",
                                    "unknown_nasal",
                                ])
                            )

                            if (
                                unmapped_type
                                == "unknown_form"
                            ):
                                template["drug"] = (
                                    "合成試験薬剤X"
                                )

                                template[
                                    "ingredient"
                                ] = (
                                    "SYN-UNMAPPED-001"
                                )

                                template["unit"] = "g"
                                sig = "1日1回"

                                synthetic_case = (
                                    "unmapped_section_A"
                                )

                            elif (
                                unmapped_type
                                == "free_comment"
                            ):
                                sig = (
                                    "フリーコメント"
                                )

                                synthetic_case = (
                                    "unmapped_section_B"
                                )

                            elif (
                                unmapped_type
                                == "truncated_sig"
                            ):
                                sig = (
                                    "1日2回 朝・夕..."
                                )

                                synthetic_case = (
                                    "truncated_sig"
                                )

                            elif (
                                unmapped_type
                                == "numeric_unit"
                            ):
                                template["drug"] = (
                                    "合成単位不明薬"
                                )

                                template[
                                    "ingredient"
                                ] = (
                                    "SYN-UNMAPPED-002"
                                )

                                template["unit"] = "10"
                                sig = "1日1回"

                                synthetic_case = (
                                    "numeric_only_unit"
                                )

                            elif (
                                unmapped_type
                                == "unknown_inhaler"
                            ):
                                template["drug"] = (
                                    "合成デバイス不明吸入剤"
                                )

                                template[
                                    "ingredient"
                                ] = (
                                    "SYN-UNMAPPED-003"
                                )

                                template["unit"] = (
                                    "吸入"
                                )

                                sig = (
                                    "1日2回吸入"
                                )

                                synthetic_case = (
                                    "unknown_inhaler_device"
                                )

                            else:
                                template["drug"] = (
                                    "合成剤形不明点鼻薬"
                                )

                                template[
                                    "ingredient"
                                ] = (
                                    "SYN-UNMAPPED-004"
                                )

                                template["unit"] = "本"
                                sig = "1日1回 点鼻"

                                synthetic_case = (
                                    "unknown_nasal_form"
                                )

                            synthetic_frequency_label = (
                                "unmapped_test"
                            )

                        # ------------------------------------
                        # 開始日・オーダ指示日の欠損
                        # ------------------------------------
                        if (
                            rng.random()
                            < missing_date_rate
                        ):
                            medication_start = None
                            order_date = None

                            synthetic_case += (
                                "|missing_start_"
                                "and_order_date"
                            )

                        if (
                            medication_start
                            is not None
                            and medication_end
                            is not None
                        ):
                            prescription_days = (
                                (
                                    medication_end
                                    - medication_start
                                ).days
                                + 1
                            )
                        else:
                            prescription_days = ""

                        medication_row = {
                            "患者ID": patient_id,
                            "患者氏名": patient_name,
                            "病棟": ward,
                            "チーム": team,
                            "薬剤名": (
                                template["drug"]
                            ),
                            "成分コード": (
                                template[
                                    "ingredient"
                                ]
                            ),
                            "1日量": daily_amount,
                            "単位": (
                                template["unit"]
                            ),
                            "用法": sig,
                            "服用開始日": (
                                date_string(
                                    medication_start
                                )
                            ),
                            "使用終了日": (
                                date_string(
                                    medication_end
                                )
                            ),
                            "オーダ指示日": (
                                date_string(
                                    order_date
                                )
                            ),
                            "処方日数・回数": (
                                prescription_days
                            ),
                            "synthetic_case": (
                                synthetic_case
                            ),
                            "synthetic_form_category": (
                                template["form"]
                            ),
                            "synthetic_frequency_label": (
                                synthetic_frequency_label
                            ),
                            "synthetic_episode_id": (
                                episode_id
                            ),
                        }

                        medication_writer.writerow(
                            medication_row
                        )

                        counters[
                            "medication_rows"
                        ] += 1

                        form_counter[
                            template["form"]
                        ] += 1

                        frequency_counter[
                            synthetic_frequency_label
                        ] += 1

                        for case_name in (
                            synthetic_case.split(
                                "|"
                            )
                        ):
                            case_counter[
                                case_name
                            ] += 1

                        # ------------------------------------
                        # 完全重複行
                        # ------------------------------------
                        if (
                            rng.random()
                            < duplicate_rate
                        ):
                            duplicate_row = dict(
                                medication_row
                            )

                            # 完全重複除去の検証のため、
                            # CSV上も完全に同一の行とする。
                            medication_writer.writerow(
                                duplicate_row
                            )

                            counters[
                                "medication_rows"
                            ] += 1

                            counters[
                                "exact_duplicate_rows"
                            ] += 1

                            case_counter[
                                "exact_duplicate"
                            ] += 1

                        # ------------------------------------
                        # 同一成分・異頻度
                        # ------------------------------------
                        if (
                            rng.random()
                            < conflicting_frequency_rate
                        ):
                            conflict_row = dict(
                                medication_row
                            )

                            original_sig = (
                                conflict_row[
                                    "用法"
                                ]
                            )

                            if (
                                "1日2回"
                                in original_sig
                            ):
                                conflict_row[
                                    "用法"
                                ] = "1日1回 朝"

                                conflict_label = (
                                    "once_daily"
                                )
                            else:
                                conflict_row[
                                    "用法"
                                ] = (
                                    "1日2回 朝・夕"
                                )

                                conflict_label = (
                                    "twice_daily"
                                )

                            conflict_row[
                                "synthetic_case"
                            ] = (
                                synthetic_case
                                + "|same_ingredient_"
                                "different_frequency"
                            )

                            conflict_row[
                                "synthetic_frequency_label"
                            ] = conflict_label

                            medication_writer.writerow(
                                conflict_row
                            )

                            counters[
                                "medication_rows"
                            ] += 1

                            counters[
                                "conflicting_frequency_rows"
                            ] += 1

                            case_counter[
                                "same_ingredient_"
                                "different_frequency"
                            ] += 1

            # ----------------------------------------
            # 不正入退院データ
            # ----------------------------------------
            invalid_count = int(
                round(
                    patients
                    * invalid_admission_rate
                )
            )

            for invalid_number in range(
                1,
                invalid_count + 1,
            ):
                invalid_type = rng.choice([
                    "missing_patient_id",
                    "missing_admission_date",
                    "invalid_admission_date",
                ])

                if (
                    invalid_type
                    == "missing_patient_id"
                ):
                    invalid_patient_id = ""
                else:
                    invalid_patient_id = (
                        "INVALID{:06d}".format(
                            invalid_number
                        )
                    )

                if (
                    invalid_type
                    == "missing_admission_date"
                ):
                    invalid_admission_date = ""

                elif (
                    invalid_type
                    == "invalid_admission_date"
                ):
                    invalid_admission_date = (
                        "日付不正"
                    )

                else:
                    invalid_admission_date = (
                        "2025-01-01"
                    )

                invalid_row = {
                    "患者ID": (
                        invalid_patient_id
                    ),
                    "患者氏名": (
                        "架空不正患者{:06d}".format(
                            invalid_number
                        )
                    ),
                    "入院ID": (
                        "INVALID-EP-{:06d}".format(
                            invalid_number
                        )
                    ),
                    "入院日": (
                        invalid_admission_date
                    ),
                    "退院日": "2025-01-15",
                    "病棟": "テスト病棟",
                    "チーム": "テストチーム",
                    "synthetic_case": (
                        invalid_type
                    ),
                }

                admission_writer.writerow(
                    invalid_row
                )

                counters[
                    "admission_rows"
                ] += 1

                counters[
                    "invalid_admission_rows"
                ] += 1

    manifest = {
        "synthetic_data": True,
        "contains_real_patient_information": False,
        "seed": seed,
        "parameters": {
            "patients": patients,
            "mean_medications": (
                mean_medications
            ),
            "max_episodes": max_episodes,
            "multiple_episode_rate": (
                multiple_episode_rate
            ),
            "missing_discharge_rate": (
                missing_discharge_rate
            ),
            "missing_date_rate": (
                missing_date_rate
            ),
            "excluded_rate": excluded_rate,
            "unmapped_rate": unmapped_rate,
            "duplicate_rate": duplicate_rate,
            "conflicting_frequency_rate": (
                conflicting_frequency_rate
            ),
            "invalid_admission_rate": (
                invalid_admission_rate
            ),
            "omit_ingredient_column": (
                omit_ingredient_column
            ),
        },
        "counts": {
            **dict(counters),
            "patients": patients,
            "valid_episodes": (
                total_valid_episodes
            ),
        },
        "section_A_template_counts": dict(
            sorted(
                form_counter.items()
            )
        ),
        "frequency_counts": dict(
            sorted(
                frequency_counter.items()
            )
        ),
        "synthetic_case_counts": dict(
            sorted(
                case_counter.items()
            )
        ),
        "files": {
            "medications": str(
                medication_file
            ),
            "admissions": str(
                admission_file
            ),
        },
    }

    manifest_file.write_text(
        json.dumps(
            manifest,
            ensure_ascii=False,
            indent=2,
        ),
        encoding="utf-8",
    )

    print(
        "Synthetic MRCI sample data generated."
    )

    print(
        "薬歴CSV:   {}".format(
            medication_file
        )
    )

    print(
        "入退院CSV: {}".format(
            admission_file
        )
    )

    print(
        "マニフェスト: {}".format(
            manifest_file
        )
    )

    print(
        "患者数: {:,}".format(
            patients
        )
    )

    print(
        "有効入院エピソード数: {:,}".format(
            total_valid_episodes
        )
    )

    print(
        "薬歴行数: {:,}".format(
            counters[
                "medication_rows"
            ]
        )
    )

    print(
        "入退院行数: {:,}".format(
            counters[
                "admission_rows"
            ]
        )
    )


# ============================================================
# Argument validation
# ============================================================

def validate_rate(
    parser: argparse.ArgumentParser,
    argument_name: str,
    value: float,
):
    if not 0 <= value <= 1:
        parser.error(
            "{}は0以上1以下で指定してください".format(
                argument_name
            )
        )


# ============================================================
# CLI
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description=(
            "MRCI自動算出プログラム用の"
            "大規模合成薬歴・入退院CSVを生成する"
        )
    )

    parser.add_argument(
        "--output-dir",
        default="sample_data",
        help=(
            "出力ディレクトリ。"
            "既定値: sample_data"
        ),
    )

    parser.add_argument(
        "--patients",
        type=int,
        default=10000,
        help=(
            "生成患者数。"
            "既定値: 10000"
        ),
    )

    parser.add_argument(
        "--mean-medications",
        type=float,
        default=12.0,
        help=(
            "1入院エピソード当たりの"
            "平均薬剤行数。既定値: 12"
        ),
    )

    parser.add_argument(
        "--max-episodes",
        type=int,
        default=2,
        help=(
            "1患者当たりの最大入院回数。"
            "既定値: 2"
        ),
    )

    parser.add_argument(
        "--multiple-episode-rate",
        type=float,
        default=0.15,
        help=(
            "複数入院患者の割合。"
            "既定値: 0.15"
        ),
    )

    parser.add_argument(
        "--missing-discharge-rate",
        type=float,
        default=0.02,
        help=(
            "退院日欠損率。"
            "既定値: 0.02"
        ),
    )

    parser.add_argument(
        "--missing-date-rate",
        type=float,
        default=0.01,
        help=(
            "薬剤開始日とオーダ日を"
            "同時欠損させる割合。"
            "既定値: 0.01"
        ),
    )

    parser.add_argument(
        "--excluded-rate",
        type=float,
        default=0.02,
        help=(
            "ワクチン・医療材料等の"
            "除外薬剤割合。既定値: 0.02"
        ),
    )

    parser.add_argument(
        "--unmapped-rate",
        type=float,
        default=0.03,
        help=(
            "意図的な未分類薬剤・用法割合。"
            "既定値: 0.03"
        ),
    )

    parser.add_argument(
        "--duplicate-rate",
        type=float,
        default=0.02,
        help=(
            "完全重複行の追加割合。"
            "既定値: 0.02"
        ),
    )

    parser.add_argument(
        "--conflicting-frequency-rate",
        type=float,
        default=0.02,
        help=(
            "同一成分・異頻度行の追加割合。"
            "既定値: 0.02"
        ),
    )

    parser.add_argument(
        "--invalid-admission-rate",
        type=float,
        default=0.005,
        help=(
            "不正入退院行の割合。"
            "既定値: 0.005"
        ),
    )

    parser.add_argument(
        "--omit-ingredient-column",
        action="store_true",
        help=(
            "成分コード列を出力しない。"
            "薬剤名推定キーのテスト用"
        ),
    )

    parser.add_argument(
        "--seed",
        type=int,
        default=20260720,
        help=(
            "乱数シード。"
            "既定値: 20260720"
        ),
    )

    args = parser.parse_args()

    if args.patients < 1:
        parser.error(
            "--patientsは1以上が必要です"
        )

    if args.mean_medications <= 0:
        parser.error(
            "--mean-medicationsは"
            "0より大きい値が必要です"
        )

    if args.max_episodes < 1:
        parser.error(
            "--max-episodesは1以上が必要です"
        )

    validate_rate(
        parser,
        "--multiple-episode-rate",
        args.multiple_episode_rate,
    )

    validate_rate(
        parser,
        "--missing-discharge-rate",
        args.missing_discharge_rate,
    )

    validate_rate(
        parser,
        "--missing-date-rate",
        args.missing_date_rate,
    )

    validate_rate(
        parser,
        "--excluded-rate",
        args.excluded_rate,
    )

    validate_rate(
        parser,
        "--unmapped-rate",
        args.unmapped_rate,
    )

    validate_rate(
        parser,
        "--duplicate-rate",
        args.duplicate_rate,
    )

    validate_rate(
        parser,
        "--conflicting-frequency-rate",
        args.conflicting_frequency_rate,
    )

    validate_rate(
        parser,
        "--invalid-admission-rate",
        args.invalid_admission_rate,
    )

    generate_sample_data(
        output_dir=args.output_dir,
        patients=args.patients,
        mean_medications=(
            args.mean_medications
        ),
        max_episodes=(
            args.max_episodes
        ),
        multiple_episode_rate=(
            args.multiple_episode_rate
        ),
        missing_discharge_rate=(
            args.missing_discharge_rate
        ),
        missing_date_rate=(
            args.missing_date_rate
        ),
        excluded_rate=(
            args.excluded_rate
        ),
        unmapped_rate=(
            args.unmapped_rate
        ),
        duplicate_rate=(
            args.duplicate_rate
        ),
        conflicting_frequency_rate=(
            args.conflicting_frequency_rate
        ),
        invalid_admission_rate=(
            args.invalid_admission_rate
        ),
        omit_ingredient_column=(
            args.omit_ingredient_column
        ),
        seed=args.seed,
    )


if __name__ == "__main__":
    main()
