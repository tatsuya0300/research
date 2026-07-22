from __future__ import annotations

from pathlib import Path

import pandas as pd


def main():
    base_dir = Path(
        "sample_dbi_data"
    )

    result_dir = (
        base_dir / "result"
    )

    actual_snapshots = pd.read_csv(
        result_dir
        / "dbi_snapshot_long.csv",
        encoding="utf-8-sig",
        dtype={
            "episode_id": str,
            "患者ID": str,
        },
    )

    expected_snapshots = pd.read_csv(
        base_dir
        / "sample_expected_snapshots.csv",
        encoding="utf-8-sig",
        dtype={
            "episode_id": str,
            "患者ID": str,
        },
    )

    actual_episodes = pd.read_csv(
        result_dir
        / "dbi_episode.csv",
        encoding="utf-8-sig",
        dtype={
            "episode_id": str,
            "患者ID": str,
        },
    )

    expected_episodes = pd.read_csv(
        base_dir
        / "sample_expected_episodes.csv",
        encoding="utf-8-sig",
        dtype={
            "episode_id": str,
            "患者ID": str,
        },
    )

    # --------------------------------------------------------
    # Snapshot validation
    # --------------------------------------------------------

    snapshot_comparison = (
        expected_snapshots.merge(
            actual_snapshots,
            on=[
                "episode_id",
                "患者ID",
                "timepoint",
                "evaluation_date",
            ],
            how="left",
            validate="one_to_one",
        )
    )

    if snapshot_comparison[
        "dbi_total"
    ].isna().any():
        missing = snapshot_comparison.loc[
            snapshot_comparison[
                "dbi_total"
            ].isna(),
            [
                "episode_id",
                "timepoint",
            ],
        ]

        raise AssertionError(
            "期待した時点結果がありません。\n"
            + missing.to_string(index=False)
        )

    tolerance = 1e-10

    snapshot_checks = {
        "dbi_total": (
            "expected_dbi_total"
        ),
        "dbi_anticholinergic": (
            "expected_dbi_anticholinergic"
        ),
        "dbi_sedative": (
            "expected_dbi_sedative"
        ),
        "dbi_drug_count": (
            "expected_dbi_drug_count"
        ),
    }

    for actual_column, expected_column in (
        snapshot_checks.items()
    ):
        difference = (
            pd.to_numeric(
                snapshot_comparison[
                    actual_column
                ],
                errors="coerce",
            )
            -
            pd.to_numeric(
                snapshot_comparison[
                    expected_column
                ],
                errors="coerce",
            )
        ).abs()

        failed = difference > tolerance

        if failed.any():
            failure_rows = (
                snapshot_comparison.loc[
                    failed,
                    [
                        "episode_id",
                        "timepoint",
                        expected_column,
                        actual_column,
                    ],
                ]
            )

            raise AssertionError(
                f"{actual_column}が期待値と"
                "一致しません。\n"
                + failure_rows.to_string(
                    index=False
                )
            )

    # --------------------------------------------------------
    # Episode validation
    # --------------------------------------------------------

    episode_comparison = (
        expected_episodes.merge(
            actual_episodes,
            on=[
                "episode_id",
                "患者ID",
            ],
            how="left",
            validate="one_to_one",
        )
    )

    if episode_comparison[
        "mean_daily_dbi"
    ].isna().any():
        missing = episode_comparison.loc[
            episode_comparison[
                "mean_daily_dbi"
            ].isna(),
            ["episode_id"],
        ]

        raise AssertionError(
            "期待したエピソード結果が"
            "ありません。\n"
            + missing.to_string(index=False)
        )

    episode_checks = {
        "hospital_days_inclusive": (
            "hospital_days_inclusive_x"
            if (
                "hospital_days_inclusive_x"
                in episode_comparison.columns
            )
            else "hospital_days_inclusive"
        ),
        "dbi_burden_days": (
            "expected_dbi_burden_days"
        ),
        "mean_daily_dbi": (
            "expected_mean_daily_dbi"
        ),
    }

    # merge後に同名列がx/yになった場合への対応
    if (
        "hospital_days_inclusive_x"
        in episode_comparison.columns
        and
        "hospital_days_inclusive_y"
        in episode_comparison.columns
    ):
        expected_hospital_days_column = (
            "hospital_days_inclusive_x"
        )

        actual_hospital_days_column = (
            "hospital_days_inclusive_y"
        )
    else:
        expected_hospital_days_column = (
            "hospital_days_inclusive"
        )

        actual_hospital_days_column = (
            "hospital_days_inclusive"
        )

    hospital_day_difference = (
        pd.to_numeric(
            episode_comparison[
                expected_hospital_days_column
            ],
            errors="coerce",
        )
        -
        pd.to_numeric(
            episode_comparison[
                actual_hospital_days_column
            ],
            errors="coerce",
        )
    ).abs()

    if (
        hospital_day_difference
        > tolerance
    ).any():
        raise AssertionError(
            "在院日数が期待値と一致しません。"
        )

    numeric_checks = [
        (
            "dbi_burden_days",
            "expected_dbi_burden_days",
        ),
        (
            "mean_daily_dbi",
            "expected_mean_daily_dbi",
        ),
    ]

    for actual_column, expected_column in (
        numeric_checks
    ):
        difference = (
            pd.to_numeric(
                episode_comparison[
                    actual_column
                ],
                errors="coerce",
            )
            -
            pd.to_numeric(
                episode_comparison[
                    expected_column
                ],
                errors="coerce",
            )
        ).abs()

        failed = difference > tolerance

        if failed.any():
            failure_rows = (
                episode_comparison.loc[
                    failed,
                    [
                        "episode_id",
                        expected_column,
                        actual_column,
                    ],
                ]
            )

            raise AssertionError(
                f"{actual_column}が期待値と"
                "一致しません。\n"
                + failure_rows.to_string(
                    index=False
                )
            )

    print(
        "All DBI sample validations passed."
    )


if __name__ == "__main__":
    main()
