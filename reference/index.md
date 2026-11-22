# Package index

## Launch the app

- [`run_databox()`](https://pawanramamali.github.io/databoxR/reference/run_databox.md)
  : Run the databoxR Shiny Application

## Reading and caching data

- [`read_sdtm_adam()`](https://pawanramamali.github.io/databoxR/reference/read_sdtm_adam.md)
  : Read SDTM/ADaM Datasets with Enhanced Features
- [`read_with_cache()`](https://pawanramamali.github.io/databoxR/reference/read_with_cache.md)
  : Enhanced file reader with caching
- [`get_file_preview()`](https://pawanramamali.github.io/databoxR/reference/get_file_preview.md)
  : Get File Preview Information
- [`safe_select_columns()`](https://pawanramamali.github.io/databoxR/reference/safe_select_columns.md)
  : Safe Column Selection
- [`validate_dataset()`](https://pawanramamali.github.io/databoxR/reference/validate_dataset.md)
  : Validate Dataset for Analysis
- [`DataCache`](https://pawanramamali.github.io/databoxR/reference/DataCache.md)
  : Simple Data Cache for Performance

## Subject profile

- [`list_subjects()`](https://pawanramamali.github.io/databoxR/reference/list_subjects.md)
  : List every subject present across a set of domains
- [`subject_records()`](https://pawanramamali.github.io/databoxR/reference/subject_records.md)
  : Pull one subject's records from every domain
- [`subject_overview()`](https://pawanramamali.github.io/databoxR/reference/subject_overview.md)
  : Count a subject's records per domain
- [`subject_demographics()`](https://pawanramamali.github.io/databoxR/reference/subject_demographics.md)
  : Demographics summary for one subject
- [`subject_timeline()`](https://pawanramamali.github.io/databoxR/reference/subject_timeline.md)
  : Merged chronological timeline for one subject
- [`subject_kpis()`](https://pawanramamali.github.io/databoxR/reference/subject_kpis.md)
  : Headline counts for one subject
- [`subject_profile_code()`](https://pawanramamali.github.io/databoxR/reference/subject_profile_code.md)
  : Reproducible R code that extracts one subject across domains

## Subject narrative

- [`subject_narrative()`](https://pawanramamali.github.io/databoxR/reference/subject_narrative.md)
  : Generate a grounded narrative for one subject
- [`subject_narrative_facts()`](https://pawanramamali.github.io/databoxR/reference/subject_narrative_facts.md)
  : Grounded facts for one subject
- [`subject_narrative_facts_text()`](https://pawanramamali.github.io/databoxR/reference/subject_narrative_facts_text.md)
  : Render subject facts as compact text
- [`subject_narrative_claude()`](https://pawanramamali.github.io/databoxR/reference/subject_narrative_claude.md)
  : Narrate one subject's facts with Claude via ellmer

## Natural-language query

- [`nl_query()`](https://pawanramamali.github.io/databoxR/reference/nl_query.md)
  : Answer a natural-language question against loaded datasets
- [`nlq_schema()`](https://pawanramamali.github.io/databoxR/reference/nlq_schema.md)
  : Describe loaded datasets for a query planner
- [`nlq_validate_plan()`](https://pawanramamali.github.io/databoxR/reference/nlq_validate_plan.md)
  : Validate a query plan against the loaded data
- [`nlq_execute_plan()`](https://pawanramamali.github.io/databoxR/reference/nlq_execute_plan.md)
  : Execute a query plan deterministically
- [`nlq_plan_code()`](https://pawanramamali.github.io/databoxR/reference/nlq_plan_code.md)
  : Generate reproducible R code for a query plan
- [`nlq_propose_claude()`](https://pawanramamali.github.io/databoxR/reference/nlq_propose_claude.md)
  : Propose a query plan with Claude via ellmer

## define.xml and conformance

- [`load_define()`](https://pawanramamali.github.io/databoxR/reference/load_define.md)
  : Load a define.xml into a metacore object
- [`define_datasets()`](https://pawanramamali.github.io/databoxR/reference/define_datasets.md)
  : Dataset-level metadata from a define
- [`define_var_metadata()`](https://pawanramamali.github.io/databoxR/reference/define_var_metadata.md)
  : Variable-level metadata (data dictionary) from a define
- [`compare_define_to_data()`](https://pawanramamali.github.io/databoxR/reference/compare_define_to_data.md)
  : Compare a loaded dataset against its define metadata (smart hints)
- [`check_conformance()`](https://pawanramamali.github.io/databoxR/reference/check_conformance.md)
  : Metadata-driven conformance checks against a define

## Demo data

- [`generate_demo_data()`](https://pawanramamali.github.io/databoxR/reference/generate_demo_data.md)
  : Generate Synthetic SDTM Demo Data
- [`get_demo_dataset()`](https://pawanramamali.github.io/databoxR/reference/get_demo_dataset.md)
  : Get Demo Dataset by Name
