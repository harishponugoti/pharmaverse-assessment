import os
import re
from io import StringIO
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd
import requests
from pydantic import BaseModel, Field

try:
    from langchain_openai import ChatOpenAI
except Exception:
    ChatOpenAI = None


# List of possible online sources for the AE dataset.
# The code tries these in order and uses the first one that works.
DATASET_URLS = [
    "https://pharmaverse.r-universe.dev/datasets/pharmaversesdtm/ae.csv",
    "https://pharmaverse.r-universe.dev/datasets/pharmaversesdtm::ae.csv",
    "https://pharmaverse.r-universe.dev/pharmaversesdtm/data/ae.csv",
    "https://pharmaverse.github.io/pharmaversesdtm/reference/ae.csv",
]

# Folder where query outputs and summaries will be written.
OUTPUT_DIR = Path("output")

# Prompt text that tells the LLM what the dataset contains and how to map
# a natural-language question to the correct AE column.
SCHEMA_TEXT = """
You are a clinical trial adverse events data assistant.

Dataset columns:
- USUBJID: Unique subject identifier.
- AETERM: Adverse event term or condition reported.
- AESEV: Severity of the adverse event. Typical values: MILD, MODERATE, SEVERE.
- AESOC: Body system / system organ class.
- AESER: Serious event flag.
- AEREL: Relationship to study drug.

Mapping rules:
- severity, intensity, mild, moderate, severe -> AESEV
- event/condition names such as headache, nausea, pain, rash -> AETERM
- body system/SOC names such as cardiac, skin, gastrointestinal -> AESOC

Return only valid structured output with:
- target_column
- filter_value
"""


# Structured output model expected from the LLM.
# The agent uses these two fields to decide which column to filter
# and what value to search for.
class QuerySchema(BaseModel):
    target_column: str = Field(description="Column to filter, such as AESEV, AETERM, or AESOC")
    filter_value: str = Field(description="Value to search for in the target column")


# Standardize column names so the code can safely compare columns
# regardless of original capitalization or extra spaces.
def normalize_colname(value: str) -> str:
    return str(value).strip().upper()


# Standardize filter values for matching.
# This makes the matching case-insensitive and consistent.
def normalize_value(value: str) -> str:
    return str(value).strip().upper()


# Create the output folder if it does not already exist.
def ensure_output_dir() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


# Try to load the AE dataset from online pharmaverse sources.
# If all web sources fail, fall back to a small demo dataset so the
# rest of the script can still run and demonstrate the logic.
def load_adae_from_online_source() -> pd.DataFrame:
    last_error = None
    headers = {"User-Agent": "Mozilla/5.0"}

    for url in DATASET_URLS:
        try:
            response = requests.get(url, headers=headers, timeout=30)
            response.raise_for_status()
            df = pd.read_csv(StringIO(response.text))
            df.columns = [normalize_colname(c) for c in df.columns]
            return df
        except Exception as err:
            last_error = err

    # Fallback data is used only if the online download fails.
    # This keeps the script runnable in restricted environments.
    fallback_data = {
        "USUBJID": ["01-001", "01-002", "01-003", "01-004", "01-005", "01-006"],
        "AETERM": ["Headache", "Nausea", "Rash", "Pain", "Fatigue", "Headache"],
        "AESEV": ["MODERATE", "MILD", "SEVERE", "MODERATE", "MILD", "MODERATE"],
        "AESOC": ["NERVOUS SYSTEM", "GASTROINTESTINAL", "SKIN", "GENERAL", "GENERAL", "NERVOUS SYSTEM"],
    }
    df = pd.DataFrame(fallback_data)
    df.columns = [normalize_colname(c) for c in df.columns]
    print("Warning: online AE dataset could not be loaded. Using fallback demo data.")
    return df


# Main agent class that handles three steps:
# 1) prepare the dataset,
# 2) parse the user's natural-language question,
# 3) execute the actual dataframe filter.
class ClinicalTrialDataAgent:
    def __init__(self, df: pd.DataFrame, api_key: Optional[str] = None):
        # Keep a private copy so the original dataframe is not modified.
        self.df = df.copy()
        self.df.columns = [normalize_colname(c) for c in self.df.columns]

        # Build a lookup from normalized column names to actual dataframe columns.
        # This is useful if the dataframe was loaded with unusual formatting.
        self.col_map = {c.upper(): c for c in self.df.columns}

        # Read the API key from the argument or environment variable.
        # If no key is available, the code will use the mock parser.
        self.api_key = api_key or os.getenv("OPENAI_API_KEY")
        self.structured_llm = None

        # If LangChain OpenAI is available and an API key exists,
        # create a structured-output LLM that returns QuerySchema objects.
        if ChatOpenAI is not None and self.api_key:
            llm = ChatOpenAI(
                model="gpt-4o-mini",
                temperature=0,
                api_key=self.api_key,
            )
            self.structured_llm = llm.with_structured_output(QuerySchema)

    # Fallback parser used when no LLM is available.
    # This function maps common words to one of the known AE columns.
    def mock_parse(self, question: str) -> Dict[str, str]:
        q = question.lower()

        # Severity-related wording maps to AESEV.
        if any(word in q for word in ["severity", "intensity", "mild", "moderate", "severe"]):
            if "mild" in q:
                return {"target_column": "AESEV", "filter_value": "MILD"}
            if "moderate" in q:
                return {"target_column": "AESEV", "filter_value": "MODERATE"}
            if "severe" in q:
                return {"target_column": "AESEV", "filter_value": "SEVERE"}
            return {"target_column": "AESEV", "filter_value": "MODERATE"}

        # SOC/body-system wording maps to AESOC.
        if any(word in q for word in ["cardiac", "skin", "gastrointestinal", "respiratory", "nervous"]):
            if "cardiac" in q:
                return {"target_column": "AESOC", "filter_value": "CARDIAC"}
            if "skin" in q:
                return {"target_column": "AESOC", "filter_value": "SKIN"}
            if "gastrointestinal" in q:
                return {"target_column": "AESOC", "filter_value": "GASTROINTESTINAL"}
            if "respiratory" in q:
                return {"target_column": "AESOC", "filter_value": "RESPIRATORY"}
            if "nervous" in q:
                return {"target_column": "AESOC", "filter_value": "NERVOUS"}

        # Specific adverse event terms map to AETERM.
        common_terms = ["headache", "nausea", "pain", "rash", "dizziness", "fatigue", "vomiting"]
        for term in common_terms:
            if term in q:
                return {"target_column": "AETERM", "filter_value": term.upper()}

        # Default fallback: treat the whole question as an AE term search.
        return {"target_column": "AETERM", "filter_value": question.strip().upper()}

    # Parse the user's question into structured instructions.
    # If an LLM is available, use it; otherwise use the rule-based parser.
    def parse_question(self, question: str) -> Dict[str, str]:
        if self.structured_llm is not None:
            parsed = self.structured_llm.invoke(SCHEMA_TEXT + "\nUser question: " + question)
            return {
                "target_column": normalize_colname(parsed.target_column),
                "filter_value": normalize_value(parsed.filter_value),
            }
        return self.mock_parse(question)

    # Use the parsed instructions to filter the dataframe.
    # The function returns:
    # - the count of unique subjects,
    # - the list of matching USUBJIDs,
    # - the full matched rows.
    def execute_filter(self, parsed: Dict[str, str]) -> Tuple[int, List[str], pd.DataFrame]:
        target_column = normalize_colname(parsed["target_column"])
        filter_value = normalize_value(parsed["filter_value"])

        # Ensure the requested column actually exists in the dataset.
        if target_column not in self.col_map:
            raise ValueError(f"Target column '{target_column}' not found in dataframe.")

        # The subject identifier is required so the code can return unique subjects.
        if "USUBJID" not in self.df.columns:
            raise ValueError("USUBJID column is required in the dataset.")

        # Convert the actual column to uppercase text for consistent matching.
        actual_col = self.col_map[target_column]
        series = self.df[actual_col].astype(str).str.upper().str.strip()

        # Search for the parsed filter value anywhere in the target column.
        # re.escape ensures special characters in the query do not break matching.
        mask = series.str.contains(re.escape(filter_value), na=False)

        # Collect all rows that match the condition.
        matched_df = self.df.loc[mask].copy()

        # Extract unique subject IDs from the matched rows.
        matched_ids = matched_df["USUBJID"].dropna().astype(str).unique().tolist()
        unique_count = len(matched_ids)

        return unique_count, matched_ids, matched_df

    # End-to-end helper that runs parsing and filtering together.
    # This is the main entry point used by the test queries.
    def ask(self, question: str) -> Dict[str, object]:
        parsed = self.parse_question(question)
        count, ids, matched_df = self.execute_filter(parsed)

        return {
            "question": question,
            "parsed": parsed,
            "unique_subject_count": count,
            "matching_usubjids": ids,
            "matched_rows": matched_df,
        }


# Print a readable summary for each query result.
# This keeps the console output compact while still showing the key outputs.
def pretty_print_result(idx: int, result: Dict[str, object]) -> None:
    print("\n" + "=" * 74)
    print(f"Query {idx}: {result['question']}")
    print("-" * 74)
    print(f"Parsed JSON     : {result['parsed']}")
    print(f"Unique subjects : {result['unique_subject_count']}")
    print(f"USUBJIDs        : {', '.join(result['matching_usubjids']) if result['matching_usubjids'] else 'None'}")
    print("=" * 74)


# Save each query's matched rows and summary text to files.
# This gives a reproducible artifact for review or submission.
def save_result_files(idx: int, result: Dict[str, object]) -> None:
    ensure_output_dir()

    csv_path = OUTPUT_DIR / f"query_{idx}_matched_rows.csv"
    txt_path = OUTPUT_DIR / f"query_{idx}_summary.txt"

    matched_df = result["matched_rows"]
    matched_df.to_csv(csv_path, index=False)

    lines = [
        f"Query {idx}: {result['question']}",
        f"Parsed JSON: {result['parsed']}",
        f"Unique subjects: {result['unique_subject_count']}",
        f"USUBJIDs: {', '.join(result['matching_usubjids']) if result['matching_usubjids'] else 'None'}",
    ]

    with open(txt_path, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))


# Script entry point.
# When the file is run directly, it loads the dataset, creates the agent,
# runs three sample queries, prints the results, and saves the output files.
if __name__ == "__main__":
    df = load_adae_from_online_source()
    agent = ClinicalTrialDataAgent(df)

    # Three example questions are used to demonstrate the parsing flow
    # and the dataframe filtering behavior.
    test_queries = [
        "Give me the subjects who had Adverse events of Moderate severity.",
        "Show subjects with Headache.",
        "List subjects with Cardiac events.",
    ]

    for i, query in enumerate(test_queries, start=1):
        result = agent.ask(query)
        pretty_print_result(i, result)
        save_result_files(i, result)

    print("\nAll results have been saved inside the 'output' folder.")