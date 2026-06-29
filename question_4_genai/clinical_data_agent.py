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


# Online locations to try for the AE dataset.
# The first valid file found will be loaded.
# This version points to the CDISC pilot project ae.xpt file first.
DATASET_URLS = [
    "https://github.com/cdisc-org/sdtm-adam-pilot-project/raw/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/tabulations/sdtm/ae.xpt",
    "https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/tabulations/sdtm/ae.xpt",
    "https://raw.githubusercontent.com/cdisc-org/sdtm-adam-pilot-project/master/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01/tabulations/sdtm/AE.XPT",
    "https://pharmaverse.r-universe.dev/datasets/pharmaversesdtm/ae.csv",
    "https://pharmaverse.r-universe.dev/datasets/pharmaversesdtm::ae.csv",
    "https://pharmaverse.r-universe.dev/pharmaversesdtm/data/ae.csv",
    "https://pharmaverse.github.io/pharmaversesdtm/reference/ae.csv",
]

# Local output folder for generated query files.
OUTPUT_DIR = Path("output")

# Prompt text that teaches the LLM how to map user questions to AE variables.
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


# Structured response expected from the model.
# This tells the agent exactly which field to filter and what value to search for.
class QuerySchema(BaseModel):
    target_column: str = Field(description="Column to filter, such as AESEV, AETERM, or AESOC")
    filter_value: str = Field(description="Value to search for in the target column")


# Standardize column names so comparisons are robust to casing and spacing.
def normalize_colname(value: str) -> str:
    return str(value).strip().upper()


# Standardize text values for matching.
def normalize_value(value: str) -> str:
    return str(value).strip().upper()


# Make sure the output folder exists before saving any files.
def ensure_output_dir() -> None:
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)


# Load the AE dataset from the CDISC pilot project first.
# If that fails, try the other online sources.
# If all online sources fail, fall back to demo data so the script still runs.
def load_adae_from_online_source() -> pd.DataFrame:
    last_error = None
    headers = {"User-Agent": "Mozilla/5.0"}

    for url in DATASET_URLS:
        try:
            response = requests.get(url, headers=headers, timeout=30)
            response.raise_for_status()

            # Read the file depending on the extension.
            # The CDISC pilot file is ae.xpt, so pandas.read_sas is used there.
            if url.lower().endswith(".xpt"):
                df = pd.read_sas(StringIO(response.text), format="xport")
            else:
                df = pd.read_csv(StringIO(response.text))

            # Decode byte columns returned by read_sas, if needed.
            if isinstance(df.columns[0], bytes):
                df.columns = [c.decode("utf-8", errors="ignore") for c in df.columns]

            df.columns = [normalize_colname(c) for c in df.columns]
            return df
        except Exception as err:
            last_error = err

    # Fallback demo data if no online source can be accessed.
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


# Main agent class that handles parsing and filtering.
# The workflow is:
# 1. prepare the dataframe,
# 2. interpret the user's question,
# 3. filter the data,
# 4. return subject counts and IDs.
class ClinicalTrialDataAgent:
    def __init__(self, df: pd.DataFrame, api_key: Optional[str] = None):
        # Keep a clean copy of the dataframe so the original is not modified.
        self.df = df.copy()
        self.df.columns = [normalize_colname(c) for c in self.df.columns]

        # Map normalized column names back to the actual dataframe columns.
        self.col_map = {c.upper(): c for c in self.df.columns}

        # API key can come from the constructor or environment variable.
        self.api_key = api_key or os.getenv("OPENAI_API_KEY")
        self.structured_llm = None

        # If LangChain OpenAI is installed and a key exists, use structured LLM output.
        if ChatOpenAI is not None and self.api_key:
            llm = ChatOpenAI(
                model="gpt-4o-mini",
                temperature=0,
                api_key=self.api_key,
            )
            self.structured_llm = llm.with_structured_output(QuerySchema)

    # Rule-based fallback parser used when no LLM is available.
    # It maps common words in the question to the most likely AE column.
    def mock_parse(self, question: str) -> Dict[str, str]:
        q = question.lower()

        # Severity-related words should map to AESEV.
        if any(word in q for word in ["severity", "intensity", "mild", "moderate", "severe"]):
            if "mild" in q:
                return {"target_column": "AESEV", "filter_value": "MILD"}
            if "moderate" in q:
                return {"target_column": "AESEV", "filter_value": "MODERATE"}
            if "severe" in q:
                return {"target_column": "AESEV", "filter_value": "SEVERE"}
            return {"target_column": "AESEV", "filter_value": "MODERATE"}

        # Body-system words should map to AESOC.
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

        # Known AE terms are mapped to AETERM.
        common_terms = ["headache", "nausea", "pain", "rash", "dizziness", "fatigue", "vomiting"]
        for term in common_terms:
            if term in q:
                return {"target_column": "AETERM", "filter_value": term.upper()}

        # Default behavior: search the whole question text in AETERM.
        return {"target_column": "AETERM", "filter_value": question.strip().upper()}

    # Convert the user's question into structured search instructions.
    # If an LLM is available, it is used; otherwise the rule-based parser runs.
    def parse_question(self, question: str) -> Dict[str, str]:
        if self.structured_llm is not None:
            parsed = self.structured_llm.invoke(SCHEMA_TEXT + "\nUser question: " + question)
            return {
                "target_column": normalize_colname(parsed.target_column),
                "filter_value": normalize_value(parsed.filter_value),
            }
        return self.mock_parse(question)

    # Apply the parsed search instruction to the dataframe.
    # Returns the number of unique subjects, the matching subject IDs, and matched rows.
    def execute_filter(self, parsed: Dict[str, str]) -> Tuple[int, List[str], pd.DataFrame]:
        target_column = normalize_colname(parsed["target_column"])
        filter_value = normalize_value(parsed["filter_value"])

        # Confirm the requested column exists before filtering.
        if target_column not in self.col_map:
            raise ValueError(f"Target column '{target_column}' not found in dataframe.")

        # USUBJID is required to return distinct subject identifiers.
        if "USUBJID" not in self.df.columns:
            raise ValueError("USUBJID column is required in the dataset.")

        # Convert the target column to uppercase text so matching is consistent.
        actual_col = self.col_map[target_column]
        series = self.df[actual_col].astype(str).str.upper().str.strip()

        # Perform a case-insensitive substring search.
        # re.escape protects against special characters in the query.
        mask = series.str.contains(re.escape(filter_value), na=False)

        # Subset the dataframe to only matching rows.
        matched_df = self.df.loc[mask].copy()

        # Extract unique subject IDs from the filtered rows.
        matched_ids = matched_df["USUBJID"].dropna().astype(str).unique().tolist()
        unique_count = len(matched_ids)

        return unique_count, matched_ids, matched_df

    # End-to-end method: parse the question and then execute the filter.
    # This is the main method used by the example test queries.
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


# Print a compact summary of one query result.
# This helps the user quickly see what was parsed and which subjects matched.
def pretty_print_result(idx: int, result: Dict[str, object]) -> None:
    print("\n" + "=" * 74)
    print(f"Query {idx}: {result['question']}")
    print("-" * 74)
    print(f"Parsed JSON     : {result['parsed']}")
    print(f"Unique subjects : {result['unique_subject_count']}")
    print(f"USUBJIDs        : {', '.join(result['matching_usubjids']) if result['matching_usubjids'] else 'None'}")
    print("=" * 74)


# Save the filtered rows and a text summary for each query.
# This creates submission-friendly artifacts in the output folder.
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


# Main script execution.
# This section only runs when the file is executed directly.
# It loads the AE dataset, creates the agent, runs three test queries,
# prints the results, and saves the output files.
if __name__ == "__main__":
    df = load_adae_from_online_source()
    agent = ClinicalTrialDataAgent(df)

    # Example queries demonstrate how natural language gets mapped
    # to a target column and filter value.
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