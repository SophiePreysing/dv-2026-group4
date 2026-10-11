<!--
  Replace the title, group members, research question and "About this
  project" below with your own project description. Keep the "Cloning" and
  "Reproducing" sections, and update them if you change how the project
  runs. Keep it short — a few lines per section is enough. This README is the
  front page of your repo, not the report itself (that's in report/);
  it just orients anyone (including us, grading) opening the repo for the
  first time.
-->

# Test Results and Secondary-School Advice in Dutch Primary Schools

**Group members:**

- Sophie Preysing
- Sander van Nieuwenhuijzen
- Natasha Medved

**Research question:** RQ3. Do schools with similar test results give similar secondary-school advice?

**Level:** Inference

## About this project

This project uses DUO's 2024–2025 doorstroomtoets data for about 5,700 regular primary schools to examine how closely the secondary-school advice that schools give their groep 8 pupils matches those pupils' test results.

For each school, we relate the share of pupils advised **HAVO or higher** to the school's **test results**: the average share of pupils reaching the national target level in maths (1S), reading (2F) and language (2F). We then test whether this relationship depends on the school's **schoolweging**, the Education Inspectorate's measure of how disadvantaged its pupil population is. We use a weighted linear regression with an interaction between test results and schoolweging, controlling for test provider.

Our final visualization shows whether two schools with similar test results give similar advice regardless of who their pupils are. It doesn't: at the same test results, schools with more disadvantaged pupils advise fewer pupils to HAVO or higher (about 17 fewer per 100 at an average test result), and this gap grows as test results rise. The finding holds across several robustness checks, but it is based on school-level data and shows an association, not a cause.

This speaks directly to the current public debate about fairness in the transition to secondary education.

## AI guidelines

Our team guidelines for using AI are in [`ai-context.Rmd`](ai-context.Rmd). In short:

- **Context:** every AI session starts with our research question, the codebook, known data quirks (e.g. `"<5"` counts, one row per school), the decisions we have already made, and our code style. We never share passwords, personal data or non-public data.
- **Constraints:** the AI must ask before making any data decision we haven't made yet, must not compare raw scores across providers, make claims about individual pupils or use causal language, and must list every transformation, filter and assumption it uses.
- **Verification:** before a result is used, we check row counts and value ranges, a teammate reproduces key numbers independently, we rerun with different `"<5"` values and other sensitivity checks, and a second team member signs off.

## Cloning this project

To get a copy of this project on your own computer, as an RStudio project:

1. On this repository's GitHub page, click the green `<> Code` button, choose **HTTPS**, and copy the URL.
2. In RStudio, make sure no project is open (top right: `Project: (None)`).
3. Go to `File` > `New Project` > `Version Control` > `Git`, paste the URL into `Repository URL`, choose where the project should live on your computer, and click `Create Project`.

RStudio opens the project, with a `Git` tab next to your `Environment` pane. Full instructions (including how to set up Git and GitHub on your computer first) are in the course's [Working with Git](https://ann1ejohansson.github.io/data-visualization-2026/documents/git-workflow.html) tutorial.

## Reproducing this project

1. Open the project in RStudio (double-click its `.Rproj` file, or clone it as described above).
2. Run `scripts/00-packages.R` to install and load the packages this project uses.
3. Run `scripts/01-get-data.R` once to download the data into `data/raw/`.
4. Knit `report/DV-Assignment2-Final-Report.Rmd` (the final report). If the cleaned data does not exist yet, knitting builds it first by running `scripts/03-clean-data.R`.