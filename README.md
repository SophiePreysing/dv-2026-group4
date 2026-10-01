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

This project uses DUO's 2024–2025 doorstroomtoets data to examine how closely the secondary-school advice that primary schools give their groep 8 pupils matches those pupils' test results. For each school, we compare the proportion of pupils receiving XYZ advice (TBC) to the proportion reaching the higher reference levels in maths (1S) and reading (2F). We then test whether this relationship depends on the school's schoolweging, the Education Inspectorate's measure of how disadvantaged its pupil population is. With our final visualization, we want to show whether two schools with similar test results give similar advice regardless of who their pupils are, or whether schools with more disadvantaged populations systematically advise lower tracks. Answering this speaks directly to the current public debate about fairness in the transition to secondary education.

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
4. Knit `report/DV-Assignment2-Part2-GroupX.Rmd` (the final report). Knitting runs both scripts above for you.
