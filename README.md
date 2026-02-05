# Capital Spend Forecasting for State Infrastructure Projects

This repository contains a real-world analytics case study focused on forecasting and monitoring capital spending for large-scale public infrastructure projects. The work was completed as part of an academic-sponsored engagement using anonymized data.

## Business Problem
Public-sector agencies managing capital construction projects require accurate monthly and annual cash-flow forecasts to effectively allocate budgets, identify at-risk projects early, and maintain fiscal oversight.

Existing forecasting approaches relied heavily on pre-project estimates and expert judgment, which often proved overly optimistic and provided limited visibility once projects were underway.

## Objectives
- Improve near-term and long-term capital spend forecasting using historical project data
- Identify atypical cash-flow patterns earlier in the project lifecycle
- Enable project-level and portfolio-level monitoring through interactive dashboards

## Data Overview
- Historical capital project spending through completion
- Project attributes including size, duration, building type, procurement method, and pay class
- Data has been anonymized and aggregated for portfolio use

## Analytical Approach
- Standardized project timelines using the first 95% of total project cost to allow consistent comparison
- Grouped projects into peer clusters based on cost, size, and duration
- Analyzed cash-flow behavior by project cluster and pay class
- Built a short-term forecasting prototype to estimate near-term monthly spending
- Designed an interactive Tableau dashboard for exploration and monitoring

## Key Insights
- Construction-phase spending dominates total project cost across nearly all projects
- Projects exhibit distinct spending archetypes that enable meaningful peer benchmarking
- Cluster-based comparisons surface abnormal cash-flow behavior earlier than raw spend tracking
- Short-term forecasts are feasible but vary in accuracy by project size and complexity

## Results
- Enabled project-level and portfolio-level visibility into capital cash-flow behavior
- Provided a repeatable framework for benchmarking new projects against historical peers
- Demonstrated feasibility of short-term forecasting to support monthly budget planning
- Delivered an interactive dashboard to support exploration, monitoring, and oversight

## Tools Used
- SQL (data preparation)
- R / Python (analysis and forecasting)
- Tableau (interactive dashboards)

## Dashboard
A public version of the dashboard is available on Tableau Public:  
**[https://public.tableau.com/app/profile/evan.foster6874/vizzes]**

## Notes
- Source data is not included due to confidentiality constraints
- This repository focuses on methodology, structure, and analytical approach
