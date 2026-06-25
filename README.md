# Review of Stressor Linkages: Skagerrak and Kattegat

A Shiny app summarising a Web of Science literature review of stressor relationships
acting in the Skagerrak, Kattegat, Baltic, and North Seas. It renders alluvial
stressor -> effect -> endpoint pathways and an interactive map of study locations.

Live app: https://gghill.shinyapps.io/Review_Stressors_SkagerrakKattegat/

## Layout

- `app/` - the Shiny app (`app.R`) and its data (`review_results_shiny_250626.csv`)
- `dependencies.R` - R packages installed into the Docker image
- `Dockerfile` / `docker-compose.yml` - container build (based on `rocker/shiny`)
- `.github/` - build & deploy GitHub Actions (modelled on NIVA's WATERS pipeline)
- `deployment/` - Kustomize manifests for the test and prod Kubernetes namespaces

## Run locally

With R:

```r
shiny::runApp("app")
```

With Docker:

```bash
docker compose up --build   # then open http://localhost:3838
```

## Deployment

Pushes to `main` build a container image and deploy to the test cluster; the prod
deploy is triggered manually (`workflow_dispatch`). Both rely on NIVA's
workload-identity secrets and self-hosted runners being configured for the repo.

Questions: please open an issue on this repository.
