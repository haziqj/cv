# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Haziq Jamil's CV, written in R Markdown and paginated with `pagedown`. One
source file (`index.Rmd`) builds both a PDF (`haziq_cv.pdf`) and an HTML page
that is deployed to GitHub Pages (haziqj.ml/cv).

## Build commands

```sh
# PDF (pdf_mode = TRUE is the default param). Renders index.html, then Chrome
# prints it to index.pdf. The setup chunk renames index.pdf to haziq_cv.pdf,
# but only at the start of the *next* render (in CI, the HTML render below).
Rscript -e 'pagedown::chrome_print(rmarkdown::render("index.Rmd", params = list(pdf_mode = TRUE)))'

# HTML for the website (renders the animated gganimate logo, which is slow)
Rscript -e 'rmarkdown::render("index.Rmd", output_dir = "site", params = list(pdf_mode = FALSE))'

# Premium-residency variant (trimmed copy of index.Rmd, writes cv_pr.pdf)
Rscript -e 'pagedown::chrome_print(rmarkdown::render("cv_pr.Rmd"))'
```

Knitting in RStudio also works: the YAML sets `knit: pagedown::chrome_print`.
Building needs Chrome/Chromium plus the packages installed in
`.github/workflows/build_pdf_cv.yml`: tidyverse, here, gganimate, gifski,
transformr, pagedown (GitHub), and `iprior` (GitHub, `haziqj/iprior`, needed by
the logo). `sync_w_gsheets.R` also needs `googlesheets4`. There are no tests
or linters.

## How it fits together

- **Data comes from Google Sheets, not the repo.** The setup chunk of
  `index.Rmd` sources `sync_w_gsheets.R`, which downloads the `cv_entries` and
  `publications` sheets (public sheet, `gs4_deauth()`) into `content/*.csv`.
  `content/` is gitignored, so each render needs network access, and edits to
  CV content belong in the sheet, not in the CSVs (they are overwritten).
- **Sections are filtered tibbles rendered with `kable()`.** Each CV section
  filters `cv_entries` by `type` (`education`, `awards`, `experience`,
  `service`, `consultancy`, `major_grants`, `talk`, `poster`) or
  `publications` by `type` (`paper`, `preprint`, `software`), builds an HTML
  string with `glue()` and the CSS classes (`cv-entry-title`,
  `cv-entry-gray`, `publication-authors`, ...), and calls `kable()`. Rows with
  a non-NA `exclude` column are dropped. Helpers at the top of the setup
  chunk abbreviate author first names and italicise "H. Jamil"
  (`replace_first_names_in_list_with_initial_and_dot`), number entries with a
  letter prefix (`padding_function`: A, W, S, T) and blank repeated years
  (`blank_year_when_repeated`).
- **Some rows are patched by index** after filtering (e.g.
  `tab_emp$year[4] <- ...`, `kable(awards[1:4, ])`, `slice(1:4)` splits).
  These silently break when sheet rows are added or reordered, so recheck them
  whenever the data changes.
- **Page layout is controlled by hand.** Tables are split across several
  `kable()` calls and `<br class="pageBreak">` is toggled (often left
  commented out) to get clean PDF page breaks. Check the rendered PDF after
  any content change.
- **`pdf_mode` param** switches the logo between a static ggplot (PDF) and a
  gganimate animation (HTML), and swaps the "view online" / "download PDF"
  link. `cv_logo.R` fits an I-prior model and builds both `p.static` and
  `p.anim`.
- **Styling.** `css/one-column-paged-academic.css` holds all layout (based on
  Ulrik Lyng's one-column pagedown CV), with Font Awesome 5.1.0 vendored in
  `css/`. The YAML sets `css: NULL` and loads both stylesheets through
  `include_css.html` instead, a workaround so relative CSS paths work on
  gh-pages. `include_css.html` also sets the `--last-updated` CSS variable
  shown in the page footer. `no_title_paged.html` is the pagedown template
  with the default title block removed.
- **`cv_pr.Rmd`** is a hand-maintained fork of `index.Rmd` (fewer sections,
  employment trimmed to UBD and KAUST). Changes to shared helpers or sections
  in `index.Rmd` do not propagate to it automatically.

## CI and deployment

`.github/workflows/build_pdf_cv.yml` runs on every push to `main` (the daily
cron is commented out). It builds the PDF and HTML on macOS, then deploys the
`site/` folder to the `gh-pages` branch. It also overwrites `README.md` in the
deployed site with session info, so the repo's `README.md` is not what appears
on gh-pages. Locally built `haziq_cv.pdf` and `cv_pr.pdf` are committed.
