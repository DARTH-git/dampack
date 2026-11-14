# The dampack logo

The hex sticker is drawn from dampack's own output on `cancer_psa_data`, the package's main
example dataset:

- **Top panel:** the cost-effectiveness acceptability curves (CEACs) of the three strategies,
  from `ceac()`, with the acceptability frontier (CEAF) as the heavy line and squares.
- **Bottom panel:** their expected loss curves (ELCs), from `calc_exp_loss()`, on a log scale.
  The heavy line is the frontier, which equals the expected value of perfect information.
- **Dotted line:** both frontiers switch from Radio to Chemo at the same willingness-to-pay
  threshold, $40,000/QALY. That is the argument for reporting ELCs alongside the CEAC and
  CEAF ([Alarid-Escudero et al., 2019](https://doi.org/10.1016/j.jval.2019.02.008)).

The colours are viridis option D, the palette of `col = "access"` in dampack's plots. The
wordmark is set in JetBrains Mono Bold (SIL Open Font License).

## Files

| File | Use |
|---|---|
| `dampack-hex.svg` | The sticker. |
| `dampack-hex-600dpi.png` | Print: 2 in tall at 600 dpi, transparent outside the hex. |
| `dampack-hex-small.svg` | ELCs only, for small sizes where two panels are too busy. |
| `dampack-hex-icon.svg` | ELCs only, without text, for favicons. |
| `../man/figures/logo.png` | README and the pkgdown home page (240 × 278 px). |
| `../pkgdown/favicon/` | Site favicons, rendered from the icon. |

The SVGs load the wordmark font from Google Fonts. Print shops that ask for outlined text
need the text in `dampack-hex.svg` converted to paths, for example with
Inkscape (*Path > Object to Path*).

## Rebuilding

From the package root:

```sh
Rscript logo/make_hex.R   # SVGs, from the current dampack
bash logo/render.sh       # PNGs and favicons, with headless Google Chrome
```

`logo/` and `pkgdown/` are excluded from the package build, so only `man/figures/logo.png`
(17 KB) is installed with dampack.
