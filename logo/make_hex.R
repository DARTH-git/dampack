# Builds the dampack hex sticker from the package's own output.
#
# The sticker shows the cost-effectiveness acceptability curves (CEACs) with the
# acceptability frontier (CEAF) above the expected loss curves (ELCs) of the three
# strategies in `cancer_psa_data`, on one willingness-to-pay (WTP) axis. Both frontiers
# switch from Radio to Chemo at the same threshold, marked by the dotted line
# (Alarid-Escudero et al. 2019, Value in Health 22(5):611-618, doi:10.1016/j.jval.2019.02.008).
#
# Run from the package root:  Rscript logo/make_hex.R
# It writes three SVGs to logo/:
#   dampack-hex.svg        the sticker (CEAC/CEAF and ELCs)
#   dampack-hex-small.svg  ELCs only, for small sizes such as a navbar
#   dampack-hex-icon.svg   ELCs only and no wordmark, for favicons
# logo/README.md explains how the PNGs in man/figures/ and pkgdown/favicon/ are made from them.

library(dampack)

# ---- data -------------------------------------------------------------------------------
data("cancer_psa_data")
l_psa <- make_psa_obj(cost = cancer_psa_data$cost,
                      effectiveness = cancer_psa_data$effectiveness,
                      parameters = cancer_psa_data$parameters,
                      strategies = cancer_psa_data$strategies)
v_wtp <- seq(0, 100000, by = 2500)
df_ceac <- as.data.frame(ceac(l_psa, wtp = v_wtp))
df_elc <- as.data.frame(calc_exp_loss(l_psa, wtp = v_wtp))

# ---- colours and geometry ---------------------------------------------------------------
# viridis option D, the palette of col = "access" in dampack's plots
l_col <- list(bg = "#eff5f2", ink = "#440154", rim = "#31688e",
              strat = c("#3b528b", "#21908c", "#5dc863"), mark = "#5dc863")
v_dash <- c("none", "3 2", "1 2")          # one line type per strategy, as in col = "bw"
hex_points <- "86.6,0 173.2,50 173.2,150 86.6,200 0,150 0,50"   # pointy-top hex, 173.2 x 200
v_log_loss <- range(log10(df_elc$Expected_Loss))

f1 <- function(x) formatC(x, format = "f", digits = 1)
wtp_x <- function(w, x0 = 34, x1 = 139) x0 + w / max(v_wtp) * (x1 - x0)
y_prob <- function(p, top, bottom) bottom - p * (bottom - top)
y_loss <- function(v, top, bottom) {
  bottom - (log10(v) - v_log_loss[1]) / diff(v_log_loss) * (bottom - top)
}

path_d <- function(x, y) paste0("M", paste(f1(x), f1(y), collapse = " L"))

# strategy curves, frontier line, and frontier squares every 12,500 for one panel
panel <- function(df, value, yfun, lwd_front, sq, x0 = 34, x1 = 139, lwd_strat = 1.2) {
  v_strat <- sort(unique(df$Strategy))
  v_curves <- vapply(seq_along(v_strat), function(i) {
    d <- df[df$Strategy == v_strat[i], ]
    d <- d[order(d$WTP), ]
    dash <- if (v_dash[i] == "none") "" else sprintf(' stroke-dasharray="%s"', v_dash[i])
    sprintf('<path d="%s" fill="none" stroke="%s" stroke-opacity="0.8" stroke-width="%s" stroke-linejoin="round"%s/>',
            path_d(wtp_x(d$WTP, x0, x1), yfun(d[[value]])), l_col$strat[i], lwd_strat, dash)
  }, character(1))
  d_f <- df[df$On_Frontier, ]
  d_f <- d_f[order(d_f$WTP), ]
  front <- sprintf('<path d="%s" fill="none" stroke="%s" stroke-width="%s" stroke-linecap="round" stroke-linejoin="round"/>',
                   path_d(wtp_x(d_f$WTP, x0, x1), yfun(d_f[[value]])), l_col$ink, lwd_front)
  d_sq <- d_f[d_f$WTP %% 12500 == 0, ]
  squares <- sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s" stroke="%s" stroke-width="1"/>',
                     f1(wtp_x(d_sq$WTP, x0, x1) - sq / 2), f1(yfun(d_sq[[value]]) - sq / 2), sq, sq,
                     l_col$mark, l_col$ink)
  c(v_curves, front, squares)
}

grid <- function(v_y) {
  sprintf('<path d="%s" stroke="%s" stroke-opacity="0.14" stroke-width="0.7"/>',
          paste(sprintf("M34,%s H139", v_y), collapse = " "), l_col$ink)
}

wordmark <- function(y) {
  sprintf('<text x="86.6" y="%s" text-anchor="middle" fill="%s" font-family="\'JetBrains Mono\', ui-monospace, Menlo, monospace" font-weight="700" font-size="24" letter-spacing="-0.6">dampack</text>',
          y, l_col$ink)
}
label <- function(x, y, txt, anchor = "end") {
  sprintf('<text x="%s" y="%s" text-anchor="%s" fill="%s" font-family="\'JetBrains Mono\', ui-monospace, Menlo, monospace" font-weight="700" font-size="6">%s</text>',
          x, y, anchor, l_col$ink, txt)
}

hex_svg <- function(body, font = TRUE) {
  c('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 173.2 200" width="173.2" height="200">',
    '<title>dampack</title>',
    if (font) "<style>@import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@700&amp;display=swap');</style>",
    sprintf('<defs><clipPath id="hex"><polygon points="%s"/></clipPath></defs>', hex_points),
    '<g clip-path="url(#hex)">',
    sprintf('<polygon points="%s" fill="%s"/>', hex_points, l_col$bg),
    body,
    sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="9"/>', hex_points, l_col$rim),
    "</g>", "</svg>")
}

# ---- the sticker: CEAC/CEAF on top, ELCs below ------------------------------------------
top <- c(41, 75)      # CEAC panel, probability 1 at the top
bot <- c(88, 122)     # ELC panel, log expected loss
d_front <- df_ceac[df_ceac$On_Frontier, ]
d_front <- d_front[order(d_front$WTP), ]
wtp_switch <- d_front$WTP[which(d_front$Strategy != d_front$Strategy[1])[1]]
d_front_elc <- df_elc[df_elc$On_Frontier, ]
d_front_elc <- d_front_elc[order(d_front_elc$WTP), ]
stopifnot(wtp_switch == d_front_elc$WTP[which(d_front_elc$Strategy != d_front_elc$Strategy[1])[1]])

v_sticker <- hex_svg(c(
  grid(c(top, bot)),
  sprintf('<path d="M%s,%s V%s" stroke="%s" stroke-opacity="0.45" stroke-width="0.8" stroke-dasharray="1.5 1.5"/>',
          f1(wtp_x(wtp_switch)), top[1] - 3, bot[2] + 3, l_col$ink),
  panel(df_ceac, "Proportion", function(p) y_prob(p, top[1], top[2]), 3.2, 4.5),
  panel(df_elc, "Expected_Loss", function(v) y_loss(v, bot[1], bot[2]), 3.2, 4.5),
  label(139, 131, "WTP →"),
  wordmark(156)
))

# ---- small version: ELCs only -----------------------------------------------------------
v_small <- hex_svg(c(
  grid(c(46, 83, 120)),
  panel(df_elc, "Expected_Loss", function(v) y_loss(v, 46, 120), 3.6, 5.5),
  label(139, 129, "WTP →"),
  wordmark(153)
))

# ---- icon: ELCs only, no wordmark, no text (so it needs no font) ------------------------
# the icon keeps only the strategy curves and the frontier, drawn wider and thicker
v_icon <- hex_svg(panel(df_elc, "Expected_Loss", function(v) y_loss(v, 48, 146), 12, 0,
                        x0 = 24, x1 = 149, lwd_strat = 6), font = FALSE)
v_icon <- v_icon[!grepl("^<rect ", v_icon)]

writeLines(v_sticker, "logo/dampack-hex.svg", useBytes = TRUE)
writeLines(v_small, "logo/dampack-hex-small.svg", useBytes = TRUE)
writeLines(v_icon, "logo/dampack-hex-icon.svg", useBytes = TRUE)
message("Wrote logo/dampack-hex.svg, logo/dampack-hex-small.svg and logo/dampack-hex-icon.svg",
        " (frontiers switch at WTP = ", format(wtp_switch, big.mark = ","), ")")
