# Create sysdata.rda with all disease model parameters

# Wheat parameters (from Savary et al. 2015)
.RcA_leaf_rust <- cbind(
  c(0.0, 0.3, 0.6, 1.0, 1.4, 1.7, 2.0),
  c(1.00, 0.96, 0.92, 0.88, 0.84, 0.80, 0.76)
)

.RcT_leaf_rust <- cbind(
  c(0, 5, 10, 15, 20, 25, 30, 35),
  c(0, 0.35, 0.7, 1.0, 0.7, 0.35, 0, 0)
)

.RcA_s_tritici_blotch <- cbind(
  c(0.0, 0.3, 0.6, 1.0, 1.4, 1.7, 2.0),
  c(0.1, 0.1, 0.2, 0.5, 0.8, 1.0, 1.0)
)

.RcT_s_tritici_blotch <- cbind(
  c(0, 5, 10, 15, 20, 22, 24, 26, 28, 30, 35),
  c(0, 0.1, 0.3, 0.6, 0.9, 1.0, 1.0, 0.9, 0.6, 0.3, 0)
)

# Rice parameters (from Savary et al. 2012)
.RcA_bacterial_blight <- cbind(
  c(0, 20, 40, 60, 80, 100, 120),
  c(1, 1, 1, 1, 1, 1, 1)
)

.RcT_bacterial_blight <- cbind(
  c(15, 20, 25, 30, 35, 40),
  c(0, 0.29, 1.0, 0.29, 0.14, 0)
)

.RcA_brown_spot <- cbind(
  c(0, 20, 40, 60, 80, 100, 120),
  c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0)
)

.RcT_brown_spot <- cbind(
  c(15, 20, 25, 30, 35),
  c(0, 0.06, 1.0, 0.85, 0.16)
)

.RcA_leaf_blast <- cbind(
  c(0, 20, 40, 60, 80, 100, 120),
  c(1, 1.0, 0.98, 0.73, 0.51, 0.34, 0)
)

.RcT_leaf_blast <- cbind(
  c(15, 20, 25, 30, 35),
  c(0, 0.5, 1.0, 0.6, 0.2)
)

.RcA_sheath_blight <- cbind(
  c(0, 30, 60, 90, 120),
  c(0.84, 0.84, 0.84, 0.84, 0.84)
)

.RcT_sheath_blight <- cbind(
  c(15, 20, 25, 30, 35, 40),
  c(0, 0.42, 0.94, 0.94, 1.0, 0.85)
)

.RcA_tungro <- cbind(
  c(0, 25, 50, 75, 100, 120),
  c(1, 1, 1, 1, 1, 1)
)

.RcT_tungro <- cbind(
  c(15, 20, 25, 30, 35, 40),
  c(0, 0.13, 0.65, 0.75, 0.83, 1.0)
)

# Kim modified models
.RcA_modified_kim_leaf_blast <- cbind(
  c(0, 10, 20, 30, 40, 50, 60, 70, 77),
  c(1, 1, 0.98, 0.8, 0.6, 0.4, 0.2, 0.1, 0)
)

.RcT_modified_kim_leaf_blast <- cbind(
  c(15, 20, 25, 30, 35),
  c(0, 0.5, 1.0, 0.6, 0.2)
)

.RRG_modified_kim_leaf_blast <- cbind(
  c(0, 10, 20, 30, 40, 50, 60, 70, 77),
  c(0.1, 0.2, 0.3, 0.4, 0.3, 0.2, 0.1, 0.05, 0)
)

.RcA_modified_kim_sheath_blight <- cbind(
  c(0, 20, 40, 60, 80, 88),
  c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
)

.RcT_modified_kim_sheath_blight <- cbind(
  c(15, 20, 25, 30, 35, 40),
  c(0, 0.42, 0.94, 0.94, 1.0, 0.85)
)

.RRG_modified_kim_sheath_blight <- cbind(
  c(0, 20, 40, 60, 80, 88),
  c(0.1, 0.2, 0.3, 0.2, 0.1, 0.05)
)

# Save all to sysdata.rda
save(
  .RcA_bacterial_blight,
  .RcT_bacterial_blight,
  .RcA_brown_spot,
  .RcT_brown_spot,
  .RcA_leaf_blast,
  .RcT_leaf_blast,
  .RcA_sheath_blight,
  .RcT_sheath_blight,
  .RcA_tungro,
  .RcT_tungro,
  .RcA_modified_kim_leaf_blast,
  .RcT_modified_kim_leaf_blast,
  .RRG_modified_kim_leaf_blast,
  .RcA_modified_kim_sheath_blight,
  .RcT_modified_kim_sheath_blight,
  .RRG_modified_kim_sheath_blight,
  .RcA_leaf_rust,
  .RcT_leaf_rust,
  .RcA_s_tritici_blotch,
  .RcT_s_tritici_blotch,
  file = "R/sysdata.rda",
  compress = "bzip2",
  version = 2
)
