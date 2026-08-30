# Build a single nested list of parameters, indexed by crop > disease
# Each disease entry has a *consistent* schema: RcA, RcT, RRG, RRS (rice) /
# RRPSN (wheat) as either a scalar (constant) or a 2-column interpolation
# matrix (x = driver value, y = modifier/rate).

.epi_params <- list(
  rice = list(
    bacterial_blight = list(
      RcA = cbind(
        x = c(0, 20, 40, 60, 80, 100, 120),
        y = c(1, 1, 1, 1, 1, 1, 1)
      ),
      RcT = cbind(
        x = c(15, 20, 25, 30, 35, 40),
        y = c(0, 0.29, 1.0, 0.29, 0.14, 0)
      ),
      RRG = 0.1,
      RRS = 0.01
    ),
    brown_spot = list(
      RcA = cbind(
        x = c(0, 20, 40, 60, 80, 100, 120),
        y = c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0)
      ),
      RcT = cbind(x = c(15, 20, 25, 30, 35), y = c(0, 0.06, 1.0, 0.85, 0.16)),
      RRG = 0.1,
      RRS = 0.01
    ),
    leaf_blast = list(
      RcA = cbind(
        x = c(0, 20, 40, 60, 80, 100, 120),
        y = c(1, 1.0, 0.98, 0.73, 0.51, 0.34, 0)
      ),
      RcT = cbind(x = c(15, 20, 25, 30, 35), y = c(0, 0.5, 1.0, 0.6, 0.2)),
      RRG = 0.1,
      RRS = 0.01
    ),
    sheath_blight = list(
      RcA = cbind(
        x = c(0, 30, 60, 90, 120),
        y = c(0.84, 0.84, 0.84, 0.84, 0.84)
      ),
      RcT = cbind(
        x = c(15, 20, 25, 30, 35, 40),
        y = c(0, 0.42, 0.94, 0.94, 1.0, 0.85)
      ),
      RRG = 0.2,
      RRS = 0.005
    ),
    tungro = list(
      RcA = cbind(x = c(0, 25, 50, 75, 100, 120), y = c(1, 1, 1, 1, 1, 1)),
      RcT = cbind(
        x = c(15, 20, 25, 30, 35, 40),
        y = c(0, 0.13, 0.65, 0.75, 0.83, 1.0)
      ),
      RRG = 0, # host growth/senescence not modelled for tungro
      RRS = 0
    )
  ),

  rice_modified_kim = list(
    leaf_blast = list(
      RcA = cbind(
        x = c(0, 10, 20, 30, 40, 50, 60, 70, 77),
        y = c(1, 1, 0.98, 0.8, 0.6, 0.4, 0.2, 0.1, 0)
      ),
      RcT = cbind(x = c(15, 20, 25, 30, 35), y = c(0, 0.5, 1.0, 0.6, 0.2)),
      RRG = cbind(
        x = c(0, 10, 20, 30, 40, 50, 60, 70, 77),
        y = c(0.1, 0.2, 0.3, 0.4, 0.3, 0.2, 0.1, 0.05, 0)
      ),
      RRS = 0.01 # unchanged from original leaf blast
    ),
    sheath_blight = list(
      RcA = cbind(
        x = c(0, 20, 40, 60, 80, 88),
        y = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
      ),
      RcT = cbind(
        x = c(15, 20, 25, 30, 35, 40),
        y = c(0, 0.42, 0.94, 0.94, 1.0, 0.85)
      ),
      RRG = cbind(
        x = c(0, 20, 40, 60, 80, 88),
        y = c(0.1, 0.2, 0.3, 0.2, 0.1, 0.05)
      ),
      RRS = 0.005 # unchanged from original sheath blight
    )
  ),

  wheat = list(
    leaf_rust = list(
      RcA = cbind(
        x = c(0.0, 0.3, 0.6, 1.0, 1.4, 1.7, 2.0),
        y = c(1.00, 0.96, 0.92, 0.88, 0.84, 0.80, 0.76)
      ),
      RcT = cbind(
        x = c(0, 5, 10, 15, 20, 25, 30, 35),
        y = c(0, 0.35, 0.7, 1.0, 0.7, 0.35, 0, 0)
      ),
      RRG = 0.1,
      # RRPSN (Groot 1987, via Willocquet et al. 2008): 0 prior to flowering
      RRPSN = cbind(
        x = c(0.0, 1.0, 1.2, 1.6, 1.8, 2.0),
        y = c(0.00, 0.00, 0.01, 0.04, 0.10, 0.10)
      )
    ),
    s_tritici_blotch = list(
      RcA = cbind(
        x = c(0.0, 0.3, 0.6, 1.0, 1.4, 1.7, 2.0),
        y = c(0.1, 0.1, 0.2, 0.5, 0.8, 1.0, 1.0)
      ),
      RcT = cbind(
        x = c(0, 5, 10, 15, 20, 22, 24, 26, 28, 30, 35),
        y = c(0, 0.1, 0.3, 0.6, 0.9, 1.0, 1.0, 0.9, 0.6, 0.3, 0)
      ),
      RRG = 0.1,
      RRPSN = cbind(
        x = c(0.0, 1.0, 1.2, 1.6, 1.8, 2.0),
        y = c(0.00, 0.00, 0.01, 0.04, 0.10, 0.10)
      )
    )
  )
)

save(.epi_params, file = "R/sysdata.rda", compress = "bzip2", version = 2)
