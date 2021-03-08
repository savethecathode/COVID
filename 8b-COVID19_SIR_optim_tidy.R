# Purpose: put infectious SIR raw and fit data using optimized parameters into
# tidy format

# Join the raw data with the optimized fit by index
# Joining by index should always have the obvious interpretation.
Cook_IL_SIR_tidy_ofit <- tidy_SIR_I(raw = Cook_IL_SIR_raw,
                                    fit = Cook_IL_SIR_ofit,
                                    join_column = "index")