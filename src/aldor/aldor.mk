aldor_lang       = lang
aldor_basics     = minimach boolean0
aldor_extensions = axextend axlit
# subsetc.as contains a definition for the SPAD builtin type
# SubsetCategory.
aldor_misc = subsetc
aldor_srcs = $(aldor_lang) $(aldor_basics) $(aldor_extensions) $(aldor_misc)

