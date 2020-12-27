set -o xtrace
ghci -XNoImplicitPrelude -XMagicHash -XGADTs -XTypeApplications \
    -XExplicitForAll -fprint-explicit-foralls -fprint-explicit-kinds $@
