# typed-encoding-encoding



Problems building encoding-0.8.3 from hackage:
cabal v2-build (with 8.2.2)
stack build with lts-14.27
both error out with:

    [9 of 9] Compiling StackSetupShim   ( /home/rpeszek/.stack/setup-exe-src/setup-shim-mPHDZzAJ.hs, /tmp/stack10595/encoding-0.8.3/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/setup/StackSetupShim.o )
    Linking /tmp/stack10595/encoding-0.8.3/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/setup/setup ...
    Configuring encoding-0.8.3...
    Preprocessing library for encoding-0.8.3..
    setup: can't find source for Data/Encoding/ISO88591 in .,
    .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/autogen,
    .stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/global-autogen

