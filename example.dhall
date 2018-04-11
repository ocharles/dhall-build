  λ ( build
    : { builder : Text, args : List Text, fingerprint : Text → Text } → Text
    )
→ λ(hash-path : Text → Text)
→ λ(PATH : Text → Text)
→ λ(file : Text → Text)
→ λ(read-file : Text → Text)
→ build
  { builder =
      PATH "ld"
  , args =
      [ build
        { builder =
            PATH "ghc"
        , args =
            [ "-c", file "A.hs" ]
        , fingerprint =
              λ(result : Text)
            → read-file
              ( build
                { builder =
                    PATH "GHC"
                , args =
                    [ "--show-iface", result ]
                , fingerprint =
                    hash-path
                }
              )
        }
      , build
        { builder =
            PATH "ghc"
        , args =
            [ "-c", file "B.hs" ]
        , fingerprint =
              λ(result : Text)
            → read-file
              ( build
                { builder =
                    PATH "GHC"
                , args =
                    [ "--show-iface", result ]
                , fingerprint =
                    hash-path
                }
              )
        }
      ]
  , fingerprint =
      hash-path
  }
