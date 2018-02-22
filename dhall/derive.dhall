  λ(opts : ./DerivationOptions.dhall )
→ λ(Derivation : Type)
→ λ(derive : ./DerivationOptions.dhall  → Derivation)
→ λ(_ : Text → Derivation)
→ derive opts
