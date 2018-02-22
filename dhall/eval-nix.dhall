  λ(src : Text)
→ λ(Derivation : Type)
→ λ(_ : ./DerivationOptions.dhall  → Derivation)
→ λ(eval : Text → Derivation)
→ eval src
