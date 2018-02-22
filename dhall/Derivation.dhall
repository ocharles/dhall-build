  ∀(Derivation : Type)
→ (./DerivationOptions.dhall  → Derivation)
→ ∀(eval-nix : Text → Derivation)
→ Derivation
