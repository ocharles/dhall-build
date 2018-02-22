  λ(output : ./dhall/Derivation.dhall  → Text)
→     let derive = ./dhall/derive.dhall 
  
  in  let eval-nix = ./dhall/eval-nix.dhall 
  
  in  let bash = eval-nix "(import <nixpkgs> {}).bash"
  
  in  let script =
            eval-nix
            ''
            (import <nixpkgs> {}).writeText "script" "echo Hello > $out"
            ''
  
  in      "A greeting awaits you in "
      ++  output
          ( derive
            { name =
                "Hello"
            , exec =
                "${output bash}/bin/bash"
            , arguments =
                [ output script ] : List Text
            }
          )
