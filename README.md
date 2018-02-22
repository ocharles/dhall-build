# dhall-build

Exploring the intersection of the Nix build system (derivations, not the Nix language) and the Dhall configuration language.

`dhall-build` evaluates Dhall expressions essentially of the type `∀(output : Derivation → Text) → A`. The special `output` function causes `dhall-build` to compile and build a Nix derivation, and returns the location in the Nix store as a result. `Derivation`'s can be constructed either purely in Dhall or by calling `nix-instantiate` (effectively importing Nix expressions).

But what is `Derivation`? A `Derivation` is:

```
∀(Derivation : Type) → ({ arguments : List Text, exec : Text, name : Text } → Derivation) → ∀(eval-nix : Text → Derivation) → Derivation
```

That is to say, a `Derivation` is either constructed by a Dhall record (this is considered the "purely in Dhall" approach), or by running a Nix expression. Here's an example:

```haskell
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
```

Ignoring the first two `let` bindings, we see that we have a `bash` derivation which builds `bash` via Nix+Nixpkgs. Next, we add a text file to the store using `pkgs.writeText` from Nixpkgs again. Given these two derivations, we can compose them into a new derivation that calls bash and runs the given script. We do this at the end using `output` (to cause the output to be built) and `derive` (to construct the derivation).

Running this through `dhall-build`, we get:

```
$ ./dhall-build hello.dhall 
...
"A greeting awaits you in /nix/store/5wdbq6v6a28r523zz681bnbz9dnadilc-Hello"
```

Which is the input Dhall expression, but normalized under the application of `output`. And what's this about a greeting?

```
$ cat /nix/store/5wdbq6v6a28r523zz681bnbz9dnadilc-Hello
Hello
```

Splendid!
