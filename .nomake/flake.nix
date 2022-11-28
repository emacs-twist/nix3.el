{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    compat = {
      flake = false;
      type = "git";
      url = "https://git.sr.ht/~pkal/compat";
    };
    dash = {
      flake = false;
      owner = "magnars";
      repo = "dash.el";
      type = "github";
    };
    magit-nix3 = {
      flake = false;
      owner = "emacs-twist";
      repo = "nix3.el";
      type = "github";
    };
    magit-section = {
      flake = false;
      owner = "magit";
      repo = "magit";
      type = "github";
    };
    nix3 = {
      flake = false;
      owner = "emacs-twist";
      repo = "nix3.el";
      type = "github";
    };
    promise = {
      flake = false;
      owner = "chuntaro";
      repo = "emacs-promise";
      type = "github";
    };
    "s" = {
      flake = false;
      owner = "magnars";
      repo = "s.el";
      type = "github";
    };
  };
  outputs = { ... }: { };
}
