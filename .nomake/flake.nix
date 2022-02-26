{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    dash = {
      flake = false;
      owner = "magnars";
      repo = "dash.el";
      type = "github";
    };
    magit-section = {
      flake = false;
      owner = "magit";
      repo = "magit";
      type = "github";
    };
    nix26 = {
      flake = false;
      owner = "emacs-twist";
      repo = "nix26.el";
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
