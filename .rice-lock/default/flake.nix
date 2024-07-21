{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    compat = {
      flake = false;
      owner = "emacs-compat";
      repo = "compat";
      type = "github";
    };
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
