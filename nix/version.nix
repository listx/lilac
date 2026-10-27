{ cabalFile, rev }:
let
  lines = builtins.filter builtins.isString
    (builtins.split "\n" (builtins.readFile cabalFile));
  matches = builtins.filter (m: m != null)
    (map (builtins.match "[Vv]ersion:[[:space:]]*([^[:space:]]+).*") lines);
  cabalVersion = builtins.head (builtins.head matches);
  dirty = if builtins.match ".*-dirty" rev != null then "-dirty" else "";
  commit = builtins.substring 0 10 rev;
in "v${cabalVersion}-g${commit}${dirty}"
