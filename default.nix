{ mkDerivation, base, intervals, stdenv, time }:
mkDerivation {
  pname = "time-patterns";
  version = "0.1.4.2";
  src = ./.;
  libraryHaskellDepends = [ base intervals time ];
  homepage = "https://github.com/j-mueller/time-patterns";
  description = "Patterns for recurring events";
  license = stdenv.lib.licenses.bsd3;
}
