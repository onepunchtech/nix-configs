{ fetchurl, mkDerivation, base, hpack, optparse-applicative, process, stdenv
, text, lib
}:
mkDerivation rec {
  pname = "opdt";
  version = "0.1.0.2";
  src = fetchurl {
    url = "https://github.com/onepunchlinux/opdt/archive/${version}.tar.gz";
    sha256 = "1ci4fjrgr2c2ay9mf696kxr0ihh53bixc2yd72icnj1ih8mfazgs";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base optparse-applicative process text ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base optparse-applicative process text
  ];
  testHaskellDepends = [ base optparse-applicative process text ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/opdt#readme";
  license = lib.licenses.bsd3;
}
