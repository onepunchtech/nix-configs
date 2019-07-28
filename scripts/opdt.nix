{ fetchurl, mkDerivation, base, hpack, optparse-applicative, process, stdenv
, text
}:
mkDerivation rec {
  pname = "opdt";
  version = "0.1.0.1";
  src = fetchurl {
    url = "https://github.com/onepunchlinux/opdt/archive/${version}.tar.gz";
    sha256 = "10mndsr4pnz25zpl5jj4dfm2nmy1jchxk5irkh5aq00v1s93sxnj";
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
  license = stdenv.lib.licenses.bsd3;
}
