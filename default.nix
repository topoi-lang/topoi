{ mkDerivation, base, bytestring, containers, lexer-applicative
, megaparsec, mtl, parser-combinators, regex-applicative, srcloc
, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "topoi";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers lexer-applicative megaparsec mtl
    parser-combinators regex-applicative srcloc text
    unordered-containers
  ];
  license = stdenv.lib.licenses.mit;
}
