{ mkDerivation, aeson, async, base, bytestring, conduit
, conduit-combinators, conduit-extra, lens, monads-tf
, mono-traversable, process, reactive-banana, rio, stdenv, stm
, stm-conduit, text, transformers, typed-process
, weechat
}:
mkDerivation {
  pname = "keywee";
  version = "0.0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring conduit conduit-combinators
    conduit-extra lens monads-tf mono-traversable process
    reactive-banana rio stm stm-conduit text transformers typed-process
  ];
  homepage = "http://github.com/jkopanski/keywee#readme";
  description = "Keybase chat plugin for WeeChat";
  license = stdenv.lib.licenses.gpl3;
}
