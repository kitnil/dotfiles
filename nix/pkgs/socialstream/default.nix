{ appimageTools, fetchurl }:
let
  pname = "socialstream";
  version = "0.3.69";
  src = fetchurl {
    url = "https://github.com/steveseguin/social_stream/releases/download/0.3.62/socialstreamninja_linux_v0.3.69_x86_64.AppImage";
    hash = "sha256-q75CELur6jkENiC7g808uyvQACAhUMUQW9m2Qg1Lh34=";
  };
in
appimageTools.wrapType2 { inherit pname version src; }
