{ appimageTools, lib, libpng, pcre2, double-conversion, fetchurl, makeWrapper
, libsecret }:

let
  pname = "nekoray-bin";
  version = "3.26";
  name = "Nekoray-${version}";

  src = fetchurl {
    url =
      "https://github.com/MatsuriDayo/nekoray/releases/download/${version}/nekoray-${version}-2023-12-09-linux-x64.AppImage";
    sha256 = "sha256-mdRI/XAquX4BlL7ecprC50+6Ylc/S4kBnqEQ4BxBU5Y=";
  };

  appimageContents = appimageTools.extract { inherit name src; };

in appimageTools.wrapType2 {
  inherit version name src;

  extraInstallCommands = ''
    mv $out/bin/${name} $out/bin/${pname}
    install -m 444 -D ${appimageContents}/nekoray.desktop -t $out/share/applications
    #cp -r ${appimageContents}/usr/share/icons $out/share
  '';

  passthru.version = version;

  extraPkgs = pkgs:
    with pkgs; [
      libsecret
      libappindicator-gtk3
      libpng
      pcre2
      double-conversion
    ];

  meta = with lib; {
    homepage = "https://matsuridayo.github.io/";
    description =
      "Qt based cross-platform GUI proxy configuration manager (backend: v2ray / sing-box) ";
    platforms = [ "x86_64-linux" ];
    license = licenses.gpl3;
  };
}