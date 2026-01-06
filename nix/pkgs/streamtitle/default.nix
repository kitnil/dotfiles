{ buildGoModule, fetchFromGitHub }:

buildGoModule {
  pname = "streamtitle";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "danirod";
    repo = "streamtitle";
    rev = "0c55514056023a6ec2572c253610eabc43bf4a86";
    hash = "sha256-78CQMTh7MHQ5KOGChh3v8FHbIVJ2QHEe7Y8ljy1KeP8=";
  };

  vendorHash = "sha256-Arp1mWq4sir610j3W1WH5RGSknABgRkkGEzdkYMjBrw=";
}
