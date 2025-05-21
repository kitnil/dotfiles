{
  description = "Terraform wrapper to deploy to GitLab";

  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
    in
    {
      packages.${system} = {
        terraform-wrapper = with nixpkgs.legacyPackages.${system};
          let
            terraform = terraform_0_12;
          in
          writeScriptBin "terraform" ''
            #!${runtimeShell} -e
            case "$1" in
                init)
                    exec -a "$0" ${terraform}/bin/terraform init \
                      -plugin-dir ${terraform-providers.docker}/bin \
                      -plugin-dir ${terraform-providers.github}/bin \
                      -plugin-dir ${terraform-providers.gitlab}/bin \
                      "''${@:2}"
                    ;;
                *)
                    exec -a "$0" ${terraform}/bin/terraform "$@"
                    ;;
            esac
          '';
      };
      defaultPackage.${system} = self.packages.${system}.terraform-wrapper;
    };
}
