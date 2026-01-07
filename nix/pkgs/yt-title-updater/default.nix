{
  lib,
  python3Packages,
  fetchFromGitHub,
  versionCheckHook,
}:
python3Packages.buildPythonApplication rec {
  pname = "yt-title-updater";
  version = "0.0.1";
  src = fetchFromGitHub {
    owner = "wfhanna1";
    repo = "YT-Title-Updater";
    rev = "680053745cf10545e666fd43fc78009cf63be52c";
    hash = "sha256-e8TbxYqyt/xrFfnC5+kBwd7t/cuMup6ZV027zui98wY=";
  };

  pyproject = true;
  build-system = with python3Packages; [
    cython
    setuptools
  ];

  dontWrapQtApps = true;

  dependencies = with python3Packages; [
    pyqt6
    urllib3
    google-api-python-client
    google-auth-oauthlib
    google-auth-httplib2
  ];
}
