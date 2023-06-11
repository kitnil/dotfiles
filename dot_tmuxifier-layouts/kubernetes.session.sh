# Set a custom session root path. Default is `$HOME`.
# Must be called before `initialize_session`.
session_root "~/src/gitlab.intr/nixos/kubernetes"

case "$KUBECONFIG" in
    /nix/store/*-config/.kube/config)
        session="cluster1-view"
        ;;
    $HOME/.kube/config-mjru-cluster1)
        session="cluster1"
        ;;
    $HOME/.kube/config-mjru-cluster2)
        session="cluster2"
        ;;
    $HOME/.kube/config-host1-k3s)
        session="host1"
        ;;
    *)
        session="home"
        ;;
esac

# Create session with specified name if it does not already exist. If no
# argument is given, session name will be based on layout file name.
if initialize_session "$session"; then

  run_cmd "tmux set-option mouse on"

  # Create a new window inline within session layout definition.
  new_window "main"
  run_cmd "nix-shell"

  if [[ $TMUXIFIER_KUBERNETES_NAMESPACE ]]
  then
      load_window "kubernetes-kube-system"
      load_window "kubernetes-flux-system"
      load_window "kubernetes-pods"
      new_window "$TMUXIFIER_KUBERNETES_NAMESPACE"
      run_cmd "kubectl watch ${TMUXIFIER_KUBERNETES_NAMESPACE}"
  else
      # Load a defined window layout.
      load_window "kubernetes-kube-system"
      load_window "kubernetes-flux-system"
      load_window "kubernetes-pods"
      load_window "kubernetes-cilium"
      load_window "kubernetes-monitoring"
      load_window "kubernetes-piraeus"
      load_window "kubernetes-opensearch"
      load_window "kubernetes-pdns"
      load_window "kubernetes-harbor"
  fi

  # Select the default active window on session creation.
  select_window 0

fi

# Finalize session creation and switch/attach to it.
finalize_and_go_to_session
