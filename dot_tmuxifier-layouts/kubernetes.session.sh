# Set a custom session root path. Default is `$HOME`.
# Must be called before `initialize_session`.
session_root "~/src/gitlab.intr/nixos/kubernetes"

case "$KUBECONFIG" in
    $HOME/.kube/config-mjru-cluster1)
        session="cluster1"
        ;;
    $HOME/.kube/config-mjru-cluster2)
        session="cluster2"
        ;;
    *)
        session="kubernetes"
        ;;
esac

# Create session with specified name if it does not already exist. If no
# argument is given, session name will be based on layout file name.
if initialize_session "$session"; then

  # Create a new window inline within session layout definition.
  new_window "main"
  run_cmd "nix-shell"

  if [[ $TMUXIFIER_KUBERNETES_NAMESPACE ]]
  then
      load_window "kubernetes-kube-system"
      load_window "kubernetes-flux"
      load_window "kubernetes-pods"
      new_window "$TMUXIFIER_KUBERNETES_NAMESPACE"
      run_cmd "kubectl watch ${TMUXIFIER_KUBERNETES_NAMESPACE}"
  else
      # Load a defined window layout.
      load_window "kubernetes-kube-system"
      load_window "kubernetes-flux"
      load_window "kubernetes-flux-logs"
      load_window "kubernetes-cilium"
      load_window "kubernetes-cert-manager-logs"
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
