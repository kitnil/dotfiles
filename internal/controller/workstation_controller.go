/*
Copyright 2025.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

package controller

import (
	"context"
	"fmt"

	corev1 "k8s.io/api/core/v1"
	apierrors "k8s.io/apimachinery/pkg/api/errors"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/runtime"
	"k8s.io/apimachinery/pkg/runtime/schema"
	"k8s.io/apimachinery/pkg/types"
	ctrl "sigs.k8s.io/controller-runtime"
	"sigs.k8s.io/controller-runtime/pkg/client"
	"sigs.k8s.io/controller-runtime/pkg/log"

	workstationv1 "wugi.info/workstation-controller/api/v1"
)

// WorkstationReconciler reconciles a Workstation object
type WorkstationReconciler struct {
	client.Client
	Scheme *runtime.Scheme
}

var bashCommand string = `set -o nounset -o errexit -o pipefail

chown 1000:998 /home/oleg
chmod 0755 /home/oleg

mkdir /home/oleg/.docker
chown 1000:998 /home/oleg/.docker

mkdir /home/oleg/.cache
chown 1000:998 /home/oleg/.cache

mkdir /home/oleg/.config
chown 1000:998 /home/oleg/.config

mkdir /home/oleg/.local
chown 1000:998 /home/oleg/.local

mkdir /home/oleg/.local/var
chown 1000:998 /home/oleg/.local/var

mkdir /home/oleg/.local/var/log
chown 1000:998 /home/oleg/.local/var/log

mkdir /home/oleg/.local/share
chown 1000:998 /home/oleg/.local/share

mkdir /home/oleg/.ssh
chown 1000:998 /home/oleg/.ssh

mkdir /mnt/nixos/home/oleg
chown 1000:998 /mnt/nixos/home/oleg

mkdir /mnt/nixos/home/oleg/.docker
chown 1000:998 /mnt/nixos/home/oleg/.docker

mkdir -p /mnt/nixos/home/oleg/.mozilla
chown 1000:998 /mnt/nixos/home/oleg/.mozilla

mkdir -p /mnt/nixos/home/oleg/.config
chown 1000:998 /mnt/nixos/home/oleg/.config

mkdir /mnt/nixos/home/oleg/.local
chown 1000:998 /mnt/nixos/home/oleg/.local

mkdir /mnt/nixos/home/oleg/.local/share
chown 1000:998 /mnt/nixos/home/oleg/.local/share

mkdir /mnt/nixos/home/oleg/.ssh
chown 1000:998 /mnt/nixos/home/oleg/.ssh
`

func (r *WorkstationReconciler) GetWorkstation(ctx context.Context, req ctrl.Request) workstationv1.Workstation {
	var workstation workstationv1.Workstation
	err := r.Get(ctx, types.NamespacedName{
		Name:      req.NamespacedName.Name,
		Namespace: req.NamespacedName.Namespace,
	}, &workstation)
	if apierrors.IsNotFound(err) {
		log.Log.Error(err, "Workstation not found")
	}
	return workstation
}

// +kubebuilder:rbac:groups=workstation.wugi.info,resources=workstations,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=workstation.wugi.info,resources=workstations/status,verbs=get;update;patch
// +kubebuilder:rbac:groups=workstation.wugi.info,resources=workstations/finalizers,verbs=update

// Reconcile is part of the main kubernetes reconciliation loop which aims to
// move the current state of the cluster closer to the desired state.
// TODO(user): Modify the Reconcile function to compare the state specified by
// the Workstation object against the actual cluster state, and then
// perform operations to make the cluster state reflect the state specified by
// the user.
//
// For more details, check Reconcile and its Result here:
// - https://pkg.go.dev/sigs.k8s.io/controller-runtime@v0.19.4/pkg/reconcile
func (r *WorkstationReconciler) Reconcile(ctx context.Context, req ctrl.Request) (ctrl.Result, error) {
	_ = log.FromContext(ctx)

	var workstation workstationv1.Workstation = r.GetWorkstation(ctx, req)
	r.CreateWorkstationPod(ctx, req, workstation)
	r.CreateWorkstationService(ctx, req, workstation)

	return ctrl.Result{}, nil
}

func (r *WorkstationReconciler) CreateWorkstationPod(ctx context.Context, req ctrl.Request, workstation workstationv1.Workstation) {
	var HostPathCharDevice corev1.HostPathType = "CharDevice"
	var HostPathDirectory corev1.HostPathType = "Directory"
	var HostPathFile corev1.HostPathType = "File"
	var HostPathSocket corev1.HostPathType = "Socket"
	var guixShmQuantity resource.Quantity = resource.MustParse("1Gi")
	var guixTmpQuantity resource.Quantity = resource.MustParse("4Gi")
	var guixRunQuantity resource.Quantity = resource.MustParse("512M")
	var nixosVarLibDockerQuantity resource.Quantity = resource.MustParse("16G")

	pod := &corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      req.NamespacedName.Name,
			Namespace: req.NamespacedName.Namespace,
			OwnerReferences: []metav1.OwnerReference{
				*metav1.NewControllerRef(&workstation, schema.GroupVersionKind{
					Group:   workstationv1.GroupVersion.Group,
					Version: workstationv1.GroupVersion.Version,
					Kind:    "Workstation",
				}),
			},
		},
		Spec: corev1.PodSpec{
			AutomountServiceAccountToken: &[]bool{false}[0],
			Affinity: &corev1.Affinity{
				NodeAffinity: &corev1.NodeAffinity{
					RequiredDuringSchedulingIgnoredDuringExecution: &corev1.NodeSelector{
						NodeSelectorTerms: []corev1.NodeSelectorTerm{
							{
								MatchFields: []corev1.NodeSelectorRequirement{
									{
										Key:      "metadata.name",
										Operator: corev1.NodeSelectorOperator("In"),
										Values: []string{
											"kube4",
										},
									},
								},
							},
						},
					},
				},
			},
			Tolerations: []corev1.Toleration{
				{
					Effect:   corev1.TaintEffectNoSchedule,
					Key:      "workstation.cluster.local",
					Operator: corev1.TolerationOpEqual,
					Value:    "true",
				},
			},
			InitContainers: []corev1.Container{
				{
					Name:            "volume-mount-hack",
					ImagePullPolicy: corev1.PullIfNotPresent,
					Image:           "busybox",
					Command: []string{
						"/bin/sh",
					},
					Args: []string{
						"-c",
						bashCommand,
					},
					VolumeMounts: []corev1.VolumeMount{
						{
							Name:      "container-home-oleg",
							MountPath: "/home/oleg",
						},
						{
							Name:      "nixos-home",
							MountPath: "/mnt/nixos/home",
						},
					},
				},
				{
					Name:            "clean-gnupg",
					ImagePullPolicy: corev1.PullIfNotPresent,
					Image:           "busybox",
					Command: []string{
						"/bin/sh",
					},
					Args: []string{
						"-c",
						`set -o nounset -o errexit -o pipefail
rm -f /home/oleg/.gnupg/gpg-agent.conf /home/oleg/.gnupg/gpg.conf`,
					},
					VolumeMounts: []corev1.VolumeMount{
						{
							Name:      "home-oleg-dot-gnupg",
							MountPath: "/home/oleg/.gnupg",
						},
					},
				},
			},
			Volumes: []corev1.Volume{
				{
					Name: "dev-dri",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/dev/dri",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "dev-input",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/dev/input",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "dev-tty9",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/dev/tty9",
							Type: &HostPathCharDevice,
						},
					},
				},
				{
					Name: "dev-tty2",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/dev/tty2",
							Type: &HostPathCharDevice,
						},
					},
				},
				{
					Name: "dev-fuse",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/dev/fuse",
							Type: &HostPathCharDevice,
						},
					},
				},
				{
					Name: "home-oleg-bash-history",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.bash_history",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "nsswitch",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/etc/nsswitch.conf",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "services",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/etc/services",
							Type: &HostPathFile,
						},
					},
				},

				{
					Name: "home-oleg",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "var-run-shepherd-socket",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/var/run/shepherd/socket",
							Type: &HostPathSocket,
						},
					},
				},
				{
					Name: "container-home-oleg",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixTmpQuantity,
						},
					},
				},
				{
					Name: "home-oleg-dot-cache-ihs",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.cache/ihs",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-config-google-chrome",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.config/google-chrome",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-config-obs-studio",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.config/obs-studio-4k",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-config-remmina",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.config/remmina",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-config-sway",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.config/sway",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-local-share-remmina",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.local/share/remmina",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-local-share-telegram",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.local/share/TelegramDesktop",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-local-share-chatterino",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.local/share/chatterino",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-mozilla",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.mozilla",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-password-store",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.password-store",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-gnupg",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.gnupg",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-robo3t",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.3T",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-mozilla-firefox",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.mozilla/firefox",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-ssh-private-key",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.ssh/id_ed25519",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "home-oleg-ssh-public-key",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.ssh/id_ed25519.pub",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "home-oleg-ssh-majordomo-gitlab-private-key",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.ssh/id_rsa_gitlab_intr_nopass",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "home-oleg-ssh-majordomo-gitlab-public-key",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.ssh/id_rsa_gitlab_intr_nopass.pub",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "home-oleg-ssh-gitlab-com-private-key",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.ssh/id_rsa_gitlab",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "home-oleg-ssh-gitlab-com-public-key",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.ssh/id_rsa_gitlab.pub",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "home-oleg-ssh-known-hosts",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.ssh/known_hosts",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "root-bash-history",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/root/.bash_history",
							Type: &HostPathFile,
						},
					},
				},
				{
					Name: "home-oleg-config-socialstream",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.config/SocialStream",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-config-qbittorrent",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.config/qBittorrent",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-dot-local-share-qbittorrent",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.local/share/qBittorrent",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-src",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/src",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "taskexecutor",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/src/gitlab.intr/hms/taskexecutor",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-local-share-chezmoi",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.local/share/chezmoi",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "srv",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/srv",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "qbittorrent-incomplete",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/mnt/qbittorrent-incomplete",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "home-oleg-config-wayvnc",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.config/wayvnc",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "mnt-web-btrfs-web99-home",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/mnt/web-btrfs/web99-home",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "mnt-web-ext4",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/mnt/web-ext4",
							Type: &HostPathDirectory,
						},
					},
				},
				{
					Name: "docker-configuration",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/home/oleg/.docker/config.json",
							Type: &HostPathFile,
						},
					},
				},
			},
		},
	}

	for _, container := range workstation.Spec.Template.Spec.Containers {
		if container.Name == "guix" {
			var containerTemplate corev1.Container = corev1.Container{
				Name:            "guix",
				ImagePullPolicy: corev1.PullIfNotPresent,
				Image:           "harbor.home.wugi.info/library/guix-image-workstation:1b2d17c0",
				Ports: []corev1.ContainerPort{
					{
						ContainerPort: 5353,
						Name:          "avahi",
						Protocol:      corev1.ProtocolUDP,
					},
					{
						ContainerPort: 16400,
						Name:          "scream",
						Protocol:      corev1.ProtocolUDP,
					},
				},
				SecurityContext: &corev1.SecurityContext{
					Capabilities: &corev1.Capabilities{
						Add: []corev1.Capability{
							corev1.Capability("SYS_ADMIN"),
						},
					},
					Privileged: &[]bool{true}[0],
				},
				TTY: true,
				VolumeMounts: []corev1.VolumeMount{
					{
						Name:             "guix-run",
						MountPath:        "/run",
						MountPropagation: &[]corev1.MountPropagationMode{"Bidirectional"}[0],
					},
					{
						Name:      "dev-dri",
						MountPath: "/dev/dri",
					},
					{
						Name:      "dev-input",
						MountPath: "/dev/input",
					},
					{
						Name:      "dev-tty2",
						MountPath: "/dev/tty0",
					},
					{
						Name:      "dev-tty2",
						MountPath: "/dev/tty2",
					},
					{
						Name:      "dev-fuse",
						MountPath: "/dev/fuse",
					},
					{
						Name:      "nsswitch",
						MountPath: "/etc/nsswitch.conf",
					},
					{
						Name:      "services",
						MountPath: "/etc/services",
					},
					{
						Name:      "guix-shm",
						MountPath: "/dev/shm",
					},
					{
						Name:      "guix-tmp",
						MountPath: "/tmp",
					},
					{
						Name:      "var-run-shepherd-socket",
						MountPath: "/mnt/guix/var/run/shepherd/socket",
					},
					{
						Name:      "container-home-oleg",
						MountPath: "/home/oleg",
					},
					{
						Name:      "home-oleg-dot-cache-ihs",
						MountPath: "/home/oleg/.cache/ihs",
					},
					{
						Name:      "home-oleg-dot-config-obs-studio",
						MountPath: "/home/oleg/.config/obs-studio",
					},
					{
						Name:      "home-oleg-dot-config-remmina",
						MountPath: "/home/oleg/.config/remmina",
					},
					{
						Name:      "home-oleg-dot-config-sway",
						MountPath: "/home/oleg/.config/sway",
					},
					{
						Name:      "home-oleg-dot-local-share-remmina",
						MountPath: "/home/oleg/.local/share/remmina",
					},
					{
						Name:      "home-oleg-dot-local-share-telegram",
						MountPath: "/home/oleg/.local/share/TelegramDesktop",
					},
					{
						Name:      "home-oleg-dot-password-store",
						MountPath: "/home/oleg/.password-store",
					},
					{
						Name:      "home-oleg-dot-gnupg",
						MountPath: "/home/oleg/.gnupg",
					},
					{
						Name:      "home-oleg-ssh-private-key",
						MountPath: "/home/oleg/.ssh/id_ed25519",
					},
					{
						Name:      "home-oleg-ssh-public-key",
						MountPath: "/home/oleg/.ssh/id_ed25519.pub",
					},
					{
						Name:      "home-oleg-ssh-majordomo-gitlab-private-key",
						MountPath: "/home/oleg/.ssh/id_rsa_gitlab_intr_nopass",
					},
					{
						Name:      "home-oleg-ssh-majordomo-gitlab-public-key",
						MountPath: "/home/oleg/.ssh/id_rsa_gitlab_intr_nopass.pub",
					},
					{
						Name:      "home-oleg-ssh-gitlab-com-private-key",
						MountPath: "/home/oleg/.ssh/id_rsa_gitlab",
					},
					{
						Name:      "home-oleg-ssh-gitlab-com-public-key",
						MountPath: "/home/oleg/.ssh/id_rsa_gitlab.pub",
					},
					{
						Name:      "home-oleg-ssh-known-hosts",
						MountPath: "/home/oleg/.ssh/known_hosts",
					},
					{
						Name:      "home-oleg-bash-history",
						MountPath: "/home/oleg/.bash_history",
					},
					{
						Name:      "root-bash-history",
						MountPath: "/root/.bash_history",
					},
					{
						Name:      "home-oleg-src",
						MountPath: "/home/oleg/src",
					},
					{
						Name:      "home-oleg-local-share-chezmoi",
						MountPath: "/home/oleg/.local/share/chezmoi",
					},
					{
						Name:      "srv",
						MountPath: "/srv",
					},
					{
						Name:      "home-oleg-config-qbittorrent",
						MountPath: "/home/oleg/.config/qBittorrent",
					},
					{
						Name:      "home-oleg-dot-local-share-qbittorrent",
						MountPath: "/home/oleg/.local/share/qBittorrent",
					},
					{
						Name:      "qbittorrent-incomplete",
						MountPath: "/mnt/qbittorrent-incomplete",
					},
					{
						Name:      "guix-var-log",
						MountPath: "/var/log",
					},
					{
						Name:      "guix-home-oleg-local-var-log",
						MountPath: "/home/oleg/.local/var/log",
					},
					{
						Name:      "docker-configuration",
						MountPath: "/home/oleg/.docker/config.json",
						ReadOnly:  true,
					},
				},
			}
			var volumes []corev1.Volume = []corev1.Volume{
				{
					Name: "guix-shm",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixShmQuantity,
						},
					},
				},
				{
					Name: "guix-tmp",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixTmpQuantity,
						},
					},
				},

				{
					Name: "guix-run",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "guix-var-log",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "guix-home-oleg-local-var-log",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
			}
			pod.Spec.Volumes = append(pod.Spec.Volumes, volumes...)
			pod.Spec.Containers = append(pod.Spec.Containers, containerTemplate)
		}
		if container.Name == "nixos" {
			var containerTemplate corev1.Container = corev1.Container{
				Name:            container.Name,
				ImagePullPolicy: corev1.PullIfNotPresent,
				Image:           container.Image,
				Command: []string{
					"/entrypoint.sh",
				},
				Lifecycle: &corev1.Lifecycle{
					PreStop: &corev1.LifecycleHandler{
						Exec: &corev1.ExecAction{
							Command: []string{
								"/bin/sh",
								"-c",
								`if /run/current-system/sw/bin/systemctl poweroff
then
:
else
exit 0
fi
								`,
							},
						},
					},
				},
				Ports: []corev1.ContainerPort{
					{
						ContainerPort: 5900,
						Name:          "vnc",
						Protocol:      corev1.ProtocolTCP,
					},
				},
				Env: []corev1.EnvVar{
					{
						Name:  "container",
						Value: "docker",
					},
				},
				SecurityContext: &corev1.SecurityContext{
					Capabilities: &corev1.Capabilities{
						Add: []corev1.Capability{
							corev1.Capability("BLOCK_SUSPEND"),
							corev1.Capability("NET_ADMIN"),
							corev1.Capability("NET_BIND_SERVICE"),
							corev1.Capability("NET_RAW"),
							corev1.Capability("SETUID"),
							corev1.Capability("SYS_ADMIN"),
							corev1.Capability("SYS_CHROOT"),
							corev1.Capability("SYS_NICE"),
							corev1.Capability("SYS_PTRACE"),
							corev1.Capability("SYS_RESOURCE"),
							corev1.Capability("SYS_TIME"),
						},
					},
					Privileged: &[]bool{true}[0],
				},
				TTY: true,
				VolumeMounts: []corev1.VolumeMount{
					{
						Name:      "dev-dri",
						MountPath: "/dev/dri",
					},
					{
						Name:      "dev-tty9",
						MountPath: "/dev/tty0",
					},
					{
						Name:      "dev-tty9",
						MountPath: "/dev/tty9",
					},
					{
						Name:      "nixos-run",
						MountPath: "/run",
					},
					{
						Name:      "guix-tmp",
						MountPath: "/mnt/guix/tmp",
					},
					{
						Name:             "guix-run",
						MountPath:        "/mnt/guix/run",
						MountPropagation: &[]corev1.MountPropagationMode{"HostToContainer"}[0],
					},
					{
						Name:      "nixos-home",
						MountPath: "/home",
					},
					{
						Name:      "home-oleg-mozilla-firefox",
						MountPath: "/home/oleg/.mozilla/firefox",
					},
					{
						Name:      "home-oleg-bash-history",
						MountPath: "/home/oleg/.bash_history",
					},
					{
						Name:      "home-oleg-dot-config-google-chrome",
						MountPath: "/home/oleg/.config/google-chrome",
					},
					{
						Name:      "root-bash-history",
						MountPath: "/root/.bash_history",
					},
					{
						Name:      "home-oleg-config-wayvnc",
						MountPath: "/home/oleg/.config/wayvnc",
					},
					{
						Name:      "home-oleg-dot-local-share-chatterino",
						MountPath: "/home/oleg/.local/share/chatterino",
					},
					{
						Name:      "nixos-var-log",
						MountPath: "/var/log",
					},
					{
						Name:      "home-oleg-ssh-majordomo-gitlab-private-key",
						MountPath: "/home/oleg/.ssh/id_rsa_gitlab_intr_nopass",
					},
					{
						Name:      "home-oleg-ssh-majordomo-gitlab-public-key",
						MountPath: "/home/oleg/.ssh/id_rsa_gitlab_intr_nopass.pub",
					},
					{
						Name:      "home-oleg-src",
						MountPath: "/home/oleg/src",
					},
					{
						Name:      "home-oleg-ssh-known-hosts",
						MountPath: "/home/oleg/.ssh/known_hosts",
					},
					{
						Name:      "home-oleg-robo3t",
						MountPath: "/home/oleg/.3T",
					},
					{
						Name:      "nixos-var-lib-docker",
						MountPath: "/var/lib/docker",
					},
					{
						Name:      "home-oleg-local-share-chezmoi",
						MountPath: "/home/oleg/.local/share/chezmoi",
					},
					{
						Name:      "mnt-web-btrfs-web99-home",
						MountPath: "/mnt/web-btrfs/web99-home",
					},
					{
						Name:      "mnt-web-ext4",
						MountPath: "/mnt/web-ext4",
					},
					{
						Name:      "docker-configuration",
						MountPath: "/home/oleg/.docker/config.json",
					},
				},
			}
			var volumes []corev1.Volume = []corev1.Volume{
				{
					Name: "nixos-run",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "nixos-home",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{},
					},
				},
				{
					Name: "nixos-var-log",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "nixos-var-lib-docker",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &nixosVarLibDockerQuantity,
						},
					},
				},
			}
			pod.Spec.Volumes = append(pod.Spec.Volumes, volumes...)
			pod.Spec.Containers = append(pod.Spec.Containers, containerTemplate)
		}
		if container.Name == "archlinux" {
			var containerTemplate corev1.Container = corev1.Container{
				Name:            container.Name,
				ImagePullPolicy: corev1.PullIfNotPresent,
				Image:           container.Image,
				Lifecycle: &corev1.Lifecycle{
					PreStop: &corev1.LifecycleHandler{
						Exec: &corev1.ExecAction{
							Command: []string{
								"/bin/sh",
								"-c",
								`if /run/current-system/sw/bin/systemctl poweroff
then
:
else
exit 0
fi
								`,
							},
						},
					},
				},
				Ports: []corev1.ContainerPort{
					{
						ContainerPort: 5900,
						Name:          "vnc",
						Protocol:      corev1.ProtocolTCP,
					},
				},
				Env: []corev1.EnvVar{
					{
						Name:  "container",
						Value: "docker",
					},
				},
				SecurityContext: &corev1.SecurityContext{
					Capabilities: &corev1.Capabilities{
						Add: []corev1.Capability{
							corev1.Capability("NET_ADMIN"),
							corev1.Capability("NET_BIND_SERVICE"),
							corev1.Capability("NET_RAW"),
							corev1.Capability("SYS_ADMIN"),
							corev1.Capability("SYS_NICE"),
							corev1.Capability("SYS_TIME"),
						},
					},
					Privileged: &[]bool{true}[0],
				},
				TTY: true,
				VolumeMounts: []corev1.VolumeMount{
					{
						Name:      "archlinux-run",
						MountPath: "/run",
					},
					{
						Name:      "archlinux-tmp",
						MountPath: "/tmp",
					},
					{
						Name:      "dev-dri",
						MountPath: "/dev/dri",
					},
					{
						Name:             "guix-run",
						MountPath:        "/mnt/guix/run",
						MountPropagation: &[]corev1.MountPropagationMode{"HostToContainer"}[0],
					},
					{
						Name:      "guix-tmp",
						MountPath: "/mnt/guix/tmp",
					},
					{
						Name:      "home-oleg-bash-history",
						MountPath: "/home/oleg/.bash_history",
					},
					{
						Name:      "root-bash-history",
						MountPath: "/root/.bash_history",
					},
					{
						Name:      "home-oleg-config-socialstream",
						MountPath: "/home/oleg/.config/SocialStream",
					},
					{
						Name:      "archlinux-var-log",
						MountPath: "/var/log",
					},
					{
						Name:      "home-oleg-src",
						MountPath: "/home/oleg/src",
					},
				},
			}
			var volumes []corev1.Volume = []corev1.Volume{
				{
					Name: "archlinux-tmp",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixTmpQuantity,
						},
					},
				},
				{
					Name: "archlinux-run",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "archlinux-var-log",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
			}
			pod.Spec.Volumes = append(pod.Spec.Volumes, volumes...)
			pod.Spec.Containers = append(pod.Spec.Containers, containerTemplate)
		}
		if container.Name == "gentoo" {
			var containerTemplate corev1.Container = corev1.Container{
				Name:            container.Name,
				Image:           container.Image,
				ImagePullPolicy: corev1.PullIfNotPresent,
				Lifecycle: &corev1.Lifecycle{
					PreStop: &corev1.LifecycleHandler{
						Exec: &corev1.ExecAction{
							Command: []string{
								"/bin/sh",
								"-c",
								`if /bin/systemctl poweroff
then
:
else
exit 0
fi
								`,
							},
						},
					},
				},
				Env: []corev1.EnvVar{
					{
						Name:  "container",
						Value: "docker",
					},
				},
				SecurityContext: &corev1.SecurityContext{
					Capabilities: &corev1.Capabilities{
						Add: []corev1.Capability{
							corev1.Capability("NET_ADMIN"),
							corev1.Capability("NET_BIND_SERVICE"),
							corev1.Capability("NET_RAW"),
							corev1.Capability("SYS_ADMIN"),
							corev1.Capability("SYS_NICE"),
							corev1.Capability("SYS_TIME"),
						},
					},
					Privileged: &[]bool{true}[0],
				},
				TTY: true,
				VolumeMounts: []corev1.VolumeMount{
					{
						Name:             "guix-run",
						MountPath:        "/mnt/guix/run",
						MountPropagation: &[]corev1.MountPropagationMode{"HostToContainer"}[0],
					},
					{
						Name:      "guix-tmp",
						MountPath: "/home/oleg/.bash_history",
					},
					{
						Name:      "root-bash-history",
						MountPath: "/root/.bash_history",
					},
					{
						Name:      "dev-dri",
						MountPath: "/dev/dri",
					},
					{
						Name:      "gentoo-run",
						MountPath: "/run",
					},
					{
						Name:      "gentoo-tmp",
						MountPath: "/tmp",
					},
					{
						Name:      "gentoo-var-log",
						MountPath: "/var/log",
					},
				},
			}
			var volumes []corev1.Volume = []corev1.Volume{
				{
					Name: "gentoo-tmp",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixTmpQuantity,
						},
					},
				},
				{
					Name: "gentoo-run",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "gentoo-var-log",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
			}
			pod.Spec.Volumes = append(pod.Spec.Volumes, volumes...)
			pod.Spec.Containers = append(pod.Spec.Containers, containerTemplate)
		}
		if container.Name == "kali-rolling" {
			var containerTemplate corev1.Container = corev1.Container{
				Name:            container.Name,
				ImagePullPolicy: corev1.PullIfNotPresent,
				Image:           container.Image,
				Lifecycle: &corev1.Lifecycle{
					PreStop: &corev1.LifecycleHandler{
						Exec: &corev1.ExecAction{
							Command: []string{
								"/bin/sh",
								"-c",
								`if /bin/systemctl poweroff
then
:
else
exit 0
fi
								`,
							},
						},
					},
				},
				Env: []corev1.EnvVar{
					{
						Name:  "container",
						Value: "docker",
					},
				},
				SecurityContext: &corev1.SecurityContext{
					Capabilities: &corev1.Capabilities{
						Add: []corev1.Capability{
							corev1.Capability("NET_ADMIN"),
							corev1.Capability("NET_BIND_SERVICE"),
							corev1.Capability("NET_RAW"),
							corev1.Capability("SYS_ADMIN"),
							corev1.Capability("SYS_NICE"),
							corev1.Capability("SYS_TIME"),
						},
					},
					Privileged: &[]bool{true}[0],
				},
				TTY: true,
				VolumeMounts: []corev1.VolumeMount{
					{
						Name:      "kali-rolling-run",
						MountPath: "/run",
					},
					{
						Name:      "kali-rolling-tmp",
						MountPath: "/tmp",
					},
					{
						Name:      "dev-dri",
						MountPath: "/dev/dri",
					},
					{
						Name:             "guix-run",
						MountPath:        "/mnt/guix/run",
						MountPropagation: &[]corev1.MountPropagationMode{"HostToContainer"}[0],
					},
					{
						Name:      "guix-tmp",
						MountPath: "/mnt/guix/tmp",
					},
					{
						Name:      "home-oleg-bash-history",
						MountPath: "/home/oleg/.bash_history",
					},
					{
						Name:      "root-bash-history",
						MountPath: "/root/.bash_history",
					},
					{
						Name:      "kali-rolling-var-log",
						MountPath: "/var/log",
					},
				},
			}
			var volumes []corev1.Volume = []corev1.Volume{
				{
					Name: "kali-rolling-run",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "kali-rolling-tmp",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixTmpQuantity,
						},
					},
				},
				{
					Name: "kali-rolling-var-log",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumMemory,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
			}
			pod.Spec.Volumes = append(pod.Spec.Volumes, volumes...)
			pod.Spec.Containers = append(pod.Spec.Containers, containerTemplate)
		}
	}

	err := r.Get(ctx, types.NamespacedName{
		Name:      req.NamespacedName.Name,
		Namespace: req.NamespacedName.Namespace,
	}, pod)

	if apierrors.IsNotFound(err) {
		log.Log.Info(fmt.Sprintf("Creating pod %s/%s", req.NamespacedName.Namespace, req.NamespacedName.Name))
		err = r.Create(ctx, pod)
		if err != nil {
			log.Log.Error(err, "Failed to create pod")
		}
	}
}

func (r *WorkstationReconciler) CreateWorkstationService(ctx context.Context, req ctrl.Request, workstation workstationv1.Workstation) {
	service := &corev1.Service{
		ObjectMeta: metav1.ObjectMeta{
			Name:      req.NamespacedName.Name,
			Namespace: req.NamespacedName.Namespace,
			OwnerReferences: []metav1.OwnerReference{
				*metav1.NewControllerRef(&workstation, schema.GroupVersionKind{
					Group:   workstationv1.GroupVersion.Group,
					Version: workstationv1.GroupVersion.Version,
					Kind:    "Workstation",
				}),
			},
		},
		Spec: corev1.ServiceSpec{
			Ports: []corev1.ServicePort{
				{
					Name:     "ssh",
					Protocol: corev1.ProtocolTCP,
					Port:     22,
				},
			},
		},
	}
	err := r.Get(ctx, types.NamespacedName{
		Name:      req.NamespacedName.Name,
		Namespace: req.NamespacedName.Namespace,
	}, service)

	if apierrors.IsNotFound(err) {
		log.Log.Info(fmt.Sprintf("Creating service %s/%s", req.NamespacedName.Namespace, req.NamespacedName.Name))
		err = r.Create(ctx, service)
		if err != nil {
			log.Log.Error(err, "Failed to create service")
		}
	}
}

// SetupWithManager sets up the controller with the Manager.
func (r *WorkstationReconciler) SetupWithManager(mgr ctrl.Manager) error {
	return ctrl.NewControllerManagedBy(mgr).
		For(&workstationv1.Workstation{}).
		Named("workstation").
		Complete(r)
}
