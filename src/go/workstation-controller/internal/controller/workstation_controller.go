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
	"path/filepath"
	"time"

	corev1 "k8s.io/api/core/v1"
	apierrors "k8s.io/apimachinery/pkg/api/errors"
	"k8s.io/apimachinery/pkg/api/resource"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/runtime"
	"k8s.io/apimachinery/pkg/types"
	ctrl "sigs.k8s.io/controller-runtime"
	"sigs.k8s.io/controller-runtime/pkg/client"
	"sigs.k8s.io/controller-runtime/pkg/controller/controllerutil"
	"sigs.k8s.io/controller-runtime/pkg/log"

	workstationv1 "wugi.info/workstation-controller/api/v1"
)

// WorkstationReconciler reconciles a Workstation object
type WorkstationReconciler struct {
	client.Client
	Scheme *runtime.Scheme
}

// +kubebuilder:rbac:groups=workstation.wugi.info,namespace=workstation,resources=workstations,verbs=get;list;watch;create;update;patch;delete
// +kubebuilder:rbac:groups=workstation.wugi.info,namespace=workstation,resources=workstations/status,verbs=get;update;patch
// +kubebuilder:rbac:groups=workstation.wugi.info,namespace=workstation,resources=workstations/finalizers,verbs=update
// +kubebuilder:rbac:groups="",namespace=workstation,resources=pods,verbs=get;list;watch;create;update
// +kubebuilder:rbac:groups="",namespace=workstation,resources=services,verbs=get;list;watch;create;update

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

	var workstation workstationv1.Workstation
	err := r.Get(ctx, types.NamespacedName{
		Name:      req.NamespacedName.Name,
		Namespace: req.NamespacedName.Namespace,
	}, &workstation)
	if apierrors.IsNotFound(err) {
		log.Log.Info(fmt.Sprintf("Workstation not found %s/%s", req.NamespacedName.Namespace, req.NamespacedName.Name))

		pod := corev1.Pod{
			ObjectMeta: metav1.ObjectMeta{
				Name:      req.NamespacedName.Name,
				Namespace: req.NamespacedName.Namespace,
			},
		}
		err := r.Get(ctx, types.NamespacedName{
			Name:      req.NamespacedName.Name,
			Namespace: req.NamespacedName.Namespace,
		}, &pod)
		if !apierrors.IsNotFound(err) {
			log.Log.Info(fmt.Sprintf("Delete pod %s/%s", req.NamespacedName.Namespace, req.NamespacedName.Name))
			r.Delete(ctx, &pod, &client.DeleteOptions{})
		}

		service := corev1.Service{
			ObjectMeta: metav1.ObjectMeta{
				Name:      req.NamespacedName.Name,
				Namespace: req.NamespacedName.Namespace,
			},
		}
		err = r.Get(ctx, types.NamespacedName{
			Name:      req.NamespacedName.Name,
			Namespace: req.NamespacedName.Namespace,
		}, &service)
		if !apierrors.IsNotFound(err) {
			log.Log.Info(fmt.Sprintf("Delete service %s/%s", req.NamespacedName.Namespace, req.NamespacedName.Name))
			r.Delete(ctx, &service, &client.DeleteOptions{})
		}
		return ctrl.Result{Requeue: false}, nil
	} else {
		r.CreateWorkstationPod(ctx, req, workstation)
		r.CreateWorkstationService(ctx, req, workstation)
		return ctrl.Result{RequeueAfter: time.Second * time.Duration(10)}, nil
	}
}

func (r *WorkstationReconciler) CreateWorkstationPod(ctx context.Context, req ctrl.Request, workstation workstationv1.Workstation) {
	var HostPathCharDevice corev1.HostPathType = "CharDevice"
	var HostPathDirectory corev1.HostPathType = "Directory"
	var HostPathFile corev1.HostPathType = "File"
	var HostPathSocket corev1.HostPathType = "Socket"
	var guixShmQuantity resource.Quantity = resource.MustParse("1Gi")
	var guixTmpQuantity resource.Quantity = resource.MustParse("32Gi")
	var guixRunQuantity resource.Quantity = resource.MustParse("512M")
	var nixosVarLibDockerQuantity resource.Quantity = resource.MustParse("32G")

	var bashCommand string

	var volumeMountHackVolumeMounts []corev1.VolumeMount
	volumeMountHackVolumeMounts = append(volumeMountHackVolumeMounts, corev1.VolumeMount{
		Name:      "guix-home",
		MountPath: "/mnt/guix/home",
	})

	volumeMountHackVolumeMounts = append(volumeMountHackVolumeMounts, corev1.VolumeMount{
		Name:      "nixos-home",
		MountPath: "/mnt/nixos/home",
	})
	for _, container := range workstation.Spec.Template.Spec.Containers {
		for _, volume := range workstation.Spec.Template.Spec.Volumes {
			if *volume.VolumeSource.HostPath.Type == corev1.HostPathDirectory {
				directory := fmt.Sprintf("/mnt/%s%s\n", container.Name, volume.HostPath.Path)
				bashCommand = bashCommand + fmt.Sprintf("mkdir -p %s", directory)
				// bashCommand = bashCommand + fmt.Sprintf("chown 1000:998 %s", directory)
				// volumeMountHackVolumeMounts = append(volumeMountHackVolumeMounts, corev1.VolumeMount{
				// 	Name:      volume.Name,
				// 	MountPath: fmt.Sprintf("/mnt/%s%s", container.Name, volume.VolumeSource.HostPath.Path),
				// })
			}
			if *volume.VolumeSource.HostPath.Type == corev1.HostPathFile {
				directory := fmt.Sprintf("/mnt/%s%s\n", container.Name, filepath.Dir(volume.HostPath.Path))
				bashCommand = bashCommand + fmt.Sprintf("mkdir -p %s", directory)
			}
		}
	}
	bashCommand = bashCommand + "chown -R 1000:998 /mnt/*/home/oleg\n"

	pod := &corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      req.NamespacedName.Name,
			Namespace: req.NamespacedName.Namespace,
			Labels: map[string]string{
				"app.kubernetes.io/name":       req.NamespacedName.Name,
				"app.kubernetes.io/created-by": req.NamespacedName.Name,
			},
		},
		Spec: corev1.PodSpec{
			AutomountServiceAccountToken: &[]bool{false}[0],
			RestartPolicy:                corev1.RestartPolicyNever,
			Affinity:                     workstation.Spec.Template.Spec.Affinity,
			Tolerations:                  workstation.Spec.Template.Spec.Tolerations,
			InitContainers: []corev1.Container{
				{
					Name:            "volume-mount-hack",
					ImagePullPolicy: corev1.PullIfNotPresent,
					Image:           "busybox",
					Command: []string{
						"/bin/sh",
					},
					Args: []string{
						"-e",
						"-c",
						bashCommand,
					},
					VolumeMounts: volumeMountHackVolumeMounts,
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
					Name: "var-run-shepherd-socket",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/var/run/shepherd/socket",
							Type: &HostPathSocket,
						},
					},
				},
			},
		},
	}

	pod.Spec.Volumes = append(pod.Spec.Volumes, workstation.Spec.Template.Spec.Volumes...)

	for _, container := range workstation.Spec.Template.Spec.Containers {
		if container.Name == "guix" {
			var containerTemplate corev1.Container = corev1.Container{
				Name:            container.Name,
				ImagePullPolicy: corev1.PullIfNotPresent,
				Image:           container.Image,
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
						Name:      "guix-home",
						MountPath: "/home",
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
						Name:      "guix-var-lib-docker",
						MountPath: "/var/lib/docker",
					},
				},
			}
			containerTemplate.VolumeMounts = append(containerTemplate.VolumeMounts, container.VolumeMounts...)
			var volumes []corev1.Volume = []corev1.Volume{
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
							Medium:    corev1.StorageMediumDefault,
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
							Medium:    corev1.StorageMediumDefault,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "guix-home",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{},
					},
				},
				{
					Name: "guix-var-lib-docker",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumDefault,
							SizeLimit: &nixosVarLibDockerQuantity,
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
						Name:      "nixos-var-log",
						MountPath: "/var/log",
					},
					{
						Name:      "nixos-var-lib-docker",
						MountPath: "/var/lib/docker",
					},
				},
			}
			containerTemplate.VolumeMounts = append(containerTemplate.VolumeMounts, container.VolumeMounts...)
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
							Medium:    corev1.StorageMediumDefault,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "nixos-var-lib-docker",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumDefault,
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
						Name:      "archlinux-var-log",
						MountPath: "/var/log",
					},
					{
						Name:      "archlinux-var-lib-docker",
						MountPath: "/var/lib/docker",
					},
				},
			}
			containerTemplate.VolumeMounts = append(containerTemplate.VolumeMounts, container.VolumeMounts...)
			var volumes []corev1.Volume = []corev1.Volume{
				{
					Name: "archlinux-tmp",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumDefault,
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
							Medium:    corev1.StorageMediumDefault,
							SizeLimit: &guixRunQuantity,
						},
					},
				},
				{
					Name: "archlinux-var-lib-docker",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumDefault,
							SizeLimit: &nixosVarLibDockerQuantity,
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
						MountPath: "/mnt/guix/tmp",
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
							Medium:    corev1.StorageMediumDefault,
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
							Medium:    corev1.StorageMediumDefault,
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
						Name:      "kali-rolling-var-log",
						MountPath: "/var/log",
					},
				},
			}
			containerTemplate.VolumeMounts = append(containerTemplate.VolumeMounts, container.VolumeMounts...)
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
							Medium:    corev1.StorageMediumDefault,
							SizeLimit: &guixTmpQuantity,
						},
					},
				},
				{
					Name: "kali-rolling-var-log",
					VolumeSource: corev1.VolumeSource{
						EmptyDir: &corev1.EmptyDirVolumeSource{
							Medium:    corev1.StorageMediumDefault,
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
	} else {
		err = r.Update(ctx, pod)
		if err != nil {
			log.Log.Error(err, fmt.Sprintf("Failed to update pod %s/%s", req.NamespacedName.Namespace, req.NamespacedName.Name))
		}
	}
	controllerutil.SetControllerReference(&workstation, pod, r.Scheme)
}

func (r *WorkstationReconciler) CreateWorkstationService(ctx context.Context, req ctrl.Request, workstation workstationv1.Workstation) {
	service := &corev1.Service{
		ObjectMeta: metav1.ObjectMeta{
			Name:      req.NamespacedName.Name,
			Namespace: req.NamespacedName.Namespace,
		},
		Spec: corev1.ServiceSpec{
			Ports: []corev1.ServicePort{
				{
					Name:     "ssh",
					Protocol: corev1.ProtocolTCP,
					Port:     22,
				},
			},
			Selector: map[string]string{
				"app.kubernetes.io/name":       req.NamespacedName.Name,
				"app.kubernetes.io/created-by": req.NamespacedName.Name,
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
	} else {
		err = r.Update(ctx, service)
		if err != nil {
			log.Log.Error(err, fmt.Sprintf("Failed to update service %s/%s", req.NamespacedName.Namespace, req.NamespacedName.Name))
		}
	}
	controllerutil.SetControllerReference(&workstation, service, r.Scheme)
}

// SetupWithManager sets up the controller with the Manager.
func (r *WorkstationReconciler) SetupWithManager(mgr ctrl.Manager) error {
	return ctrl.NewControllerManagedBy(mgr).
		For(&workstationv1.Workstation{}).
		Named("workstation").
		Complete(r)
}
