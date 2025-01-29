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

	corev1 "k8s.io/api/core/v1"
	apierrors "k8s.io/apimachinery/pkg/api/errors"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
	"k8s.io/apimachinery/pkg/runtime"
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

	var HostPathDirectory corev1.HostPathType = "Directory"

	// TODO(user): your logic here
	pod := &corev1.Pod{
		ObjectMeta: metav1.ObjectMeta{
			Name:      req.NamespacedName.Name,
			Namespace: req.NamespacedName.Namespace,
			// OwnerReferences: []metav1.OwnerReference{
			// 	*metav1.NewControllerRef(&workstationv1.Workstation{}, schema.GroupVersionKind{
			// 		Group:   workstationv1.GroupVersion.Group,
			// 		Version: workstationv1.GroupVersion.Version,
			// 		Kind:    "Workstation",
			// 	}),
			// },
		},
		Spec: corev1.PodSpec{
			Containers: []corev1.Container{
				{
					Name:  "busybox",
					Image: "busybox",
					Command: []string{
						"/bin/sleep",
					},
					Args: []string{
						"infinity",
					},
					VolumeMounts: []corev1.VolumeMount{
						{
							Name:      "root",
							MountPath: "/host-rootfs",
						},
					},
				},
			},
			Volumes: []corev1.Volume{
				{
					Name: "root",
					VolumeSource: corev1.VolumeSource{
						HostPath: &corev1.HostPathVolumeSource{
							Path: "/",
							Type: &HostPathDirectory,
						},
					},
				},
			},
		},
	}

	err := r.Get(ctx, types.NamespacedName{
		Name:      req.NamespacedName.Name,
		Namespace: req.NamespacedName.Namespace,
	}, pod)

	if apierrors.IsNotFound(err) {
		err = r.Create(ctx, pod)
		if err != nil {
			log.Log.Error(err, "Failed to create pod")
		}
	}

	return ctrl.Result{}, nil
}

// SetupWithManager sets up the controller with the Manager.
func (r *WorkstationReconciler) SetupWithManager(mgr ctrl.Manager) error {
	return ctrl.NewControllerManagedBy(mgr).
		For(&workstationv1.Workstation{}).
		Named("workstation").
		Complete(r)
}
