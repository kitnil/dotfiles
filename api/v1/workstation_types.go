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

package v1

import (
	v1 "k8s.io/api/core/v1"
	metav1 "k8s.io/apimachinery/pkg/apis/meta/v1"
)

// EDIT THIS FILE!  THIS IS SCAFFOLDING FOR YOU TO OWN!
// NOTE: json tags are required.  Any new fields you add must have json tags for the fields to be serialized.

// WorkstationSpec defines the desired state of Workstation.
type WorkstationSpec struct {
	// INSERT ADDITIONAL SPEC FIELDS - desired state of cluster
	// Important: Run "make" to regenerate code after modifying this file

	// Foo is an example field of Workstation. Edit workstation_types.go to remove/update
	Foo string `json:"foo,omitempty"`

	// template is the object that describes the pod that will be created if
	// insufficient replicas are detected. Each pod stamped out by the StatefulSet
	// will fulfill this Template, but have a unique identity from the rest
	// of the StatefulSet. Each pod will be named with the format
	// <statefulsetname>-<podindex>. For example, a pod in a StatefulSet named
	// "web" with index number "3" would be named "web-3".
	// The only allowed template.spec.restartPolicy value is "Always".
	Template v1.PodTemplateSpec `json:"template" protobuf:"bytes,3,opt,name=template"`
}

// WorkstationStatus defines the observed state of Workstation.
type WorkstationStatus struct {
	// INSERT ADDITIONAL STATUS FIELD - define observed state of cluster
	// Important: Run "make" to regenerate code after modifying this file
}

// +kubebuilder:object:root=true
// +kubebuilder:subresource:status

// Workstation is the Schema for the workstations API.
type Workstation struct {
	metav1.TypeMeta   `json:",inline"`
	metav1.ObjectMeta `json:"metadata,omitempty"`

	Spec   WorkstationSpec   `json:"spec,omitempty"`
	Status WorkstationStatus `json:"status,omitempty"`
}

// +kubebuilder:object:root=true

// WorkstationList contains a list of Workstation.
type WorkstationList struct {
	metav1.TypeMeta `json:",inline"`
	metav1.ListMeta `json:"metadata,omitempty"`
	Items           []Workstation `json:"items"`
}

func init() {
	SchemeBuilder.Register(&Workstation{}, &WorkstationList{})
}
