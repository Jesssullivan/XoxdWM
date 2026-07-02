//! Native compositor-local spatial anchors.
//!
//! This is not XR_EXT_spatial_anchor support. It stores named surface poses
//! inside the compositor so app-layer clients can save/restore window
//! positions without owning the scene policy.

use std::collections::BTreeMap;

use super::scene::{Quat, Transform3D, Vec3};

#[derive(Debug, Clone)]
pub struct SpatialAnchor {
    pub name: String,
    pub surface_id: u64,
    pub transform: Transform3D,
}

impl SpatialAnchor {
    pub fn new(name: String, surface_id: u64, transform: Transform3D) -> Self {
        Self {
            name,
            surface_id,
            transform,
        }
    }

    pub fn to_sexp(&self) -> String {
        format!(
            "(:name \"{}\" :surface-id {} :position (:x {:.3} :y {:.3} :z {:.3}) :rotation (:x {:.4} :y {:.4} :z {:.4} :w {:.4}))",
            escape_string(&self.name),
            self.surface_id,
            self.transform.position.x,
            self.transform.position.y,
            self.transform.position.z,
            self.transform.rotation.x,
            self.transform.rotation.y,
            self.transform.rotation.z,
            self.transform.rotation.w,
        )
    }
}

#[derive(Debug, Default)]
pub struct AnchorManager {
    anchors: BTreeMap<String, SpatialAnchor>,
    pub active_anchor: Option<String>,
}

impl AnchorManager {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn create_or_update(
        &mut self,
        name: String,
        surface_id: u64,
        transform: Transform3D,
    ) -> &SpatialAnchor {
        self.anchors
            .entry(name.clone())
            .and_modify(|anchor| {
                anchor.surface_id = surface_id;
                anchor.transform = transform;
            })
            .or_insert_with(|| SpatialAnchor::new(name, surface_id, transform))
    }

    pub fn remove(&mut self, name: &str) -> Option<SpatialAnchor> {
        if self.active_anchor.as_deref() == Some(name) {
            self.active_anchor = None;
        }
        self.anchors.remove(name)
    }

    pub fn get(&self, name: &str) -> Option<&SpatialAnchor> {
        self.anchors.get(name)
    }

    pub fn activate(&mut self, name: &str) -> Option<&SpatialAnchor> {
        if self.anchors.contains_key(name) {
            self.active_anchor = Some(name.to_string());
        }
        self.anchors.get(name)
    }

    pub fn count(&self) -> usize {
        self.anchors.len()
    }

    pub fn to_sexp(&self) -> String {
        let mut s = String::from("(");
        for anchor in self.anchors.values() {
            s.push_str(&anchor.to_sexp());
        }
        s.push(')');
        s
    }

    pub fn status_sexp(&self) -> String {
        let active = self
            .active_anchor
            .as_ref()
            .map(|name| format!("\"{}\"", escape_string(name)))
            .unwrap_or_else(|| "nil".to_string());
        format!("(:count {} :active {})", self.count(), active)
    }
}

pub fn transform_from_parts(position: Vec3, rotation: Quat) -> Transform3D {
    Transform3D {
        position,
        rotation,
        ..Transform3D::default()
    }
}

fn escape_string(s: &str) -> String {
    s.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_update_remove_anchor() {
        let mut anchors = AnchorManager::new();
        let transform = Transform3D::at(1.0, 2.0, -3.0);

        anchors.create_or_update("main".to_string(), 7, transform);
        assert_eq!(anchors.count(), 1);
        assert_eq!(anchors.get("main").unwrap().surface_id, 7);

        anchors.create_or_update("main".to_string(), 9, Transform3D::default());
        assert_eq!(anchors.count(), 1);
        assert_eq!(anchors.get("main").unwrap().surface_id, 9);

        assert!(anchors.remove("main").is_some());
        assert_eq!(anchors.count(), 0);
    }

    #[test]
    fn status_and_list_are_sexps() {
        let mut anchors = AnchorManager::new();
        anchors.create_or_update("main".to_string(), 7, Transform3D::default());
        anchors.activate("main");

        let status = anchors.status_sexp();
        assert!(status.contains(":count 1"));
        assert!(status.contains(":active \"main\""));

        let list = anchors.to_sexp();
        assert!(list.contains(":name \"main\""));
        assert!(list.contains(":surface-id 7"));
    }
}
