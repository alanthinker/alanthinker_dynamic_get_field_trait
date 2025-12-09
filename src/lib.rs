use std::any::Any;

pub trait DynamicGetter {
    fn get_field(&self, name: &str) -> Option<&dyn std::any::Any>;

    /// 判断某个字段是否存在
    fn has_field(&self, name: &str) -> bool;
}

// 定义 MethodInfo 用于运行时注册（inventory 使用）
#[derive(Debug)]
pub struct MethodInfo {
    pub name: &'static str,
    pub call: fn(obj: &dyn Any, args: &[&dyn Any]) -> Option<Box<dyn Any>>,
}

impl MethodInfo {
    pub fn matches(&self, name: &str) -> bool {
        self.name == name
    }

    pub fn call(&self, obj: &dyn Any, args: &[&dyn Any]) -> Option<Box<dyn Any>> {
        (self.call)(obj, args)
    }
}

pub use inventory;
inventory::collect!(MethodInfo);

pub fn find_method(name: &str) -> Option<&'static MethodInfo> {
    for method_info in inventory::iter::<MethodInfo> {
        if method_info.name == name {
            return Some(method_info);
        }
    }
    None
}
