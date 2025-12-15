use anyhow::{anyhow, bail, Result};
use std::any::{Any, TypeId};

/// 动态字段获取 trait
pub trait DynamicGetter {
    /// 根据字段名获取字段值（返回 Any 引用）
    fn get_field(&self, name: &str) -> Option<&dyn Any>;
    /// 判断某个字段是否存在
    fn has_field(&self, name: &str) -> bool;
    /// 获取所有字段名称
    fn field_names(&self) -> Vec<String>;
}

/// 为 DynamicGetter 添加扩展方法
pub trait DynamicGetterExt: DynamicGetter {
    /// 安全地获取字段（使用 anyhow 包装）
    fn get_field_safe(&self, name: &str) -> Result<&dyn Any> {
        self.get_field(name)
            .ok_or_else(|| anyhow!("Field '{}' not found", name))
    }
    /// 获取字段并尝试转换为指定类型引用
    fn get_field_as<T: 'static>(&self, name: &str) -> Result<&T> {
        let field = self.get_field_safe(name)?;
        field.downcast_ref::<T>().ok_or_else(|| {
            anyhow!(
                "Field '{}' is not of type '{}' (actual type: '{:?}')",
                name,
                std::any::type_name::<T>(),
                field.type_id()
            )
        })
    }
    /// 获取所有字段（名称和值）
    fn get_all_fields(&self) -> Result<Vec<(String, &dyn Any)>> {
        let mut fields = Vec::new();
        for name in self.field_names() {
            if let Some(value) = self.get_field(&name) {
                fields.push((name, value));
            }
        }
        Ok(fields)
    }
    /// 检查并获取多个字段
    fn get_multiple_fields(&self, names: &[&str]) -> Result<Vec<&dyn Any>> {
        let mut fields = Vec::with_capacity(names.len());
        for name in names {
            fields.push(self.get_field_safe(name)?);
        }
        Ok(fields)
    }
    /// 批量获取字段并转换为指定类型
    fn get_multiple_fields_as<'a, T: 'static>(&'a self, names: &[&str]) -> Result<Vec<&'a T>> {
        let mut fields = Vec::with_capacity(names.len());
        for name in names {
            fields.push(self.get_field_as::<T>(name)?);
        }
        Ok(fields)
    }
    /// 判断结构体是否包含所有指定字段
    fn has_all_fields(&self, names: &[&str]) -> bool {
        names.iter().all(|name| self.has_field(name))
    }
    /// 获取字段值并尝试克隆（如果字段实现了 Clone）
    fn get_field_cloned<T: 'static + Clone>(&self, name: &str) -> Result<T> {
        let field_ref = self.get_field_as::<T>(name)?;
        Ok(field_ref.clone())
    }
    /// 将字段转换为字符串表示
    fn get_field_as_string(&self, name: &str) -> Result<String> {
        let field = self.get_field_safe(name)?;
        if let Some(string) = field.downcast_ref::<String>() {
            return Ok(string.clone());
        }
        if let Some(str_ref) = field.downcast_ref::<&str>() {
            return Ok(str_ref.to_string());
        }
        if let Some(int) = field.downcast_ref::<i32>() {
            return Ok(int.to_string());
        }
        if let Some(float) = field.downcast_ref::<f64>() {
            return Ok(float.to_string());
        }
        if let Some(bool_val) = field.downcast_ref::<bool>() {
            return Ok(bool_val.to_string());
        }
        // 对于其他类型，使用 Debug trait（如果可用）
        Err(anyhow!("Field '{}' cannot be converted to string", name))
    }
    /// 查找字段名称（支持模糊匹配）
    fn search_field_name(&self, pattern: &str) -> Option<String> {
        self.field_names()
            .into_iter()
            .find(|name| name.contains(pattern) || pattern.contains(name))
    }
}

// 为所有实现 DynamicGetter 的类型自动实现 DynamicGetterExt
impl<T: DynamicGetter> DynamicGetterExt for T {}

/// 方法信息
#[derive(Debug)]
pub struct MethodInfo {
    pub type_id: TypeId,
    pub name: &'static str,
    pub kind: MethodKind,
}

#[derive(Debug)]
pub enum MethodKind {
    Immutable {
        call: fn(obj: &dyn Any, args: &[&dyn Any]) -> Option<Box<dyn Any>>,
    },
    Mutable {
        call: fn(obj: &mut dyn Any, args: &[&dyn Any]) -> Option<Box<dyn Any>>,
    },
    Static {
        call: fn(args: &[&dyn Any]) -> Option<Box<dyn Any>>,
    },
}

impl MethodInfo {
    pub fn name(&self) -> &str {
        self.name
    }
    pub fn matches(&self, name: &str) -> bool {
        self.name() == name
    }
    pub fn is_static(&self) -> bool {
        matches!(&self.kind, MethodKind::Static { .. })
    }
    pub fn call(&self, obj: Option<&dyn Any>, args: &[&dyn Any]) -> Result<Box<dyn Any>> {
        match &self.kind {
            MethodKind::Immutable { call, .. } => {
                if let Some(o) = obj {
                    call(o, args)
                        .ok_or_else(|| anyhow!("Failed to call immutable method '{}'", self.name()))
                } else {
                    bail!(
                        "Immutable method '{}' requires object reference",
                        self.name()
                    )
                }
            }
            MethodKind::Mutable { .. } => {
                bail!("Cannot call mutable method with immutable reference")
            }
            MethodKind::Static { call, .. } => {
                call(args).ok_or_else(|| anyhow!("Failed to call static method '{}'", self.name()))
            }
        }
    }
    pub fn call_mut(&self, obj: Option<&mut dyn Any>, args: &[&dyn Any]) -> Result<Box<dyn Any>> {
        match &self.kind {
            MethodKind::Immutable { call, .. } => {
                if let Some(o) = obj {
                    let obj_ref: &dyn Any = o; // 修复：用 coerce 替换 &**o
                    call(obj_ref, args)
                        .ok_or_else(|| anyhow!("Failed to call immutable method '{}'", self.name()))
                } else {
                    bail!(
                        "Immutable method '{}' requires object reference",
                        self.name()
                    )
                }
            }
            MethodKind::Mutable { call, .. } => {
                if let Some(o) = obj {
                    call(o, args)
                        .ok_or_else(|| anyhow!("Failed to call mutable method '{}'", self.name()))
                } else {
                    bail!(
                        "Mutable method '{}' requires mutable object reference",
                        self.name()
                    )
                }
            }
            MethodKind::Static { call, .. } => {
                call(args).ok_or_else(|| anyhow!("Failed to call static method '{}'", self.name()))
            }
        }
    }
    pub fn is_mutable(&self) -> bool {
        matches!(&self.kind, MethodKind::Mutable { .. })
    }
    pub fn is_immutable(&self) -> bool {
        matches!(&self.kind, MethodKind::Immutable { .. })
    }
}

// 首先尝试正确的 inventory 使用方式
inventory::collect!(MethodInfo);

/// 查找方法的辅助函数集合
pub mod find {
    use super::*;
    use anyhow::{anyhow, Result};
    /// 根据名称查找方法（返回第一个匹配的）
    pub fn find_method<T: 'static>(name: &str) -> Result<&'static MethodInfo> {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id && method.matches(name) {
                return Ok(method);
            }
        }
        Err(anyhow!("Method '{}' not found", name))
    }
    /// 根据名称查找方法，如果没找到返回 None
    pub fn try_find_method<T: 'static>(name: &str) -> Option<&'static MethodInfo> {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id && method.matches(name) {
                return Some(method);
            }
        }
        None
    }
    /// 查找所有匹配名称的方法（支持重载）
    pub fn find_all_methods<T: 'static>(name: &str) -> Vec<&'static MethodInfo> {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        let mut methods = Vec::new();
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id && method.matches(name) {
                methods.push(method);
            }
        }
        methods
    }
    /// 查找不可变方法
    pub fn find_immutable_method<T: 'static>(name: &str) -> Result<&'static MethodInfo> {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id && method.matches(name) && method.is_immutable() {
                return Ok(method);
            }
        }
        Err(anyhow!("Immutable method '{}' not found", name))
    }
    /// 查找可变方法
    pub fn find_mutable_method<T: 'static>(name: &str) -> Result<&'static MethodInfo> {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id && method.matches(name) && method.is_mutable() {
                return Ok(method);
            }
        }
        Err(anyhow!("Mutable method '{}' not found", name))
    }
    /// 获取所有方法列表
    pub fn all_methods<T: 'static>() -> Vec<&'static MethodInfo> {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        let mut methods = Vec::new();
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id {
                methods.push(method);
            }
        }
        methods
    }
    /// 获取所有方法名称（去重）
    pub fn all_method_names<T: 'static>() -> Vec<&'static str> {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        let mut names: Vec<&str> = Vec::new();
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id {
                names.push(method.name());
            }
        }
        names.sort();
        names.dedup();
        names
    }
    /// 获取方法数量
    pub fn method_count<T: 'static>() -> usize {
        use inventory::iter;
        let target_type_id = TypeId::of::<T>();
        let mut count = 0;
        for method in iter::<MethodInfo> {
            if method.type_id == target_type_id {
                count += 1;
            }
        }
        count
    }
    /// 检查方法是否存在
    pub fn has_method<T: 'static>(name: &str) -> bool {
        try_find_method::<T>(name).is_some()
    }
    /// 检查是否存在不可变方法
    pub fn has_immutable_method<T: 'static>(name: &str) -> bool {
        try_find_method::<T>(name)
            .map(|m| m.is_immutable())
            .unwrap_or(false)
    }
    /// 检查是否存在可变方法
    pub fn has_mutable_method<T: 'static>(name: &str) -> bool {
        try_find_method::<T>(name)
            .map(|m| m.is_mutable())
            .unwrap_or(false)
    }
}

/// 方便的调用函数
pub mod call {
    use super::*;
    use anyhow::{anyhow, bail, Context, Result};
    /// 通过不可变引用调用方法（仅限不可变或静态方法）
    pub fn call<T: Any>(
        method_name: &str,
        obj: Option<&T>,
        args: &[&dyn Any],
    ) -> Result<Box<dyn Any>> {
        let method = find::find_method::<T>(method_name)
            .with_context(|| format!("Failed to find method '{}'", method_name))?;
        let obj_any = obj.map(|o| o as &dyn Any);
        method
            .call(obj_any, args)
            .with_context(|| format!("Failed to call method '{}'", method_name))
    }
    /// 通过不可变引用调用方法，并尝试转换为特定类型
    pub fn call_and_downcast<T: Any, R: Any>(
        method_name: &str,
        obj: Option<&T>,
        args: &[&dyn Any],
    ) -> Result<R> {
        let result = call(method_name, obj, args)?;
        result.downcast::<R>().map(|boxed| *boxed).map_err(|_| {
            anyhow!(
                "Failed to downcast result of method '{}' to type '{}'",
                method_name,
                std::any::type_name::<R>()
            )
        })
    }
    /// 通过可变引用调用方法（支持所有方法）
    pub fn call_mut<T: Any>(
        method_name: &str,
        obj: Option<&mut T>,
        args: &[&dyn Any],
    ) -> Result<Box<dyn Any>> {
        let method = find::find_method::<T>(method_name)
            .with_context(|| format!("Failed to find method '{}'", method_name))?;
        let obj_any_mut = obj.map(|o| o as &mut dyn Any);
        method
            .call_mut(obj_any_mut, args)
            .with_context(|| format!("Failed to call method '{}'", method_name))
    }
    /// 通过可变引用调用方法，并尝试转换为特定类型
    pub fn call_mut_and_downcast<T: Any, R: Any>(
        method_name: &str,
        obj: Option<&mut T>,
        args: &[&dyn Any],
    ) -> Result<R> {
        let result = call_mut(method_name, obj, args)?;
        result.downcast::<R>().map(|boxed| *boxed).map_err(|_| {
            anyhow!(
                "Failed to downcast result of method '{}' to type '{}'",
                method_name,
                std::any::type_name::<R>()
            )
        })
    }
    /// 尝试调用方法，自动判断是否需要可变性
    pub fn try_call<T: Any>(method_name: &str, obj: &T, args: &[&dyn Any]) -> Result<Box<dyn Any>> {
        let method = find::find_method::<T>(method_name)
            .with_context(|| format!("Failed to find method '{}'", method_name))?;
        if method.is_immutable() {
            method
                .call(Some(obj as &dyn Any), args)
                .with_context(|| format!("Failed to call immutable method '{}'", method_name))
        } else {
            bail!("Method '{}' requires mutable reference", method_name)
        }
    }
    /// 调用方法并忽略返回值（用于有副作用的调用）
    pub fn call_void<T: Any>(method_name: &str, obj: &mut T, args: &[&dyn Any]) -> Result<()> {
        call_mut(method_name, Some(obj), args)?; // 修复：添加 Some
        Ok(())
    }
    /// 检查是否可调用
    pub fn can_call<T: Any>(method_name: &str, _obj: &T) -> bool {
        find::try_find_method::<T>(method_name)
            .map(|method| method.is_immutable())
            .unwrap_or(false)
    }
    /// 检查是否可调用（可变）
    pub fn can_call_mut<T: Any>(method_name: &str, _obj: &mut T) -> bool {
        find::try_find_method::<T>(method_name).is_some()
    }
}

/// 高级调用工具
pub mod util {
    use super::*;
    use anyhow::{anyhow, Context, Result};
    /// 批量调用多个方法
    pub fn batch_call<T: Any>(
        obj: &T,
        calls: &[(&str, Vec<&dyn Any>)],
    ) -> Result<Vec<Box<dyn Any>>> {
        let mut results = Vec::with_capacity(calls.len());
        for (method_name, args) in calls {
            let result = call::call(method_name, Some(obj), args)  // 修复：添加 Some
                .with_context(|| format!("Failed to call method '{}' in batch", method_name))?;
            results.push(result);
        }
        Ok(results)
    }
    /// 链式调用构建器
    pub struct MethodChain<'a, T: Any> {
        obj: &'a T,
        calls: Vec<(&'static str, Vec<&'a dyn Any>)>,
    }
    impl<'a, T: Any> MethodChain<'a, T> {
        /// 创建新的链式调用
        pub fn new(obj: &'a T) -> Self {
            Self {
                obj,
                calls: Vec::new(),
            }
        }
        /// 添加一个方法调用
        pub fn call(mut self, method_name: &'static str, args: Vec<&'a dyn Any>) -> Self {
            self.calls.push((method_name, args));
            self
        }
        /// 执行所有链式调用
        pub fn execute(self) -> Result<Vec<Box<dyn Any>>> {
            batch_call(self.obj, &self.calls)
        }
        /// 执行链式调用，只返回最后一个结果
        pub fn execute_last(self) -> Result<Box<dyn Any>> {
            let results = self.execute()?;
            results
                .into_iter()
                .last()
                .ok_or_else(|| anyhow!("No methods to call"))
        }
    }
    /// 动态调用包装器（不可变版本）
    pub struct DynamicInvoker<'a, T: Any> {
        obj: &'a T,
    }
    impl<'a, T: Any> DynamicInvoker<'a, T> {
        /// 为不可变对象创建调用器
        pub fn new(obj: &'a T) -> Self {
            Self { obj }
        }
        /// 调用方法
        pub fn invoke(&self, method_name: &str, args: &[&dyn Any]) -> Result<Box<dyn Any>> {
            call::try_call(method_name, self.obj, args)
        }
        /// 调用方法并转换类型
        pub fn invoke_as<R: Any>(&self, method_name: &str, args: &[&dyn Any]) -> Result<R> {
            let result = self.invoke(method_name, args)?;
            result.downcast::<R>().map(|boxed| *boxed).map_err(|_| {
                anyhow!(
                    "Failed to downcast result of method '{}' to type '{}'",
                    method_name,
                    std::any::type_name::<R>()
                )
            })
        }
        /// 检查方法是否存在且可调用
        pub fn can_invoke(&self, method_name: &str) -> bool {
            find::try_find_method::<T>(method_name)
                .map(|method| method.is_immutable())
                .unwrap_or(false)
        }
    }
    /// 动态调用包装器（可变版本）
    pub struct DynamicInvokerMut<'a, T: Any> {
        obj: &'a mut T,
    }
    impl<'a, T: Any> DynamicInvokerMut<'a, T> {
        /// 为可变对象创建调用器
        pub fn new(obj: &'a mut T) -> Self {
            Self { obj }
        }
        /// 调用方法
        pub fn invoke(&mut self, method_name: &str, args: &[&dyn Any]) -> Result<Box<dyn Any>> {
            call::call_mut(method_name, Some(self.obj), args) // 修复：添加 Some
        }
        /// 调用方法并转换类型
        pub fn invoke_as<R: Any>(&mut self, method_name: &str, args: &[&dyn Any]) -> Result<R> {
            let result = self.invoke(method_name, args)?;
            result.downcast::<R>().map(|boxed| *boxed).map_err(|_| {
                anyhow!(
                    "Failed to downcast result of method '{}' to type '{}'",
                    method_name,
                    std::any::type_name::<R>()
                )
            })
        }
        /// 检查方法是否存在且可调用
        pub fn can_invoke(&self, method_name: &str) -> bool {
            find::try_find_method::<T>(method_name).is_some()
        }
        /// 获取内部对象的可变引用
        pub fn into_inner(self) -> &'a mut T {
            self.obj
        }
        /// 获取内部对象的不可变引用
        pub fn as_ref(&self) -> &T {
            self.obj
        }
        /// 获取内部对象的可变引用
        pub fn as_mut(&mut self) -> &mut T {
            self.obj
        }
    }
    /// 通用的动态调用器（根据对象可变性自动选择）
    pub enum DynamicCaller<'a, T: Any> {
        Immutable(DynamicInvoker<'a, T>),
        Mutable(DynamicInvokerMut<'a, T>),
    }
    impl<'a, T: Any> DynamicCaller<'a, T> {
        /// 为不可变对象创建调用器
        pub fn new(obj: &'a T) -> Self {
            Self::Immutable(DynamicInvoker::new(obj))
        }
        /// 为可变对象创建调用器
        pub fn new_mut(obj: &'a mut T) -> Self {
            Self::Mutable(DynamicInvokerMut::new(obj))
        }
        /// 消耗自身并返回内部对象
        pub fn into_inner(self) -> &'a mut T {
            match self {
                Self::Immutable(_invoker) => {
                    panic!("Cannot get mutable reference from immutable caller")
                }
                Self::Mutable(invoker) => {
                    // 注意：这需要修改 DynamicInvokerMut 的结构
                    invoker.into_inner()
                }
            }
        }
        /// 通过内部方法访问值，避免借用冲突
        pub fn get_value(&self) -> Option<&T> {
            match self {
                Self::Immutable(invoker) => Some(invoker.obj),
                Self::Mutable(invoker) => Some(invoker.as_ref()),
            }
        }
        /// 调用方法
        pub fn invoke(&mut self, method_name: &str, args: &[&dyn Any]) -> Result<Box<dyn Any>> {
            match self {
                Self::Immutable(invoker) => invoker.invoke(method_name, args),
                Self::Mutable(invoker) => invoker.invoke(method_name, args),
            }
        }
        /// 调用方法并转换类型
        pub fn invoke_as<R: Any>(&mut self, method_name: &str, args: &[&dyn Any]) -> Result<R> {
            match self {
                Self::Immutable(invoker) => invoker.invoke_as(method_name, args),
                Self::Mutable(invoker) => invoker.invoke_as(method_name, args),
            }
        }
        /// 检查方法是否存在且可调用
        pub fn can_invoke(&self, method_name: &str) -> bool {
            match self {
                Self::Immutable(invoker) => invoker.can_invoke(method_name),
                Self::Mutable(invoker) => invoker.can_invoke(method_name),
            }
        }
        /// 转换为不可变调用器（如果有可变引用，则降级为不可变）
        pub fn as_immutable(&self) -> DynamicInvoker<'_, T> {
            match self {
                Self::Immutable(invoker) => DynamicInvoker { obj: invoker.obj },
                Self::Mutable(invoker) => DynamicInvoker { obj: invoker.obj },
            }
        }
    }
}

/// 错误类型定义
pub mod error {
    use anyhow::Error;
    use std::fmt;
    /// 方法调用错误
    #[derive(Debug)]
    pub struct MethodError {
        pub method_name: String,
        pub error: Error,
    }
    impl fmt::Display for MethodError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "Method '{}' error: {}", self.method_name, self.error)
        }
    }
    impl std::error::Error for MethodError {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            Some(self.error.as_ref())
        }
    }
    /// 创建方法错误
    pub fn method_error(method_name: &str, error: Error) -> MethodError {
        MethodError {
            method_name: method_name.to_string(),
            error,
        }
    }
}
