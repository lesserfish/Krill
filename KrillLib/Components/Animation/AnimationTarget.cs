using System;
using System.Reflection;

namespace Nez.Extension;

public interface IAnimationTarget<T> where T : struct {
    void SetValue(T value);
    T GetValue();
    object GetTargetObject();
}

public class PropertyTarget<T> : IAnimationTarget<T> where T : struct {

    public PropertyTarget(object target, string property){
        _target = target;

        // try to fetch the field. if we dont find it this is a property
        if ((_fieldInfo = ReflectionUtils.GetFieldInfo(target, property)) == null)
        {
            _setter = ReflectionUtils.SetterForProperty<Action<T>>(target, property);
            _getter = ReflectionUtils.GetterForProperty<Func<T>>(target, property);
        }

        Insist.IsTrue(_setter != null || _fieldInfo != null,
                "either the property (" + property+ ") setter or getter could not be found on the object " +
                target);
    }

    public void SetValue(T value){
        if (_fieldInfo != null)
            _fieldInfo.SetValue(_target, value);
        else
            _setter(value);
    }

    public T GetValue(){
        if (_fieldInfo != null)
            return (T) _fieldInfo.GetValue(_target);

        return _getter();
    }

    public object GetTargetObject(){
        return _target;
    }

    FieldInfo _fieldInfo;
    private object _target;
    private Action<T> _setter;
    private Func<T> _getter;
}
