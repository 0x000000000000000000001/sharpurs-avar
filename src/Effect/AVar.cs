using System;

namespace Effect.AVar;

public static class FFI {
    public static object Empty() => throw new NotImplementedException("Not implemented: empty");
    public static object _NewVar(object arg1) => throw new NotImplementedException("Not implemented: _newVar");
    public static object _KillVar(object arg1, object arg2, object arg3) => throw new NotImplementedException("Not implemented: _killVar");
    public static object _PutVar(object arg1, object arg2, object arg3, object arg4) => throw new NotImplementedException("Not implemented: _putVar");
    public static object _TryPutVar(object arg1, object arg2, object arg3) => throw new NotImplementedException("Not implemented: _tryPutVar");
    public static object _TakeVar(object arg1, object arg2, object arg3) => throw new NotImplementedException("Not implemented: _takeVar");
    public static object _TryTakeVar(object arg1, object arg2) => throw new NotImplementedException("Not implemented: _tryTakeVar");
    public static object _ReadVar(object arg1, object arg2, object arg3) => throw new NotImplementedException("Not implemented: _readVar");
    public static object _TryReadVar(object arg1, object arg2) => throw new NotImplementedException("Not implemented: _tryReadVar");
    public static object _Status(object arg1, object arg2) => throw new NotImplementedException("Not implemented: _status");
}
