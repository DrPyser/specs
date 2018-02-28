import abc
import funklib.core.prelude as prelude
import functools as ft
import operator as op
import itertools as it


class Validated(abc.ABC):
    def verify(self, valid, invalid):
        if type(self) is Valid:
            return valid(self[0])
        elif type(self) is Invalid:
            return invalid(self[0])
        else:
            raise TypeError

    def is_valid(self):
        return self.verify(prelude.const(True), prelude.const(False))

    def is_invalid(self):
        return self.verify(prelude.const(False), prelude.const(True))

    def then(self, f):
        return self.verify(f, prelude.const(self))

    def map(self, f):
        return self.verify(lambda x: Valid(f(x)), prelude.const(self))

    def catch(self, f):
        return self.verify(
            prelude.const(self),
            f
        )

    def map_error(self, f):
        return self.verify(
            prelude.const(self),
            lambda e: Invalid(f(e))
        )

    def ap(self, other):
        return self.verify(
            lambda f: other.map(f),
            lambda e: other.verify(
                prelude.const(self),
                lambda e: Invalid(e + e)
            )
        )

    def throw(self, f=prelude.identity):
        return self.verify(
            prelude.const(self),
            lambda e: prelude.throw(f(e))
        )

    def __repr__(self):
        return "{}({!r})".format(
            type(self).__name__,
            self[0]
        )

    @classmethod
    def collect(cls, validated):
        vals, invs = map(
            list, prelude.sep_by(cls.is_valid, validated)
        )
        if len(invs) > 0:
            return Invalid(list(map(op.itemgetter(0), invs)))
        else:
            return Valid(list(map(op.itemgetter(0), vals)))
        
    @classmethod
    def wrap(cls, valid, value):
        return Valid(value) if valid else Invalid(value)

    @classmethod
    def from_predicate(cls, pred, value):
        return cls.wrap(pred(value), value)
    
class Valid(tuple, Validated):
    def __new__(cls, x):
        return tuple.__new__(cls, (x,))

    __repr__ = Validated.__repr__

    
class Invalid(tuple, Validated):
    def __new__(cls, e):
        return tuple.__new__(cls, (e,))

    __repr__ = Validated.__repr__
    

class Spec(abc.ABC):
    def __init__(self, name=None):
        self.name = name
        
    @abc.abstractmethod
    def conform(self, value):
        pass

    @abc.abstractmethod
    def validate(self, value):
        pass


    
class SpecError(Exception):
    def __init__(self, value, spec):
        super().__init__(self)
        self.value = value
        self.spec = spec

    def __str__(self):
        return "value {!r} failed Spec {!r}".format(
            self.value, self.spec
        )

    def __repr__(self):
        return "{}(value={!r}, spec={})".format(
            type(self).__name__,
            self.value,
            self.spec
        )
    

class Predicate(Spec):
    """Predicate-based spec."""
    def __init__(self, pred, name=None):
        super().__init__(name=name or getattr(pred, "__name__", ""))
        self.pred = pred

    def conform(self, value):
        if self.pred(value):
            return Valid(value)
        else:
            return Invalid(SpecError(value, self))

    def validate(self, value):
        return bool(self.pred(value))

    def __repr__(self):
        return "{}({})".format(
            type(self).__name__,
            self.pred.__name__
        )


class Member(Predicate):
    """Predicate-based spec."""
    def __init__(self, values, name=None):
        values = frozenset(values)
        super().__init__(
            ft.partial(op.contains, values),
            name=name or "one_of({})".format(
                ", ".join(map(str, values))
            )
        )
        self.values = values

    def __repr__(self):
        return "Member({})".format(",".join(map(str, self.values)))

    
class Type(Predicate):
    """Predicate-based spec."""
    def __init__(self, type, name=None):
        super().__init__(
            lambda x: isinstance(x, type),
            name=name or "instance_of({})".format(
                type.__name__
            )
        )
        self.type = type

    def __repr__(self):
        return "Type({})".format(self.type.__name__)
    

class Alt(Spec):
    """Specify a branching point, i.e. alternatives.
    Conforming produce a tagged value
    """
    def __init__(self, *tagged_specs, name=None):
        super().__init__(name=name)
        self.tagged_specs = tagged_specs

    def conform(self, value):
        for tag, spec in self.tagged_specs:
            res = spec.conform(value).map(lambda x: (tag, x))
            if res.is_valid():
                return res
        else:
            return Invalid(SpecError(value, self))

    def validate(self, value):
        return any(spec.validate(value) for (_, spec) in self.tagged_specs)

    def __repr__(self):
        return "Alt({})".format(
            ", ".join(
                "{}={}".format(
                    tag, spec
                ) for tag, spec in self.specs
            )
        )

    
class Chain(Spec):
    """Specify a composition of specs
    """
    def __init__(self, *specs, name=None):
        super().__init__(name=name)
        self.specs = specs

    def conform(self, value):
        res = Valid(value)
        return ft.reduce(
            lambda acc, spec: acc.then(spec.conform),
            self.specs, res
        )

    def validate(self, value):
        return self.conform(value).is_valid()

    def __repr__(self):
        return "Chain({})".format(
            ", ".join(
                map(repr, self.specs)
            )
        )

class All(Predicate):
    def __init__(self, *specs, name=None):
        super().__init__(
            lambda x: all(spec.validate(x) for spec in specs),
            name=name
        )
        self.specs = specs

    def __repr__(self):
        return "All({})".format(
            ", ".join(map(repr, self.specs))
        )

        
class Keys(Spec):
    def __init__(self, required=(), optional=(), strict=True, name=None):
        super().__init__(name=name)
        self.required = frozenset(required)
        self.optional = frozenset(optional)
        self.strict = strict

    def conform(self, value):
        if self.validate(value):
            return Valid(value)
        else:
            return Invalid(SpecError(value=value, spec=self))

    def validate(self, value):
        keys = value.keys()
        return self.required <= keys and (
            not self.strict or
            keys <= (self.required | self.optional)
        )

    def __repr__(self):
        return "{}(required={}, optional={}, strict={})".format(
            type(self).__name__,
            self.required,
            self.optional,
            self.strict
        )


class Key(Spec):
    def __init__(self, key, spec, default=None, name=None):
        super().__init__(name)
        self.key = key
        self.spec = spec
        self.default = default
        
    def conform(self, value):
        try:
            k = value[self.key]            
        except KeyError:
            return Valid((self.key, self.default))
        else:
            return self.spec.conform(k).map(lambda x:(self.key, x))

    def validate(self, value):
        return (self.key not in value) or\
            self.spec.validate(value[self.key])

    def __repr__(self):
        return "{}({}, default={})".format(
            type(self).__name__,
            "{}={}".format(self.key, self.spec),
            self.default
        )
            
# class Cat(Spec):
#     def __init__(self, *specs, name=None):
#         super().__init__(name)
#         self.specs = specs
        
#     def conform(self, value):
#         seq = enumerate(value)
#         for (tag, spec) in self.specs:
#             res = spec.conform(it)
            
