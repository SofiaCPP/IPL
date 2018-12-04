---
title: "Objects in the VM"
date: 2018-12-05T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---
# Objects in the VM

---
## Contents

1. API
2. Getting properties

---
## API

### TODO:

- link to header with doxygen commens

---
## Enhancing `Value`

- `toString`
- `getProperty`
- `setProperty`

---
### `toString`

    Value Value::toString(Spasm* vm) const
    {
        switch(get_type())
        {
            case Number:
            // ...
        }
    }

---
### `getProperty`

    Value Value::getProperty(Spasm* vm, Value key) const
    {
        switch(get_type())
        {
            case Number:
            // ...
        }
    }

---
### ?

- lots of duplication
- lots of casting

---
### Visitor again

---

    template <typename V>
    typename V::ResultType Visit(V& v) {
        switch(get_type())
        {
            case Number: return v.Visit(get_double());
            case Null: return v.VisitNull();
            case Undefined: return v.VisitUndefined();
            case String: return v.Visit(get_string());
            // ...
        }
    }

---
#### `toString`

---

    struct ToStringVisitor
    {
        typedef Value ResultType;

        Value Visit(double d) const;
        Value VisitUndefined() const;
        Value VisitNull() const;
        Value Visit(const StringValue& value) const;
        Value Visit(const ArrayValue& value) const;
        Value Visit(const ObjectValue& value) const;
        Value Visit(const FunctionValue& value) const;
    };

---
## Properties

---

    struct Properties
    {
        typedef std::unordered_map<StringValue* name, Value> PropertyMap;

        Value GetProperty(StringValue* name);
    }

---
# ?
