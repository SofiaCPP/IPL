---
title: "Objects in the VM"
date: 2018-12-05T17:45:53+03:00
draft: false
outputs: ["Reveal"]
---
# Objects in the VM

---
## Contents

1. JavaScript Language Features
2. Getting properties
3. Objects
4. Optimizations
4. Talking to the world

---
## JavaScript Language Features

> We will be discussing only EcmaScript 3 inheritance.

1. Prototype-based inheritance
2. Everything is an object

---
### Inheritance

JavaScript is dynamically typed, so inheritance is about properties

    console.log(answer.value)

---
#### Property access

When looking-up a property in a object:

1. Look for the property in the object, if not found
2. Look for the property in the object's prototype, if not found
3. Look for the property in the object's prototype's prototype ...

---

    Value GetProperty(object, name) {
        Value result = object.GetProps().GetProperty(name);
        while (result == undefined && object.GetPrototype()) {
            object = object.GetPrototype();
            result = object.GetProps().GetProperty(name);
        }
        return result;
    }

---
### Properties

We need to map from string to a value.

---

    struct Properties
    {
        typedef std::unordered_map<StringValue* name, Value> PropertyMap;
        PropertyMap m_Props;

        Value GetProperty(StringValue* name) {
            const auto i = m_Props.find(name);
            return i != m_Props.end()? i->second : Value::kUndefined;
        }
    };

---
### Settting a property?

Set on the object directly

    void SetProperty(Object* object, StringValue* name,
                     Value value)
    {
        object->GetProps().Set(name, value);
    }

---
### Prototypes

    struct Object {
        Value GetProperty(StringValue* name);
        Object* GetPrototype() { return m_Prototype; }
        Properties& GetProps() { return m_OwnProps;}

        Object* m_Prototype;
        Properties m_OwnProps;
    };

---
## Optimizations

---
### Inline Cache

> Skip the name look up for a property at the place where the look is done

---

    var s = 0;
    for (var i = 0; i < strings.length; ++i) {
        s += strings[i].length; // << Here is the interesting part
    }

---

    getprop $t, $strings, $i
    getprop $r, $t, "length" // Goes in to the runtime and does the hash lookup
    add $s, $s, $r

---

    getprop $t, $strings, $i
    if (typeof $t == "string") {
        $r = getfast($t, 0); // (size_t*)(*t+8);
    } else {
        getprop $r, $t, "length" 
    }

---

[JSC impl](http://www.filpizlo.com/papers.html)

---
## Hidden structures


---
## Implementation

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
### Talking to the world


    typedef Value (*Function)(Value thisObject, Value function, Value* args,
            size_t count, Value* exception)

    typedef Value (*Getter)(Value thisObject, StringValue* name,
            Value* exception);

    typedef void (*Setter)(Value thisObject, StringValue* name, Value value,
            Value* exception);

---
### Property descriptors

Special kind of object that has:

    struct PropertyDescriptor {
        Getter m_Getter;
        Setter m_Setter;
    };

---
# ?
