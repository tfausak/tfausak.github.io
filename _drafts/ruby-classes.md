---
layout: post
title: Ruby Classes
---

## Class

``` rb
klass = Class.new
object = klass.new
```

Is `object` an instance of `klass`?

``` rb
case object
when klass
  true
end
```

``` rb
if klass === object
  true
end
```

1.  TODO

    ``` rb
    klass === object
    # => true
    klass.class_exec { def self.===(*) false end }
    klass === object
    # => false
    ```

2.  TODO

    ``` rb
    object.is_a?(klass)
    # => true
    klass.class_exec { def is_a?(*) false end }
    object.is_a?(klass)
    # => false
    ```

3.  TODO

    ``` rb
    object.kind_of?(klass)
    # => true
    klass.class_exec { def kind_of?(*) false end }
    object.kind_of?(klass)
    # => false
    ```

4.  TODO

    ``` rb
    object.instance_of?(klass)
    # => true
    klass.class_exec { def instance_of?(*) false end }
    object.instance_of?(klass)
    # => false
    ```

5.  TODO

    ``` rb
    klass == object.class
    # => true
    klass.class_exec { def self.==(*) false end }
    klass == object.class
    # => false
    ```

6.  TODO

    ``` rb
    klass.eql?(object.class)
    # => true
    klass.class_exec { def self.eql?(*) false end }
    klass.eql?(object.class)
    # => false
    ```

7.  TODO

    ``` rb
    klass.equal?(object.class)
    # => true
    klass.class_exec { def self.equal?(*) false end }
    klass.equal?(object.class)
    # => false
    ```

8.  TODO

    ``` rb
    klass != object.class
    # => false
    klass.class_exec { def self.!=(*) true end }
    klass != object.class
    # => true
    ```

9.  TODO

    ``` rb
    klass <= object.class
    # => true
    klass.class_exec { def self.<=(*) false end }
    klass <= object.class
    # => false
    ```

10. TODO

    ``` rb
    klass >= object.class
    # => true
    klass.class_exec { def self.>=(*) false end }
    klass >= object.class
    # => false
    ```

11. TODO

    ``` rb
    klass <=> object.class
    # => 0
    klass.class_exec { def self.<=>(*) nil end }
    klass <=> object.class
    # => nil
    ```

12. TODO

    ``` rb
    klass.object_id == object.class.object_id
    # => true
    klass.class_exec { def self.object_id; rand(26) end }
    klass.object_id == object.class.object_id
    # => false
    ```

13. TODO

    ``` rb
    klass.__id__ == object.class.__id__
    # => true
    klass.class_exec { def self.__id__; rand(26) end }
    klass.__id__ == object.class.__id__
    # => false
    ```

14. TODO

    ``` rb
    klass.ancestors.include?(instance.class)
    # => true
    klass.class_exec { def self.ancestors; [] end }
    klass.ancestors.include?(instance.class)
    # => false
    ```

15. TODO

    ``` rb
    klass.name == instance.class.name
    # => true
    klass.class_exec { def self.name; ('A'..'Z').to_a.sample end }
    klass.name == instance.class.name
    # => false
    ```

16. TODO

    ``` rb
    klass.to_s == instance.class.to_s
    # => true
    klass.class_exec { def self.to_s; ('A'..'Z').to_a.sample end }
    klass.to_s == instance.class.to_s
    # => false
    ```

17. TODO

    ``` rb
    klass.inspect == instance.class.inspect
    # => true
    klass.class_exec { def self.inspect; ('A'..'Z').to_a.sample end }
    klass.inspect == instance.class.inspect
    # => false
    ```

18. TODO

    ``` rb
    instance.class == instance.class
    # => true
    klass.class_exec { def self.class; Class.new end }
    instance.class == instance.class
    # => false
    ```

## Subclass

``` rb
other = Class.new
klass = Class.new(other)
```

Is `klass` a subclass of `other`?

1.  TODO

    ```rb
    klass < other
    # => true
    klass.class_exec { def self.<(*) false end }
    klass < other
    # => false
    ```

2.  TODO

    ```rb
    klass.superclass == other
    # => true
    klass.class_exec { def self.superclass; Class.new end }
    klass.superclass == other
    # => false
    ```

## Superclass

``` rb
klass = Class.new
other = Class.new(klass)
```

Is `klass` a superclass of `other`?

1.  TODO

    ``` rb
    klass > other
    # => true
    klass.class_exec { def self.>(*) false end }
    klass > other
    # => false
    ```

## Module

``` rb
other = Module.new
klass = Class.new { include other }
```

Does `klass` include `other`?

1.  TODO

    ``` rb
    klass.include?(other)
    # => true
    klass.class_exec { def self.include?(*) false end }
    klass.include?(other)
    # => false
    ```

2.  TODO

    ``` rb
    klass.included_modules.include?(other)
    # => true
    klass.class_exec { def self.included_modules; [] end }
    klass.included_modules.include?(other)
    # => false
    ```
