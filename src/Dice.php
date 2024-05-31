<?php

namespace Dice;

/**
 * A minimal dependency injection container
 *
 * Because `Dice` is immutable, methods that change its configuration return a
 * new object.
 */
class Dice
{
    const CONSTANT   = 'Dice::CONSTANT';
    const GLOBAL     = 'Dice::GLOBAL';
    const INSTANCE   = 'Dice::INSTANCE';
    const CHAIN_CALL = 'Dice::CHAIN_CALL';
    const SELF       = 'Dice::SELF';

    /**
     * Rule name => rule
     *
     * Names are normalised with:
     *
     * ```php
     * ltrim(strtolower($name), '\\')
     * ```
     *
     * @var array<string,array>
     * @see Dice::addRule()
     * @see Dice::addRules()
     */
    private $rules = [];

    /**
     * Name => instance factory
     *
     * Ensures each class is only reflected once.
     *
     * @var array<string,\Closure>
     */
    private $cache = [];

    /**
     * Name => shared instance
     *
     * For performance reasons, shared instances are added to this array with
     * **and** without a global namespace prefix.
     *
     * @var array<string,object>
     */
    private $instances = [];

    /**
     * Name => shared instance
     *
     * A backup copy of instances provided via {@see Dice::addShared()}.
     *
     * @var array<string,object>
     */
    private $shared = [];

    /**
     * Name => callback array
     *
     * Callbacks with a `$callbackId` have string keys, otherwise they have
     * integer keys.
     *
     * @var array<string,array<int|string,callable>>
     * @see Dice::addCallback()
     */
    private $callbacks = [];

    /**
     * @var bool
     */
    private $mutable = false;

    /**
     * Add a rule to a class or named instance
     *
     * The container is configured using rules provided by associative arrays.
     * See {@link https://r.je/dice.html#example3} for a description of the
     * rules.
     *
     * Note that if a shared instance of `$name` has been cached, calling
     * `addRule($name)` will remove it.
     *
     * @return Dice A new instance with `$rule` applied to `$name`.
     */
    public function addRule(string $name, array $rule): self
    {
        return $this->callMutable(
            static function (Dice $dice) use ($name, $rule) {
                return self::addRuleTo($dice, $name, $rule);
            }
        );
    }

    /**
     * Add multiple rules
     *
     * Equivalent to calling {@see Dice::addRule()} for each `$name => $rule`
     * pair in `$rules`.
     *
     * Example:
     *
     * ```php
     * $dice = $dice->addRules(json_decode(file_get_contents('rules.json')));
     * ```
     *
     * @param array<string,array>|string $rules An array of rules (`[$name =>
     * $rule, ...]`) or a JSON string.
     * @return Dice A new instance with `$rules` applied.
     */
    public function addRules($rules): self
    {
        if (is_string($rules)) {
            $rules = json_decode(file_get_contents($rules), true);
        }

        return $this->callMutable(
            static function (Dice $dice) use ($rules) {
                foreach ($rules as $name => $rule) {
                    self::addRuleTo($dice, $name, $rule);
                }

                return $dice;
            }
        );
    }

    /**
     * Add a substitution to the default rule
     *
     * @param mixed $substitution
     * @return Dice A new instance with `$name` mapped to `$substitution` in the
     * default rule's `'substitutions'` array.
     */
    public function addSubstitution(string $name, $substitution): self
    {
        $_rule                                     = $this->getDefaultRule();
        $rule                                      = $_rule;
        $rule['substitutions'][$name]              = $substitution;
        $rule['substitutions'][ltrim($name, '\\')] = $substitution;

        return $this->callMutable(
            static function (Dice $dice) use ($name, $rule) {
                $dice->rules['*'] = $rule;

                return self::flush($dice, $name);
            }
        );
    }

    /**
     * Provide an existing shared instance for a class or named instance
     *
     * @return Dice A new instance that resolves `$name` to `$instance`.
     */
    public function addShared(string $name, object $instance): self
    {
        $_name                   = ltrim($name, '\\');
        $dice                    = $this->removeRule($name);
        $dice->instances[$_name] = $dice->instances['\\' . $_name] = $instance;
        $dice->shared[$_name]    = $dice->shared['\\' . $_name]    = $instance;

        return $dice;
    }

    /**
     * Apply a callback to every object created for the given class or named
     * instance
     *
     * If `$name` is `'*'`, `$callback` will be applied to every object
     * instantiated by the container.
     *
     * If `$callbackId` is set and an existing callback was added with the same
     * `$callbackId`, it will be replaced, otherwise `$callback` will be added
     * after any existing callbacks.
     *
     * If `$first` is `true`, `$callback` will be added before any existing
     * callbacks.
     *
     * If a callback sets `$continue = false`, no more callbacks will be run on
     * the instance.
     *
     * Example:
     *
     * ```php
     * $dice = $dice->addCallback('*', function (object $instance, string $name, bool &$continue): object { return $instance->hydrate(); });
     * ```
     */
    public function addCallback(string $name, callable $callback, ?string $callbackId = null, bool $first = false): self
    {
        return $this->callMutable(
            static function (Dice $dice) use ($name, $callback, $callbackId, $first) {
                if ($first && !empty($dice->callbacks[$name])) {
                    if (is_null($callbackId)) {
                        $insert = [$callback];
                    } else {
                        $insert = [$callbackId => $callback];
                        unset($dice->callbacks[$name][$callbackId]);
                    }
                    array_splice($dice->callbacks[$name], 0, 0, $insert);
                } elseif (is_null($callbackId)) {
                    $dice->callbacks[$name][] = $callback;
                } else {
                    $dice->callbacks[$name][$callbackId] = $callback;
                }

                return self::flush($dice, $name, true);
            }
        );
    }

    private static function addRuleTo(Dice $dice, string $name, array $rule): self
    {
        if (isset($rule['instanceOf']) && (!array_key_exists('inherit', $rule) || $rule['inherit'] === true)) {
            $rule = array_replace_recursive($dice->getRule($rule['instanceOf']), $rule);
        }
        // Allow substitutions to be defined with a leading a slash
        if (isset($rule['substitutions'])) {
            foreach ($rule['substitutions'] as $key => $value) {
                $rule['substitutions'][ltrim($key, '\\')] = $value;
            }
        }
        // Clear any existing closures or instances for this class
        self::flush($dice, $name);
        $dice->rules[ltrim(strtolower($name), '\\')] = array_replace_recursive($dice->getRule($name), $rule);

        return $dice;
    }

    private static function flush(Dice $dice, string $name, bool $cacheOnly = false): self
    {
        if ($name === '*') {
            if (!$cacheOnly) {
                $dice->instances = $dice->shared;
            }
            $dice->cache = [];

            return $dice;
        }

        if (!$cacheOnly) {
            $_name = ltrim($name, '\\');
            unset($dice->instances[$_name], $dice->instances['\\' . $_name],
                $dice->shared[$_name], $dice->shared['\\' . $_name]);
        }
        unset($dice->cache[$name]);

        return $dice;
    }

    /**
     * If a rule has been added to the given class or named instance, remove it
     *
     * Any shared instances of `$name` will also be removed.
     */
    public function removeRule(string $name): self
    {
        return $this->callMutable(
            static function (Dice $dice) use ($name) {
                unset($dice->rules[ltrim(strtolower($name), '\\')]);
                self::flush($dice, $name);

                return $dice;
            }
        );
    }

    private function callMutable(callable $callback, $clone = true)
    {
        $dice = $this->mutable || !$clone
            ? $this
            : clone $this;
        list($mutable, $this->mutable) = [$this->mutable, true];
        try {
            return $callback($dice);
        } finally {
            $this->mutable = $mutable;
        }
    }

    /**
     * Returns true if a rule has been added to the class $name
     */
    public function hasRule(string $name): bool
    {
        return isset($this->rules[ltrim(strtolower($name), '\\')]);
    }

    /**
     * Returns the default rule
     *
     * If no default rule has been set, an empty array will be returned.
     */
    public function getDefaultRule(): array
    {
        return isset($this->rules['*']) ? $this->rules['*'] : [];
    }

    /**
     * Returns the rule applied to the given class or named instance
     */
    public function getRule(string $name): array
    {
        $_name = ltrim(strtolower($name), '\\');
        if (isset($this->rules[$_name])) {
            return $this->rules[$_name];
        }
        foreach ($this->rules as $key => $rule) {
            // Find a fallback rule for $name where:
            // - It's not a named instance, the rule is applied to a class name
            // - It's not the default rule
            // - The rule is applied to a parent class
            // - And that rule should be inherited to subclasses
            if (
                empty($rule['instanceOf']) &&
                $key !== '*' &&
                is_subclass_of($name, $key) &&
                (!array_key_exists('inherit', $rule) || $rule['inherit'] === true)
            ) {
                return $rule;
            }
        }
        // Return the default rule if no matching rule is found
        return $this->getDefaultRule();
    }

    /**
     * Returns true if a shared instance of $name is available
     */
    public function hasShared(string $name): bool
    {
        return !empty($this->instances[$name]);
    }

    /**
     * Returns a fully constructed object based on $name using $args and $share as constructor arguments if supplied
     *
     * @param string $name The name of the class to instantiate
     * @param array $args An array with any additional arguments to be passed into the constructor upon instantiation
     * @param array $share a list of defined in shareInstances for objects higher up the object graph, should only be
     * used internally
     * @return object A fully constructed object based on the specified input arguments
     */
    public function create(string $name, array $args = [], array &$share = [])
    {
        // Is there a shared instance set? Return it. Better here than a closure for this, calling a closure is slower.
        if (!empty($this->instances[$name])) {
            return $this->instances[$name];
        }

        // Create a closure for creating the object if there isn't one already
        if (empty($this->cache[$name])) {
            $this->cache[$name] = $this->getClosure(ltrim($name, '\\'), $this->getRule($name));
        }

        // Call the cached closure which will return a fully constructed object of type $name
        return $this->callMutable(
            static function (Dice $dice) use ($name, $args, &$share) {
                return $dice->cache[$name]($dice, $args, $share);
            },
            false
        );
    }

    /**
     * Returns a closure for creating object $name based on $rule, caching the reflection object for later use
     *
     * @param string $name the Name of the class to get the closure for
     * @param array $rule The container can be fully configured using rules provided by associative arrays. See
     * {@link https://r.je/dice.html#example3} for a description of the rules.
     * @return callable A closure
     */
    private function getClosure(string $name, array $rule)
    {
        $instanceOf = isset($rule['instanceOf']) ? $rule['instanceOf'] : null;
        if (!$instanceOf) {
            $instanceOf = $name;
        }

        // Throw an exception instead of crashing with a fatal error when PHP
        // fails to instantiate an interface
        if (interface_exists($instanceOf)) {
            return static function () use ($instanceOf) {
                throw new DiceException(sprintf('Cannot instantiate interface: %s', $instanceOf), $instanceOf);
            };
        }

        // Throw an exception when an instance of a non-existent class is
        // requested, instead of a ReflectionException now
        if (!class_exists($instanceOf)) {
            return static function () use ($instanceOf) {
                throw new DiceException(sprintf('Class does not exist: %s', $instanceOf), $instanceOf);
            };
        }
        // Reflect the class and constructor (this should only need to be done
        // once per class)
        $class = new \ReflectionClass($instanceOf);

        $constructor = $class->getConstructor();
        // Create a parameter generation closure so
        // $constructor->getParameters() is only called once
        $params = $constructor ? $this->getParams($constructor, $rule) : null;

        $maybeShare = static function (Dice $dice, $instance, array &$share) use ($name) {
            // The `shareInstances` loop below sets $share[$name] to `null`
            // before passing $share to $this->create() by reference, allowing
            // the newly created $instance to be shared early enough to avoid
            // infinite recursion if the object graph contains circular
            // dependencies
            if (array_key_exists($name, $share) && is_null($share[$name])) {
                $share[$name] = $instance;
            }

            return $instance;
        };
        if (!empty($rule['shared'])) {
            $_name = $class->isInternal() ? $class->name : $name;
            $maybeShare = static function (Dice $dice, $instance, array &$share) use ($maybeShare, $_name) {
                $dice->instances[$_name] = $dice->instances['\\' . $_name] = $instance;

                return $maybeShare($dice, $instance, $share);
            };
        }
        if ($params) {
            // Internal classes don't have circular dependencies and may not
            // work with newInstanceWithoutConstructor(), so they are always
            // constructed normally
            if ($class->isInternal() || empty($rule['shared'])) {
                $closure = static function (Dice $dice, array $args, array &$share) use ($class, $params, $maybeShare) {
                    // Call $params to generate dependencies from $args and $share
                    return $maybeShare($dice, new $class->name(...$params($dice, $args, $share)), $share);
                };
            }
            // Other classes can be instantiated without calling the constructor
            // until dependencies are resolved, allowing circular dependencies
            // in the object graph. This is dangerous and probably shouldn't be
            // accommodated, but is relied upon by some Dice users.
            if (!$class->isInternal()) {
                $circularClosure = static function (Dice $dice, array $args, array &$share) use ($class, $constructor, $params, $maybeShare) {
                    $instance = $maybeShare($dice, $class->newInstanceWithoutConstructor(), $share);
                    $constructor->invokeArgs($instance, $params($dice, $args, $share));

                    return $instance;
                };
                // Only use $circularClosure if:
                // - $rule['shared'] is set, or
                // - the class is listed in a $rule['shareInstances'] higher in
                //   the object graph (only known at runtime)
                $closure = !isset($closure)
                    ? $circularClosure
                    : static function (Dice $dice, array $args, array &$share) use ($closure, $circularClosure, $name) {
                        if (array_key_exists($name, $share)) {
                            return $circularClosure($dice, $args, $share);
                        }

                        return $closure($dice, $args, $share);
                    };
            }
        } else {
            // Or just instantiate the class
            $closure = static function (Dice $dice, array $args, array &$share) use ($class, $maybeShare) {
                return $maybeShare($dice, new $class->name, $share);
            };
        }

        // If there are shared instances, create and merge them with shared
        // instances higher in the object graph
        if (isset($rule['shareInstances'])) {
            $closure = static function (Dice $dice, array $args, array &$share) use ($closure, $rule) {
                // Copy $share to prevent shared instances lower in the object
                // graph tainting higher ones
                $_share = $share;
                foreach (array_diff($rule['shareInstances'], array_keys($_share)) as $instance) {
                    // This allows $maybeShare to detect recursion (see above)
                    $_share[$instance] = null;
                    $_share[$instance] = $dice->create($instance, [], $_share);
                }

                return $closure($dice, $args, $_share);
            };
        }

        // When $rule['call'] is set, wrap the closure in another closure which will call the required methods after
        // constructing the object By putting this in a closure, the loop is never executed unless call is actually set
        if (isset($rule['call'])) {
            $closure = static function (Dice $dice, array $args, array &$share) use ($closure, $class, $rule, $name) {
                // Construct the object using the original closure
                $object = $closure($dice, $args, $share);

                foreach ($rule['call'] as $call) {
                    // Generate the method arguments using getParams() and call the returned closure
                    $params = $dice->getParams(
                        $class->getMethod($call[0]),
                        ['shareInstances' => isset($rule['shareInstances']) ? $rule['shareInstances'] : []]
                    )($dice, $dice->expand(isset($call[1]) ? $call[1] : []), $share);
                    $return = $object->{$call[0]}(...$params);
                    if (isset($call[2])) {
                        if ($call[2] === self::CHAIN_CALL) {
                            if (!empty($rule['shared'])) {
                                $dice->instances[$name] = $return;
                            }
                            if (is_object($return)) {
                                $class = new \ReflectionClass(get_class($return));
                            }
                            $object = $return;
                        } else {
                            if (is_callable($call[2])) {
                                call_user_func($call[2], $return);
                            }
                        }
                    }
                }

                return $object;
            };
        }

        // If callbacks have been added for $name, enclose $closure in another
        // closure that applies them
        $callbacks = array_merge(
            isset($this->callbacks[$name]) ? array_values($this->callbacks[$name]) : [],
            isset($this->callbacks['*']) ? array_values($this->callbacks['*']) : []
        );
        if ($callbacks) {
            $closure = static function (Dice $dice, array $args, array &$share) use ($closure, $callbacks, $name) {
                $object   = $closure($dice, $args, $share);
                $continue = true;
                foreach ($callbacks as $callback) {
                    $object = $callback($object, $name, $continue);
                    if (!$continue) {
                        break;
                    }
                }

                return $object;
            };
        }

        // If $rule['inherit'] is true, $rule['instanceOf'] is set and the class
        // it refers to has a ['shared' => true] rule, add a closure that:
        // - returns the shared instance of $rule['instanceOf'] if it already
        //   exists, or
        // - creates an object normally and makes it the shared instance of
        //   $rule['instanceOf']
        if (
            $instanceOf &&
            !empty($this->getRule($instanceOf)['shared']) &&
            (!array_key_exists('inherit', $rule) || $rule['inherit'] === true)
        ) {
            $closure = static function (Dice $dice, array $args, array &$share) use ($closure, $instanceOf) {
                if (!empty($dice->instances[$instanceOf])) {
                    return $dice->instances[$instanceOf];
                }

                return $dice->instances[$instanceOf] = $dice->instances['\\' . $instanceOf] = $closure($dice, $args, $share);
            };
        }

        return $closure;
    }

    /**
     * Looks for Dice::INSTANCE, Dice::GLOBAL or Dice::CONSTANT array keys in $param and when found returns an object
     * based on the value see {@link https://r.je/dice.html#example3-1}
     *
     * @param mixed $param Either a string or an array,
     * @param array $share Array of instances from 'shareInstances', required for calls to `create`
     * @param bool $createFromString
     * @return mixed
     */
    private function expand($param, array &$share = [], bool $createFromString = false)
    {
        $this->expandParam($param, $share, $createFromString);

        return $param;
    }

    /**
     * Identical to expand, but references are preserved
     *
     * @see Dice::expand()
     */
    private function &expandByRef(&$param, array &$share = [], bool $createFromString = false)
    {
        $this->expandParam($param, $share, $createFromString);

        return $param;
    }

    private function expandParam(&$param, array &$share = [], bool $createFromString = false)
    {
        if (is_array($param)) {
            // if a rule specifies Dice::INSTANCE, look up the relevant instance
            if (isset($param[self::INSTANCE])) {
                if ($param[self::INSTANCE] === self::SELF) {
                    $param = $this;

                    return;
                }
                // Support Dice::INSTANCE by creating/fetching the specified instance
                if (is_array($param[self::INSTANCE])) {
                    $this->expandParam($param[self::INSTANCE][0], $share, true);
                }
                if (is_callable($param[self::INSTANCE])) {
                    $param = call_user_func($param[self::INSTANCE]);

                    return;
                } else {
                    $param = $this->create($param[self::INSTANCE], $share);

                    return;
                }
            } else {
                if (isset($param[self::GLOBAL])) {
                    $param = $GLOBALS[$param[self::GLOBAL]];

                    return;
                } else {
                    if (isset($param[self::CONSTANT])) {
                        $param = constant($param[self::CONSTANT]);

                        return;
                    } else {
                        foreach ($param as &$value) {
                            $this->expandParam($value, $share);
                        }
                    }
                }
            }
        }

        if (is_string($param) && $createFromString) {
            $param = $this->create($param, $share);
        }
    }

    /**
     * Searches for an object or `null` in $search that can be passed to $param
     *
     * If found, the value is returned and optionally removed from the array.
     *
     * @param bool $remove If set (the default), the matched value will be
     * removed from `$search`.
     * @return object|null|false `false` if no suitable value was found.
     */
    private function matchParam(\ReflectionParameter $param, ?string $class, array &$search, bool $remove = true)
    {
        if (!$class) {
            return false;
        }
        foreach ($search as $key => $value) {
            if ($value instanceof $class || ($value === null && $param->allowsNull())) {
                // Remove the value from $search so it won't wrongly match
                // another parameter
                if ($remove) {
                    unset($search[$key]);
                }

                return $value;
            }
        }

        return false;
    }

    /**
     * Identical to matchParam, but references in $search are preserved
     *
     * @see Dice::matchParam()
     */
    private function &matchParamByRef(\ReflectionParameter $param, ?string $class, array &$search, bool $remove = true)
    {
        $false = false;
        if (!$class) {
            return $false;
        }
        foreach ($search as $key => &$value) {
            if ($value instanceof $class || ($value === null && $param->allowsNull())) {
                // Remove the value from $search so it won't wrongly match
                // another parameter
                if ($remove) {
                    unset($search[$key]);
                }

                return $value;
            }
        }

        return $false;
    }

    /**
     * Returns a closure that generates arguments for $method based on $rule and any $args passed into the closure
     *
     * @param \ReflectionMethod $method An instance of ReflectionMethod (see:
     * {@link http://php.net/manual/en/class.reflectionmethod.php})
     * @param array $rule The container can be fully configured using rules provided by associative arrays. See
     * {@link https://r.je/dice.html#example3} for a description of the rules.
     * @return callable A closure that uses the cached information to generate the arguments for the method
     */
    private function getParams(\ReflectionMethod $method, array $rule)
    {
        // Cache some information about the parameter in $paramInfo so (slow) reflection isn't needed every time
        $paramInfo = [];
        $skipAfter = -1;
        $index     = -1;
        foreach ($method->getParameters() as $param) {
            $index++;
            if (!$param->isOptional()) {
                $skipAfter = $index;
            }
            $type        = $param->getType();
            $class       = $type instanceof \ReflectionNamedType && !$type->isBuiltIn() ? $type->getName() : null;
            $byRef       = $param->isPassedByReference();
            $paramInfo[] = [$class, $byRef, $param, isset($rule['substitutions']) && array_key_exists($class, $rule['substitutions'])];
        }

        // Return a closure that uses the cached information to generate the arguments for the method
        return static function (Dice $dice, array $args, array &$share = []) use ($paramInfo, $skipAfter, $rule) {
            // If the rule has construtParams set, construct any classes reference and use them as $args
            if (isset($rule['constructParams'])) {
                $args = array_merge($args, $dice->expand($rule['constructParams'], $share));
            }

            // Array of matched parameters
            $parameters = [];

            // Fnd a value for each method argument
            foreach ($paramInfo as $index => list($class, $byRef, $param, $sub)) {
                // Bail out if there are no more $args to assign and no
                // mandatory parameters to resolve
                if (!$args && $index > $skipAfter) {
                    return $parameters;
                }
                // Loop through $args and see whether or not each value can match the current parameter based on type
                // hint
                if ($args && $byRef && ($match = &$dice->matchParamByRef($param, $class, $args)) !== false) {
                    $parameters[] = &$match;
                } elseif ($args && !$byRef && ($match = $dice->matchParam($param, $class, $args)) !== false) {
                    $parameters[] = $match;
                }
                // Do the same with $share
                else {
                    unset($match);
                    if ($share && ($match = $dice->matchParam($param, $class, $share, false)) !== false) {
                        $parameters[] = $match;
                    }
                    // When nothing from $args or $share matches but a class is type hinted, create an instance to use,
                    // using a substitution if set
                    else {
                        if ($class) {
                            if ($sub) {
                                $parameters[] = $dice->expand($rule['substitutions'][$class], $share, true);
                            } else {
                                $parameters[] = !$param->allowsNull() ? $dice->create($class, [], $share) : null;
                            }
                        } else {
                            // Support PHP 7 scalar type hinting, is_a('string', 'foo') doesn't work so this is a hacky
                            // AF workaround: call_user_func('is_' . $type, '')

                            // Find a match in $args for scalar types
                            if ($args && $byRef && $param->getType()) {
                                foreach ($args as $key => &$value) {
                                    if (call_user_func('is_' . $param->getType()->getName(), $value) || (is_null($value) && $param->allowsNull())) {
                                        $parameters[] = &$value;
                                        unset($args[$key], $value);
                                        break;
                                    }
                                }
                            } elseif ($args && !$byRef && $param->getType()) {
                                foreach ($args as $key => $value) {
                                    if (call_user_func('is_' . $param->getType()->getName(), $value) || (is_null($value) && $param->allowsNull())) {
                                        $parameters[] = $value;
                                        unset($args[$key]);
                                        break;
                                    }
                                }
                            } else {
                                if ($args && $byRef) {
                                    reset($args);
                                    $parameters[] = &$dice->expandByRef($args[key($args)]);
                                } elseif ($args && !$byRef) {
                                    $parameters[] = $dice->expand(array_shift($args));
                                }
                                // For variadic parameters, provide remaining $args
                                else {
                                    if ($param->isVariadic()) {
                                        $parameters = array_merge($parameters, $args);
                                    }
                                    // There's no type hint and nothing left in $args, provide the default value or null
                                    else {
                                        $parameters[] = $param->isDefaultValueAvailable() ? $param->getDefaultValue() : null;
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return $parameters;
        };
    }
}
