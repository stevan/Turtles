# Minimal Object VM Language Specification

## Overview

This specification defines a minimal object-oriented virtual machine with just 2 opcodes. The language is built around pure message passing between objects, with no special syntax for variables, control flow, or data structures - everything is accomplished through objects responding to messages.

## 1. The World Concept

### World Object

The **world** is the root object that contains all global state. There are no global variables in the traditional sense - instead, everything lives as slots in the world object. The world acts as the global namespace and bootstrap environment.

### Object Manipulation

All objects (including the world) support these fundamental slot operations:

- `setSlot:value: slotName value` - Set a slot to a value
- `getSlot: slotName` - Retrieve a slot's value
- `hasSlot: slotName` - Check if slot exists
- `removeSlot: slotName` - Delete a slot
- `slotNames` - Get array of all slot names

### Method Selector Syntax

Method names use Smalltalk-style syntax with colons indicating parameters:

- `toString` - Zero parameters
- `setValue: value` - One parameter
- `at:put: index value` - Two parameters
- `if:then:else: condition trueBlock falseBlock` - Three parameters

The number of colons in the selector determines the method's arity. Parameters are bound to `arg1`, `arg2`, etc. during method execution.

### Examples

```
// Store a value in the world
PUSH 42
PUSH world
SEND setSlot:value: "answer"

// Retrieve a value from the world
PUSH world
SEND getSlot: "answer"

// Create and store an object
PUSH world
SEND getSlot: "Object"
SEND new
PUSH world
SEND setSlot:value: "myObject"
```

## 2. Opcodes

The VM has exactly **2 opcodes**:

### PUSH value

Pushes a literal value or reference onto the execution stack.

**Supported values:**
- Numbers: `PUSH 42`, `PUSH 3.14`
- Booleans: `PUSH true`, `PUSH false`
- Strings: `PUSH "hello"`
- Special values: `PUSH nil`, `PUSH world`
- Bytecode blocks: `PUSH [PUSH 1, PUSH 2, SEND +]`

### SEND selector

Sends a message to an object, implementing method dispatch.

**Behavior:**
1. Pop the receiver object from the stack
2. Determine arity from selector (count colons)
3. Pop N arguments from the stack (in reverse order)
4. Bind arguments to `arg1`, `arg2`, etc. in the receiver's local scope
5. Look up method in receiver's method table
6. Execute method bytecode with receiver as `self`
7. Push return value onto stack

**Example:**
```
PUSH 5        // First argument
PUSH 3        // Second argument
PUSH calc     // Receiver
SEND add:to:  // Method with 2 parameters
```

## 3. Core Data Types

### Numbers

Represents integer and floating-point values.

**Methods:**
- `+: other` - Addition
- `-: other` - Subtraction
- `*: other` - Multiplication
- `/: other` - Division
- `==: other` - Equality comparison
- `>: other` - Greater than
- `<: other` - Less than
- `>=: other` - Greater than or equal
- `<=: other` - Less than or equal
- `toString` - Convert to string representation

### Booleans

Represents true/false values, enabling conditional execution.

**Methods:**
- `ifTrue: block` - Execute block if true
- `ifFalse: block` - Execute block if false
- `ifTrue:ifFalse: trueBlock falseBlock` - Conditional execution
- `not` - Logical negation
- `and: other` - Logical AND
- `or: other` - Logical OR
- `toString` - Convert to string ("true" or "false")

### Strings

Represents text data.

**Methods:**
- `==: other` - String equality
- `+: other` - String concatenation
- `size` - Get string length
- `at: index` - Get character at position
- `toString` - Returns self

### Objects

The fundamental building block - containers for slots and methods.

**Methods:**
- `new` - Create new instance
- `setSlot:value: slotName value` - Set slot value
- `getSlot: slotName` - Get slot value
- `hasSlot: slotName` - Check slot existence
- `removeSlot: slotName` - Delete slot
- `slotNames` - Get array of slot names
- `setMethod:body: selector bytecodeBlock` - Define method
- `getMethod: selector` - Get method bytecode
- `hasMethod: selector` - Check method existence
- `removeMethod: selector` - Delete method
- `toString` - String representation

### Pairs

Fundamental data structure for building lists, trees, and complex data.

**Methods:**
- `new:with: first second` - Create new pair
- `car` / `first` - Get first element
- `cdr` / `second` - Get second element
- `setCar: value` - Set first element
- `setCdr: value` - Set second element
- `toString` - String representation "(first . second)"

### Nil

Represents absence of value and list termination.

**Methods:**
- `isNil` - Always returns true
- `ifNil: block` - Execute block
- `ifNotNil: block` - Do nothing, return nil
- `toString` - Returns "nil"
- Most other messages return nil (graceful failure)

### Method Blocks

Bytecode sequences that can be executed as methods.

**Structure:**
Array of opcode/operand pairs that form executable code.

**Usage:**
```
// Define a method
PUSH [PUSH arg1, PUSH arg2, SEND +]  // Bytecode block
PUSH "add:to:"                        // Method selector
PUSH myObject                         // Target object
SEND setMethod:body:                  // Install method
```

## Bootstrap Environment

The world object is initialized with the following built-in types:

- `world.Object` - Object prototype
- `world.Number` - Number prototype
- `world.Boolean` - Boolean prototype
- `world.Pair` - Pair prototype
- `world.nil` - Nil singleton
- `world.true` - Boolean true
- `world.false` - Boolean false

## Example Program

```
// Create a calculator object
PUSH world
SEND getSlot: "Object"
SEND new
PUSH world
SEND setSlot:value: "Calculator"

// Define add:to: method
PUSH [PUSH arg1, PUSH arg2, SEND +]
PUSH "add:to:"
PUSH world
SEND getSlot: "Calculator"
SEND setMethod:body:

// Use the calculator
PUSH 5
PUSH 3
PUSH world
SEND getSlot: "Calculator"
SEND add:to:
// Result: 8 on stack
```

This creates a complete, minimal object-oriented language with maximum expressiveness through pure message passing.
