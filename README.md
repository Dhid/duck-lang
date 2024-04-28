# Duck Lang
A simple and certainly not useful programming language, and the goal is to learn about the Rust programming language, through trial and error.  
It's clearly inspired by Crafting interpreters and its implementations in Rust, that book is just perfect.  
If anyone is interested in contributing, any help is welcome, and anything, even maybe wrong will be useful to learn something together.  
## Examples
### Functions, variabled and contracts
```
fn foo(x, y):
  @require(y < 10),
  @require(x < 10),
{
  const s = x + y;
  return s;
}

const a = 8;
const b = 9;
const x = foo(a, b);
print(x);
```

### Objects
```
const bobby = #{
    age: 3,
    name: "Boccy the good boy",
};
print(">>> The first dog is called: ");
print(bobby.name);
print(">>> Age:");
print(bobby.age);
```

## Roadmap
A list of things to do, in not any particular order

1. mutability checks for variables and functions parameters via `const` and `mut`
2. modules for scripts in multiple files
3. standard library
4. a funny garbage collector
5. errors handling
6. anything you want to add

## Contribute
Help. Please. Anything.
