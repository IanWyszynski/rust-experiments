use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

use std::thread;
use std::time::Duration;

use std::sync::{mpsc, Arc, Mutex};

#[derive(Debug)]
pub enum List {
    // Rc<T> allows multiple immutable references to the same
    // data. RefCell<T> allows interior mutability (mutable access
    // in an immutable context).
    //
    // Rc<RefCell<T>> allows multiple mutable references, with
    // owernship rules checked at runtime.
    Cons(Rc<RefCell<i32>>, Rc<List>),
    Nil,
}

pub struct MyBox<T>(T);

impl<T> MyBox<T> {
    pub fn new(x: T) -> MyBox<T> {
        MyBox(x)
    }
}

impl<T> Deref for MyBox<T> {
    type Target = T;

    // Note that the deref function returns a reference because,
    // in most cases where we use the dereference operator, we're
    // not interested in moving the contained value out of the struct.i128
    // Rather, we're just interested in borrowing it, and following
    // the borrowed reference.
    fn deref(&self) -> &T {
        &self.0
    }
}

pub trait Messenger {
    fn send(&self, msg: &str);
}

pub struct LimitTracker<'a, T: Messenger> {
    messenger: &'a T,
    value: usize,
    max: usize,
}

impl<'a, T> LimitTracker<'a, T>
where
    T: Messenger,
{
    // Note the first lifetime elision rule here, there is
    // only one reference argument, so the lifetime of the
    // returned reference is implicitly smaller than the lifetime
    // of the messenger struct.
    pub fn new(messenger: &T, max: usize) -> LimitTracker<T> {
        LimitTracker {
            messenger,
            value: 0,
            max,
        }
    }

    pub fn set_value(&mut self, value: usize) {
        self.value = value;
        let pct = self.value as f64 / self.max as f64;
        if pct >= 1.0 {
            self.messenger.send("Over quota!")
        } else if pct >= 0.9 {
            self.messenger.send("Close to quota!")
        } else if pct >= 0.75 {
            self.messenger.send("Approaching quota!")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;

    struct MockMessenger {
        // Function signature of Messenger#send means that we
        // can't mutate the list of sent messages if we just stored
        // a Vec<String> here.
        //
        // Note that we pay a small performance penalty to check the
        // ownership rules at runtime.
        //
        // The RefCell keeps track of the number of mutable and immutable
        // references created through the `borrow()` and `borrow_mut()` APIs.
        // The API panics if we violate Rust's ownership rules.
        sent: RefCell<Vec<String>>,
    }

    impl MockMessenger {
        fn new() -> MockMessenger {
            MockMessenger {
                sent: RefCell::new(vec![]),
            }
        }
    }

    impl Messenger for MockMessenger {
        fn send(&self, message: &str) {
            // borrow_mut returns a RefMut<Vec<String>>
            self.sent.borrow_mut().push(String::from(message));
        }
    }

    #[test]
    fn approaching_threshold_test() {
        let mock = MockMessenger::new();
        let mut tracker = LimitTracker::new(&mock, 100);
        tracker.set_value(80);
        // borrow returns a Ref<Vec<String>>, this implements
        // the deref trait so it can be treated like any reference.
        assert_eq!(mock.sent.borrow().len(), 1);
    }
}

#[allow(dead_code)]
fn refcells() {
    let value = Rc::new(RefCell::new(5));

    // Recall that Rc::clone is equivalent to value.clone(). Rust's
    // convention is that implementations of the Clone trait perform
    // deep copies of the data.
    let a = Rc::new(List::Cons(Rc::clone(&value), Rc::new(List::Nil)));

    let b = List::Cons(Rc::new(RefCell::new(3)), Rc::clone(&a));
    let c = List::Cons(Rc::new(RefCell::new(4)), Rc::clone(&a));

    // Thanks to interior mutability, we can borrow this
    // reference, which is currently shared triply-owned
    // and update it's value. The updated value will be
    // reflect in each of the owners.

    // There's an additional bit of magic going on here.
    // value is actually an Rc<RefCell<i32>>. Despite that,
    // we're calling borrow_mut() on it as if value were a
    // RefCell<i32>. Rust is doing "automatic dereferencing"
    // here. Namely, it is using:
    //
    // 1. The name of the method being called (`borrow_mut`)
    // 2. The type of the borrowed `self` parameter in the
    //    definition of that method
    //
    // To figure out what sequence of &, &mut, or * is needed
    // so that the method-receiver (`value` in this case), assumes
    // the necessary type.
    //
    // In this case, the code is transformed into *(*value).borrow_mut().
    // TODO: What happens if Rust can't uniquely identify the receiver type
    // because the reference, and the value it points to, both have an identically
    // named method?
    *value.borrow_mut() += 10;

    // println!("a = {:?}", a);
    // println!("b = {:?}", b);
    // println!("c = {:?}", c);
}

#[allow(dead_code)]
fn basic_threads() {
    // By default the thread spawned here will be stopped
    // when the main thread stops.
    let handle = thread::spawn(|| {
        for i in 1..10 {
            println!("thread prints {}", i);
            thread::sleep(Duration::from_millis(1));
        }
    });

    handle.join().unwrap();

    for i in 1..5 {
        println!("main thread prints {}", i);
        thread::sleep(Duration::from_millis(1));
    }
}

#[allow(dead_code, unused_variables)]
fn move_captures_threads() {
    let v = vec![1, 2, 3];

    // If we don't use a move-capture keyword, Rust will infer
    // that the vector can be captured by borrowing it (probably
    // because this is the lowest level of ownership needed?
    // -- it's being conservative).
    //
    // However, because Rust doesn't know how long the function
    // will run, it _MUST_ assume that the function (and therefore
    // the captured reference) may outlive the vector.
    let capture = thread::spawn(move || println!("captured vector: {:?}", v));

    capture.join().unwrap();
}

#[allow(dead_code)]
fn mpsc_basic() {
    let (tx, rx) = mpsc::channel::<std::string::String>();

    // thread::spawn(move || drop(tx));

    // // When the Send<T> side of a channel is dropped, the receiver
    // // gets a RecvError.
    // match rx.recv() {
    //     Ok(value) => println!("{}", value),
    //     Err(e) => println!("{:?}", e),
    // }
    let tx_clone = mpsc::Sender::clone(&tx);
    thread::spawn(move || {
        let vals = vec![
            String::from("a"),
            String::from("b"),
            String::from("c"),
            String::from("d"),
        ];

        for val in vals {
            tx.send(val).unwrap();
            thread::sleep(Duration::from_secs(1));
        }
    });

    thread::spawn(move || {
        let vals = vec![
            String::from("1"),
            String::from("2"),
            String::from("3"),
            String::from("4"),
        ];

        for val in vals {
            tx_clone.send(val).unwrap();
            thread::sleep(Duration::from_secs(1));
        }
    });

    for received in rx {
        println!("got: {}", received);
    }
}

#[allow(dead_code)]
fn mutex_basics() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for i in 0..10 {
        // Atomically bump the reference count.
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            // Recall that automatic dereferencing is at play here because counter is an Arc<Mutex<T>> .
            // Further, lock(&self) actually returns a LockResult<MutexGuard<T>>. The reason we return
            // a LockResult is that Rust mutexes implement Poisoning: if the thread
            // currently holding the mutex panics, all threads blocked on that
            // mutex will receive an Err(poisoned) from lock or try_lock.
            //
            // Recall further that Mutex<T>::lock().unwrap() returns a MutexGuard<T>, which is a smart
            // pointer. If you don't store that smart pointer in a variable, it will go out of scope an
            // call it's Deref trait drop implementation immediately. It can also be useful to explicitly
            // std::mem::drop a mutex early.
            let mut num = counter.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    println!("result: {}", *counter.lock().unwrap());
}

#[allow(dead_code)]
fn shared_state_concurrency() {
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    let counter1 = Arc::clone(&counter);
    handles.push(thread::spawn(move || {
        let _ = counter1.lock().unwrap();
        thread::sleep(Duration::from_secs(10));
        panic!(format!("thread {:?} panic!", thread::current().id()));
    }));

    let counter2 = Arc::clone(&counter);
    handles.push(thread::spawn(move || match counter2.lock() {
        Ok(num) => println!(
            "thread {:?} acquires lock with value {}!",
            thread::current().id(),
            num
        ),
        Err(err) => println!("thread {:?} poisoned! err: {}", thread::current().id(), err),
    }));

    for handle in handles {
        println!("joining thread {:?}", handle.thread().id());
        match handle.join() {
            Ok(()) => println!("main thread joined successfully on handle for thread",),
            Err(e) => println!("main thread failed to join on handle {:?}", e),
        }
    }
}

pub trait Draw {
    fn draw(&self);
}

pub struct Screen {
    // Box<dyn Draw> points to two things:
    // 1. The trait `Draw`
    // 2. A table used to lookup trait methods on the concrete type _at runtime_
    pub components: Vec<Box<dyn Draw>>,
}

pub struct ScreenTraitBound<T: Draw> {
    pub components: Vec<T>,
}

pub struct Button {
    pub width: u32,
    pub height: u32,
    pub label: String,
}

impl Draw for Button {
    fn draw(&self) {}
}

struct SelectBox {
    width: u32,
    height: u32,
    options: Vec<String>,
}

impl Draw for SelectBox {
    fn draw(&self) {
        // code to actually draw a select box
    }
}

// Note that using a trait bound is different than using a
// trait object. Trait bounds allow us to fill in a single
// type per `ScreenTraitBound`. The struct is impl is then
// monomorphized.

// Trait objects don't perform monomorphization. They instead
// resolve the concrete type trait method implementations
// at runtime. When using homogenous collections, use trait bounds.
impl<T> ScreenTraitBound<T>
where
    T: Draw,
{
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}

impl Screen {
    pub fn run(&self) {
        for component in self.components.iter() {
            component.draw();
        }
    }
}

fn polymorphism_with_trait_objects() {
    let screen = Screen {
        components: vec![
            // To create trait objects, we wrap objects implementing
            // the trait in the Box smart pointer.
            Box::new(SelectBox {
                width: 1,
                height: 2,
                options: vec![String::from("Option")],
            }),
            Box::new(Button {
                width: 1,
                height: 2,
                label: String::from("OK"),
            }),
        ],
    };
    screen.run();
}

trait State {
    // Note that the `request_review` method here takes a Box<Self>,
    // which is not a borrow. We are actually consuming the state here.
    // Also note that we are using `Self` -- a stand in type for the
    // concrete type implementing the state trait. This is because many
    // type may implement this trait.
    fn request_review(self: Box<Self>) -> Box<dyn State>;
}

struct Draft {}

struct PendingReview {}

impl State for PendingReview {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        self
    }
}

impl State for Draft {
    fn request_review(self: Box<Self>) -> Box<dyn State> {
        Box::new(PendingReview {})
    }
}

pub struct Post {
    state: Option<Box<dyn State>>,
    content: String,
}

impl Post {
    pub fn new() -> Post {
        Post {
            state: Some(Box::new(Draft {})),
            content: String::new(),
        }
    }

    pub fn request_review(&mut self) {
        // `take` takes the value out of an option, leaving
        // None in it's place. Note that we are literally
        // consuming the current state of the post, and creating
        // a new state. This makes sense, because the Post should
        // only ever be in one state. Also note that this code
        // explains why we need the type of the state member to
        // be an Option.

        // Rust does not allow unpopulated struct fields. In order
        // to consume the state, we need to leave some default value
        // in the Post struct member. The other option would be to
        // borrow the field (why wouldn't we do that?)
        if let Some(s) = self.state.take() {
            self.state = Some(s.request_review())
        }
    }

    pub fn content(&self) -> &str {
        // TODO: Should eventually read the state that the post
        // is in and return content appropriately.
        ""
    }

    pub fn add_text(&mut self, text: &str) {
        self.content.push_str(text);
    }
}

enum Message {
    Hello { id: i32 },
}

fn patterns() {
    let x = 5;
    let y = Some(5);

    match y {
        Some(x) if x > 5 => println!("x = {}", x),
        _ => println!("no match"),
    }

    let msg = Message::Hello { id: 5 };

    // @ bindings allow one to store the value of a matched
    // variable while matching it.
    match msg {
        Message::Hello {
            id: id_variable @ 3..=7,
        } => println!("Found id in range: {}", id_variable),
        // You can see how in this arm, the actual value that
        // was matched is not reference-able in the body of the
        // arm.
        Message::Hello { id: 10..=12 } => println!("Found an id in another range:"),
        // Here, we have a reference to the id member of the
        // struct, but we haven't performed any test on it.
        Message::Hello { id } => println!("Found another id: {}", id),
    }
}

fn object_oriented() {
    let mut post = Post::new();

    post.add_text("I'm going to NYC today.");
    assert_eq!("", post.content());

    post.request_review();
    assert_eq!("", post.content());

    // post.approve();
    // assert_eq!("I'm going to NYC today.", post.content());
}

unsafe fn dangerous() {}

fn unsafe_basics() {
    // Not cool, unsafe functions need to be called in unsafe
    // blocks.
    unsafe {
        dangerous();
    }

    let mut num = 5;

    // Note that we can create raw pointers outside of
    // unsafe blocks.
    let r1 = &num as *const i32;
    let r2 = &mut num as *mut i32;

    unsafe {
        println!("r1 = {}", *r1);
        println!("r2 = {}", *r2);
    }
    let mut v = vec![1, 2, 3, 4];

    let r = &mut v[..];

    // The method `split_at_mut` is a safe wrapper over
    // an unsafe implementation.
    let (a, b) = r.split_at_mut(3);

    assert_eq!(a, &mut [1, 2, 3]);
    assert_eq!(b, &mut [4]);
}

fn split_at_mut(slice: &mut [i32], split: usize) -> (&mut [i32], &mut [i32]) {
    use std::slice;

    let len = slice.len();
    // Returns a raw pointer to the buffer. If the buffer is modified,
    // it's memory may be re-allocated -- this could invalidate the pointer.
    let ptr = slice.as_mut_ptr();
    assert!(split <= len);
    // Note that in this function, we are returning references to
    // disjoint parts of the same slice. This is okay, since the
    // memory being pointed to doesn't overlap, but the Rust compiler
    // can't figure this out. All it knows is that two mutable borrows
    // are occurring for the same slice.
    // (&mut slice[..split], &mut slice[split..])
    unsafe {
        (
            // Note that in order to use from_raw_parts_mut correctly, not
            // only must the allocated memory be contiguous, it must also
            // be part of the same "allocated object."
            slice::from_raw_parts_mut(ptr, split),
            slice::from_raw_parts_mut(ptr.add(split), len - split),
        )
    }
}

// Tells the Rust compiler which application binary interface (ABI),
// the external functions use. The ABI defines how to call a function
// at the assembly level.
extern "C" {
    fn abs(input: i32) -> i32;
}

// This is an example of how to declare a function that is callable from
// another programming language using the C ABI. Note that we must disable
// name-mangling. Mangling is a process by which the compiler re-assigns
// the names of functions so that they contain additional information that
// may be used to optimize compilation.
#[no_mangle]
pub extern "C" fn call_from_c() {
    println!("called a Rust function from C");
}

// In Rust, global variables are called "static" variables. By default,
// such variables have the 'static lifetime, which means they are alive
// for the duration of the process.
//
// There are some differences between constants an immutable static variables.
//
// 1. Immutable static variables have a fixed address in memory. The same
//    memory dereference takes place for each access to the variable.
//
// 2. Static variables can be mutable, constants cannot.
//
// Accessing and modifying mutable static variables is _unsafe_.
// (Because it could result in data races? Y)
static HELLO_WORLD: &str = "Hello, world!";

// Traits can also be unsafe if at least one of it's methods is unsafe.
// Recall the Send + Sync traits. We may want to create a type T that contains
// a raw pointer, by make T Send and Sync. In such a situation we'll need to
// use unsafe, because Rust can't verify that a raw pointer can be sent safely
// across threads.
unsafe trait Foo {}

// Here we are implementing an unsafe trait. In doing this, we are promising
// that we uphold the invariants that the compiler can't verify in the def
// of Foo.
unsafe impl Foo for i32 {}

// An associated type connects a type placeholder with a trait such that the
// trait method definitions can use the placeholder types in their signatures.

// In this case, the associated type is `Item`.
pub trait Iterator {
    type Item;

    // Notice that `Self` is the concrete type of the struct on which
    // this trait is being implemented -- we are using it's definition
    // of what `Item` is (it could be anything that the trait implementor
    // specifies).
    fn next(&mut self) -> Option<Self::Item>;
}

struct Counter {
    #[allow(dead_code)]
    count: i32,
}

// Why use associated types if you have generics?
impl Iterator for Counter {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
        None
    }
}

// Q: Why not just make the Iterator trait something like this?
// A: When you have a generic type, you have a separate trait implementation
// for each instance of the generic type:
// - impl GenericIterator<String> for Counter
// - impl GenericIterator<i32> for Counter
// ...
// Which means that when you call `next` on a Counter instance, you need
// to include a type annotation to indicate which trait implementation you
// want to use.
//
// Using the Iterator trait with an associated type, we can only do one
// impl Iterator for Counter, so the associated type is fixed a single time.
// With this, the compiler is able to determine what associated type to use
// when we call `next()` on this instance of Counter.
pub trait GenericIterator<T> {
    fn next(&mut self) -> Option<T>;
}

// Rust has defaults for generic type parameters.
trait Add<Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
}

struct Point {
    x: i32,
    y: i32,
}

// The generic type is silently defaulted to the concrete type of the
// implementor. You can also override the generics by explicitly specifying
// a generic type. Default generic type parameters are useful if you want
// to add functionality to an existing trait without breaking current
// implementors (sort of like default arguments).
impl Add for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Point {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

// A few things about traits:
// 1. You can have trait A and trait B with an identically named method M.
// 2. You can have a struct C with a named method M.
// 3. You can implement A and B for C.
//
// In this situation, in order to call M on C, you need to specify which M
// you need. There is special syntax for this.
//
// If you write:
// c.M(),
//
// Rust will call the structs implementation directly -- not the trait
// implementation.

trait A {
    fn m(&self);
}

trait B {
    fn m(&self);
}

struct C;

impl A for C {
    fn m(&self) {
        println!("A::M");
    }
}

impl B for C {
    fn m(&self) {
        println!("B::M");
    }
}

impl C {
    #[allow(dead_code)]
    fn m(&self) {
        println!("C::M");
    }
}

// Typically, you can have two types D and E implement trait F. For methods
// in F that take a `self` parameter, Rust can determine which method to
// call automatically based on the type of `self`. If a method in F _doesn't_
// take a `self`, then there is ambiguity!

trait F {
    fn m() -> String;
}

struct D;

impl D {
    fn m() -> String {
        String::from("D::M")
    }
}

impl F for D {
    fn m() -> String {
        String::from("F::M")
    }
}

// When defining a trait, you might want to rely on functionality provided
// by another trait. The relied-on trait is called a "super-trait".
//
// In order to implement a trait A that has a super trait B, the implementing
// type must implement B.

trait SuperTrait {
    fn super_trait_method(&self);
}

trait SubTrait: SuperTrait {
    fn sub_trait_method(&self) {
        // We can call this method becuase we know that the implementor
        // also implements `SuperTrait`.
        self.super_trait_method();
    }
}

// Recall the constraint that in order to implement a trait on a type,
// either the trait or the type must live in the crate that contains
// the implementation.

// This prevents ambiguity might occur if some external consumer depended
// on our crate and the crates holding the trait/type (since those crates
// could now contain another implementation). This is called the orphan rule.

// There's a way to get around this. It's called the `newtype pattern`. It
// involves creating a tuple-struct that wraps the (potentially non-local) type
// we want to implement a (potentially non-local) trait for. When the compiler
// sees that we are using this pattern, it will elide the tuple struct --
// there will be no runtime cost for using this pattern.

struct Wrapper(Vec<String>);

use std::fmt;

impl Wrapper {
    fn new() -> Wrapper {
        Wrapper(vec![])
    }
}

// Notice that the trait and the wrapped type (Vec<String>) are both non-local,
// but because `Wrapper` is so thin, we're basically implementing fmt::Display
// for Vec<String>.

// The downside of using `Wrapper` here is that it's not _actually_ the original
// type. In order to use the functionality that `Vec<String>` provides, we'd need
// to add a bunch of methods that proxy calls to self.0. One nice shorter
// solution is to just implement the Deref trait.
impl fmt::Display for Wrapper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]", self.0.join(", "))
    }
}

impl Deref for Wrapper {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

use std::ops::DerefMut;

// Note that Deref is a super-trait of DerefMut, this means that
// it inherit's Deref's associated type!
impl DerefMut for Wrapper {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[allow(dead_code)]
fn advanced_traits() {
    let wrapper = Wrapper::new();

    println!("{}", wrapper.len());
    // let c = C;

    // A::M(&c); // calls C's A implementation.
    // B::M(&c); // calls C's B implementation.
    // C::M(&c); // calls C's M implementation.
    // c.M(); // calls C's M implementation.

    println!("{}", D::m());

    // Note that we can't do this, because we don't know which implementation
    // of F we want.
    // println!("{}", F::M());

    // We need to fully qualify this. This syntax says "call implementation of
    // the trait method F::M defined on the type D".
    println!("{}", <D as F>::m());
}

#[allow(dead_code)]
fn advanced_types() {
    // We can declare a type alias. The alias can be used wherever the target
    // type is used -- there's no additional type safety we get here.
    type Kilometeres = i32;

    // The main use case of type alias is to reduce repetition for complex
    // types.

    let f: Box<dyn Fn() + Send + 'static> = Box::new(|| println!("hi!"));

    // By the way, thunk is a name for code that shouldbe evaluated later.
    type Thunk = dyn Fn() + Send + 'static;

    // Type aliases can also use generics -- and can also _hide_ generic type
    // parameters if they are, for example, known to be fixed in a given module.
    type Result<T> = std::result::Result<T, std::io::Error>;

    // Rust has a crazy type called the never type. The never type is a type
    // that has no values. One common use case for the never type is to declare
    // functions that never return. Such functions are called "diverging functions".
    fn forever() -> ! {
        loop {}
    }

    pub enum MyOption<T> {
        MyNone,
        MySome(T),
    }

    // A similar sort of thing happens in the definition of Option::unwrap.
    impl<T> MyOption<T> {
        pub fn unwrap(self) -> T {
            match self {
                MyOption::MySome(val) => val,
                // In this situation, panic! returns the never type !, so the
                // return type of the unwrap function is coerced into T.
                MyOption::MyNone => panic!("option::unwrap called on None"),
            }
        }
    }

    // Why is never useful, consider this code:
    let num: std::result::Result<u32, i32> = Ok(1);
    loop {
        // We're allowed to do this. What does continue return? What is the
        // type of g. In a typically situation, having a match statement with
        // heterogenous return types is not allowed because the assigned variable
        // would have to have multiple types for that to work.
        //
        // In this case, `continue` returns the never type. Since the never type,
        // it is _coerced_ to the type of the other branch. This is needed to pass
        // type checker, but of course g will never be assigned in the Err case.
        let g = match num {
            Ok(n) => n,
            Err(_) => continue,
        };
    }

    // In Rust, every value of a type must use the same amount of memory,
    // and Rust also needs to know how much memory to allocate for a particular
    // type.

    // However, there are cases where we jdon't know the size of a type value!
    // Consider strs. We can't know the size of a str before we create one at
    // runtime. That's why &str actually holds a pointer to a str and the size
    // of that str.

    // In this case, `str` is known as a dynamically sized type. In general, you
    // have to use DSTs behind references that store size metadata. For instance,
    // oyu can use Box<str>, or Rc<str>.

    // Traits are DSTs! Recall the usage of `dyn T` for some trait T required us
    // to put that trait behind a reference: Box<dyn T> for example. We can only
    // know the size of the implementing object at runtime.

    // Rust has a marker trait called `Sized` which is used to describe types
    // which have a known size at compile time. A `Sized` trait bound is implicitly
    // injected into all generic parameters -- meaning that generic functions will
    // only work on sized types by default. This restriction can be lifted:
    fn generic<T: ?Sized>(t: &T) {}
}
