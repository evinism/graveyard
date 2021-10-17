
// Problem: I want to index by an object!
// ---
class Animal {}

const cat = new Animal();
const dog = new Animal();

const talk = {
  [cat]: 'meow',
  [dog]: 'woof'
};

console.log(talk[dog]); // "woof"
console.log(talk[cat]); // "woof", oh no!!!!

// Solution: Indexable base class!
// ---
class Indexable {
  constructor(){
    this._indexer = Symbol();
  }
  [Symbol.toPrimitive](){ return this._indexer }
}

class Animal extends Indexable {}

const cat = new Animal();
const dog = new Animal();

const talk = {
  [cat]: 'meow',
  [dog]: 'woof'
};

console.log(talk[cat]); // "meow", hooray!!!
console.log(talk[dog]); // "woof"


// Problem: I inherently rely on more in my closure than I want
// ---

// Solution: Nuke the closure
// ---

function only(definedItems){
  return new Proxy({}, {
    has(_, prop){
      return !definedItems.includes(prop);
    },
    get(_, prop){
      throw new Error("Referenced variable " + prop.toString() + " out of scope!");
    },
    set(_, prop){
      throw new Error("Referenced variable " + prop.toString() + " out of scope!");
    }
  });
}

const a = 'available';
const b = 'unavailable';

with (only['a']) {
  console.log(a); // "available"
  console.log(b); // Throws!!
}

// Problem: I hate quotation marks
// --- 

// Solution: Pollute the closure 
// ---

const bareStrings = new Proxy({}, {
  has(_, prop){
    return window[prop] === undefined;
  },
  get(_, prop){
    return prop.toString();
  }
});

with(bareStrings){
  console.log(Hello); // Logs "Hello"!
}

// Problem: People keep commenting out my code!

function uncomment(fn){
  const body = fn.toString().replace(/\/\//, '');
  return new Function(`return (${body})(...arguments)`);
}

function fn(a){
  // return a + 1;
  return a
}

fn(1) // returns 1
uncomment(fn)(1) // returns 2!

